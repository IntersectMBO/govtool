/**
 * Identifier translation between Blockfrost and the contract.
 *
 * Blockfrost speaks CIP-129 for DRep ids: `drep_id` is the bech32 form and
 * `hex` is the credential **with** its one-byte credential-type header (0x22
 * key, 0x23 script), so 29 bytes. The contract's `VoterRef.hash` is the bare
 * 28-byte credential, and `cip105Id` is the older header-less bech32 that the
 * legacy GovTool API returns — so both are derived here.
 *
 * Governance actions are `txHash` + `certIndex` in Blockfrost and CIP-129
 * `gov_action1…` in the contract, so those are converted in both directions.
 */

import { bech32 } from 'bech32';

import { invalidInput } from './errors';

/** bech32's 90-character default is far below Cardano's ids. */
const BECH32_LIMIT = 1023;

const CIP129_DREP_KEY_HEADER = 0x22;
const CIP129_DREP_SCRIPT_HEADER = 0x23;
const CREDENTIAL_HASH_BYTES = 28;
const TX_HASH_BYTES = 32;

const HEX = /^[0-9a-fA-F]+$/;

function isHex(value: string): boolean {
  return HEX.test(value) && value.length % 2 === 0;
}

function tryDecode(value: string): { prefix: string; bytes: Buffer } | null {
  try {
    const decoded = bech32.decode(value, BECH32_LIMIT);
    return {
      prefix: decoded.prefix,
      bytes: Buffer.from(bech32.fromWords(decoded.words)),
    };
  } catch {
    return null;
  }
}

/** Blockfrost's `hex` (29 bytes, header included) → the bare credential hash. */
export function stripCredentialHeader(hexWithHeader: string): string {
  const bytes = Buffer.from(hexWithHeader, 'hex');
  if (bytes.length === CREDENTIAL_HASH_BYTES + 1) {
    return bytes.subarray(1).toString('hex');
  }
  // Already header-less, or something unexpected: return it unchanged rather
  // than truncating a hash we do not recognise.
  return hexWithHeader.toLowerCase();
}

/** The pre-CIP-129 bech32 the legacy GovTool API exposes as `view`. */
export function encodeCip105DRepId(
  credentialHex: string,
  isScript: boolean,
): string {
  return bech32.encode(
    isScript ? 'drep_script' : 'drep',
    bech32.toWords(Buffer.from(credentialHex, 'hex')),
    BECH32_LIMIT,
  );
}

export function encodeCip129DRepId(
  credentialHex: string,
  isScript: boolean,
): string {
  const header = isScript ? CIP129_DREP_SCRIPT_HEADER : CIP129_DREP_KEY_HEADER;
  return bech32.encode(
    'drep',
    bech32.toWords(
      Buffer.concat([Buffer.from([header]), Buffer.from(credentialHex, 'hex')]),
    ),
    BECH32_LIMIT,
  );
}

/**
 * Any DRep identifier the caller might hold → the CIP-129 bech32 that
 * Blockfrost's path parameters expect.
 *
 * Accepts CIP-129 (passed through), CIP-105 `drep1…`/`drep_script1…`, and a
 * raw hex credential with or without the header byte. A bare hash cannot say
 * whether it is script-based, so `drep` is assumed — the same assumption the
 * legacy API's hex ids carry.
 */
export function toBlockfrostDRepId(id: string): string {
  if (isHex(id)) {
    const bytes = Buffer.from(id, 'hex');
    if (bytes.length === CREDENTIAL_HASH_BYTES + 1) {
      const header = bytes[0];
      return encodeCip129DRepId(
        bytes.subarray(1).toString('hex'),
        header === CIP129_DREP_SCRIPT_HEADER,
      );
    }
    if (bytes.length === CREDENTIAL_HASH_BYTES) {
      return encodeCip129DRepId(id.toLowerCase(), false);
    }
    throw invalidInput('Not a valid DRep id', { id });
  }

  const decoded = tryDecode(id);
  if (decoded === null) {
    throw invalidInput('Not a valid DRep id', { id });
  }
  if (
    decoded.prefix === 'drep' &&
    decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1
  ) {
    return id; // already CIP-129
  }
  if (
    decoded.prefix === 'drep' &&
    decoded.bytes.length === CREDENTIAL_HASH_BYTES
  ) {
    return encodeCip129DRepId(decoded.bytes.toString('hex'), false);
  }
  if (
    decoded.prefix === 'drep_script' &&
    decoded.bytes.length === CREDENTIAL_HASH_BYTES
  ) {
    return encodeCip129DRepId(decoded.bytes.toString('hex'), true);
  }
  throw invalidInput('Not a valid DRep id', { id });
}

export function encodeCip129GovActionId(
  txHashHex: string,
  index: number,
): string {
  return bech32.encode(
    'gov_action',
    bech32.toWords(
      Buffer.concat([Buffer.from(txHashHex, 'hex'), Buffer.from([index])]),
    ),
    BECH32_LIMIT,
  );
}

export interface GovActionIdParts {
  txHash: string;
  index: number;
}

/** CIP-129 `gov_action1…` or the `txHash#index` form → its parts. */
export function parseGovActionId(id: string): GovActionIdParts {
  const decoded = tryDecode(id);
  if (decoded !== null && decoded.prefix === 'gov_action') {
    if (decoded.bytes.length !== TX_HASH_BYTES + 1) {
      throw invalidInput('Not a valid governance action id', { id });
    }
    return {
      txHash: decoded.bytes.subarray(0, TX_HASH_BYTES).toString('hex'),
      index: decoded.bytes[TX_HASH_BYTES] as number,
    };
  }

  const [txHash, rawIndex, ...rest] = id.split('#');
  if (!txHash || rawIndex === undefined || rawIndex === '' || rest.length > 0) {
    throw invalidInput('Not a valid governance action id', { id });
  }
  if (!isHex(txHash) || txHash.length !== TX_HASH_BYTES * 2) {
    throw invalidInput('Not a valid governance action id', { id });
  }
  const index = Number(rawIndex);
  if (!Number.isInteger(index) || index < 0) {
    throw invalidInput('Not a valid governance action id', { id });
  }
  return { txHash: txHash.toLowerCase(), index };
}

/** A stake identifier → the bech32 `stake…` Blockfrost's paths expect. */
export function toBlockfrostStakeAddress(id: string): string {
  const decoded = tryDecode(id);
  if (
    decoded !== null &&
    (decoded.prefix === 'stake' || decoded.prefix === 'stake_test')
  ) {
    return id;
  }
  // A bare hash cannot be turned into a stake address without knowing the
  // network id, and Blockfrost only accepts the bech32 form.
  throw invalidInput(
    'Blockfrost requires a bech32 stake address (stake1…/stake_test1…)',
    { id },
  );
}

/** The bech32 stake address → its hex credential hash, when derivable. */
export function stakeAddressToHash(stakeAddress: string): string | undefined {
  const decoded = tryDecode(stakeAddress);
  if (decoded === null) return undefined;
  // A stake address is a 1-byte header plus the 28-byte credential.
  if (decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
    return decoded.bytes.subarray(1).toString('hex');
  }
  return undefined;
}

/** Stake addresses whose header marks a script credential (type 0xe*/ /* vs 0xf*). */
export function isScriptStakeAddress(stakeAddress: string): boolean {
  const decoded = tryDecode(stakeAddress);
  if (decoded === null || decoded.bytes.length === 0) return false;
  // Shelley address header: bits 4-7 are the type; stake-script types are
  // 0b1111 (0xf0) and stake-key types 0b1110 (0xe0).
  return ((decoded.bytes[0] as number) & 0xf0) === 0xf0;
}
