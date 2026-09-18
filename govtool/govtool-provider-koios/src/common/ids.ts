/**
 * Identifier encoding and normalisation.
 *
 * Koios already speaks CIP-129 on the way out — `drep_id`, `cc_hot_id`,
 * `cc_cold_id` and `proposal_id` all come back in the contract's canonical
 * form, which is the one real advantage it has over db-sync here. The work
 * left is inbound: callers hold raw hex or a CIP-105 `drep1…` from an older
 * wallet, and Koios accepts neither, so everything is normalised to CIP-129
 * before it reaches a query string.
 */

import { bech32 } from 'bech32';

import { invalidInput } from './errors';
import { assertHexText, isHexText } from './hex';

/** bech32 has a 90-char limit by spec; Cardano ids exceed it, so every call raises it. */
const BECH32_LIMIT = 1023;

/** CIP-129 credential-type header bytes. */
const CIP129_DREP_KEY_HEADER = 0x22;
const CIP129_DREP_SCRIPT_HEADER = 0x23;
const CIP129_CC_HOT_KEY_HEADER = 0x02;
const CIP129_CC_HOT_SCRIPT_HEADER = 0x03;

const CREDENTIAL_HASH_BYTES = 28;
const TX_HASH_BYTES = 32;

/**
 * The two predefined delegation targets, spelled as Koios spells them.
 * They are accepted by `/drep_info` but are not credentials, so they must
 * never reach the bech32 path.
 */
export const KOIOS_ALWAYS_ABSTAIN = 'drep_always_abstain';
export const KOIOS_ALWAYS_NO_CONFIDENCE = 'drep_always_no_confidence';

export function isPredefinedDRep(id: string): boolean {
  return id === KOIOS_ALWAYS_ABSTAIN || id === KOIOS_ALWAYS_NO_CONFIDENCE;
}

function tryDecodeBech32(
  value: string,
): { prefix: string; bytes: Buffer } | null {
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

export function encodeCip129DRepId(hashHex: string, isScript: boolean): string {
  const header = isScript ? CIP129_DREP_SCRIPT_HEADER : CIP129_DREP_KEY_HEADER;
  return encodeWithHeader('drep', header, hashHex);
}

export function encodeCip129CcHotId(
  hashHex: string,
  isScript: boolean,
): string {
  const header = isScript
    ? CIP129_CC_HOT_SCRIPT_HEADER
    : CIP129_CC_HOT_KEY_HEADER;
  return encodeWithHeader('cc_hot', header, hashHex);
}

/** Pre-CIP-129 form: the bare hash, no credential-type header. */
export function encodeCip105DRepId(hashHex: string, isScript: boolean): string {
  return bech32.encode(
    isScript ? 'drep_script' : 'drep',
    bech32.toWords(Buffer.from(hashHex, 'hex')),
    BECH32_LIMIT,
  );
}

function encodeWithHeader(
  prefix: string,
  header: number,
  hashHex: string,
): string {
  const bytes = Buffer.concat([
    Buffer.from([header]),
    Buffer.from(hashHex, 'hex'),
  ]);
  return bech32.encode(prefix, bech32.toWords(bytes), BECH32_LIMIT);
}

export function encodeCip129GovActionId(
  txHashHex: string,
  index: number,
): string {
  const bytes = Buffer.concat([
    Buffer.from(txHashHex, 'hex'),
    Buffer.from([index]),
  ]);
  return bech32.encode('gov_action', bech32.toWords(bytes), BECH32_LIMIT);
}

/**
 * The raw credential hash inside any bech32 Cardano identifier, lowercase hex.
 *
 * Works for every voter form Koios returns, not just DReps: a `pool1…` is a
 * bare 28-byte key hash, while `drep1…` / `cc_hot1…` / `cc_cold1…` in CIP-129
 * carry a one-byte credential-type header in front of the same 28 bytes.
 * Returns `''` for anything it cannot decode, because this is used on read
 * paths where a single odd identifier must not fail the whole page.
 */
export function bech32CredentialHash(id: string): string {
  if (isHexText(id)) {
    return id.toLowerCase();
  }
  const decoded = tryDecodeBech32(id);
  if (decoded === null) {
    return '';
  }
  if (decoded.bytes.length === CREDENTIAL_HASH_BYTES) {
    return decoded.bytes.toString('hex');
  }
  if (decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
    return decoded.bytes.subarray(1).toString('hex');
  }
  return '';
}

/**
 * Whether a bech32 credential carries a script header.
 *
 * CIP-129 uses the odd value of each credential-type pair for scripts: DRep
 * 0x22/0x23, committee hot 0x02/0x03, committee cold 0x12/0x13. A 28-byte
 * identifier has no header, so script-ness is carried by the prefix instead.
 */
export function isScriptCredential(id: string): boolean {
  const decoded = tryDecodeBech32(id);
  if (decoded === null) {
    return false;
  }
  if (decoded.prefix.endsWith('_script')) {
    return true;
  }
  if (decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
    // Every CIP-129 credential-type header is odd for a script.
    return ((decoded.bytes[0] as number) & 0x01) === 1;
  }
  return false;
}

/** Decodes any DRep identifier to its raw credential hash, lowercase hex. */
export function toDRepHash(id: string): string {
  if (isHexText(id)) {
    return id.toLowerCase();
  }
  const decoded = tryDecodeBech32(id);
  if (
    decoded &&
    (decoded.prefix === 'drep' || decoded.prefix === 'drep_script')
  ) {
    if (decoded.bytes.length === CREDENTIAL_HASH_BYTES) {
      return decoded.bytes.toString('hex');
    }
    if (decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
      return decoded.bytes.subarray(1).toString('hex');
    }
  }
  throw invalidInput('Not a valid DRep id', { id });
}

/** True when a CIP-129 DRep id carries the script header. */
export function isScriptDRepId(id: string): boolean {
  const decoded = tryDecodeBech32(id);
  if (decoded?.prefix === 'drep_script') {
    return true;
  }
  if (decoded && decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
    return decoded.bytes[0] === CIP129_DREP_SCRIPT_HEADER;
  }
  return false;
}

/**
 * Any DRep identifier → the CIP-129 `drep1…` Koios binds to `_drep_id`.
 *
 * A raw hash carries no credential type, so it is assumed to be key-based —
 * the same assumption every CIP-105 encoder made, and wrong only for script
 * DReps, which a caller should identify by bech32 rather than by hash.
 */
export function normalizeDRepId(id: string): string {
  if (isPredefinedDRep(id)) {
    return id;
  }
  if (isHexText(id)) {
    return encodeCip129DRepId(id.toLowerCase(), false);
  }
  const decoded = tryDecodeBech32(id);
  if (decoded?.prefix === 'drep' || decoded?.prefix === 'drep_script') {
    if (decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1) {
      return id;
    }
    return encodeCip129DRepId(
      decoded.bytes.toString('hex'),
      decoded.prefix === 'drep_script',
    );
  }
  throw invalidInput('Not a valid DRep id', { id });
}

/**
 * Any stake identifier → the bech32 `stake…` Koios binds. Koios takes only
 * bech32 here, so a raw hash cannot be normalised without knowing the network
 * discriminant, and is rejected rather than guessed at.
 */
export function normalizeStakeAddress(id: string): string {
  const decoded = tryDecodeBech32(id);
  if (
    decoded &&
    (decoded.prefix === 'stake' || decoded.prefix === 'stake_test') &&
    decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1
  ) {
    return id;
  }
  throw invalidInput(
    'Not a valid stake address; Koios requires the bech32 form',
    { id },
  );
}

/** The raw stake key hash inside a bech32 stake address, lowercase hex. */
export function stakeKeyHash(stakeAddress: string): string {
  const decoded = tryDecodeBech32(stakeAddress);
  if (!decoded || decoded.bytes.length !== CREDENTIAL_HASH_BYTES + 1) {
    throw invalidInput('Not a valid stake address', { id: stakeAddress });
  }
  return decoded.bytes.subarray(1).toString('hex');
}

/** A stake address whose header nibble marks a script credential. */
export function isScriptStakeAddress(stakeAddress: string): boolean {
  const decoded = tryDecodeBech32(stakeAddress);
  if (!decoded || decoded.bytes.length !== CREDENTIAL_HASH_BYTES + 1) {
    return false;
  }
  return ((decoded.bytes[0] as number) & 0x10) !== 0;
}

export interface GovActionIdParts {
  txHash: string;
  index: number;
}

/** Any governance action identifier → `{ txHash, index }`. */
export function parseGovActionId(id: string): GovActionIdParts {
  const decoded = tryDecodeBech32(id);
  if (decoded && decoded.prefix === 'gov_action') {
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
  assertHexText(txHash);
  const index = Number(rawIndex);
  if (!Number.isInteger(index) || index < 0) {
    throw invalidInput('Not a valid governance action id', { id });
  }
  return { txHash: txHash.toLowerCase(), index };
}

/** Any governance action identifier → the CIP-129 id Koios binds. */
export function normalizeGovActionId(id: string): string {
  const { txHash, index } = parseGovActionId(id);
  return encodeCip129GovActionId(txHash, index);
}

export function formatLegacyGovActionId(parts: GovActionIdParts): string {
  return `${parts.txHash}#${parts.index}`;
}
