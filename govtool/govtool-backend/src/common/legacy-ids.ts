import { BadRequestException } from '@nestjs/common';
import { bech32 } from 'bech32';

/**
 * Legacy identifier forms <-> the one form per entity the provider contract
 * accepts (SPEC.md §3.1).
 *
 * The legacy REST surface took, and still takes from the frontend, db-sync's
 * renderings: a raw 56-hex DRep credential hash, a CIP-105 `drep1…` view, a
 * `txHash#index` action id, a 58-hex reward address. A conforming provider
 * rejects every one of those, so they are translated here, in the backend,
 * before any provider call — and translated back on the way out where the
 * legacy response carried them.
 *
 * Everything is DECODED and validated, never string-matched: CIP-105 and
 * CIP-129 DRep ids share the `drep1` prefix and differ only in a header byte.
 * Anything that does not decode to an expected form is a 400 and never reaches
 * a provider.
 */

/** Cardano ids exceed bech32's 90-character default. */
const BECH32_LIMIT = 1023;
const HASH_BYTES = 28;
const TX_HASH_BYTES = 32;

/** CIP-129 DRep header: key type 0x2 in the high nibble; 0x2 key hash, 0x3 script hash. */
const DREP_KEY_HEADER = 0x22;
const DREP_SCRIPT_HEADER = 0x23;

const HEX_HASH = /^[0-9a-fA-F]{56}$/;
const HEX_REWARD_ADDRESS = /^[0-9a-fA-F]{58}$/;
const LEGACY_ACTION_ID = /^([0-9a-fA-F]{64})#(\d{1,3})$/;

export interface DRepCredential {
  /** Lowercase hex of the 28-byte credential hash. */
  hash: string;
  isScript: boolean;
}

function invalid(message: string): BadRequestException {
  return new BadRequestException({ errorType: 'ValidationError', message });
}

function decodeBech32(
  value: string,
): { prefix: string; bytes: Buffer } | undefined {
  // Mixed case is invalid bech32, and the library throws on it; the length
  // cap keeps a hostile value from being decoded at all.
  if (value.length > BECH32_LIMIT) {
    return undefined;
  }
  try {
    const decoded = bech32.decode(value, BECH32_LIMIT);
    return {
      prefix: decoded.prefix,
      bytes: Buffer.from(bech32.fromWords(decoded.words)),
    };
  } catch {
    return undefined;
  }
}

function encodeBech32(prefix: string, bytes: Buffer): string {
  return bech32.encode(prefix, bech32.toWords(bytes), BECH32_LIMIT);
}

/* ------------------------------------------------------------------------- */
/* DReps                                                                      */
/* ------------------------------------------------------------------------- */

export function encodeCip129DRepId(credential: DRepCredential): string {
  const header = credential.isScript ? DREP_SCRIPT_HEADER : DREP_KEY_HEADER;
  return encodeBech32(
    'drep',
    Buffer.concat([Buffer.from([header]), Buffer.from(credential.hash, 'hex')]),
  );
}

/** A CIP-129 DRep id's credential, or `undefined` if it is not one. */
export function decodeCip129DRepId(id: string): DRepCredential | undefined {
  const decoded = decodeBech32(id);
  if (
    decoded === undefined ||
    decoded.prefix !== 'drep' ||
    decoded.bytes.length !== HASH_BYTES + 1
  ) {
    return undefined;
  }
  const header = decoded.bytes[0];
  if (header !== DREP_KEY_HEADER && header !== DREP_SCRIPT_HEADER) {
    return undefined;
  }
  return {
    hash: decoded.bytes.subarray(1).toString('hex'),
    isScript: header === DREP_SCRIPT_HEADER,
  };
}

/**
 * The CIP-129 ids a legacy DRep identifier may denote, most likely first.
 *
 *  - CIP-129 `drep1…` (29 bytes, valid header): itself.
 *  - CIP-105 `drep1…` (28 bytes): the key-hash id; `drep_script1…`: the
 *    script-hash id.
 *  - 56-hex credential hash: BOTH, key first. A bare hash does not say whether
 *    it hashes a key or a script, and the ledger keys a DRep by the pair, so
 *    the caller tries the key id and falls back to the script id on
 *    NOT_FOUND. Key first because that is what the frontend sends (the hash of
 *    the wallet's CIP-95 DRep key) and what nearly every DRep is.
 *
 * `undefined` for anything else.
 */
export function tryLegacyDRepCandidates(input: string): string[] | undefined {
  const value = input;

  if (HEX_HASH.test(value)) {
    const hash = value.toLowerCase();
    return [
      encodeCip129DRepId({ hash, isScript: false }),
      encodeCip129DRepId({ hash, isScript: true }),
    ];
  }

  const decoded = decodeBech32(value);
  if (decoded === undefined) {
    return undefined;
  }

  if (decoded.prefix === 'drep' && decoded.bytes.length === HASH_BYTES + 1) {
    const credential = decodeCip129DRepId(value);
    // Re-encoded rather than passed through, so an all-uppercase (valid)
    // bech32 id reaches the provider in canonical lowercase.
    return credential === undefined
      ? undefined
      : [encodeCip129DRepId(credential)];
  }

  if (
    (decoded.prefix === 'drep' || decoded.prefix === 'drep_script') &&
    decoded.bytes.length === HASH_BYTES
  ) {
    return [
      encodeCip129DRepId({
        hash: decoded.bytes.toString('hex'),
        isScript: decoded.prefix === 'drep_script',
      }),
    ];
  }

  return undefined;
}

/** As `tryLegacyDRepCandidates`, but a 400 for anything unrecognised. */
export function legacyDRepCandidates(input: string): string[] {
  const candidates = tryLegacyDRepCandidates(input);
  if (candidates === undefined) {
    throw invalid(
      'Not a DRep id: expected a CIP-129 or CIP-105 bech32 id, or a 56-hex credential hash',
    );
  }
  return candidates;
}

function credentialOf(cip129: string): DRepCredential {
  const credential = decodeCip129DRepId(cip129);
  if (credential === undefined) {
    // A provider handed back an id that is not CIP-129: a provider bug, not
    // bad input, so it is not dressed up as a 400.
    throw new Error(`Provider returned a non-CIP-129 DRep id: ${cip129}`);
  }
  return credential;
}

/** The legacy `hashRaw` / `drepId` / `drepHash`: the raw credential hash. */
export function drepIdToHex(cip129: string): string {
  return credentialOf(cip129).hash;
}

/**
 * The legacy `view` / `drepView`: CIP-105. db-sync rendered script DReps as
 * `drep1…` too, and the frontend re-prefixes those to `drep_script1…`
 * (fixViewForScriptBasedDRep); emitting `drep_script1…` directly is what that
 * correction produces, and the frontend leaves it alone.
 */
export function drepIdToCip105(cip129: string): string {
  const { hash, isScript } = credentialOf(cip129);
  return encodeBech32(
    isScript ? 'drep_script' : 'drep',
    Buffer.from(hash, 'hex'),
  );
}

/* ------------------------------------------------------------------------- */
/* Governance actions                                                         */
/* ------------------------------------------------------------------------- */

export interface GovActionRef {
  txHash: string;
  index: number;
}

export function encodeCip129GovActionId({
  txHash,
  index,
}: GovActionRef): string {
  return encodeBech32(
    'gov_action',
    Buffer.concat([Buffer.from(txHash, 'hex'), Buffer.from([index])]),
  );
}

function decodeCip129GovActionId(id: string): GovActionRef | undefined {
  const decoded = decodeBech32(id);
  if (
    decoded === undefined ||
    decoded.prefix !== 'gov_action' ||
    decoded.bytes.length !== TX_HASH_BYTES + 1
  ) {
    return undefined;
  }
  return {
    txHash: decoded.bytes.subarray(0, TX_HASH_BYTES).toString('hex'),
    index: decoded.bytes[TX_HASH_BYTES],
  };
}

/**
 * A governance action id in either form — CIP-129 `gov_action1…` or the
 * legacy `txHash#index` — as both its parts and its CIP-129 id. CIP-129 has
 * one byte for the index, so an index above 255 is not an action id any
 * provider can be asked about.
 */
export function tryLegacyGovActionId(
  input: string,
): (GovActionRef & { id: string }) | undefined {
  const legacy = LEGACY_ACTION_ID.exec(input);
  if (legacy !== null) {
    const index = Number(legacy[2]);
    if (index > 255) {
      return undefined;
    }
    const ref = { txHash: legacy[1].toLowerCase(), index };
    return { ...ref, id: encodeCip129GovActionId(ref) };
  }

  const ref = decodeCip129GovActionId(input);
  return ref === undefined
    ? undefined
    : { ...ref, id: encodeCip129GovActionId(ref) };
}

export function legacyGovActionId(
  input: string,
): GovActionRef & { id: string } {
  const parsed = tryLegacyGovActionId(input);
  if (parsed === undefined) {
    throw invalid(
      'Not a governance action id: expected a CIP-129 gov_action id or txHash#index (64 hex, index 0..255)',
    );
  }
  return parsed;
}

/* ------------------------------------------------------------------------- */
/* Stake addresses                                                            */
/* ------------------------------------------------------------------------- */

/** Reward-address header: 0xe_ key credential, 0xf_ script; low nibble network id. */
function stakePrefixFor(header: number): string | undefined {
  const kind = header & 0xf0;
  const network = header & 0x0f;
  if (kind !== 0xe0 && kind !== 0xf0) {
    return undefined;
  }
  if (network === 1) {
    return 'stake';
  }
  // Every testnet (preview, preprod, sanchonet) uses network id 0.
  return network === 0 ? 'stake_test' : undefined;
}

/** Reward-address header for a KEY-hash credential: 0xe0 | network id. */
const KEY_REWARD_HEADER = 0xe0;

/**
 * The ledger network id a network name denotes: 1 for mainnet, 0 for every
 * testnet (preview, preprod, sanchonet, a private testnet). Cardano defines
 * only those two values in addresses.
 */
export function networkIdOf(network: string): 0 | 1 {
  return network === 'mainnet' ? 1 : 0;
}

/** True for a bare 28-byte credential hash, the one form that needs a network. */
export function isBareStakeKeyHash(input: string): boolean {
  return HEX_HASH.test(input);
}

/**
 * A stake address in the contract's `stake1…` / `stake_test1…` form.
 *
 *  - bech32 `stake1…` / `stake_test1…`: itself, after checking the header
 *    agrees with the prefix.
 *  - 58-hex reward address (header byte + 28-byte credential): what the
 *    frontend sends (wallet context, `RewardAddress…to_hex()`) and what the
 *    legacy API matched against db-sync's `stake_address.hash_raw`. The header
 *    carries both the credential type and the network, so the conversion is
 *    exact.
 *  - 56-hex stake KEY hash: the legacy API accepted it too (the integration
 *    suite sends it). It names neither the network nor the credential type,
 *    so it is read the way the legacy API matched it: as a key-hash reward
 *    address on the network the backend serves. `network` is that network;
 *    without it the bare hash is refused, because converting it would be a
 *    guess.
 */
export function legacyStakeAddress(input: string, network?: string): string {
  const value = input;

  if (HEX_HASH.test(value)) {
    if (network !== undefined) {
      const header = KEY_REWARD_HEADER | networkIdOf(network);
      const bytes = Buffer.concat([
        Buffer.from([header]),
        Buffer.from(value, 'hex'),
      ]);
      return encodeBech32(stakePrefixFor(header)!, bytes);
    }
  } else if (HEX_REWARD_ADDRESS.test(value)) {
    const bytes = Buffer.from(value, 'hex');
    const prefix = stakePrefixFor(bytes[0]);
    if (prefix !== undefined) {
      return encodeBech32(prefix, bytes);
    }
  } else {
    const decoded = decodeBech32(value);
    if (
      decoded !== undefined &&
      decoded.bytes.length === HASH_BYTES + 1 &&
      stakePrefixFor(decoded.bytes[0]) === decoded.prefix
    ) {
      return encodeBech32(decoded.prefix, decoded.bytes);
    }
  }

  throw invalid(
    'Not a stake address: expected stake1…/stake_test1…, a 58-hex reward address or a 56-hex stake key hash',
  );
}

/* ------------------------------------------------------------------------- */
/* Fallback across candidates                                                 */
/* ------------------------------------------------------------------------- */

function isNotFound(error: unknown): boolean {
  return (
    typeof error === 'object' &&
    error !== null &&
    (error as { code?: unknown }).code === 'NOT_FOUND'
  );
}

/**
 * Runs `read` against each candidate id in turn, moving on only on NOT_FOUND;
 * any other failure is final. The last NOT_FOUND is rethrown if none match.
 */
export async function firstFound<T>(
  candidates: readonly string[],
  read: (id: string) => Promise<T>,
): Promise<T> {
  if (candidates.length === 0) {
    throw new Error('firstFound needs at least one candidate id');
  }
  let lastNotFound: unknown;
  for (const id of candidates) {
    try {
      return await read(id);
    } catch (error) {
      if (!isNotFound(error)) {
        throw error;
      }
      lastNotFound = error;
    }
  }
  throw lastNotFound;
}
