/**
 * Identifier encoding and normalisation.
 *
 * db-sync stores raw credential hashes and, for DReps, the pre-CIP-129 bech32
 * `view`. The contract's canonical ids are CIP-129, so this module encodes
 * them from the hash — and accepts all three forms (raw hex, CIP-105, CIP-129)
 * on the way in, because callers hold whichever their wallet gave them.
 */

import { bech32 } from 'bech32';

import { invalidInput } from './errors';
import { assertHexText, isHexText } from './hex';

/** bech32 has a 90-char limit by spec; Cardano ids exceed it, so every call raises it. */
const BECH32_LIMIT = 1023;

/** CIP-129 credential-type header bytes for DRep ids. */
const CIP129_DREP_KEY_HEADER = 0x22;
const CIP129_DREP_SCRIPT_HEADER = 0x23;

const CREDENTIAL_HASH_BYTES = 28;
const TX_HASH_BYTES = 32;

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
  const bytes = Buffer.concat([
    Buffer.from([header]),
    Buffer.from(hashHex, 'hex'),
  ]);
  return bech32.encode('drep', bech32.toWords(bytes), BECH32_LIMIT);
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
 * Any DRep identifier → the lowercase hex credential hash the SQL binds.
 * Accepts raw hex (what the legacy API takes), CIP-105 `drep1…` /
 * `drep_script1…` (28 bytes) and CIP-129 `drep1…` (29 bytes, header stripped).
 */
export function normalizeDRepId(id: string): string {
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

/**
 * Any stake identifier → the lowercase hex stake key hash the SQL binds.
 * Accepts raw hex and bech32 `stake…` / `stake_test…` (header byte stripped).
 */
export function normalizeStakeKey(id: string): string {
  if (isHexText(id)) {
    return id.toLowerCase();
  }
  const decoded = tryDecodeBech32(id);
  if (
    decoded &&
    (decoded.prefix === 'stake' || decoded.prefix === 'stake_test') &&
    decoded.bytes.length === CREDENTIAL_HASH_BYTES + 1
  ) {
    return decoded.bytes.subarray(1).toString('hex');
  }
  throw invalidInput('Not a valid stake key', { id });
}

export interface GovActionIdParts {
  txHash: string;
  index: number;
}

/**
 * Any governance action identifier → `{ txHash, index }`.
 * Accepts CIP-129 `gov_action1…` (33 bytes) and the `txHash#index` form.
 */
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

export function formatLegacyGovActionId(parts: GovActionIdParts): string {
  return `${parts.txHash}#${parts.index}`;
}
