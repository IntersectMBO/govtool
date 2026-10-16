'use strict';

const bech32 = require('./bech32');

const HASH_BYTES = 28;

/**
 * A reward address to db-sync's `stake_address.hash_raw` (header byte plus
 * 28-byte credential). `null` when the input is not a reward address.
 */
function parseStakeAddress(input) {
  const decoded = bech32.decode(input);
  if (!decoded) return null;
  if (decoded.prefix !== 'stake' && decoded.prefix !== 'stake_test') return null;
  if (decoded.bytes.length !== HASH_BYTES + 1) return null;
  const kind = decoded.bytes[0] & 0xf0;
  if (kind !== 0xe0 && kind !== 0xf0) return null;
  const mainnet = (decoded.bytes[0] & 0x0f) === 1;
  if (mainnet !== (decoded.prefix === 'stake')) return null;
  return { hashRaw: decoded.bytes, isScript: kind === 0xf0 };
}

/**
 * A DRep id to db-sync's `drep_hash` key (`raw`, `has_script`).
 *
 * Accepts CIP-129 (`drep1` over a header byte, 0x22 key / 0x23 script),
 * CIP-105 (`drep1` over a bare key hash, `drep_script1` over a script hash)
 * and a bare 56-hex key hash. `null` for anything else, including CIP-105
 * `drep_vk1`, which would need a blake2b-224 of the key.
 */
function parseDRepId(input) {
  if (typeof input !== 'string') return null;
  if (/^[0-9a-fA-F]{56}$/.test(input)) {
    return { raw: Buffer.from(input, 'hex'), hasScript: false };
  }
  const decoded = bech32.decode(input);
  if (!decoded) return null;
  const { prefix, bytes } = decoded;
  if (prefix === 'drep' && bytes.length === HASH_BYTES) {
    return { raw: bytes, hasScript: false };
  }
  if (prefix === 'drep_script' && bytes.length === HASH_BYTES) {
    return { raw: bytes, hasScript: true };
  }
  if (prefix === 'drep' && bytes.length === HASH_BYTES + 1) {
    if (bytes[0] === 0x22) return { raw: bytes.subarray(1), hasScript: false };
    if (bytes[0] === 0x23) return { raw: bytes.subarray(1), hasScript: true };
  }
  return null;
}

/** The CIP-129 form Blockfrost reports as `drep_id`. */
function cip129DRepId(raw, hasScript) {
  return bech32.encode('drep', Buffer.concat([Buffer.from([hasScript ? 0x23 : 0x22]), raw]));
}

module.exports = { parseStakeAddress, parseDRepId, cip129DRepId };
