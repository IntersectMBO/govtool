import { createHash } from 'node:crypto';

const BASE32 = 'abcdefghijklmnopqrstuvwxyz234567';

/** RFC 4648 base32, lowercase, no padding: the multibase `b` alphabet. */
function base32(bytes: Uint8Array): string {
  let out = '';
  let bits = 0;
  let value = 0;
  for (const byte of bytes) {
    value = (value << 8) | byte;
    bits += 8;
    while (bits >= 5) {
      out += BASE32[(value >>> (bits - 5)) & 31];
      bits -= 5;
    }
  }
  if (bits > 0) out += BASE32[(value << (5 - bits)) & 31];
  return out;
}

/**
 * Content that fits in one IPFS block is stored as a single `raw` block, and
 * its CID is a function of the bytes alone. 256 KiB is the default chunk size.
 */
export const SINGLE_BLOCK_MAX_BYTES = 256 * 1024;

/**
 * The CIDv1 of `data` as a single `raw` block with a sha2-256 multihash, as
 * `bafkrei…`: what IPFS assigns content up to one block with raw leaves.
 */
export function rawBlockCid(data: Uint8Array): string {
  const digest = createHash('sha256').update(data).digest();
  // CIDv1 (0x01), raw codec (0x55), sha2-256 (0x12), 32-byte digest (0x20).
  const bytes = Buffer.concat([Buffer.from([0x01, 0x55, 0x12, 0x20]), digest]);
  return `b${base32(bytes)}`;
}
