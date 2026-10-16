import * as blake from 'blakejs';

/**
 * blake2b-256 over the exact bytes, hex — the hash the on-chain anchor
 * carries. A string is hashed as its UTF-8 bytes, which is what the legacy
 * metadata validator did too, so hashes agree across the two services.
 */
export function blake2b256Hex(content: string | Uint8Array): string {
  const bytes =
    typeof content === 'string' ? Buffer.from(content, 'utf8') : content;
  return blake.blake2bHex(bytes, undefined, 32);
}
