import * as blake from 'blakejs';

import type { Anchor } from '@govtool/data-providers/metadata';

/**
 * `MetadataId` as the contract defines it:
 * `blake2b-256( utf8(url) ‖ 0x23 ‖ bytes(dataHash) )`, hex.
 *
 * Byte-for-byte the construction `@govtool/provider-dbsync` uses, so the same
 * anchor gets the same id whichever provider produced it — which is the only
 * reason the id is specified at all.
 */
export function computeMetadataId(anchor: Anchor): string {
  const hash =
    /^[0-9a-fA-F]*$/.test(anchor.dataHash) && anchor.dataHash.length % 2 === 0
      ? Buffer.from(anchor.dataHash, 'hex')
      : Buffer.from(anchor.dataHash, 'utf8');

  return blake.blake2bHex(
    Buffer.concat([Buffer.from(anchor.url, 'utf8'), Buffer.from([0x23]), hash]),
    undefined,
    32,
  );
}
