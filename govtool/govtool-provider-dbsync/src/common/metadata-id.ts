import * as blake from 'blakejs';

import type { Anchor } from '@govtool/data-providers/metadata';

/**
 * `MetadataId` as the contract defines it:
 * `blake2b-256( utf8(url) ‖ 0x23 ‖ bytes(dataHash) )`, hex.
 *
 * `bytes(dataHash)` decodes the hex. An anchor whose `dataHash` is not valid
 * hex can exist on chain (the ledger does not validate it), so those fall
 * back to the utf8 bytes of the string — still deterministic, still unique
 * per (url, hash) pair, and never a thrown error on a read path.
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
