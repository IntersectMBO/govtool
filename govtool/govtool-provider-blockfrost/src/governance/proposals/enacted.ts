/**
 * The enacted head of a lineage (SPEC.md §5.2), for `getEnacted` and the
 * constitution.
 *
 * Keyed by PURPOSE: UpdateCommittee and NoConfidence share `committee`. Only
 * the index's own `governance_type` narrows the reads, so a lineage costs one
 * record per action of its types (a few dozen on mainnet), not all of them.
 * Enacted actions are ordered by their predecessor links, never by epoch —
 * several can be enacted at one boundary.
 */
import type { GovActionLineage, GovActionRef, NetworkId } from '@govtool/data-providers/chain-data';

import type { Session } from '../../context';
import { orderLineage } from '../lineage';
import { LINEAGE_TYPES } from './body';
import { decodeRecord, loadIndex, requireRecord } from './records';

export async function enactedHead(s: Session, lineage: GovActionLineage, network: NetworkId): Promise<GovActionRef | null> {
  const types = LINEAGE_TYPES[lineage];
  const index = await loadIndex(s);
  const records = await Promise.all(index.filter((e) => types.includes(e.type)).map((e) => requireRecord(s, e)));
  const enacted = records
    .filter((r) => r.enacted !== null)
    .map((r) => ({ id: r.id, prevId: decodeRecord(r, network).previous?.id ?? null, ref: { id: r.id, txHash: r.txHash, index: r.index } }));
  const ordered = orderLineage(lineage, enacted);
  return ordered[ordered.length - 1]?.ref ?? null;
}
