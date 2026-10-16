/**
 * Ordering the enacted actions of one governance lineage.
 *
 * Several actions of one lineage can be enacted at the same epoch boundary, so
 * `enacted_epoch` alone does not order them. The ledger does: each enacted
 * action names the previous enacted head of its lineage, so the enacted actions
 * form one chain from the genesis state. Walking that chain gives the order,
 * and its last link is the enacted head that `getEnacted` would report.
 */
import { internal } from '../../errors';

export interface LineageLink {
  /** `gov_action_proposal.id` as text. */
  id: string;
  /** `prev_gov_action_proposal` as text; null at the start of the lineage. */
  prevId: string | null;
}

/**
 * Order enacted actions from the oldest to the enacted head. Every enacted
 * action must sit on the one chain: a fork or an orphan means the database
 * disagrees with the ledger rule, and answering from it would be a guess.
 */
export function orderLineage<T extends LineageLink>(lineage: string, enacted: readonly T[]): T[] {
  const byPrev = new Map<string, T[]>();
  for (const link of enacted) {
    const key = link.prevId ?? '';
    const siblings = byPrev.get(key);
    if (siblings) siblings.push(link);
    else byPrev.set(key, [link]);
  }
  const ordered: T[] = [];
  let prev = '';
  for (;;) {
    const next = byPrev.get(prev);
    if (!next) break;
    if (next.length > 1) {
      throw internal(`db-sync records two enacted ${lineage} actions with the same predecessor`);
    }
    const link = next[0]!;
    ordered.push(link);
    prev = link.id;
  }
  if (ordered.length !== enacted.length) {
    throw internal(`db-sync records enacted ${lineage} actions that are not on the enacted chain`);
  }
  return ordered;
}
