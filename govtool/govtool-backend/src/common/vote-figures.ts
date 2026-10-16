import type {
  GovAction,
  VoteAggregate,
} from '@govtool/data-providers/chain-data';

import { compareIntegers, dbInteger, type ApiInteger } from './integer';

/**
 * One choice off an action's vote aggregate, as a whole number: lovelace for
 * the DRep and pool roles, a head count for the committee.
 *
 * `null` when the figure is not known, never 0, which would read as "nobody
 * voted": the provider serves no aggregates for the action (Blockfrost on a
 * concluded one), or the role's aggregate is a `percent`, which has no whole
 * number to give. A role left out of aggregates the provider does serve does
 * not vote on the action, so it has cast nothing: 0, as the Haskell backend
 * counted it.
 */
export function voteFigure(
  action: GovAction,
  role: VoteAggregate['role'],
  choice: 'yes' | 'no' | 'abstain',
): ApiInteger | null {
  if (action.voteAggregates === undefined) return null;
  const aggregate = action.voteAggregates.find((a) => a.role === role);
  if (aggregate === undefined) return 0;
  if (aggregate.representation === 'percent') return null;
  return dbInteger(aggregate[choice]);
}

/** Descending, with an unknown figure after every known one. */
export function compareFiguresDescending(
  a: ApiInteger | null,
  b: ApiInteger | null,
): number {
  if (a === null || b === null) {
    return a === b ? 0 : a === null ? 1 : -1;
  }
  return compareIntegers(b, a);
}
