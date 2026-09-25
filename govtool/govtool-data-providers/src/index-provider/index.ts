/**
 * Governance Index v1 — search. Optional (SPEC.md §7).
 *
 * Its own component because a text index is a different kind of thing from a
 * ledger read, and because the searchable text lives in RESOLVED documents —
 * so an index consumes chain data and the metadata service both.
 *
 * The three halves are independently implementable. A dedicated indexer
 * implements this and nothing else; a chain-data provider that also indexes
 * implements both types, which are unrelated. An implementer supplies what it
 * has.
 */

import type { PageRequest, PagedEnvelope } from '../chain-data/common';
import type {
  DRep,
  DRepStatus,
  DRepKind,
} from '../chain-data/governance/dreps';
import type {
  GovAction,
  GovActionStatus,
  GovActionType,
} from '../chain-data/governance/proposals';
import type { SpoVoter } from '../chain-data/governance/pools';

export interface DRepSearchQuery extends PageRequest {
  term: string;
  status?: DRepStatus[];
  kind?: DRepKind[];
}

export interface ProposalSearchQuery extends PageRequest {
  term: string;
  type?: GovActionType[];
  status?: GovActionStatus[];
}

export interface PoolSearchQuery extends PageRequest {
  term: string;
}

export interface DRepSearchApi {
  searchDReps(q: DRepSearchQuery): Promise<PagedEnvelope<DRep>>;
}

export interface ProposalSearchApi {
  searchProposals(q: ProposalSearchQuery): Promise<PagedEnvelope<GovAction>>;
}

/**
 * Pools are indexed so a pool's historical voting record can be found and
 * shown. Pairs with the optional pool vote history on chain data: that serves
 * one pool's votes, this makes pools findable in the first place.
 */
export interface PoolSearchApi {
  searchPools(q: PoolSearchQuery): Promise<PagedEnvelope<SpoVoter>>;
}

export interface GovernanceIndexV1 {
  dreps?: DRepSearchApi;
  proposals?: ProposalSearchApi;
  pools?: PoolSearchApi;
}
