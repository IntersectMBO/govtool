/**
 * Chain Data API — `/governance/votes/*`
 *
 * Votes are their own resource rather than a field of a proposal or a DRep,
 * because every surface reaches them from a different direction: a proposal's
 * "who voted", a DRep's voting record, and a global feed. The two list routes
 * on `proposals` and `dreps` are filtered views of this same record.
 */

import type {
  Envelope,
  EpochStamp,
  PagedEnvelope,
  PageRequest,
  TxRef,
  VotingPower,
} from '../common';
import type { GovActionRef, VoteChoice, VoterRef, VoterRole } from '../refs';
import type { MetadataProjection, VoteRationaleBody } from '../../metadata';
import type { GovAction, GovActionType } from './proposals';

export interface VoteRecord {
  proposal: GovActionRef;
  voter: VoterRef;
  vote: VoteChoice;
  txRef: TxRef;
  /**
   * When the vote was cast. Optional: a provider that indexes votes per
   * proposal returns the voter, the choice and the vote's transaction, but
   * dating it means joining that transaction to its block — one extra read
   * per vote. `txRef` always identifies it.
   */
  at?: EpochStamp;
  /** Voting power applied when the vote was counted (`basis: "active"`). */
  votingPower: VotingPower | null;
  rationale: MetadataProjection<VoteRationaleBody> | null;
  /** False when a later vote by the same voter on the same action replaced it. */
  isCurrent: boolean;
}

/** Vote plus the full action — what a voter's "votes" tab renders. */
export interface VotedGovAction {
  vote: VoteRecord | null;
  proposal: GovAction;
}

export type VoteExpand = 'votingPower' | 'rationale' | 'proposal';

/**
 * Ordering for every vote listing — `votes.list`, `dreps.listVotes`,
 * `proposals.listVotes` and `pools.listVotes` all take it.
 *
 * Named rather than inline since 0.4.0, because a capability cannot be declared
 * for something the contract cannot express: "this provider lists votes
 * chronologically but cannot order them by the voter's power" needs
 * `votingPower` to be a requestable sort before a provider can refuse it.
 *
 * `votingPower` orders by the power applied to each vote when it was counted
 * (`VoteRecord.votingPower`, `basis: "active"`), descending. A provider that
 * does not record per-vote power refuses it — see `EnumSupport<VoteSort>` in
 * `../capabilities`.
 */
export type VoteSort = 'newest' | 'oldest' | 'votingPower';

export interface VoteListQuery extends PageRequest {
  expand?: VoteExpand[];
  proposalType?: GovActionType[];
  vote?: VoteChoice[];
  role?: VoterRole[];
  sort?: VoteSort;
  search?: string;
  /** Include votes later superseded by a re-vote. Default false. */
  includeSuperseded?: boolean;
}

export interface VotesApi {
  /** `GET /governance/votes` — cross-cutting feed. */
  list(
    q?: VoteListQuery & { voterId?: string; proposalId?: string },
  ): Promise<PagedEnvelope<VoteRecord>>;

  /** `GET /governance/votes/{txHash}` — a single vote by its transaction. */
  get(txHash: string, q?: { index?: number }): Promise<Envelope<VoteRecord>>;
}
