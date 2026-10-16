/**
 * Chain Data API — votes.
 *
 * Votes are NEVER independently addressable. There is no cross-cutting feed and
 * no lookup of a vote by its own transaction hash: a vote is reachable only
 * through its DRep or its governance action (SPEC.md §5.2, §5.3).
 */

import type { Anchor, Bech32, EpochStamp, TxRef, VotingPower } from '../common';
import type { VoteCastBy, VoteChoice } from '../refs';
import type { GovActionType } from './proposals';

export type VoteSort = 'newest' | 'oldest';

/** One vote cast on one governance action. */
export interface VoteRecord {
  voter: VoteCastBy;
  choice: VoteChoice;
  /**
   * The CIP-100 rationale anchor. `null` = the voter attached none, which is
   * legal and common. The document itself is the metadata service's business.
   */
  anchor: Anchor | null;
  txRef: TxRef;
  at?: EpochStamp;
  /** The weight this vote carried, where the provider computes it. */
  votingPower?: VotingPower;
}

/**
 * The action a DRep's vote listing row is about.
 *
 * `id` and `type` are required; `title` is the ONE documented denormalization
 * of resolved metadata into chain data, present so a listing renders without a
 * metadata lookup per row. Nothing else from the document travels.
 */
export interface VotedActionSummary {
  id: Bech32;
  type: GovActionType;
  title?: string;
}

/**
 * A row in a DRep's voting activity.
 *
 * The listing covers actions the DRep voted on AND actions it did not, with a
 * filter between them — so the participation stat's denominator is the length
 * of the unfiltered listing and the two cannot disagree.
 *
 * A not-voted row carries no choice and no anchor: those fields are absent
 * rather than null-filled.
 */
export type DRepVoteRow =
  | {
      voted: true;
      action: VotedActionSummary;
      choice: VoteChoice;
      anchor: Anchor | null;
      txRef: TxRef;
      at?: EpochStamp;
    }
  | {
      voted: false;
      action: VotedActionSummary;
    };

export interface DRepVoteListQuery {
  /** Omitted = both. */
  voted?: boolean;
  sort?: VoteSort;
}
