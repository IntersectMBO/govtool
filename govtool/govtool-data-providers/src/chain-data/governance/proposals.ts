/**
 * Chain Data API — `/governance/proposals/*`
 */

import type {
  Anchor,
  Bech32,
  EpochNo,
  EpochStamp,
  Envelope,
  Hex,
  Lovelace,
  PageRequest,
  PagedEnvelope,
  Ratio,
  TxRef,
} from '../common';
import type { GovActionLineage, GovActionRef, VoterRole } from '../refs';
import type { VoteAggregateRepresentation } from '../capabilities';
import type { VoteRecord } from './votes';
import type { ProtocolParams } from '../network';

export type GovActionType =
  | 'ParameterChange'
  | 'HardForkInitiation'
  | 'TreasuryWithdrawals'
  | 'NoConfidence'
  | 'UpdateCommittee'
  | 'NewConstitution'
  | 'InfoAction';

export type GovActionStatus =
  'live' | 'ratified' | 'enacted' | 'expired' | 'dropped';

export type GovActionSort =
  | 'newest'
  | 'oldest'
  | 'soonestToExpire'
  | 'mostYesVotes'
  | 'highestParticipation';

/**
 * What the action proposes, typed, discriminated by `type`.
 *
 * REQUIRED for all seven variants. A source that cannot decode an action's
 * description cannot serve proposals — there is no untyped fallback field.
 */
export type GovActionBody =
  | { type: 'InfoAction' }
  | { type: 'NoConfidence' }
  | {
      type: 'ParameterChange';
      changes: Partial<ProtocolParams>;
      guardrailsScriptHash?: Hex | null;
    }
  | {
      type: 'HardForkInitiation';
      protocolVersion: { major: number; minor: number };
    }
  | {
      type: 'TreasuryWithdrawals';
      withdrawals: { stakeAddress: Bech32; amount: Lovelace }[];
      totalAmount: Lovelace;
      guardrailsScriptHash?: Hex | null;
    }
  | {
      type: 'UpdateCommittee';
      added: { coldCredential: Bech32; termExpiryEpoch: EpochNo }[];
      removed: { coldCredential: Bech32 }[];
      quorum: Ratio;
    }
  | {
      type: 'NewConstitution';
      anchor: Anchor;
      guardrailsScriptHash?: Hex | null;
    };

export interface GovActionLifecycle {
  status: GovActionStatus;
  submitted: EpochStamp;
  submittedTx: TxRef;
  expires: EpochStamp | null;
  ratifiedAt: EpochStamp | null;
  enactedAt: EpochStamp | null;
  droppedAt: EpochStamp | null;
  expiredAt: EpochStamp | null;
}

/**
 * Per-role vote totals for one action.
 *
 * `representation` says how to read the figures and therefore how to render
 * them — a `count` rendered with an ada prefix is the failure this field
 * exists to prevent. Values are strings in every representation: lovelace for
 * `stake`, an integer for `count`, a 0..1 fraction for `percent`.
 *
 * Serving an aggregate means serving its denominator and threshold. The
 * denominator is the total AS IT STOOD FOR THIS ACTION, not the current total,
 * so a percentage stays reproducible after the fact.
 */
export interface VoteAggregate {
  role: VoterRole;
  representation: VoteAggregateRepresentation;
  yes: string;
  no: string;
  abstain: string;
  notVoted: string;
  totalEligible: string;
  threshold: Ratio;
  passing?: boolean;
}

export interface GovAction extends GovActionRef {
  type: GovActionType;
  body: GovActionBody;
  lifecycle: GovActionLifecycle;
  /** The CIP-108 anchor. The document is the metadata service's business. */
  anchor: Anchor | null;
  deposit: Lovelace | null;
  /** The reward account the deposit returns to. There is no separate proposer
   *  identity on chain — `proposal_procedure` carries only this. */
  depositReturnAddress: Bech32 | null;
  /** Previous action in the same LINEAGE, or null at the head of one. */
  previousAction: GovActionRef | null;
  /** Required whenever the provider serves aggregates at all. */
  voteAggregates?: VoteAggregate[];
  /** Optional; see SPEC.md §5.2. */
  protocolParamsAtSubmission?: ProtocolParams | null;
  protocolParamsAtEnactment?: ProtocolParams | null;
}

export interface ProposalListQuery extends PageRequest {
  type?: GovActionType[];
  /** Optional filter — a provider that does not support it REJECTS it. */
  status?: GovActionStatus[];
  sort?: GovActionSort;
  search?: string;
  /**
   * Annotate rows with this voter's vote. OPTIONAL on a listing — a provider
   * declares `proposals.voterContextOnList`. Declining it makes a consumer hide
   * its voted/not-voted filter rather than issue one request per row.
   */
  voterId?: string;
  /** With `voterId`: restrict to actions this voter has or has not voted on. */
  voted?: boolean;
}

export interface ProposalsApi {
  list(q: ProposalListQuery): Promise<PagedEnvelope<GovAction>>;

  get(
    id: string,
    q?: { voterId?: string },
  ): Promise<Envelope<GovAction & { myVote?: VoteRecord | null }>>;

  /**
   * The last ENACTED action in a lineage, for `prevGovActionId` when
   * constructing a governance transaction. Required — without it GovTool cannot
   * submit proposals at all.
   *
   * Keyed by LINEAGE, not by type: `UpdateCommittee` and `NoConfidence` share
   * the `committee` lineage. `null` means nothing of this lineage has ever been
   * enacted, which is the genesis case.
   */
  getEnacted(lineage: GovActionLineage): Promise<Envelope<GovActionRef | null>>;

  /** Optional — the individual "who voted" listing. */
  listVotes?(id: string, q: PageRequest): Promise<PagedEnvelope<VoteRecord>>;

  /** Optional — the lifecycle feed a detail page renders. */
  listActivity?(
    id: string,
    q: PageRequest,
  ): Promise<PagedEnvelope<{ status: GovActionStatus; at: EpochStamp }>>;
}
