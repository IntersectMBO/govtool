/**
 * Chain Data API — `/governance/proposals/*`
 *
 * Governance actions: identity, typed body, lifecycle, tallies and thresholds.
 * "Proposal" is the GovTool-facing name; the ledger calls these governance
 * actions, and the ids are CIP-129 `gov_action1…`.
 */

import type {
  Anchor,
  Bech32,
  Envelope,
  EpochNo,
  EpochStamp,
  Hex,
  Lovelace,
  PagedEnvelope,
  PageRequest,
  Ratio,
  TxRef,
} from '../common';
import type { GovActionRef, VoteChoice, VoterRef, VoterRole } from '../refs';
import type { ProtocolParams } from '../network';
import type { GovActionMetadataBody, MetadataProjection } from '../../metadata';
import type { VoteListQuery, VoteRecord } from './votes';

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

/**
 * Action body, discriminated by `type`.
 *
 * `guardrailsScriptHash` is optional on the variants that carry one: it lives
 * in the action's on-chain description, which some sources store pre-digested
 * without it. `null` still means "known to be absent".
 */
export type GovActionBody =
  | { type: 'InfoAction' }
  | { type: 'NoConfidence' }
  | {
      type: 'ParameterChange';
      changes: Record<string, unknown>;
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
      added: {
        coldCredential: Hex;
        isScriptBased: boolean;
        termExpiryEpoch: EpochNo;
      }[];
      removed: { coldCredential: Hex; isScriptBased: boolean }[];
      quorum: Ratio;
    }
  | {
      type: 'NewConstitution';
      anchor: Anchor;
      guardrailsScriptHash?: Hex | null;
    };

/**
 * Tally for one voter role. Whether a role is counted by stake or by head
 * is a ledger fact — DReps and SPOs by stake, the committee by count — so a
 * provider fills the field that applies and leaves the other `undefined`.
 * A provider with both figures fills both.
 */
export interface RoleTally {
  role: VoterRole;
  stake?: Record<VoteChoice, Lovelace>;
  count?: Record<VoteChoice, number>;
  /** Stake eligible but not voted — needed for threshold bars. */
  notVotedStake?: Lovelace;
  /** The denominator percentages must be computed against (always `active` basis). */
  totalEligibleStake?: Lovelace;
  /** Threshold that applies to THIS action type for THIS role. */
  threshold?: Ratio;
  /** Whether this role's threshold is currently met. */
  passing?: boolean;
}

export interface GovActionLifecycle {
  status: GovActionStatus;
  /**
   * When the action was submitted. Optional because a per-entity HTTP API can
   * return an action's whole lifecycle *except* its submission point —
   * Blockfrost's proposal record carries the ratified / enacted / dropped /
   * expired epochs and the expiry, but nothing about submission. The
   * submitting transaction is always known, so `submittedTx` stays required.
   */
  submitted?: EpochStamp;
  submittedTx: TxRef;
  expires: EpochStamp | null;
  ratifiedAt: EpochStamp | null;
  enactedAt: EpochStamp | null;
  droppedAt: EpochStamp | null;
  expiredAt: EpochStamp | null;
}

export interface GovAction extends GovActionRef {
  type: GovActionType;
  /**
   * Typed body. Optional: a provider fills it for the variants it can build
   * without guessing and puts the source's own rendering in `rawBody`. A
   * consumer that needs the body for a type the provider does not type reads
   * `rawBody` knowing it is provider-shaped.
   */
  body?: GovActionBody;
  /** The action description as the provider stores it, untyped. */
  rawBody?: unknown;
  lifecycle: GovActionLifecycle;
  /** Optional: the deposit is on the proposal certificate, which not every read joins. */
  deposit?: Lovelace | null;
  depositReturnAddress?: Bech32 | null;
  proposedBy?: Bech32 | null;
  previousAction: GovActionRef | null;
  metadata: MetadataProjection<GovActionMetadataBody> | null;
  /** Present when `expand` includes `tallies`. */
  tallies?: RoleTally[];
  /** Protocol parameters in force when the action was submitted. */
  protocolParamsAtSubmission?: ProtocolParams | null;
  /** Populated once enacted, for before/after diffs. */
  protocolParamsAtEnactment?: ProtocolParams | null;
  /** Present only when the request carried a `voterId`. */
  myVote?: VoteRecord | null;
}

export interface GovActionActivityEvent {
  type: 'submitted' | 'voted' | 'ratified' | 'enacted' | 'expired' | 'dropped';
  at: EpochStamp;
  txRef: TxRef | null;
  voter?: VoterRef;
  vote?: VoteChoice;
}

/** Currently-enacted action of a given type, for comparison views. */
export interface EnactedActionSummary {
  type: GovActionType;
  action: GovActionRef;
  /** Optional: the enactment epoch is on the proposal row, which a minimal read may not select. */
  enactedAt?: EpochStamp;
  /** The transaction that submitted the action, when the provider reports it separately from `action`. */
  submittedTx?: TxRef;
  body?: GovActionBody;
  /** See `GovAction.rawBody`. */
  rawBody?: unknown;
}

/* ------------------------------------------------------------------------- */
/* Queries                                                                    */
/* ------------------------------------------------------------------------- */

export type GovActionSort =
  | 'newest'
  | 'oldest'
  | 'soonestToExpire'
  | 'mostYesVotes'
  | 'highestParticipation';

/** Fields the caller opts into; each adds a join the list read otherwise skips. */
export type GovActionExpand =
  'tallies' | 'thresholds' | 'metadata' | 'myVote' | 'protocolParams';

export interface ProposalListQuery extends PageRequest {
  expand?: GovActionExpand[];
  type?: GovActionType[];
  status?: GovActionStatus[];
  sort?: GovActionSort;
  /** Free text over title/abstract, or an exact CIP-129 id. */
  search?: string;
  /** Adds `myVote` to each element. */
  voterId?: string;
}

export interface ProposalsApi {
  /** `GET /governance/proposals` */
  list(q?: ProposalListQuery): Promise<PagedEnvelope<GovAction>>;

  /**
   * `GET /governance/proposals/{id}` — CIP-129 id. Providers also accept the
   * `txHash#index` form, since that is what wallets and older clients hold.
   * `NOT_FOUND` when no action matches.
   */
  get(
    id: string,
    q?: { expand?: GovActionExpand[]; voterId?: string },
  ): Promise<Envelope<GovAction>>;

  /** `GET /governance/proposals/{id}/votes` */
  listVotes(id: string, q?: VoteListQuery): Promise<PagedEnvelope<VoteRecord>>;

  /** `GET /governance/proposals/{id}/tallies` */
  getTallies(
    id: string,
    q?: { role?: VoterRole },
  ): Promise<Envelope<RoleTally[]>>;

  /** `GET /governance/proposals/{id}/activity` */
  listActivity(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<GovActionActivityEvent>>;

  /** `GET /governance/proposals/enacted?type=…` */
  getEnacted(
    type: GovActionType,
  ): Promise<Envelope<EnactedActionSummary | null>>;

  /** `GET /governance/proposals?txHash=…` — post-submission confirmation. */
  listByTx(txHash: string): Promise<Envelope<GovActionRef[]>>;
}
