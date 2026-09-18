/**
 * Chain Data API — `/governance/dreps/*`
 *
 * DRep profiles, registration lifecycle, voting power, delegators and activity.
 * Covers both real DReps and direct ("sole") voters, distinguished by `kind`
 * rather than by a separate entity.
 */

import type {
  Bech32,
  Envelope,
  EpochNo,
  EpochStamp,
  Lovelace,
  PagedEnvelope,
  PageRequest,
  StakeBalance,
  StakeBasis,
  TxRef,
  VotingPower,
} from '../common';
import type { Anchor } from '../common';
import type { DRepTarget, VoterRef } from '../refs';
import type { DRepMetadataBody, MetadataProjection } from '../../metadata';
import type { VoteListQuery, VotedGovAction } from './votes';

export type DRepStatus = 'active' | 'inactive' | 'retired';

/** DRep proper vs. a stake key registered only to vote for itself. */
export type DRepKind = 'drep' | 'directVoter';

export interface Registration {
  /**
   * Optional because `active` vs `inactive` is an activity computation, not a
   * registration fact: a provider that answers a single-DRep read from the
   * registration certificates alone knows whether the credential is
   * registered, not whether it has been voting.
   */
  status?: DRepStatus;
  /**
   * The lifecycle fields are optional, not nullable-required: `null` means
   * "known not to have happened" (an active DRep has no retirement), while
   * absent means the read did not cover it. db-sync's directory query knows
   * the latest registration's time and tx but nothing about a retirement;
   * its single-DRep query knows the certificates but not their times.
   */
  registeredAt?: EpochStamp | null;
  registrationTx?: TxRef | null;
  retiredAt?: EpochStamp | null;
  retirementTx?: TxRef | null;
  deposit: Lovelace | null;
}

/**
 * Registration state for one `DRepKind` on a credential. The same stake key
 * can hold a DRep registration (with an anchor) and a direct-voter
 * registration (without) over its lifetime; `DRep.registration` summarises
 * the latest, this keeps them apart.
 */
export interface DRepKindRegistration {
  isRegistered: boolean;
  wasRegistered: boolean;
  registrationTx: TxRef | null;
  retirementTx: TxRef | null;
}

export interface DRepActivity {
  /**
   * Distinct governance actions voted on within the provider's activity
   * window — db-sync's is the trailing 365 days. Not a lifetime count.
   */
  votesCast: number;
  /** Votable actions the DRep did not vote on, over the same window. */
  notVotedCount?: number;
  /** Optional: a count-only read (db-sync's directory) has no per-vote timestamps. */
  lastVotedAt?: EpochStamp | null;
  /** Epoch at which the DRep goes inactive without activity (`drepActivity`). */
  inactiveFromEpoch?: EpochNo | null;
  /** votes cast / actions votable during registration, 0..1 */
  participationRate?: number;
}

export interface DRep extends VoterRef {
  role: 'drep';
  kind: DRepKind;
  registration: Registration;
  /**
   * Per-kind registration state; see `DRepKindRegistration`. Optional: only a
   * provider that walks the full certificate history can split them.
   */
  registrationByKind?: Record<DRepKind, DRepKindRegistration>;
  metadata: MetadataProjection<DRepMetadataBody> | null;
  /** Optional: requires the metadata to have been validated, not just fetched. */
  isCip119Compliant?: boolean;
  /** Epoch-snapshot power — the one the ledger counts. */
  votingPower: VotingPower | null;
  /** Current power, if the provider can compute it. `basis: "live"`. */
  liveVotingPower?: VotingPower | null;
  delegators?: { active?: number; live?: number };
  activity?: DRepActivity;
  adaHandles?: string[];
}

export interface DRepDelegator {
  stakeAddress: Bech32;
  basis: StakeBasis;
  balance: StakeBalance;
  since: EpochStamp | null;
  txRef: TxRef | null;
}

/** Delegations arriving at / leaving a DRep. */
export interface DRepDelegationEvent {
  action: 'joined' | 'left';
  stakeAddress: Bech32;
  at: EpochStamp;
  txRef: TxRef;
}

/** Registration / update / retirement history (#4226). */
export interface DRepHistoryEvent {
  type: 'registered' | 'updated' | 'retired';
  /** Optional: a certificate listing may name the transaction but not date it. */
  at?: EpochStamp;
  txRef: TxRef;
  /** Optional where the source lists the certificate without its anchor. */
  anchor?: Anchor | null;
  /** Field-level diff where the provider can compute one. */
  changes?: Record<string, { from: unknown; to: unknown }>;
}

/**
 * One row of the batch voting-power read.
 *
 * `subject` is a `DRepTarget` rather than a `VoterRef` because a source's
 * DRep listing includes the predefined options, which hold real voting power
 * but have no credential — db-sync's `drep_hash` rows for
 * `drep_always_no_confidence` and `drep_always_abstain` have a NULL `raw`.
 */
export interface DRepVotingPowerEntry {
  subject: DRepTarget;
  votingPower: VotingPower | null;
  /** Carried so a list can be labelled without a second read per DRep. */
  givenName?: string | null;
}

/* ------------------------------------------------------------------------- */
/* Queries                                                                    */
/* ------------------------------------------------------------------------- */

export type DRepSort =
  'votingPower' | 'registrationDate' | 'activity' | 'status' | 'random';

export type DRepExpand =
  'metadata' | 'liveVotingPower' | 'delegators' | 'activity';

export interface DRepListQuery extends PageRequest {
  expand?: DRepExpand[];
  status?: DRepStatus[];
  /** Omitted = every kind. A directory UI usually passes `['drep']`. */
  kind?: DRepKind[];
  sort?: DRepSort;
  /** Required for stable pagination when `sort === "random"`. */
  seed?: string;
  /** Name, DRep id, or Ada Handle. */
  search?: string;
}

export interface DRepsApi {
  /** `GET /governance/dreps` */
  list(q?: DRepListQuery): Promise<PagedEnvelope<DRep>>;

  /** `GET /governance/dreps/{id}` — `NOT_FOUND` when the credential was never registered. */
  get(id: string, q?: { expand?: DRepExpand[] }): Promise<Envelope<DRep>>;

  /** `GET /governance/dreps/{id}/votes` */
  listVotes(
    id: string,
    q?: VoteListQuery,
  ): Promise<PagedEnvelope<VotedGovAction>>;

  /** `GET /governance/dreps/{id}/delegators` */
  listDelegators(
    id: string,
    q?: PageRequest & { basis?: StakeBasis; includeBalance?: boolean },
  ): Promise<PagedEnvelope<DRepDelegator>>;

  /** `GET /governance/dreps/{id}/delegation-events` */
  listDelegationEvents(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<DRepDelegationEvent>>;

  /** `GET /governance/dreps/{id}/history` */
  listHistory(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<DRepHistoryEvent>>;

  /** `GET /governance/dreps/{id}/voting-power` — history when `from`/`to` given. */
  getVotingPower(
    id: string,
    q?: { basis?: StakeBasis; fromEpoch?: EpochNo; toEpoch?: EpochNo },
  ): Promise<Envelope<VotingPower[]>>;

  /**
   * `GET /governance/dreps/voting-power?ids=…` — batch, for list hydration.
   * `ids` omitted or empty = every DRep the provider knows. Each id may be a
   * CIP-129 id, a CIP-105 id or a raw hash; the entry echoes the DRep's
   * identity so the caller can match rows back however it asked.
   */
  getVotingPowers(ids?: string[]): Promise<Envelope<DRepVotingPowerEntry[]>>;
}
