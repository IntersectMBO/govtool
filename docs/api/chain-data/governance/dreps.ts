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
} from "../common";
import type { Anchor } from "../common";
import type { VoterRef } from "../refs";
import type { DRepMetadataBody, MetadataProjection } from "../../metadata";
import type { VoteListQuery, VotedGovAction } from "./votes";

export type DRepStatus = "active" | "inactive" | "retired";

/** DRep proper vs. a stake key registered only to vote for itself. */
export type DRepKind = "drep" | "directVoter";

export interface Registration {
  status: DRepStatus;
  registeredAt: EpochStamp | null;
  registrationTx: TxRef | null;
  retiredAt: EpochStamp | null;
  retirementTx: TxRef | null;
  deposit: Lovelace | null;
}

export interface DRepActivity {
  votesCast: number;
  /** Votable actions the DRep did not vote on, over the same window. */
  notVotedCount?: number;
  lastVotedAt: EpochStamp | null;
  /** Epoch at which the DRep goes inactive without activity (`drepActivity`). */
  inactiveFromEpoch: EpochNo | null;
  /** votes cast / actions votable during registration, 0..1 */
  participationRate?: number;
}

export interface DRep extends VoterRef {
  role: "drep";
  kind: DRepKind;
  registration: Registration;
  metadata: MetadataProjection<DRepMetadataBody> | null;
  isCip119Compliant: boolean;
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
  action: "joined" | "left";
  stakeAddress: Bech32;
  at: EpochStamp;
  txRef: TxRef;
}

/** Registration / update / retirement history (#4226). */
export interface DRepHistoryEvent {
  type: "registered" | "updated" | "retired";
  at: EpochStamp;
  txRef: TxRef;
  anchor: Anchor | null;
  /** Field-level diff where the provider can compute one. */
  changes?: Record<string, { from: unknown; to: unknown }>;
}

/* ------------------------------------------------------------------------- */
/* Queries                                                                    */
/* ------------------------------------------------------------------------- */

export type DRepSort =
  | "votingPower"
  | "registrationDate"
  | "activity"
  | "status"
  | "random";

export type DRepExpand = "metadata" | "liveVotingPower" | "delegators" | "activity";

export interface DRepListQuery extends PageRequest {
  expand?: DRepExpand[];
  status?: DRepStatus[];
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

  /** `GET /governance/dreps/{id}` */
  get(id: string, q?: { expand?: DRepExpand[] }): Promise<Envelope<DRep>>;

  /** `GET /governance/dreps/{id}/votes` */
  listVotes(id: string, q?: VoteListQuery): Promise<PagedEnvelope<VotedGovAction>>;

  /** `GET /governance/dreps/{id}/delegators` */
  listDelegators(
    id: string,
    q?: PageRequest & { basis?: StakeBasis; includeBalance?: boolean },
  ): Promise<PagedEnvelope<DRepDelegator>>;

  /** `GET /governance/dreps/{id}/delegation-events` */
  listDelegationEvents(id: string, q?: PageRequest): Promise<PagedEnvelope<DRepDelegationEvent>>;

  /** `GET /governance/dreps/{id}/history` */
  listHistory(id: string, q?: PageRequest): Promise<PagedEnvelope<DRepHistoryEvent>>;

  /** `GET /governance/dreps/{id}/voting-power` — history when `from`/`to` given. */
  getVotingPower(
    id: string,
    q?: { basis?: StakeBasis; fromEpoch?: EpochNo; toEpoch?: EpochNo },
  ): Promise<Envelope<VotingPower[]>>;

  /** `GET /governance/dreps/voting-power?ids=…` — batch, for list hydration. */
  getVotingPowers(ids: string[]): Promise<Envelope<Record<string, VotingPower | null>>>;
}
