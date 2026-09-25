/**
 * Chain Data API — `/governance/dreps/*`
 */

import type {
  Anchor,
  Bech32,
  EpochNo,
  EpochStamp,
  Envelope,
  Lovelace,
  PageRequest,
  PagedEnvelope,
  TxRef,
  VotingPower,
} from '../common';
import type { VoterRef } from '../refs';
import type { DRepVoteListQuery, DRepVoteRow } from './votes';

/** From the ledger's DRep expiry; see `DRep.status`. */
export type DRepStatus = 'active' | 'inactive' | 'retired';

/**
 * A DRep that registered with NO ANCHOR is `anonymous`.
 *
 * That is the definition — an observable fact, not an inference about intent.
 * `kind` is derived from the anchor and exists as a filter key, not as
 * independent information: a provider must never report `drep` for a DRep with
 * no anchor, or vice versa.
 */
export type DRepKind = 'drep' | 'anonymous';

export type DRepSort =
  'votingPower' | 'registrationDate' | 'activity' | 'random';

/** A registration or update certificate, dated. */
export interface RegistrationEvent {
  txRef: TxRef;
  /** Required: the stated purpose of carrying these is showing a date. */
  at: EpochStamp;
  /** The anchor as of this certificate. */
  anchor?: Anchor | null;
  deposit?: Lovelace | null;
}

export interface Registration {
  /** The latest registration certificate. */
  latest: RegistrationEvent;
  /** The latest registration-UPDATE certificate, if the DRep has updated. */
  latestUpdate: RegistrationEvent | null;
  retiredAt?: EpochStamp | null;
}

/**
 * Participation since the DRep registered — NOT a rolling window.
 *
 * `votable` is the number of actions that were votable during the DRep's
 * registration, which is the length of the unfiltered vote listing.
 */
export interface DRepActivity {
  voted: number;
  votable: number;
}

export interface DRepDelegator {
  stakeAddress: Bech32;
  activeVotingPower: Lovelace;
  delegatedAt?: EpochStamp;
  liveVotingPower?: Lovelace;
  /** The DRep this delegator switched away from, where the source records it. */
  previousDRepId?: Bech32;
}

/** Counts over the whole directory. All three, or none. */
export interface DRepCounts {
  totalRegistered: number;
  totalActive: number;
  totalInactive: number;
  /** DReps with no anchor. */
  anonymous?: number;
}

export interface DRep extends VoterRef {
  role: 'drep';
  kind: DRepKind;
  /**
   * The CIP-119 anchor. `null` = registered without one, which makes this DRep
   * `anonymous`. The document is the metadata service's business.
   */
  anchor: Anchor | null;
  registration: Registration;
  /**
   * From the ledger's DRep expiry, which is pushed forward by `drepActivity`
   * whenever the DRep votes or re-registers; inactive means
   * `currentEpoch > expiry`. READ THE EXPIRY — do not reconstruct activity
   * from vote timestamps.
   */
  status: DRepStatus;
  /** The epoch this DRep goes inactive without further activity. */
  expiryEpoch?: EpochNo;
  /** The epoch snapshot the ledger counts. Required. */
  votingPower: VotingPower | null;
  /** Optional. */
  liveVotingPower?: VotingPower | null;
  /** Optional. */
  activity?: DRepActivity;
  /** Optional. */
  delegatorCount?: number;
}

export interface DRepListQuery extends PageRequest {
  status?: DRepStatus[];
  kind?: DRepKind[];
  sort?: DRepSort;
  /**
   * One term. The caller does NOT name a mode; the provider applies whatever it
   * supports and declares which kinds of input will match.
   */
  search?: string;
}

export interface DRepsApi {
  /**
   * The directory. Default ordering is RANDOM, so it does not become a rich
   * list; a randomly ordered read is not paged — it returns `size` rows and a
   * provider rejects any `page` beyond the first.
   */
  list(q: DRepListQuery): Promise<PagedEnvelope<DRep>>;

  get(id: string): Promise<Envelope<DRep>>;

  /** Optional. Voted and not-voted actions, filterable. */
  listVotes?(
    id: string,
    q: PageRequest & DRepVoteListQuery,
  ): Promise<PagedEnvelope<DRepVoteRow>>;

  /** Optional. */
  listDelegators?(
    id: string,
    q: PageRequest,
  ): Promise<PagedEnvelope<DRepDelegator>>;

  /**
   * Optional. The DRep's METADATA-CHANGE feed: each row an anchor and a date.
   * Not a general certificate feed.
   */
  listUpdateHistory?(
    id: string,
    q: PageRequest & { sort?: 'asc' | 'desc' },
  ): Promise<PagedEnvelope<RegistrationEvent>>;

  /** Optional. All-or-nothing, apart from `anonymous`. */
  getCounts?(): Promise<Envelope<DRepCounts>>;
}
