/**
 * Chain Data API — `/accounts/*`
 *
 * Stake accounts: registration, balance, voting power, and both kinds of
 * delegation (governance → DRep, and stake → pool). This is the "connected
 * wallet" module; it knows nothing about proposals.
 */

import type {
  Bech32,
  BlockNo,
  Envelope,
  EpochStamp,
  Hex,
  PagedEnvelope,
  PageRequest,
  SlotNo,
  StakeBalance,
  TxRef,
  VotingPower,
} from './common';
import type { PredefinedDelegation, VoterRef } from './refs';
import type { DRep } from './governance/dreps';

export type { PredefinedDelegation };

export type DelegationTarget =
  | { kind: 'drep'; drep: VoterRef }
  | { kind: 'predefined'; option: PredefinedDelegation }
  | { kind: 'none' };

/** Governance delegation. */
export interface Delegation {
  target: DelegationTarget;
  txRef: TxRef | null;
  /** Optional: db-sync's current-delegation read returns the certificate's tx but not its epoch or time. */
  since?: EpochStamp | null;
}

/** Stake-pool delegation — separate concern from governance delegation. */
export interface PoolDelegation {
  poolId: Bech32 | null;
  txRef: TxRef | null;
  since: EpochStamp | null;
}

/** Stake key registration / deregistration certificate. */
export interface StakeRegistrationEvent {
  action: 'registered' | 'deregistered';
  /** Optional: a certificate listing may name the transaction but not date it. */
  at?: EpochStamp;
  /**
   * Optional: a source can record the certificate without the block it landed
   * in. Koios' `/account_updates` reports the absolute slot but no block
   * height, so a provider that has one and not the other fills what it has
   * rather than inventing the rest.
   */
  slot?: SlotNo;
  block?: BlockNo;
  txRef: TxRef;
}

export interface DelegationHistoryEvent {
  kind: 'governance' | 'pool';
  at?: EpochStamp;
  txRef: TxRef;
  /**
   * Optional: a delegation-history listing gives the target each certificate
   * set, not the one it replaced. Deriving `from` means walking the whole
   * history in order, which only a provider that fetched all of it can do.
   */
  from?: DelegationTarget | { kind: 'pool'; poolId: Bech32 | null };
  to: DelegationTarget | { kind: 'pool'; poolId: Bech32 | null };
}

/**
 * Fields the caller opts into. `votingPower` and `delegation` are listed
 * because each is its own read on every provider surveyed; a wallet-connect
 * screen that only needs registration state should not pay for them.
 */
export type AccountExpand =
  | 'balance'
  | 'votingPower'
  | 'delegation'
  | 'drep'
  | 'poolDelegation'
  | 'adaHandles';

export interface Account {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
  isRegistered: boolean;
  isScriptBased: boolean;
  /** Provider-native opaque identifier, for compatibility only; see `TxRef.providerId`. */
  providerId?: string;
  balance?: StakeBalance;
  /** Present when `expand` includes `votingPower`; `null` = no power on record. */
  votingPower?: VotingPower | null;
  /** Present when `expand` includes `delegation`. */
  delegation?: Delegation;
  poolDelegation?: PoolDelegation;
  latestRegistration?: StakeRegistrationEvent | null;
  latestDeregistration?: StakeRegistrationEvent | null;
  /** Set when this stake key is itself registered as a DRep / direct voter. */
  drep?: DRep | null;
  adaHandles?: string[];
}

export interface AccountsApi {
  /** `GET /accounts/{stakeAddress}` */
  get(
    stakeAddress: string,
    q?: { expand?: AccountExpand[] },
  ): Promise<Envelope<Account>>;

  /** `GET /accounts/{stakeAddress}/delegations` — governance and pool history. */
  listDelegationHistory(
    stakeAddress: string,
    q?: PageRequest & { kind?: ('governance' | 'pool')[] },
  ): Promise<PagedEnvelope<DelegationHistoryEvent>>;

  /** `GET /accounts/{stakeAddress}/stake-events` */
  listStakeEvents(
    stakeAddress: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<StakeRegistrationEvent>>;

  /** `GET /accounts/{stakeAddress}/voting-power` */
  getVotingPower(
    stakeAddress: string,
    q?: { epoch?: number },
  ): Promise<Envelope<VotingPower | null>>;

  /**
   * `GET /accounts/{stakeAddress}/delegation` — the current governance
   * delegation on its own. It is the hottest read after wallet connect and
   * a single lookup on every provider, so it does not require fetching the
   * whole account. `null` = no delegation on record.
   */
  getDelegation(stakeAddress: string): Promise<Envelope<Delegation | null>>;
}
