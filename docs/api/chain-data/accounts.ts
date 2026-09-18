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
} from "./common";
import type { VoterRef } from "./refs";
import type { DRep } from "./governance/dreps";

/** The two ledger-defined delegation targets that are not a real DRep. */
export type PredefinedDelegation = "alwaysAbstain" | "alwaysNoConfidence";

export type DelegationTarget =
  | { kind: "drep"; drep: VoterRef }
  | { kind: "predefined"; option: PredefinedDelegation }
  | { kind: "none" };

/** Governance delegation. */
export interface Delegation {
  target: DelegationTarget;
  txRef: TxRef | null;
  since: EpochStamp | null;
}

/** Stake-pool delegation — separate concern from governance delegation. */
export interface PoolDelegation {
  poolId: Bech32 | null;
  txRef: TxRef | null;
  since: EpochStamp | null;
}

/** Stake key registration / deregistration certificate. */
export interface StakeRegistrationEvent {
  action: "registered" | "deregistered";
  at: EpochStamp;
  slot: SlotNo;
  block: BlockNo;
  txRef: TxRef;
}

export interface DelegationHistoryEvent {
  kind: "governance" | "pool";
  at: EpochStamp;
  txRef: TxRef;
  from: DelegationTarget | { kind: "pool"; poolId: Bech32 | null };
  to: DelegationTarget | { kind: "pool"; poolId: Bech32 | null };
}

export type AccountExpand = "balance" | "drep" | "poolDelegation" | "adaHandles";

export interface Account {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
  isRegistered: boolean;
  isScriptBased: boolean;
  balance?: StakeBalance;
  votingPower: VotingPower | null;
  delegation: Delegation;
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
    q?: PageRequest & { kind?: ("governance" | "pool")[] },
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
}
