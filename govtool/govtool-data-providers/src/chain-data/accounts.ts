/**
 * Chain Data API — `/accounts/*`
 *
 * The connected wallet's own stake key. Everything beyond identity and the
 * current governance delegation is optional (SPEC.md §5.4).
 */

import type {
  Bech32,
  EpochStamp,
  Envelope,
  Hex,
  PageRequest,
  PagedEnvelope,
  StakeBalance,
  TxRef,
  VotingPower,
} from './common';
import type { DelegationTarget } from './refs';

export interface Delegation {
  target: DelegationTarget;
  txRef: TxRef | null;
  since?: EpochStamp | null;
}

export interface PoolDelegation {
  poolId: Bech32 | null;
  txRef: TxRef | null;
  since: EpochStamp | null;
}

/** A past delegation, governance or pool. */
export interface DelegationHistoryEvent {
  kind: 'governance' | 'pool';
  /** Present for a governance delegation. */
  target?: DelegationTarget;
  /** Present for a pool delegation. */
  poolId?: Bech32;
  at: EpochStamp;
  txRef: TxRef;
}

export interface Account {
  stakeAddress: Bech32;
  stakeKeyHash: Hex;
  isRegistered: boolean;
  /** Optional — derivable from the address form. */
  isScriptBased?: boolean;
  /** Optional. Serve the components or do not serve it — see `StakeBalance`. */
  balance?: StakeBalance;
}

export interface AccountsApi {
  get(stakeAddress: string): Promise<Envelope<Account>>;

  /**
   * The wallet's current governance delegation. `null` means delegated to
   * nobody — which is different from this provider not serving delegation, and
   * that case cannot arise because this method is required.
   */
  getDelegation(stakeAddress: string): Promise<Envelope<Delegation | null>>;

  /** Optional. */
  getPoolDelegation?(
    stakeAddress: string,
  ): Promise<Envelope<PoolDelegation | null>>;

  /**
   * Optional. The value the ledger counts for this account.
   *
   * A provider that cannot compute it omits this method. It never returns 0 —
   * a zero here is indistinguishable from "no stake" and has already shipped as
   * a wrong number on the screen where a user decides whether to delegate.
   */
  getVotingPower?(stakeAddress: string): Promise<Envelope<VotingPower | null>>;

  /** Optional. Governance and pool delegations over time. */
  listDelegationHistory?(
    stakeAddress: string,
    q: PageRequest & { kind?: 'governance' | 'pool' },
  ): Promise<PagedEnvelope<DelegationHistoryEvent>>;
}
