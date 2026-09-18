import type {
  Delegation,
  DelegationTarget,
  PoolDelegation,
  StakeBalance,
  StakeRegistrationEvent,
  TxRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import {
  isScriptDRepId,
  isScriptStakeAddress,
  KOIOS_ALWAYS_ABSTAIN,
  KOIOS_ALWAYS_NO_CONFIDENCE,
  encodeCip105DRepId,
  stakeKeyHash,
  toDRepHash,
} from '../common/ids';
import {
  sumLovelace,
  toIsoString,
  toLovelace,
  toStrictInteger,
} from '../common/numbers';
import type { AccountInfoRow, AccountUpdateEntry } from '../rows';

/** Koios renamed `proposal-refund` to `proposal_refund`; both are read. */
function proposalRefund(row: AccountInfoRow): string {
  return row.proposal_refund ?? row['proposal-refund'] ?? '0';
}

/**
 * The non-staking rewards db-sync calls `reward_rest`: treasury and reserve
 * payouts (MIR certificates) and governance deposit refunds. Koios splits
 * them into three columns instead of one.
 */
export function rewardsRest(row: AccountInfoRow): string {
  return sumLovelace(row.reserves, row.treasury, proposalRefund(row));
}

/**
 * `total` is **recomputed**, not read from Koios' `total_balance`.
 *
 * Koios computes `total_balance` as `utxo + rewards − withdrawals`, omitting
 * `reward_rest` (reserves, treasury payouts and governance deposit refunds)
 * from the credit side while still subtracting the full withdrawals. For an
 * account whose refunds exceed its staking rewards that goes **negative** —
 * observed on mainnet at −98,218,279,141 lovelace for an account holding a
 * 100,000 ada governance deposit refund.
 *
 * The contract defines `total` as the sum that counts toward voting power, so
 * a negative value is not a rounding quirk, it is wrong. It is rebuilt with
 * the same guarded sum `mapVotingPower` uses, which keeps the two agreeing —
 * `StakeBalance.total` and `VotingPower.amount` are the same quantity.
 */
export function mapBalance(row: AccountInfoRow): StakeBalance {
  return {
    total: reconstructedBalance(row),
    utxo: toLovelace(row.utxo),
    rewards: toLovelace(row.rewards_available),
    rewardsRest: rewardsRest(row),
  };
}

/**
 * `utxo + rewards + reward_rest − withdrawals`, with withdrawals subtracted
 * only when the rewards columns exceed them — the guard
 * `get-stake-key-voting-power.sql` applies, because a source can be missing
 * reward rows for recent epochs and the subtraction would then go negative.
 */
function reconstructedBalance(row: AccountInfoRow): string {
  const rewards = BigInt(row.rewards);
  const rest = BigInt(rewardsRest(row));
  const withdrawals = BigInt(row.withdrawals);
  const deduction = rewards + rest > withdrawals ? withdrawals : 0n;
  return String(BigInt(row.utxo) + rewards + rest - deduction);
}

/**
 * Voting power for a stake account, reconstructed to match GovTool's own
 * definition rather than Koios' `total_balance`.
 *
 * `get-stake-key-voting-power.sql` computes
 *
 *     utxo + rewards + reward_rest − withdrawals
 *
 * with one guard: withdrawals are subtracted **only** when the rewards
 * tables exceed them, because db-sync can be missing reward rows for recent
 * epochs and the subtraction would otherwise go negative.
 *
 * Koios' own `total_balance` is `utxo + rewards − withdrawals` — it omits
 * `reward_rest` and applies no guard, so it returns a negative number for any
 * account that has withdrawn more than its recorded rewards. Recomputing from
 * the component columns, which Koios does expose, is what makes this route
 * agree with the db-sync provider instead of quietly disagreeing with it.
 *
 * The result is a live balance, not an epoch snapshot, so `basis` is `live` —
 * the same basis the db-sync provider reports for the same figure.
 */
export function mapVotingPower(row: AccountInfoRow): VotingPower {
  return { amount: reconstructedBalance(row), basis: 'live' };
}

/**
 * The current governance delegation.
 *
 * Koios reports only the target, never the certificate that set it, so
 * `txRef` and `since` come from a separate `/account_updates` read and are
 * `null` when the caller did not ask for one.
 */
export function mapDelegation(
  row: AccountInfoRow,
  txRef: TxRef | null = null,
  since: Delegation['since'] = null,
): Delegation | null {
  if (row.delegated_drep === null) {
    return null;
  }
  return { target: mapDelegationTarget(row.delegated_drep), txRef, since };
}

export function mapDelegationTarget(drepId: string): DelegationTarget {
  if (drepId === KOIOS_ALWAYS_ABSTAIN) {
    return { kind: 'predefined', option: 'alwaysAbstain' };
  }
  if (drepId === KOIOS_ALWAYS_NO_CONFIDENCE) {
    return { kind: 'predefined', option: 'alwaysNoConfidence' };
  }
  const hash = toDRepHash(drepId);
  const isScript = isScriptDRepId(drepId);
  return {
    kind: 'drep',
    drep: {
      role: 'drep',
      id: drepId,
      hash,
      isScriptBased: isScript,
      cip105Id: encodeCip105DRepId(hash, isScript),
    },
  };
}

export function mapPoolDelegation(
  row: AccountInfoRow,
  txRef: TxRef | null = null,
): PoolDelegation {
  return { poolId: row.delegated_pool, txRef, since: null };
}

export function mapStakeEvent(
  entry: AccountUpdateEntry,
): StakeRegistrationEvent | null {
  if (
    entry.action_type !== 'registration' &&
    entry.action_type !== 'deregistration'
  ) {
    return null;
  }
  return {
    action:
      entry.action_type === 'registration' ? 'registered' : 'deregistered',
    at: {
      epoch: toStrictInteger(entry.epoch_no),
      time: toIsoString(entry.block_time),
    },
    // Koios reports the absolute slot but never the block height here, so
    // `block` stays absent rather than being derived from the slot.
    slot: toStrictInteger(entry.absolute_slot),
    txRef: toTxRef(entry),
  };
}

export function toTxRef(entry: AccountUpdateEntry): TxRef {
  return {
    txHash: entry.tx_hash,
    at: {
      epoch: toStrictInteger(entry.epoch_no),
      time: toIsoString(entry.block_time),
    },
  };
}

export function accountIdentity(stakeAddress: string): {
  stakeKeyHash: string;
  isScriptBased: boolean;
} {
  return {
    stakeKeyHash: stakeKeyHash(stakeAddress),
    isScriptBased: isScriptStakeAddress(stakeAddress),
  };
}
