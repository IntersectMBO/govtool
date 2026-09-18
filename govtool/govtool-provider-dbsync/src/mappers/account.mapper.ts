import type {
  Account,
  Delegation,
  DelegationTarget,
  TransactionState,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { encodeCip129DRepId } from '../common/ids';
import { toLovelace } from '../common/numbers';
import type {
  AccountInfoRow,
  CurrentDelegationRow,
  StakeKeyVotingPowerRow,
  TransactionStatusRow,
} from '../rows';

/** db-sync's names for the two ledger-defined targets; they have a `NULL` raw hash. */
const PREDEFINED_VIEWS: Record<string, DelegationTarget> = {
  drep_always_abstain: { kind: 'predefined', option: 'alwaysAbstain' },
  drep_always_no_confidence: {
    kind: 'predefined',
    option: 'alwaysNoConfidence',
  },
};

export function mapAccountInfoRow(hash: string, row: AccountInfoRow): Account {
  return {
    stakeAddress: row.view,
    stakeKeyHash: hash,
    isRegistered: row.is_registered,
    isScriptBased: row.is_script_based,
    providerId: String(row.id),
  };
}

export function mapDelegationRow(row: CurrentDelegationRow): Delegation {
  let target: DelegationTarget;
  if (row.drep_raw === null) {
    target = PREDEFINED_VIEWS[row.drep_view] ?? { kind: 'none' };
  } else {
    target = {
      kind: 'drep',
      drep: {
        role: 'drep',
        id: encodeCip129DRepId(row.drep_raw, row.has_script),
        hash: row.drep_raw,
        isScriptBased: row.has_script,
        cip105Id: row.drep_view,
      },
    };
  }
  return { target, txRef: { txHash: row.encode } };
}

/**
 * The stake-key statement sums the account's current UTxO and rewards — the
 * live balance, not an epoch snapshot — so the basis is `live`.
 */
export function mapStakeKeyVotingPowerRow(
  row: StakeKeyVotingPowerRow,
): VotingPower {
  return { amount: toLovelace(row.total_balance), basis: 'live' };
}

export function mapTransactionStatusRow(
  txHash: string,
  row: TransactionStatusRow,
): TransactionState {
  return {
    txHash,
    status: row.tx_exists ? 'confirmed' : 'unknown',
    votingProcedures: row.voting_procedures,
  };
}
