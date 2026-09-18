import type {
  GovernanceMetrics,
  NetworkInfo,
  ProtocolParams,
  StakeDistribution,
} from '@govtool/data-providers/chain-data';

import { internal } from '../common/errors';
import { toLovelace, toStrictInteger } from '../common/numbers';
import type {
  NetworkInfoRow,
  NetworkMetricsRow,
  NetworkTotalStakeRow,
} from '../rows';

/** Legacy message, byte for byte — consumers matched on it. */
export const NETWORK_INFO_ERROR =
  'Could not query the network info. This should never happen.';

export function mapNetworkInfoRow(row: NetworkInfoRow): NetworkInfo {
  if (
    row.current_epoch === null ||
    row.current_block === null ||
    row.network_name === null
  ) {
    throw internal(NETWORK_INFO_ERROR);
  }
  const epoch = toStrictInteger(row.current_epoch);
  return {
    network: row.network_name,
    tip: { epoch, block: toStrictInteger(row.current_block) },
    epoch: { no: epoch },
  };
}

export function mapTotalStakeRow(row: NetworkTotalStakeRow): StakeDistribution {
  return {
    totalStakeControlledByDReps: toLovelace(
      row.total_stake_controlled_by_active_dreps,
    ),
    totalStakeControlledBySPOs: toLovelace(row.total_stake_controlled_by_spos),
    alwaysAbstainVotingPower: toLovelace(row.always_abstain_voting_power),
    alwaysNoConfidenceVotingPower: toLovelace(
      row.always_no_confidence_voting_power,
    ),
  };
}

/**
 * Counters are validated with the legacy rule (must already be integers);
 * the one lovelace figure is passed through as a string for the consumer to
 * validate the same way, so the failure message is identical either side.
 */
export function mapMetricsRow(row: NetworkMetricsRow): GovernanceMetrics {
  const metrics: GovernanceMetrics = {
    uniqueDelegators: toStrictInteger(row.unique_delegators),
    totalDelegations: toStrictInteger(row.total_delegations),
    totalGovernanceActions: toStrictInteger(row.total_gov_action_proposals),
    totalDRepVotes: toStrictInteger(row.total_drep_votes),
    totalRegisteredDReps: toStrictInteger(row.total_registered_dreps),
    totalActiveDReps: toStrictInteger(row.total_active_dreps),
    totalInactiveDReps: toStrictInteger(row.total_inactive_dreps),
    totalActiveCip119CompliantDReps: toStrictInteger(
      row.total_active_cip119_compliant_dreps,
    ),
    totalRegisteredDirectVoters: toStrictInteger(
      row.total_registered_direct_voters,
    ),
    committee: {
      size: toStrictInteger(row.no_of_committee_members),
      quorum: {
        numerator: toStrictInteger(row.quorum_numerator),
        denominator: toStrictInteger(row.quorum_denominator),
      },
    },
  };
  if (row.total_drep_distr !== null) {
    metrics.totalDRepDistribution = toLovelace(row.total_drep_distr);
  }
  return metrics;
}

function numberField(
  raw: Record<string, unknown>,
  key: string,
): number | undefined {
  const value = raw[key];
  return typeof value === 'number' && Number.isFinite(value)
    ? value
    : undefined;
}

function lovelaceField(
  raw: Record<string, unknown>,
  key: string,
): string | undefined {
  const value = raw[key];
  if (typeof value === 'string' && value !== '') return value;
  if (typeof value === 'number' && Number.isFinite(value)) return String(value);
  return undefined;
}

/**
 * `ROW_TO_JSON(epoch_param)` → `ProtocolParams`.
 *
 * The raw row is the contract — it is what a ParameterChange diff is computed
 * against, and it is passed through untouched. The scalar fields with a
 * one-to-one db-sync column are lifted out on top of it as a convenience.
 *
 * The `dvt`/`pvt` threshold ratios are deliberately not lifted: db-sync
 * stores them as double precision, and a float cannot be turned back into the
 * on-chain numerator/denominator. A consumer that needs them reads `raw`.
 */
export function mapEpochParams(raw: Record<string, unknown>): ProtocolParams {
  const epoch = numberField(raw, 'epoch_no');
  if (epoch === undefined) {
    throw internal('epoch_param row has no epoch_no');
  }

  const params: ProtocolParams = { epoch, raw };

  const assign = <K extends keyof ProtocolParams>(
    key: K,
    value: ProtocolParams[K] | undefined,
  ): void => {
    if (value !== undefined) {
      params[key] = value;
    }
  };

  assign('govActionDeposit', lovelaceField(raw, 'gov_action_deposit'));
  assign('drepDeposit', lovelaceField(raw, 'drep_deposit'));
  assign('keyDeposit', lovelaceField(raw, 'key_deposit'));
  assign('poolDeposit', lovelaceField(raw, 'pool_deposit'));
  assign('coinsPerUtxoByte', lovelaceField(raw, 'coins_per_utxo_size'));

  assign('minFeeA', numberField(raw, 'min_fee_a'));
  assign('minFeeB', numberField(raw, 'min_fee_b'));
  assign(
    'minFeeRefScriptCostPerByte',
    numberField(raw, 'min_fee_ref_script_cost_per_byte'),
  );
  assign('govActionLifetime', numberField(raw, 'gov_action_lifetime'));
  assign('drepActivity', numberField(raw, 'drep_activity'));
  assign('committeeMinSize', numberField(raw, 'committee_min_size'));
  assign(
    'committeeMaxTermLength',
    numberField(raw, 'committee_max_term_length'),
  );

  const major = numberField(raw, 'protocol_major');
  const minor = numberField(raw, 'protocol_minor');
  if (major !== undefined && minor !== undefined) {
    params.protocolVersion = { major, minor };
  }

  return params;
}
