/**
 * NetworkApi over db-sync (SPEC.md §5.1).
 *
 * Tables: `block` (tip), `meta` (network name), `epoch_param` (one row per
 * epoch, so past epochs are served), `epoch_stake` + `epoch_stake_progress`
 * (active stake), `drep_distr` and `pool_stat` (governance breakdowns),
 * `ada_pots` (treasury and reserves, one row per epoch).
 */
import type {
  CostModels,
  DRepThresholds,
  NetworkApi,
  NetworkInfo,
  OptionalArgument,
  PlutusLanguage,
  PoolThresholds,
  ProtocolParams,
  Ratio,
  StakeDistribution,
  Treasury,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { internal, notFound } from './errors';
import { TIP_EPOCH_CTE, TIP_SQL, parseEpoch, staleData, toStamp, type BlockCols } from './network/chain';
import { toRatio } from './governance/proposals/ratio';
import { toInt, toLovelace } from './numbers';

type DbNumber = number | string;

/**
 * Era by protocol major version. db-sync records no era name, so it is read
 * off the protocol version of the tip block. Versions 9 to 11 are all Conway
 * (10 and 11 are intra-era hard forks). An unknown version is refused rather
 * than guessed.
 */
export function eraOf(protocolMajor: number): string {
  if (protocolMajor <= 1) return 'byron';
  if (protocolMajor === 2) return 'shelley';
  if (protocolMajor === 3) return 'allegra';
  if (protocolMajor === 4) return 'mary';
  if (protocolMajor <= 6) return 'alonzo';
  if (protocolMajor <= 8) return 'babbage';
  if (protocolMajor <= 11) return 'conway';
  if (protocolMajor === 12) return 'dijkstra';
  throw internal(`unknown era for protocol major version ${protocolMajor}`);
}

/* ------------------------------------------------------------------------- */
/* Protocol parameters                                                         */
/* ------------------------------------------------------------------------- */

const PARAM_COLUMNS = `ep.epoch_no, ep.protocol_major, ep.protocol_minor,
  ep.gov_action_lifetime, ep.gov_action_deposit, ep.drep_deposit, ep.drep_activity,
  ep.committee_min_size, ep.committee_max_term_length,
  ep.dvt_motion_no_confidence, ep.dvt_committee_normal, ep.dvt_committee_no_confidence,
  ep.dvt_update_to_constitution, ep.dvt_hard_fork_initiation, ep.dvt_p_p_network_group,
  ep.dvt_p_p_economic_group, ep.dvt_p_p_technical_group, ep.dvt_p_p_gov_group,
  ep.dvt_treasury_withdrawal,
  ep.pvt_motion_no_confidence, ep.pvt_committee_normal, ep.pvt_committee_no_confidence,
  ep.pvt_hard_fork_initiation, ep.pvtpp_security_group,
  ep.key_deposit, ep.pool_deposit, ep.coins_per_utxo_size, ep.min_fee_a, ep.min_fee_b,
  ep.max_tx_size, ep.max_val_size,
  ep.min_fee_ref_script_cost_per_byte, ep.max_block_size, ep.max_bh_size,
  ep.max_tx_ex_mem, ep.max_tx_ex_steps, ep.max_block_ex_mem, ep.max_block_ex_steps,
  ep.collateral_percent, ep.max_collateral_inputs, ep.price_mem, ep.price_step,
  ep.max_epoch, ep.optimal_pool_count, ep.influence, ep.monetary_expand_rate,
  ep.treasury_growth_rate, ep.min_pool_cost,
  (SELECT cm.costs FROM cost_model cm WHERE cm.id = ep.cost_model_id) AS cost_models`;

/**
 * One row, always: the tip epoch, plus the `epoch_param` row for the requested
 * epoch (or the tip epoch) when there is one. `epoch_param` has no index on
 * `epoch_no`, but it holds one row per epoch (about 1.5k on preview, fewer on
 * mainnet), so the scan is trivial at any scale.
 */
export const PROTOCOL_PARAMS_SQL = `WITH ${TIP_EPOCH_CTE}
SELECT tip.epoch_no AS tip_epoch, ${PARAM_COLUMNS}
  FROM tip
  LEFT JOIN LATERAL (
    SELECT * FROM epoch_param WHERE epoch_no = COALESCE($1::int, tip.epoch_no) ORDER BY id DESC LIMIT 1
  ) ep ON TRUE`;

export interface ProtocolParamsRow {
  tip_epoch: number | null;
  epoch_no: number | null;
  protocol_major: number;
  protocol_minor: number;
  gov_action_lifetime: DbNumber | null;
  gov_action_deposit: DbNumber | null;
  drep_deposit: DbNumber | null;
  drep_activity: DbNumber | null;
  committee_min_size: DbNumber | null;
  committee_max_term_length: DbNumber | null;
  dvt_motion_no_confidence: number | null;
  dvt_committee_normal: number | null;
  dvt_committee_no_confidence: number | null;
  dvt_update_to_constitution: number | null;
  dvt_hard_fork_initiation: number | null;
  dvt_p_p_network_group: number | null;
  dvt_p_p_economic_group: number | null;
  dvt_p_p_technical_group: number | null;
  dvt_p_p_gov_group: number | null;
  dvt_treasury_withdrawal: number | null;
  pvt_motion_no_confidence: number | null;
  pvt_committee_normal: number | null;
  pvt_committee_no_confidence: number | null;
  pvt_hard_fork_initiation: number | null;
  pvtpp_security_group: number | null;
  key_deposit: DbNumber;
  pool_deposit: DbNumber;
  coins_per_utxo_size: DbNumber | null;
  min_fee_a: DbNumber;
  min_fee_b: DbNumber;
  max_tx_size: DbNumber;
  max_val_size: DbNumber | null;
  min_fee_ref_script_cost_per_byte: number | null;
  max_block_size: DbNumber;
  max_bh_size: DbNumber;
  max_tx_ex_mem: DbNumber | null;
  max_tx_ex_steps: DbNumber | null;
  max_block_ex_mem: DbNumber | null;
  max_block_ex_steps: DbNumber | null;
  collateral_percent: DbNumber | null;
  max_collateral_inputs: DbNumber | null;
  price_mem: number | null;
  price_step: number | null;
  max_epoch: DbNumber;
  optimal_pool_count: DbNumber;
  influence: number;
  monetary_expand_rate: number;
  treasury_growth_rate: number;
  min_pool_cost: DbNumber;
  /** `cost_model.costs`: `{ "PlutusV1": [...], ... }`, jsonb. */
  cost_models: unknown;
}

/**
 * A protocol parameter's rational from db-sync's double. Short decimals are
 * exact as written (`0.0000721 -> 721/10000000`, a price no denominator cap of
 * 1000 can hold); long ones are float renderings of a rational such as 2/3,
 * recovered by continued fractions (SPEC.md §3.2). No protocol parameter is
 * negative, so a negative value is refused.
 */
export function toParamRatio(value: number | string | null | undefined): Ratio | undefined {
  const ratio = toRatio(value);
  return ratio && ratio.numerator >= 0 ? ratio : undefined;
}

const PLUTUS_LANGUAGES: readonly PlutusLanguage[] = ['PlutusV1', 'PlutusV2', 'PlutusV3'];

/**
 * `cost_model.costs` as the contract's `CostModels`: one integer array per
 * language, in the ledger's parameter order, which is how db-sync stores it.
 * An older db-sync kept a named-key object per language; that cannot be put
 * back in ledger order without the parameter-name table, so it is refused
 * rather than reordered by guesswork.
 */
export function toCostModels(value: unknown): CostModels | undefined {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) return undefined;
  const source = value as Record<string, unknown>;
  const models: CostModels = {};
  for (const language of PLUTUS_LANGUAGES) {
    const costs = source[language];
    if (costs === undefined || costs === null) continue;
    if (!Array.isArray(costs) || costs.length === 0 || !costs.every((c) => Number.isSafeInteger(c))) return undefined;
    models[language] = costs as number[];
  }
  return models;
}

/** A positive safe integer from a db column, or undefined. */
function toCount(value: DbNumber | null | undefined): number | undefined {
  if (value === null || value === undefined) return undefined;
  const n = Number(value);
  return Number.isSafeInteger(n) && n >= 0 ? n : undefined;
}

/**
 * A contract `ProtocolParams` from an `epoch_param` row, or `undefined` when
 * the row lacks a required parameter — which is the case for every epoch before
 * Conway, whose rows carry no governance parameters. Never fills a gap.
 */
export function mapProtocolParams(row: ProtocolParamsRow): ProtocolParams | undefined {
  const ratios: Record<string, Ratio | undefined> = {};
  const ratio = (key: keyof ProtocolParamsRow) => (ratios[key] = toParamRatio(row[key] as number | null));
  const drepThresholds = {
    motionNoConfidence: ratio('dvt_motion_no_confidence'),
    committeeNormal: ratio('dvt_committee_normal'),
    committeeNoConfidence: ratio('dvt_committee_no_confidence'),
    updateToConstitution: ratio('dvt_update_to_constitution'),
    hardForkInitiation: ratio('dvt_hard_fork_initiation'),
    ppNetworkGroup: ratio('dvt_p_p_network_group'),
    ppEconomicGroup: ratio('dvt_p_p_economic_group'),
    ppTechnicalGroup: ratio('dvt_p_p_technical_group'),
    ppGovGroup: ratio('dvt_p_p_gov_group'),
    treasuryWithdrawal: ratio('dvt_treasury_withdrawal'),
  };
  const poolThresholds = {
    motionNoConfidence: ratio('pvt_motion_no_confidence'),
    committeeNormal: ratio('pvt_committee_normal'),
    committeeNoConfidence: ratio('pvt_committee_no_confidence'),
    hardForkInitiation: ratio('pvt_hard_fork_initiation'),
    ppSecurityGroup: ratio('pvtpp_security_group'),
  };
  const rational = {
    minFeeRefScriptCostPerByte: toParamRatio(row.min_fee_ref_script_cost_per_byte),
    priceMem: toParamRatio(row.price_mem),
    priceStep: toParamRatio(row.price_step),
    poolPledgeInfluence: toParamRatio(row.influence),
    monetaryExpansion: toParamRatio(row.monetary_expand_rate),
    treasuryCut: toParamRatio(row.treasury_growth_rate),
  };
  const counts = {
    maxTxExMem: toCount(row.max_tx_ex_mem),
    maxTxExSteps: toCount(row.max_tx_ex_steps),
    maxBlockExMem: toCount(row.max_block_ex_mem),
    maxBlockExSteps: toCount(row.max_block_ex_steps),
    collateralPercentage: toCount(row.collateral_percent),
    maxCollateralInputs: toCount(row.max_collateral_inputs),
  };
  const costModels = toCostModels(row.cost_models);
  const required = [
    row.epoch_no,
    row.gov_action_lifetime,
    row.gov_action_deposit,
    row.drep_deposit,
    row.drep_activity,
    row.committee_min_size,
    row.committee_max_term_length,
    row.coins_per_utxo_size,
    row.max_val_size,
    costModels,
    ...Object.values(rational),
    ...Object.values(counts),
  ];
  if (required.some((v) => v === null || v === undefined) || Object.values(ratios).some((r) => r === undefined)) {
    return undefined;
  }
  return {
    epoch: toInt(row.epoch_no!),
    protocolVersion: { major: toInt(row.protocol_major), minor: toInt(row.protocol_minor) },
    govActionLifetime: toInt(row.gov_action_lifetime!),
    govActionDeposit: toLovelace(row.gov_action_deposit!),
    drepDeposit: toLovelace(row.drep_deposit!),
    drepActivity: toInt(row.drep_activity!),
    committeeMinSize: toInt(row.committee_min_size!),
    committeeMaxTermLength: toInt(row.committee_max_term_length!),
    drepThresholds: drepThresholds as DRepThresholds,
    poolThresholds: poolThresholds as PoolThresholds,
    minFeeA: toInt(row.min_fee_a),
    minFeeB: toInt(row.min_fee_b),
    minFeeRefScriptCostPerByte: rational.minFeeRefScriptCostPerByte!,
    keyDeposit: toLovelace(row.key_deposit),
    poolDeposit: toLovelace(row.pool_deposit),
    coinsPerUtxoByte: toLovelace(row.coins_per_utxo_size!),
    maxBlockBodySize: toInt(row.max_block_size),
    maxBlockHeaderSize: toInt(row.max_bh_size),
    maxTxSize: toInt(row.max_tx_size),
    maxValSize: toInt(row.max_val_size!),
    maxTxExecutionUnits: { memory: counts.maxTxExMem!, steps: counts.maxTxExSteps! },
    maxBlockExecutionUnits: { memory: counts.maxBlockExMem!, steps: counts.maxBlockExSteps! },
    collateralPercentage: counts.collateralPercentage!,
    maxCollateralInputs: counts.maxCollateralInputs!,
    executionUnitPrices: { memory: rational.priceMem!, steps: rational.priceStep! },
    costModels: costModels!,
    poolRetireMaxEpoch: toInt(row.max_epoch),
    stakePoolTargetNum: toInt(row.optimal_pool_count),
    poolPledgeInfluence: rational.poolPledgeInfluence!,
    monetaryExpansion: rational.monetaryExpansion!,
    treasuryCut: rational.treasuryCut!,
    minPoolCost: toLovelace(row.min_pool_cost),
  };
}

/* ------------------------------------------------------------------------- */
/* Stake distribution                                                          */
/* ------------------------------------------------------------------------- */

/**
 * Everything for the tip epoch in one round trip.
 *
 * - Active stake: `epoch_stake` for the tip epoch, the same figure Koios
 *   (`epoch_info.active_stake`) and Blockfrost (`network.stake.active`) report.
 *   Index on `epoch_no`; about 80k rows per epoch on preview, about 1.3M on
 *   mainnet, where this sum is the slowest part of the query.
 * - DRep breakdown: `drep_distr` for the tip epoch. Its only index leads with
 *   `hash_id`, so this is a sequential scan (1.4M rows on preview, ~0.2 s;
 *   mainnet has far fewer DReps, so it is smaller there).
 * - SPO breakdown: `pool_stat.voting_power` for the tip epoch. No index on
 *   `epoch_no`; a sequential scan of a table of pools x epochs.
 */
export const STAKE_DISTRIBUTION_SQL = `WITH ${TIP_EPOCH_CTE},
dd AS (
  SELECT count(*) AS n,
         sum(d.amount) FILTER (WHERE h.raw IS NOT NULL AND d.active_until >= d.epoch_no) AS active_dreps,
         sum(d.amount) FILTER (WHERE h.raw IS NULL AND h.view = 'drep_always_abstain') AS always_abstain,
         sum(d.amount) FILTER (WHERE h.raw IS NULL AND h.view = 'drep_always_no_confidence') AS always_no_confidence
    FROM drep_distr d JOIN drep_hash h ON h.id = d.hash_id
   WHERE d.epoch_no = (SELECT epoch_no FROM tip)
),
ps AS (
  SELECT count(*) AS n, count(voting_power) AS n_vp, sum(voting_power) AS voting_power
    FROM pool_stat WHERE epoch_no = (SELECT epoch_no FROM tip)
)
SELECT tip.epoch_no,
       (SELECT completed FROM epoch_stake_progress WHERE epoch_no = tip.epoch_no) AS stake_complete,
       (SELECT sum(amount) FROM epoch_stake WHERE epoch_no = tip.epoch_no) AS active_stake,
       dd.n AS drep_rows, dd.active_dreps, dd.always_abstain, dd.always_no_confidence,
       ps.n AS pool_rows, ps.n_vp AS pool_rows_with_vp, ps.voting_power AS pool_voting_power
  FROM tip, dd, ps`;

export interface StakeDistributionRow {
  epoch_no: number | null;
  stake_complete: boolean | null;
  active_stake: DbNumber | null;
  drep_rows: DbNumber;
  active_dreps: DbNumber | null;
  always_abstain: DbNumber | null;
  always_no_confidence: DbNumber | null;
  pool_rows: DbNumber;
  pool_rows_with_vp: DbNumber;
  pool_voting_power: DbNumber | null;
}

/**
 * Omitted versus zero, per SPEC.md §3.2: a DRep field is present only when
 * `drep_distr` has been computed for the epoch, and then a target nobody
 * delegates to is a known `"0"`. The SPO total is present only when every pool
 * row for the epoch carries a voting power. `totalLiveStake` is never served:
 * it needs the live UTxO of every delegated address, which is a full scan.
 */
export function mapStakeDistribution(row: StakeDistributionRow): StakeDistribution {
  if (row.epoch_no === null) throw staleData('db-sync has no blocks');
  if (row.stake_complete !== true || row.active_stake === null) {
    throw staleData('db-sync has not finished the stake snapshot for the current epoch', { epoch: row.epoch_no });
  }
  const out: StakeDistribution = { epoch: toInt(row.epoch_no), totalActiveStake: toLovelace(row.active_stake) };
  if (toInt(row.drep_rows) > 0) {
    out.totalStakeControlledByDReps = toLovelace(row.active_dreps ?? 0);
    out.alwaysAbstainVotingPower = toLovelace(row.always_abstain ?? 0);
    out.alwaysNoConfidenceVotingPower = toLovelace(row.always_no_confidence ?? 0);
  }
  const pools = toInt(row.pool_rows);
  if (pools > 0 && toInt(row.pool_rows_with_vp) === pools && row.pool_voting_power !== null) {
    out.totalStakeControlledBySPOs = toLovelace(row.pool_voting_power);
  }
  return out;
}

/* ------------------------------------------------------------------------- */
/* Treasury                                                                    */
/* ------------------------------------------------------------------------- */

/** `ada_pots` has one row per epoch (about 1.4k), unindexed on `epoch_no` but tiny. */
export const TREASURY_SQL = `WITH ${TIP_EPOCH_CTE}
SELECT tip.epoch_no AS tip_epoch, ap.epoch_no, ap.treasury, ap.reserves
  FROM tip
  LEFT JOIN LATERAL (
    SELECT epoch_no, treasury, reserves FROM ada_pots
     WHERE epoch_no = COALESCE($1::int, tip.epoch_no) ORDER BY id DESC LIMIT 1
  ) ap ON TRUE`;

interface TreasuryRow {
  tip_epoch: number | null;
  epoch_no: number | null;
  treasury: DbNumber | null;
  reserves: DbNumber | null;
}

/* ------------------------------------------------------------------------- */
/* API                                                                         */
/* ------------------------------------------------------------------------- */

interface TipRow extends BlockCols {
  proto_major: number;
  proto_minor: number;
  network_name: string | null;
}

export function createNetworkApi(ctx: Ctx): NetworkApi {
  /** A requested epoch with no row: after the tip it is not yet known; before it, not recorded. */
  const missingEpoch = (what: string, epoch: number, tipEpoch: number | null) =>
    notFound(
      tipEpoch !== null && epoch > tipEpoch
        ? `${what} for epoch ${epoch} are not known yet (current epoch is ${tipEpoch})`
        : `no ${what} recorded for epoch ${epoch}`,
      { epoch },
    );

  return {
    async getNetworkInfo() {
      const [row] = await ctx.db.query<TipRow>(
        `SELECT t.*, (SELECT network_name FROM meta ORDER BY id LIMIT 1) AS network_name FROM (${TIP_SQL}) t`,
      );
      if (!row) throw staleData('db-sync has no blocks');
      if (row.network_name && row.network_name !== ctx.network) {
        // Stake addresses would be encoded for the wrong network: a configuration error, not data.
        throw internal(`provider is configured for ${ctx.network} but the database follows ${row.network_name}`);
      }
      const tip = toStamp(row);
      const info: NetworkInfo = {
        network: ctx.network,
        era: eraOf(toInt(row.proto_major)),
        tip,
        currentEpoch: tip.epoch,
      };
      return ctx.envelope(info);
    },

    async getProtocolParams(q) {
      const epoch = parseEpoch(q?.epoch);
      const [row] = await ctx.db.query<ProtocolParamsRow>(PROTOCOL_PARAMS_SQL, [epoch ?? null]);
      if (!row || row.tip_epoch === null) throw staleData('db-sync has no blocks');
      if (row.epoch_no === null) {
        if (epoch !== undefined) throw missingEpoch('protocol parameters', epoch, row.tip_epoch);
        throw staleData('db-sync has not recorded the parameters for the current epoch yet', { epoch: row.tip_epoch });
      }
      const params = mapProtocolParams(row);
      if (!params) {
        if (epoch !== undefined) {
          throw notFound(`epoch ${epoch} predates the Conway governance parameters`, { epoch });
        }
        throw internal(`epoch_param for epoch ${row.epoch_no} lacks a required parameter`);
      }
      return ctx.envelope(params);
    },

    async getStakeDistribution() {
      const [row] = await ctx.db.query<StakeDistributionRow>(STAKE_DISTRIBUTION_SQL);
      if (!row) throw staleData('db-sync has no blocks');
      return ctx.envelope(mapStakeDistribution(row));
    },

    async getTreasury(q) {
      const epoch = parseEpoch(q?.epoch);
      const [row] = await ctx.db.query<TreasuryRow>(TREASURY_SQL, [epoch ?? null]);
      if (!row || row.tip_epoch === null) throw staleData('db-sync has no blocks');
      if (row.epoch_no === null || row.treasury === null || row.reserves === null) {
        if (epoch !== undefined) throw missingEpoch('ada pots', epoch, row.tip_epoch);
        throw staleData('db-sync has not recorded the ada pots for the current epoch yet', { epoch: row.tip_epoch });
      }
      const treasury: Treasury = {
        epoch: toInt(row.epoch_no),
        balance: toLovelace(row.treasury),
        reserves: toLovelace(row.reserves),
      };
      return ctx.envelope(treasury);
    },
  };
}

/** `getProtocolParams({ epoch })` is served: `epoch_param` keeps one row per epoch. */
export const NETWORK_OPTIONAL_ARGUMENTS: OptionalArgument[] = ['protocolParams.epoch'];
