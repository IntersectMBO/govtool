/**
 * NetworkApi over Koios (SPEC.md §5.1).
 *
 * Endpoints: `/tip`, `/genesis` (network check and the genesis constants), `/epoch_params` (one row per
 * epoch, so past epochs are served), `/epoch_info` (active stake), `/drep_info`
 * (the predefined targets and the DRep directory), `/pool_voting_power_history`
 * (the SPO distribution), `/totals` (treasury and reserves, per epoch).
 *
 * Thresholds arrive as IEEE-754 doubles (`0.67`); they are reconstructed as
 * exact rationals by `ratio.toRatio` (bounded continued fractions, SPEC.md §3.2).
 */
import type {
  CostModels,
  DRepThresholds,
  GenesisParams,
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

import { parseEpoch } from './chain';
import type { Ctx } from './context';
import { internal, notFound, staleData } from './errors';
import { loadDRepInfo, drepStatusOf } from './governance/dreps/directory';
import { toInt, toIso, toLovelace } from './numbers';
import { toRatio } from './ratio';
import type { EpochInfoRow, EpochParamsRow, GenesisRow, TotalsRow, VotingPowerHistoryRow } from './rows';

/** `getProtocolParams({ epoch })` is served: `/epoch_params` keeps one row per epoch. */
export const NETWORK_OPTIONAL_ARGUMENTS: OptionalArgument[] = ['protocolParams.epoch'];

const DREP_THRESHOLD_COLUMNS: Record<keyof DRepThresholds, keyof EpochParamsRow> = {
  motionNoConfidence: 'dvt_motion_no_confidence',
  committeeNormal: 'dvt_committee_normal',
  committeeNoConfidence: 'dvt_committee_no_confidence',
  updateToConstitution: 'dvt_update_to_constitution',
  hardForkInitiation: 'dvt_hard_fork_initiation',
  ppNetworkGroup: 'dvt_p_p_network_group',
  ppEconomicGroup: 'dvt_p_p_economic_group',
  ppTechnicalGroup: 'dvt_p_p_technical_group',
  ppGovGroup: 'dvt_p_p_gov_group',
  treasuryWithdrawal: 'dvt_treasury_withdrawal',
};

const POOL_THRESHOLD_COLUMNS: Record<keyof PoolThresholds, keyof EpochParamsRow> = {
  motionNoConfidence: 'pvt_motion_no_confidence',
  committeeNormal: 'pvt_committee_normal',
  committeeNoConfidence: 'pvt_committee_no_confidence',
  hardForkInitiation: 'pvt_hard_fork_initiation',
  ppSecurityGroup: 'pvtpp_security_group',
};

function group<K extends string>(row: EpochParamsRow, columns: Record<K, keyof EpochParamsRow>): Record<K, Ratio> | undefined {
  const out = {} as Record<K, Ratio>;
  for (const [key, column] of Object.entries(columns) as [K, keyof EpochParamsRow][]) {
    const ratio = toRatio(row[column] as number | null);
    if (!ratio) return undefined;
    out[key] = ratio;
  }
  return out;
}

/** The Plutus languages the contract names, in ledger order. */
export const PLUTUS_LANGUAGES: readonly PlutusLanguage[] = ['PlutusV1', 'PlutusV2', 'PlutusV3'];

/**
 * Cost models as the contract carries them: per language, the integer array
 * in ledger parameter order, exactly as the source lists it (never re-sorted,
 * since the script data hash is computed over that order). A language absent
 * from the source is absent from the result. `undefined` when the value is not
 * an object of integer arrays; a named-map rendering is refused rather than
 * ordered by guesswork.
 */
export function toCostModels(value: unknown): CostModels | undefined {
  if (typeof value !== 'object' || value === null || Array.isArray(value)) return undefined;
  const source = value as Record<string, unknown>;
  const out: CostModels = {};
  for (const language of PLUTUS_LANGUAGES) {
    const costs = source[language];
    if (costs === undefined || costs === null) continue;
    if (!Array.isArray(costs) || costs.length === 0) return undefined;
    const ints = costs.map((c) => (typeof c === 'string' && /^-?\d+$/.test(c) ? Number(c) : c));
    if (!ints.every((c) => typeof c === 'number' && Number.isSafeInteger(c))) return undefined;
    out[language] = ints as number[];
  }
  return out;
}

/**
 * A contract `ProtocolParams` from an `/epoch_params` row, or `undefined` when
 * the row lacks a required parameter — the case for every epoch before Conway,
 * whose rows carry no governance parameters. Never fills a gap.
 *
 * `price_mem`, `price_step`, `influence`, `monetary_expand_rate` and
 * `treasury_growth_rate` arrive as doubles (`7.21e-05`); they are short
 * decimals, so `toRatio` takes them exactly as written (721/10000000).
 */
export function mapProtocolParams(row: EpochParamsRow): ProtocolParams | undefined {
  const drepThresholds = group(row, DREP_THRESHOLD_COLUMNS);
  const poolThresholds = group(row, POOL_THRESHOLD_COLUMNS);
  const ratios = {
    minFeeRefScriptCostPerByte: toRatio(row.min_fee_ref_script_cost_per_byte),
    priceMemory: toRatio(row.price_mem),
    priceSteps: toRatio(row.price_step),
    poolPledgeInfluence: toRatio(row.influence),
    monetaryExpansion: toRatio(row.monetary_expand_rate),
    treasuryCut: toRatio(row.treasury_growth_rate),
  };
  const costModels = toCostModels(row.cost_models);
  const required = [
    row.protocol_major,
    row.protocol_minor,
    row.gov_action_lifetime,
    row.gov_action_deposit,
    row.drep_deposit,
    row.drep_activity,
    row.committee_min_size,
    row.committee_max_term_length,
    row.key_deposit,
    row.pool_deposit,
    row.coins_per_utxo_size,
    row.min_fee_a,
    row.min_fee_b,
    row.max_tx_size,
    row.max_val_size,
    row.max_block_size,
    row.max_bh_size,
    row.max_tx_ex_mem,
    row.max_tx_ex_steps,
    row.max_block_ex_mem,
    row.max_block_ex_steps,
    row.collateral_percent,
    row.max_collateral_inputs,
    row.max_epoch,
    row.optimal_pool_count,
    row.min_pool_cost,
  ];
  if (
    !drepThresholds ||
    !poolThresholds ||
    !costModels ||
    Object.values(ratios).some((r) => r === undefined) ||
    required.some((v) => v === null || v === undefined)
  ) {
    return undefined;
  }
  return {
    epoch: toInt(row.epoch_no),
    protocolVersion: { major: toInt(row.protocol_major!), minor: toInt(row.protocol_minor!) },
    govActionLifetime: toInt(row.gov_action_lifetime!),
    govActionDeposit: toLovelace(row.gov_action_deposit!),
    drepDeposit: toLovelace(row.drep_deposit!),
    drepActivity: toInt(row.drep_activity!),
    committeeMinSize: toInt(row.committee_min_size!),
    committeeMaxTermLength: toInt(row.committee_max_term_length!),
    drepThresholds,
    poolThresholds,
    minFeeA: toInt(row.min_fee_a!),
    minFeeB: toInt(row.min_fee_b!),
    minFeeRefScriptCostPerByte: ratios.minFeeRefScriptCostPerByte!,
    keyDeposit: toLovelace(row.key_deposit!),
    poolDeposit: toLovelace(row.pool_deposit!),
    coinsPerUtxoByte: toLovelace(row.coins_per_utxo_size!),
    maxBlockBodySize: toInt(row.max_block_size!, 'max_block_size'),
    maxBlockHeaderSize: toInt(row.max_bh_size!, 'max_bh_size'),
    maxTxSize: toInt(row.max_tx_size!),
    maxValSize: toInt(row.max_val_size!),
    maxTxExecutionUnits: { memory: toInt(row.max_tx_ex_mem!, 'max_tx_ex_mem'), steps: toInt(row.max_tx_ex_steps!, 'max_tx_ex_steps') },
    maxBlockExecutionUnits: {
      memory: toInt(row.max_block_ex_mem!, 'max_block_ex_mem'),
      steps: toInt(row.max_block_ex_steps!, 'max_block_ex_steps'),
    },
    collateralPercentage: toInt(row.collateral_percent!, 'collateral_percent'),
    maxCollateralInputs: toInt(row.max_collateral_inputs!, 'max_collateral_inputs'),
    executionUnitPrices: { memory: ratios.priceMemory!, steps: ratios.priceSteps! },
    costModels,
    poolRetireMaxEpoch: toInt(row.max_epoch!, 'max_epoch'),
    stakePoolTargetNum: toInt(row.optimal_pool_count!, 'optimal_pool_count'),
    poolPledgeInfluence: ratios.poolPledgeInfluence!,
    monetaryExpansion: ratios.monetaryExpansion!,
    treasuryCut: ratios.treasuryCut!,
    minPoolCost: toLovelace(row.min_pool_cost!, 'min_pool_cost'),
  };
}

/**
 * The contract `GenesisParams` from Koios' `/genesis` row. Every value but
 * `systemstart` (UNIX seconds) arrives as a string; `activeslotcoeff` is a
 * short decimal (`0.05`), taken exactly. A missing field is an internal fault.
 */
export function mapGenesisParams(row: GenesisRow): GenesisParams {
  const need = <T>(value: T | null | undefined, what: string): T => {
    if (value === null || value === undefined || value === '') throw internal(`Koios genesis lacks ${what}`);
    return value;
  };
  const networkId = need(row.networkid, 'networkid');
  if (networkId !== 'Mainnet' && networkId !== 'Testnet') throw internal(`Koios genesis has an unknown networkid ${networkId}`);
  const slotLength = Number(need(row.slotlength, 'slotlength'));
  if (!Number.isFinite(slotLength) || slotLength <= 0) throw internal('Koios genesis has no slot length');
  const activeSlotsCoefficient = toRatio(need(row.activeslotcoeff, 'activeslotcoeff'));
  if (!activeSlotsCoefficient || activeSlotsCoefficient.denominator <= 0) throw internal('Koios genesis activeslotcoeff is not a rational');
  return {
    networkMagic: toInt(need(row.networkmagic, 'networkmagic'), 'networkmagic'),
    networkId,
    systemStart: toIso(toInt(need(row.systemstart, 'systemstart'), 'systemstart')),
    epochLength: toInt(need(row.epochlength, 'epochlength'), 'epochlength'),
    slotLength,
    activeSlotsCoefficient,
    securityParam: toInt(need(row.securityparam, 'securityparam'), 'securityparam'),
    slotsPerKesPeriod: toInt(need(row.slotsperkesperiod, 'slotsperkesperiod'), 'slotsperkesperiod'),
    maxKesEvolutions: toInt(need(row.maxkesrevolutions, 'maxkesrevolutions'), 'maxkesrevolutions'),
    updateQuorum: toInt(need(row.updatequorum, 'updatequorum'), 'updatequorum'),
    maxLovelaceSupply: toLovelace(need(row.maxlovelacesupply, 'maxlovelacesupply'), 'maxlovelacesupply'),
  };
}

/** Koios names the era (`Conway`); the contract carries it lowercase, as db-sync's provider does. */
export function eraName(era: string | null | undefined): string {
  if (!era) throw internal('Koios tip carries no era');
  return era.toLowerCase();
}

export function createNetworkApi(ctx: Ctx): NetworkApi {
  const missingEpoch = (what: string, epoch: number, tipEpoch: number) =>
    notFound(
      epoch > tipEpoch
        ? `${what} for epoch ${epoch} are not known yet (current epoch is ${tipEpoch})`
        : `no ${what} recorded for epoch ${epoch}`,
      { epoch },
    );

  return {
    async getNetworkInfo() {
      const [tip] = await Promise.all([ctx.chain.tip(), ctx.chain.genesis()]);
      const info: NetworkInfo = {
        network: ctx.network,
        era: eraName(tip.era),
        tip: {
          epoch: tip.epoch_no,
          slot: tip.abs_slot,
          ...((tip.block_height ?? tip.block_no) == null ? {} : { block: (tip.block_height ?? tip.block_no)! }),
          time: new Date(tip.block_time * 1000).toISOString().replace('.000Z', 'Z'),
        },
        currentEpoch: tip.epoch_no,
      };
      return ctx.envelope(info);
    },

    async getProtocolParams(q) {
      const requested = parseEpoch(q?.epoch);
      const tip = await ctx.chain.tip();
      const epoch = requested ?? tip.epoch_no;
      const { rows } = await ctx.http.get<EpochParamsRow>('epoch_params', { _epoch_no: epoch });
      const row = rows.find((r) => r.epoch_no === epoch);
      if (!row) {
        if (requested !== undefined) throw missingEpoch('protocol parameters', epoch, tip.epoch_no);
        throw staleData('Koios has not recorded the parameters for the current epoch yet', { epoch });
      }
      const params = mapProtocolParams(row);
      if (!params) {
        if (requested !== undefined) throw notFound(`epoch ${epoch} predates the Conway governance parameters`, { epoch });
        throw internal(`Koios epoch_params for epoch ${epoch} lacks a required parameter`);
      }
      return ctx.envelope(params);
    },

    /**
     * `totalActiveStake` from `/epoch_info` (the pool snapshot for the
     * epoch), the predefined targets from `/drep_info`, the SPO figure as the
     * sum of `/pool_voting_power_history` for the epoch, and the DRep figure as
     * the stake of every ACTIVE registered DRep — the same definition the
     * db-sync provider uses, which needs the whole directory (one `/drep_info`
     * POST per ~70 DReps). `totalLiveStake` is not served: Koios has no
     * un-snapshotted network total.
     */
    async getStakeDistribution() {
      const tip = await ctx.chain.tip();
      const epoch = tip.epoch_no;
      const [{ rows: info }, pools, directory] = await Promise.all([
        ctx.http.get<EpochInfoRow>('epoch_info', { _epoch_no: epoch, _include_next_epoch: false }, { select: 'epoch_no,active_stake' }),
        ctx.http.getAll<VotingPowerHistoryRow>('pool_voting_power_history', { _epoch_no: epoch }, { select: 'pool_id_bech32,amount' }),
        loadDRepInfo(ctx, { includePredefined: true }),
      ]);
      const active = info[0]?.active_stake;
      if (active === null || active === undefined) {
        throw staleData('Koios has no active stake for the current epoch yet', { epoch });
      }
      const out: StakeDistribution = { epoch, totalActiveStake: toLovelace(active) };

      const abstain = directory.predefined.get('drep_always_abstain');
      const noConfidence = directory.predefined.get('drep_always_no_confidence');
      if (abstain?.amount != null && noConfidence?.amount != null) {
        let dreps = 0n;
        for (const row of directory.dreps.values()) {
          if (drepStatusOf(row, epoch) === 'active' && row.amount !== null) dreps += BigInt(toLovelace(row.amount));
        }
        out.totalStakeControlledByDReps = dreps.toString();
        out.alwaysAbstainVotingPower = toLovelace(abstain.amount);
        out.alwaysNoConfidenceVotingPower = toLovelace(noConfidence.amount);
      }
      if (pools.length > 0 && pools.every((p) => p.amount !== null)) {
        out.totalStakeControlledBySPOs = pools.reduce((sum, p) => sum + BigInt(toLovelace(p.amount!)), 0n).toString();
      }
      return ctx.envelope(out);
    },

    /** Read once and kept: genesis constants never change (see ./chain). */
    async getGenesisParams() {
      return ctx.envelope(mapGenesisParams(await ctx.chain.genesis()));
    },

    async getTreasury(q) {
      const requested = parseEpoch(q?.epoch);
      const tip = await ctx.chain.tip();
      const epoch = requested ?? tip.epoch_no;
      const { rows } = await ctx.http.get<TotalsRow>('totals', { _epoch_no: epoch });
      const row = rows.find((r) => r.epoch_no === epoch);
      if (!row || row.treasury === null || row.reserves === null) {
        if (requested !== undefined) throw missingEpoch('ada pots', epoch, tip.epoch_no);
        throw staleData('Koios has not recorded the ada pots for the current epoch yet', { epoch });
      }
      const treasury: Treasury = { epoch, balance: toLovelace(row.treasury), reserves: toLovelace(row.reserves) };
      return ctx.envelope(treasury);
    },
  };
}
