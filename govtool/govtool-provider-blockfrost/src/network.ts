/**
 * NetworkApi over Blockfrost (SPEC.md §5.1).
 *
 *   getNetworkInfo        /blocks/latest, /epochs/latest/parameters (era), /genesis
 *   getProtocolParams     /epochs/{latest|n}/parameters — past epochs are served
 *   getStakeDistribution  /epochs/latest, /network, and the DRep directory
 *   getTreasury           /network (current epoch only)
 *   getGenesisParams      /genesis
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

import { loadClock, parseEpoch, type BfEpoch } from './chain';
import type { Ctx, Session } from './context';
import { internal, notFound, unsupported } from './errors';
import { loadDirectory } from './governance/dreps/directory';
import { big, isoFromUnix, toInt, toLovelace } from './numbers';
import { toRatio } from './ratio';

/** `/epochs/{n}/parameters`, the fields read. Governance fields are null before Conway. */
export interface BfParams {
  epoch: number;
  protocol_major_ver: number;
  protocol_minor_ver: number;
  min_fee_a: number;
  min_fee_b: number;
  max_tx_size: number;
  key_deposit: string;
  pool_deposit: string;
  max_val_size: string | null;
  coins_per_utxo_size: string | null;
  gov_action_lifetime: string | null;
  gov_action_deposit: string | null;
  drep_deposit: string | null;
  drep_activity: string | null;
  committee_min_size: string | null;
  committee_max_term_length: string | null;
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
  min_fee_ref_script_cost_per_byte: number | null;
  max_block_size: number | null;
  max_block_header_size: number | null;
  /** Execution budgets arrive as decimal strings. */
  max_tx_ex_mem: string | null;
  max_tx_ex_steps: string | null;
  max_block_ex_mem: string | null;
  max_block_ex_steps: string | null;
  collateral_percent: number | null;
  max_collateral_inputs: number | null;
  price_mem: number | null;
  price_step: number | null;
  /**
   * Per language, the integer array in ledger parameter order. Read instead
   * of `cost_models`, which is keyed by parameter NAME: its key order is not
   * the ledger's, so it cannot rebuild the array a script data hash is over.
   */
  cost_models_raw: Record<string, unknown> | null;
  e_max: number | null;
  n_opt: number | null;
  a0: number | null;
  rho: number | null;
  tau: number | null;
  min_pool_cost: string | null;
}

/** `/genesis`, the Shelley genesis. */
export interface BfGenesis {
  active_slots_coefficient: number | string | null;
  update_quorum: number | null;
  max_lovelace_supply: string | null;
  network_magic: number | null;
  epoch_length: number | null;
  /** UNIX seconds. */
  system_start: number | null;
  slots_per_kes_period: number | null;
  /** Seconds. */
  slot_length: number | null;
  max_kes_evolutions: number | null;
  security_param: number | null;
}

interface BfBlock {
  time: number;
  height: number | null;
  slot: number | null;
  epoch: number | null;
}

interface BfNetwork {
  supply: { treasury: string; reserves: string };
  stake: { live: string; active: string };
}

/** Genesis network magic, to catch a provider pointed at the wrong network. */
const MAGIC: Record<string, number> = { mainnet: 764824073, preprod: 1, preview: 2 };

/**
 * Era by protocol major version. Blockfrost reports no era name, so it is read
 * off the protocol version in force. Versions 9 to 11 are all Conway. An
 * unknown version is refused rather than guessed.
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

/** The Plutus languages the contract names, in ledger order. */
export const PLUTUS_LANGUAGES: readonly PlutusLanguage[] = ['PlutusV1', 'PlutusV2', 'PlutusV3'];

/**
 * Cost models as the contract carries them: per language, the integer array
 * in ledger parameter order, exactly as the source lists it (never re-sorted,
 * since the script data hash is computed over that order). A language absent
 * from the source is absent from the result. `undefined` when the value is not
 * an object of integer arrays.
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
 * Blockfrost serves thresholds and the other rationals as floats (`0.67`,
 * `7.21e-05`), as db-sync stores them. A short decimal is taken as written and
 * anything longer is reconstructed by bounded-denominator continued fractions
 * (SPEC.md §3.2); see ./ratio. Execution budgets arrive as strings and must be
 * safe integers (steps are ~2e10); `toInt` refuses anything else.
 */
export function mapProtocolParams(row: BfParams): ProtocolParams | undefined {
  const ratios: Record<string, Ratio | undefined> = {};
  const ratio = (key: keyof BfParams) => (ratios[key] = toRatio(row[key] as number | null));
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
  const minFeeRefScriptCostPerByte = ratio('min_fee_ref_script_cost_per_byte');
  const priceMemory = ratio('price_mem');
  const priceSteps = ratio('price_step');
  const poolPledgeInfluence = ratio('a0');
  const monetaryExpansion = ratio('rho');
  const treasuryCut = ratio('tau');
  const costModels = toCostModels(row.cost_models_raw);
  const required = [
    row.gov_action_lifetime,
    row.gov_action_deposit,
    row.drep_deposit,
    row.drep_activity,
    row.committee_min_size,
    row.committee_max_term_length,
    row.coins_per_utxo_size,
    row.max_val_size,
    row.max_block_size,
    row.max_block_header_size,
    row.max_tx_ex_mem,
    row.max_tx_ex_steps,
    row.max_block_ex_mem,
    row.max_block_ex_steps,
    row.collateral_percent,
    row.max_collateral_inputs,
    row.e_max,
    row.n_opt,
    row.min_pool_cost,
  ];
  if (
    !costModels ||
    required.some((v) => v === null || v === undefined) ||
    Object.values(ratios).some((r) => r === undefined)
  ) {
    return undefined;
  }
  return {
    epoch: toInt(row.epoch, 'epoch'),
    protocolVersion: { major: toInt(row.protocol_major_ver, 'protocol_major_ver'), minor: toInt(row.protocol_minor_ver, 'protocol_minor_ver') },
    govActionLifetime: toInt(row.gov_action_lifetime, 'gov_action_lifetime'),
    govActionDeposit: toLovelace(row.gov_action_deposit, 'gov_action_deposit'),
    drepDeposit: toLovelace(row.drep_deposit, 'drep_deposit'),
    drepActivity: toInt(row.drep_activity, 'drep_activity'),
    committeeMinSize: toInt(row.committee_min_size, 'committee_min_size'),
    committeeMaxTermLength: toInt(row.committee_max_term_length, 'committee_max_term_length'),
    drepThresholds: drepThresholds as DRepThresholds,
    poolThresholds: poolThresholds as PoolThresholds,
    minFeeA: toInt(row.min_fee_a, 'min_fee_a'),
    minFeeB: toInt(row.min_fee_b, 'min_fee_b'),
    minFeeRefScriptCostPerByte: minFeeRefScriptCostPerByte!,
    keyDeposit: toLovelace(row.key_deposit, 'key_deposit'),
    poolDeposit: toLovelace(row.pool_deposit, 'pool_deposit'),
    coinsPerUtxoByte: toLovelace(row.coins_per_utxo_size, 'coins_per_utxo_size'),
    maxBlockBodySize: toInt(row.max_block_size, 'max_block_size'),
    maxBlockHeaderSize: toInt(row.max_block_header_size, 'max_block_header_size'),
    maxTxSize: toInt(row.max_tx_size, 'max_tx_size'),
    maxValSize: toInt(row.max_val_size, 'max_val_size'),
    maxTxExecutionUnits: { memory: toInt(row.max_tx_ex_mem, 'max_tx_ex_mem'), steps: toInt(row.max_tx_ex_steps, 'max_tx_ex_steps') },
    maxBlockExecutionUnits: {
      memory: toInt(row.max_block_ex_mem, 'max_block_ex_mem'),
      steps: toInt(row.max_block_ex_steps, 'max_block_ex_steps'),
    },
    collateralPercentage: toInt(row.collateral_percent, 'collateral_percent'),
    maxCollateralInputs: toInt(row.max_collateral_inputs, 'max_collateral_inputs'),
    executionUnitPrices: { memory: priceMemory!, steps: priceSteps! },
    costModels,
    poolRetireMaxEpoch: toInt(row.e_max, 'e_max'),
    stakePoolTargetNum: toInt(row.n_opt, 'n_opt'),
    poolPledgeInfluence: poolPledgeInfluence!,
    monetaryExpansion: monetaryExpansion!,
    treasuryCut: treasuryCut!,
    minPoolCost: toLovelace(row.min_pool_cost, 'min_pool_cost'),
  };
}

/** The contract `GenesisParams` from `/genesis`. A missing field is an internal fault. */
export function mapGenesisParams(row: BfGenesis): GenesisParams {
  const need = <T>(value: T | null | undefined, what: string): T => {
    if (value === null || value === undefined) throw internal(`Blockfrost genesis lacks ${what}`);
    return value;
  };
  const networkMagic = toInt(need(row.network_magic, 'network_magic'), 'network_magic');
  const slotLength = need(row.slot_length, 'slot_length');
  if (typeof slotLength !== 'number' || !Number.isFinite(slotLength) || slotLength <= 0) throw internal('Blockfrost genesis has no slot length');
  const activeSlotsCoefficient = toRatio(need(row.active_slots_coefficient, 'active_slots_coefficient'));
  if (!activeSlotsCoefficient || activeSlotsCoefficient.denominator <= 0) throw internal('Blockfrost genesis active_slots_coefficient is not a rational');
  return {
    networkMagic,
    // Blockfrost does not send the network id; mainnet's magic is the only one whose genesis says Mainnet.
    networkId: networkMagic === MAGIC['mainnet'] ? 'Mainnet' : 'Testnet',
    systemStart: isoFromUnix(toInt(need(row.system_start, 'system_start'), 'system_start')),
    epochLength: toInt(need(row.epoch_length, 'epoch_length'), 'epoch_length'),
    slotLength,
    activeSlotsCoefficient,
    securityParam: toInt(need(row.security_param, 'security_param'), 'security_param'),
    slotsPerKesPeriod: toInt(need(row.slots_per_kes_period, 'slots_per_kes_period'), 'slots_per_kes_period'),
    maxKesEvolutions: toInt(need(row.max_kes_evolutions, 'max_kes_evolutions'), 'max_kes_evolutions'),
    updateQuorum: toInt(need(row.update_quorum, 'update_quorum'), 'update_quorum'),
    maxLovelaceSupply: toLovelace(need(row.max_lovelace_supply, 'max_lovelace_supply'), 'max_lovelace_supply'),
  };
}

/** The parameters in force now, loaded once per call. */
export const loadCurrentParams = (s: Session): Promise<BfParams> =>
  s.once('params:latest', () => s.http.get<BfParams>('/epochs/latest/parameters'));

export function createNetworkApi(ctx: Ctx): NetworkApi {
  const checkMagic = (genesis: BfGenesis) => {
    const expected = MAGIC[ctx.network];
    if (expected !== undefined && genesis.network_magic !== expected) {
      // Stake addresses would be encoded for the wrong network: a configuration error, not data.
      throw internal(`provider is configured for ${ctx.network} but Blockfrost serves network magic ${genesis.network_magic}`);
    }
  };

  return {
    async getNetworkInfo() {
      const s = ctx.session();
      const [tip, params, genesis] = await Promise.all([
        s.http.get<BfBlock>('/blocks/latest'),
        loadCurrentParams(s),
        s.http.get<BfGenesis>('/genesis'),
      ]);
      checkMagic(genesis);
      if (tip.epoch === null) throw internal('Blockfrost returned a tip with no epoch');
      const info: NetworkInfo = {
        network: ctx.network,
        era: eraOf(toInt(params.protocol_major_ver, 'protocol_major_ver')),
        tip: {
          epoch: tip.epoch,
          ...(tip.slot === null ? {} : { slot: tip.slot }),
          ...(tip.height === null ? {} : { block: tip.height }),
          time: isoFromUnix(tip.time),
        },
        currentEpoch: tip.epoch,
      };
      return ctx.envelope(info);
    },

    async getProtocolParams(q) {
      const epoch = parseEpoch(q?.epoch);
      const s = ctx.session();
      const row =
        epoch === undefined
          ? await loadCurrentParams(s)
          : await s.http.getOrNull<BfParams>(`/epochs/${epoch}/parameters`);
      if (!row) throw notFound(`no protocol parameters for epoch ${epoch}`, { epoch });
      const params = mapProtocolParams(row);
      if (!params) {
        if (epoch !== undefined) throw notFound(`epoch ${epoch} predates the Conway governance parameters`, { epoch });
        throw internal(`Blockfrost's parameters for epoch ${row.epoch} lack a required parameter`);
      }
      return ctx.envelope(params);
    },

    /**
     * `totalStakeControlledBySPOs` is the epoch's active stake: the SPO voting
     * distribution IS the active stake distribution by pool, so the two totals
     * are one figure. The DRep breakdown reads the DRep directory (18 pages on
     * mainnet): active = neither retired nor expired, as in the ledger.
     */
    async getStakeDistribution() {
      const s = ctx.session();
      const [epoch, network, directory] = await Promise.all([
        s.http.get<BfEpoch>('/epochs/latest'),
        s.http.get<BfNetwork>('/network'),
        loadDirectory(s),
      ]);
      if (epoch.active_stake === null) throw internal('Blockfrost has no active stake for the current epoch');
      let dreps = 0n;
      for (const d of directory.dreps) if (d.status === 'active') dreps += big(d.amount);
      const out: StakeDistribution = {
        epoch: epoch.epoch,
        totalActiveStake: toLovelace(epoch.active_stake, 'active_stake'),
        totalLiveStake: toLovelace(network.stake.live, 'stake.live'),
        totalStakeControlledByDReps: dreps.toString(),
        totalStakeControlledBySPOs: toLovelace(epoch.active_stake, 'active_stake'),
        alwaysAbstainVotingPower: directory.alwaysAbstain,
        alwaysNoConfidenceVotingPower: directory.alwaysNoConfidence,
      };
      return ctx.envelope(out);
    },

    async getGenesisParams() {
      const genesis = await ctx.session().http.get<BfGenesis>('/genesis');
      checkMagic(genesis);
      return ctx.envelope(mapGenesisParams(genesis));
    },

    async getTreasury(q) {
      const epoch = parseEpoch(q?.epoch);
      const s = ctx.session();
      const [clock, network] = await Promise.all([loadClock(s), s.http.get<BfNetwork>('/network')]);
      // `/network` is the live ada pots; there is no per-epoch history of them.
      if (epoch !== undefined && epoch !== clock.epoch) throw unsupported(`the treasury at a past epoch (${epoch})`);
      const treasury: Treasury = {
        epoch: clock.epoch,
        balance: toLovelace(network.supply.treasury, 'supply.treasury'),
        reserves: toLovelace(network.supply.reserves, 'supply.reserves'),
      };
      return ctx.envelope(treasury);
    },
  };
}

/** `getProtocolParams({ epoch })` is served: `/epochs/{n}/parameters` answers for any past epoch. */
export const NETWORK_OPTIONAL_ARGUMENTS: OptionalArgument[] = ['protocolParams.epoch'];
