import { Inject, Injectable } from '@nestjs/common';
import type {
  ChainDataApiV1,
  ProtocolParams,
  Ratio,
} from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp } from 'src/common/errors';
import { dbInteger, type ApiInteger } from 'src/common/integer';
import { CHAIN_DATA } from 'src/providers/providers.module';
import type {
  LegacyEpochParams,
  LegacyParamColumns,
  LegacyParamProposal,
} from './epoch.type';

/**
 * db-sync stored each threshold as a double (`0.67`); the contract carries the
 * exact rational. Dividing gives back the double db-sync held. A zero
 * denominator is not a ratio, so it is reported as unknown rather than as
 * Infinity.
 */
function ratioToFloat(ratio: Ratio): number | null {
  const { numerator, denominator } = ratio;
  if (
    !Number.isFinite(numerator) ||
    !Number.isFinite(denominator) ||
    denominator === 0
  ) {
    return null;
  }
  return numerator / denominator;
}

const lovelace = (value: string | undefined): ApiInteger | null =>
  value === undefined ? null : dbInteger(value);

const float = (ratio: Ratio | undefined): number | null =>
  ratio === undefined ? null : ratioToFloat(ratio);

const orNull = <T>(value: T | undefined): T | null => value ?? null;

/**
 * Contract parameters → the legacy snake_case columns, null where the object
 * does not set one. Only contract fields are read, so nothing a provider adds
 * on a subtype reaches the wire (D2). Lovelace goes through `dbInteger`, like
 * every other legacy lovelace field: a JSON number, exact above the safe
 * range. Rationals become the double db-sync stored.
 */
export function toLegacyParamColumns(
  params: Partial<ProtocolParams>,
): LegacyParamColumns {
  const dvt = params.drepThresholds;
  const pvt = params.poolThresholds;
  return {
    min_fee_a: orNull(params.minFeeA),
    min_fee_b: orNull(params.minFeeB),
    max_block_size: orNull(params.maxBlockBodySize),
    max_tx_size: orNull(params.maxTxSize),
    max_bh_size: orNull(params.maxBlockHeaderSize),
    key_deposit: lovelace(params.keyDeposit),
    pool_deposit: lovelace(params.poolDeposit),
    max_epoch: orNull(params.poolRetireMaxEpoch),
    optimal_pool_count: orNull(params.stakePoolTargetNum),
    influence: float(params.poolPledgeInfluence),
    monetary_expand_rate: float(params.monetaryExpansion),
    treasury_growth_rate: float(params.treasuryCut),
    decentralisation: null,
    protocol_major: orNull(params.protocolVersion?.major),
    protocol_minor: orNull(params.protocolVersion?.minor),
    min_utxo_value: null,
    min_pool_cost: lovelace(params.minPoolCost),
    price_mem: float(params.executionUnitPrices?.memory),
    price_step: float(params.executionUnitPrices?.steps),
    max_tx_ex_mem: orNull(params.maxTxExecutionUnits?.memory),
    max_tx_ex_steps: orNull(params.maxTxExecutionUnits?.steps),
    max_block_ex_mem: orNull(params.maxBlockExecutionUnits?.memory),
    max_block_ex_steps: orNull(params.maxBlockExecutionUnits?.steps),
    max_val_size: orNull(params.maxValSize),
    collateral_percent: orNull(params.collateralPercentage),
    max_collateral_inputs: orNull(params.maxCollateralInputs),
    coins_per_utxo_size: lovelace(params.coinsPerUtxoByte),
    pvt_motion_no_confidence: float(pvt?.motionNoConfidence),
    pvt_committee_normal: float(pvt?.committeeNormal),
    pvt_committee_no_confidence: float(pvt?.committeeNoConfidence),
    pvt_hard_fork_initiation: float(pvt?.hardForkInitiation),
    pvtpp_security_group: float(pvt?.ppSecurityGroup),
    dvt_motion_no_confidence: float(dvt?.motionNoConfidence),
    dvt_committee_normal: float(dvt?.committeeNormal),
    dvt_committee_no_confidence: float(dvt?.committeeNoConfidence),
    dvt_update_to_constitution: float(dvt?.updateToConstitution),
    dvt_hard_fork_initiation: float(dvt?.hardForkInitiation),
    dvt_p_p_network_group: float(dvt?.ppNetworkGroup),
    dvt_p_p_economic_group: float(dvt?.ppEconomicGroup),
    dvt_p_p_technical_group: float(dvt?.ppTechnicalGroup),
    dvt_p_p_gov_group: float(dvt?.ppGovGroup),
    dvt_treasury_withdrawal: float(dvt?.treasuryWithdrawal),
    committee_min_size: orNull(params.committeeMinSize),
    committee_max_term_length: orNull(params.committeeMaxTermLength),
    gov_action_lifetime: orNull(params.govActionLifetime),
    gov_action_deposit: lovelace(params.govActionDeposit),
    drep_deposit: lovelace(params.drepDeposit),
    drep_activity: orNull(params.drepActivity),
    min_fee_ref_script_cost_per_byte: float(params.minFeeRefScriptCostPerByte),
    cost_model:
      params.costModels === undefined
        ? null
        : { id: null, costs: params.costModels, hash: null },
  };
}

/** `ProtocolParams` → the legacy `/epoch/params` row. */
export function toLegacyEpochParams(params: ProtocolParams): LegacyEpochParams {
  // Babbage (protocol 7) removed `d` and `minUTxOValue`; db-sync has written 0
  // for both since, so 0 is what the row held, not an invented value.
  const removedInBabbage = params.protocolVersion.major >= 7 ? 0 : null;
  return {
    // db-sync bookkeeping, dropped from the contract by D1. No source, and an
    // invented id would be worse than none.
    id: null,
    epoch_no: params.epoch,
    nonce: null,
    cost_model_id: null,
    block_id: null,
    extra_entropy: null,
    ...toLegacyParamColumns(params),
    decentralisation: removedInBabbage,
    min_utxo_value: removedInBabbage,
  };
}

/** A ParameterChange body's `changes` → the legacy `param_proposal` row. */
export function toLegacyParamProposal(
  changes: Partial<ProtocolParams>,
): LegacyParamProposal {
  return {
    id: null,
    epoch_no: null,
    key: null,
    entropy: null,
    cost_model_id: null,
    registered_tx_id: null,
    ...toLegacyParamColumns(changes),
  };
}

@Injectable()
export class EpochService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  /**
   * The legacy endpoint returned db-sync's `epoch_param` row verbatim, and the
   * frontend still reads its column names. The contract's `ProtocolParams` is
   * mapped back to that shape here, at the wire, and nowhere else.
   */
  async getCurrentEpochParams(): Promise<LegacyEpochParams> {
    return this.cacheService.getOrSet('currentEpochParams', 'default', () =>
      asHttp(async () => {
        const { data } = await this.chain.network.getProtocolParams();
        return toLegacyEpochParams(data);
      }),
    );
  }
}
