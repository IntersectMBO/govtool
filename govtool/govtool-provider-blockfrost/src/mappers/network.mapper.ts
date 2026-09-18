import type {
  BlockSummary,
  ChainPoint,
  EpochSummary,
  NetworkInfo,
  ProtocolParams,
  StakeDistribution,
} from '@govtool/data-providers/chain-data';

import { internal } from '../common/errors';
import {
  asFiniteNumber,
  asLovelace,
  toIsoFromUnixSeconds,
} from '../common/numbers';
import type {
  BfBlock,
  BfEpoch,
  BfEpochParameters,
  BfGenesis,
} from '../http/types';

/** Mainnet's magic; anything else is matched by the genesis value. */
const NETWORK_BY_MAGIC: Record<number, string> = {
  764824073: 'mainnet',
  1: 'preprod',
  2: 'preview',
  4: 'sanchonet',
};

export function toChainPoint(block: BfBlock): ChainPoint {
  const epoch = block.epoch;
  const height = block.height;
  if (epoch === null || height === null) {
    throw internal('Blockfrost returned a block with no epoch or height');
  }
  const point: ChainPoint = {
    epoch,
    block: height,
    blockHash: block.hash,
    time: toIsoFromUnixSeconds(block.time),
  };
  if (block.slot !== null) point.slot = block.slot;
  return point;
}

/**
 * Network identity comes from `/genesis` and the tip from `/blocks/latest`.
 * The era is not reported by either, so it is omitted rather than guessed
 * from the protocol version.
 */
export function mapNetworkInfo(input: {
  genesis: BfGenesis;
  tip: BfBlock;
  epoch: BfEpoch | null;
}): NetworkInfo {
  const tip = toChainPoint(input.tip);
  const info: NetworkInfo = {
    network: NETWORK_BY_MAGIC[input.genesis.network_magic] ?? 'unknown',
    networkMagic: input.genesis.network_magic,
    tip,
    epoch: { no: tip.epoch },
  };
  if (input.epoch !== null) {
    info.epoch = {
      no: input.epoch.epoch,
      startTime: toIsoFromUnixSeconds(input.epoch.start_time),
      endTime: toIsoFromUnixSeconds(input.epoch.end_time),
    };
  }
  return info;
}

export function mapEpochSummary(epoch: BfEpoch): EpochSummary {
  return {
    epoch: epoch.epoch,
    startTime: toIsoFromUnixSeconds(epoch.start_time),
    endTime: toIsoFromUnixSeconds(epoch.end_time),
    durationSeconds: epoch.end_time - epoch.start_time,
  };
}

export function mapBlockSummary(block: BfBlock): BlockSummary {
  const point = toChainPoint(block);
  return {
    block: point.block,
    blockHash: block.hash,
    slot: block.slot ?? 0,
    epoch: point.epoch,
    time: toIsoFromUnixSeconds(block.time),
    txCount: block.tx_count,
  };
}

/**
 * Only `totalActiveStake` — the epoch's own snapshot, and the one figure a
 * tally denominator needs.
 *
 * The governance breakdown (stake by DReps, by SPOs, and the two predefined
 * options) is not derivable: it means summing the whole DRep distribution,
 * which on a per-entity HTTP API is one request per DRep. Blockfrost's
 * `/network` endpoint would carry the supply and stake totals, but this
 * deployment answers it with a 500.
 */
export function mapStakeDistribution(epoch: BfEpoch): StakeDistribution {
  const distribution: StakeDistribution = { epoch: epoch.epoch };
  if (epoch.active_stake !== null) {
    distribution.totalActiveStake = epoch.active_stake;
  }
  return distribution;
}

/**
 * `/epochs/{n}/parameters` is the richest protocol-parameter source of any
 * provider surveyed: every governance parameter is present and named, so all
 * the typed scalars are filled.
 *
 * The one gap is shared with db-sync: `dvt_*` / `pvt_*` thresholds arrive as
 * floating-point, and the contract wants exact ratios. They stay in `raw`.
 */
export function mapProtocolParams(raw: BfEpochParameters): ProtocolParams {
  const epoch = asFiniteNumber(raw.epoch);
  if (epoch === undefined) {
    throw internal('Blockfrost returned parameters with no epoch');
  }

  const params: ProtocolParams = { epoch, raw };

  const assign = <K extends keyof ProtocolParams>(
    key: K,
    value: ProtocolParams[K] | undefined,
  ): void => {
    if (value !== undefined) params[key] = value;
  };

  assign('govActionDeposit', asLovelace(raw.gov_action_deposit));
  assign('drepDeposit', asLovelace(raw.drep_deposit));
  assign('keyDeposit', asLovelace(raw.key_deposit));
  assign('poolDeposit', asLovelace(raw.pool_deposit));
  assign('coinsPerUtxoByte', asLovelace(raw.coins_per_utxo_size));

  assign('minFeeA', asFiniteNumber(raw.min_fee_a));
  assign('minFeeB', asFiniteNumber(raw.min_fee_b));
  assign(
    'minFeeRefScriptCostPerByte',
    asFiniteNumber(raw.min_fee_ref_script_cost_per_byte),
  );
  assign('govActionLifetime', asFiniteNumber(raw.gov_action_lifetime));
  assign('drepActivity', asFiniteNumber(raw.drep_activity));
  assign('committeeMinSize', asFiniteNumber(raw.committee_min_size));
  assign(
    'committeeMaxTermLength',
    asFiniteNumber(raw.committee_max_term_length),
  );

  const major = asFiniteNumber(raw.protocol_major_ver);
  const minor = asFiniteNumber(raw.protocol_minor_ver);
  if (major !== undefined && minor !== undefined) {
    params.protocolVersion = { major, minor };
  }

  // `dvt` / `pvt` stay unset. Blockfrost reports every threshold as a
  // double (`dvt_motion_no_confidence: 0.67`), exactly as db-sync stores
  // them, and a float cannot be turned back into the on-chain
  // numerator/denominator. Reconstructing one would make a wrong ratio look
  // authoritative, so they are left in `raw` for a caller that accepts the
  // float. This is the only governance parameter gap on this provider.

  return params;
}
