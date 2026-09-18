import type {
  BlockSummary,
  ChainPoint,
  EpochSummary,
  NetworkId,
  ProtocolParams,
  Treasury,
} from '@govtool/data-providers/chain-data';

import { internal } from '../common/errors';
import {
  toInteger,
  toIsoString,
  toLovelace,
  toNullableInteger,
  toNullableLovelace,
  toStrictInteger,
} from '../common/numbers';
import type {
  BlockRow,
  EpochInfoRow,
  EpochParamsRow,
  TipRow,
  TotalsRow,
} from '../rows';

/** The magics of the networks GovTool deploys against. */
const NETWORK_BY_MAGIC: Record<string, NetworkId> = {
  '764824073': 'mainnet',
  '1': 'preprod',
  '2': 'preview',
};

/**
 * `/genesis` reports the magic, not a name. An unrecognised magic is passed
 * through as `testnet-<magic>` rather than guessed at — the contract's
 * `NetworkId` admits free text for exactly this case.
 */
export function networkFromMagic(magic: string | null | undefined): NetworkId {
  if (magic === null || magic === undefined) return 'mainnet';
  return NETWORK_BY_MAGIC[magic] ?? `testnet-${magic}`;
}

/**
 * The tip, with every coordinate Koios reports.
 *
 * `block_no` and `block_height` are the same number under two names: the
 * deployment sends both, the published spec documents only one, and which one
 * that is has changed between releases.
 */
export function mapTip(row: TipRow): ChainPoint {
  const block = row.block_no ?? row.block_height;
  if (block === null || block === undefined) {
    throw internal('Koios returned a tip with no block height.');
  }
  return {
    epoch: toStrictInteger(row.epoch_no),
    block: toStrictInteger(block),
    slot: toStrictInteger(row.abs_slot),
    blockHash: row.hash,
    time: toIsoString(row.block_time),
  };
}

export function mapEpochSummary(row: EpochInfoRow): EpochSummary {
  return {
    epoch: toStrictInteger(row.epoch_no),
    startTime: toIsoString(row.start_time),
    endTime: toIsoString(row.end_time),
    durationSeconds: toInteger(row.end_time) - toInteger(row.start_time),
  };
}

export function mapBlockSummary(row: BlockRow): BlockSummary {
  if (row.block_height === null) {
    throw internal('Koios returned a block with no height.');
  }
  return {
    block: toStrictInteger(row.block_height),
    blockHash: row.hash,
    slot: toStrictInteger(row.abs_slot),
    epoch: toStrictInteger(row.epoch_no),
    time: toIsoString(row.block_time),
    txCount: toInteger(row.tx_count),
  };
}

/**
 * Protocol parameters, with the thresholds deliberately left out.
 *
 * `dvt_*` and `pvt_*` arrive as IEEE-754 doubles (`0.67`, `0.51`) because
 * that is how db-sync stores them, and the contract's `Ratio` is an exact
 * on-chain numerator/denominator. `0.67` is not `2/3`, and any reconstruction
 * would be a guess that a UI would then render as fact — so the typed `dvt`
 * and `pvt` fields stay `undefined` and the floats are available in `raw`.
 * This is the same gap `@govtool/provider-dbsync` reports, for the same
 * reason: it is the ledger-to-db-sync hop that loses the ratio, not the
 * provider.
 */
export function mapProtocolParams(row: EpochParamsRow): ProtocolParams {
  return {
    epoch: toStrictInteger(row.epoch_no),
    govActionDeposit: toNullableLovelace(row.gov_action_deposit) ?? undefined,
    drepDeposit: toNullableLovelace(row.drep_deposit) ?? undefined,
    keyDeposit: toNullableLovelace(row.key_deposit) ?? undefined,
    poolDeposit: toNullableLovelace(row.pool_deposit) ?? undefined,
    minFeeA: toNullableInteger(row.min_fee_a) ?? undefined,
    minFeeB: toNullableInteger(row.min_fee_b) ?? undefined,
    minFeeRefScriptCostPerByte: row.min_fee_ref_script_cost_per_byte,
    coinsPerUtxoByte: toNullableLovelace(row.coins_per_utxo_size),
    govActionLifetime: toNullableInteger(row.gov_action_lifetime) ?? undefined,
    drepActivity: toNullableInteger(row.drep_activity) ?? undefined,
    committeeMinSize: toNullableInteger(row.committee_min_size) ?? undefined,
    committeeMaxTermLength:
      toNullableInteger(row.committee_max_term_length) ?? undefined,
    protocolVersion:
      row.protocol_major === null
        ? undefined
        : {
            major: toInteger(row.protocol_major),
            minor: toInteger(row.protocol_minor ?? 0),
          },
    raw: row as unknown as Record<string, unknown>,
  };
}

/**
 * Treasury and reserves for one epoch. `delta` needs the previous epoch's
 * row, which the caller fetches in the same request by asking for two.
 */
export function mapTreasury(row: TotalsRow, previous?: TotalsRow): Treasury {
  const treasury: Treasury = {
    epoch: toStrictInteger(row.epoch_no),
    balance: toLovelace(row.treasury),
    reserves: toLovelace(row.reserves),
  };
  if (previous !== undefined) {
    treasury.delta = String(BigInt(row.treasury) - BigInt(previous.treasury));
  }
  return treasury;
}
