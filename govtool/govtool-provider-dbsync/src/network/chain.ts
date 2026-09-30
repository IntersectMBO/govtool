/**
 * Chain-coordinate helpers shared by the network, accounts and transactions
 * areas: the tip, epoch arguments, and dating a row by its block.
 */
import { ChainDataError, type EpochStamp } from '@govtool/data-providers/chain-data';

import { invalidInput } from '../errors';
import { toInt, toIso } from '../numbers';

/**
 * The newest block. Walks the primary key backwards, so it is an index read on
 * any size of database. `block_no IS NULL` only on Byron epoch-boundary blocks.
 */
export const TIP_SQL = `SELECT epoch_no, slot_no, block_no, time, proto_major, proto_minor
  FROM block WHERE block_no IS NOT NULL ORDER BY id DESC LIMIT 1`;

/** The same, as a CTE body yielding just the tip epoch. */
export const TIP_EPOCH_CTE = `tip AS (SELECT epoch_no FROM block WHERE block_no IS NOT NULL ORDER BY id DESC LIMIT 1)`;

type DbNumber = number | string | bigint;

/** The block columns a row carries to be dated. */
export interface BlockCols {
  epoch_no: DbNumber | null;
  slot_no: DbNumber | null;
  block_no: DbNumber | null;
  time: Date | string | null;
}

/** An `EpochStamp` from a row's block columns. `epoch` is always known on a Shelley-or-later block. */
export function toStamp(row: BlockCols): EpochStamp {
  if (row.epoch_no === null) throw new ChainDataError('INTERNAL', 'block has no epoch');
  return {
    epoch: toInt(row.epoch_no),
    ...(row.slot_no === null ? {} : { slot: toInt(row.slot_no) }),
    ...(row.block_no === null ? {} : { block: toInt(row.block_no) }),
    ...(row.time === null ? {} : { time: toIso(row.time) }),
  };
}

/** Validate an optional `epoch` argument: a non-negative integer, or absent. */
export function parseEpoch(value: unknown): number | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0) {
    throw invalidInput('epoch must be a non-negative integer', { epoch: value });
  }
  return value;
}

/** The source has not caught up with the tip yet; retrying shortly will succeed. */
export const staleData = (message: string, details?: Record<string, unknown>) =>
  new ChainDataError('STALE_DATA', message, { retryable: true, ...(details ? { details } : {}) });
