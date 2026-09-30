/**
 * TransactionsApi over db-sync (SPEC.md §5.5): whether a transaction is on
 * chain, nothing more. A lookup on `tx.hash`, which is unique-indexed.
 *
 * A hash db-sync has not seen is `onChain: false` — the true answer whether the
 * transaction is still in a mempool or never existed — not NOT_FOUND. Telling
 * those two apart is the optional transaction-monitoring component's job.
 *
 * A transaction whose scripts failed phase-2 validation (`valid_contract =
 * false`) is on chain — its collateral was taken — but its certificates and
 * votes did not take effect. The contract carries no field for that, so it
 * reports `onChain: true` like any other included transaction.
 */
import type { TransactionState, TransactionsApi } from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { invalidInput } from './errors';
import { isHex } from './ids';
import { toStamp, type BlockCols } from './network/chain';

export const TRANSACTION_SQL = `SELECT b.epoch_no, b.slot_no, b.block_no, b.time
  FROM tx JOIN block b ON b.id = tx.block_id
 WHERE tx.hash = decode($1, 'hex')`;

export function parseTxHash(value: unknown): string {
  if (typeof value !== 'string' || !isHex(value.trim(), 32)) {
    throw invalidInput('txHash must be 64 hex characters', { txHash: value });
  }
  return value.trim().toLowerCase();
}

export function createTransactionsApi(ctx: Ctx): TransactionsApi {
  return {
    async get(txHash) {
      const hash = parseTxHash(txHash);
      const [row] = await ctx.db.query<BlockCols>(TRANSACTION_SQL, [hash]);
      const state: TransactionState = row
        ? { txHash: hash, onChain: true, includedAt: toStamp(row) }
        : { txHash: hash, onChain: false };
      return ctx.envelope(state);
    },
  };
}
