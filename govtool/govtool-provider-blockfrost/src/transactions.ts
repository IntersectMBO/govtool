/**
 * TransactionsApi over Blockfrost (SPEC.md §5.5): whether a transaction is on
 * chain, nothing more. `/txs/{hash}`.
 *
 * A hash Blockfrost answers 404 for is `onChain: false` — the true answer
 * whether the transaction is still in a mempool or never existed — not
 * NOT_FOUND. Telling those apart is the transaction-monitoring component's job.
 */
import type { TransactionState, TransactionsApi } from '@govtool/data-providers/chain-data';

import { loadClock, loadTx, stampOf } from './chain';
import type { Ctx } from './context';
import { invalidInput } from './errors';
import { isHex } from './ids';

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
      const s = ctx.session();
      const tx = await loadTx(s, hash);
      const state: TransactionState = tx
        ? { txHash: hash, onChain: true, includedAt: stampOf(tx, await loadClock(s)) }
        : { txHash: hash, onChain: false };
      return ctx.envelope(state);
    },
  };
}
