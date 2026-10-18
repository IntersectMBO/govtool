/**
 * TransactionsApi over Koios (SPEC.md §5.5): whether a transaction is on
 * chain, nothing more. One `/tx_info` POST with every optional section off.
 *
 * A hash Koios has not seen is `onChain: false` — the true answer whether the
 * transaction is still in a mempool or never existed — not NOT_FOUND. Telling
 * those apart is the optional transaction-monitoring component's job.
 */
import type { TransactionState, TransactionsApi } from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { invalidInput } from './errors';
import { isHex } from './ids';
import { toIso } from './numbers';
import type { TxInfoRow } from './rows';

export function parseTxHash(value: unknown): string {
  if (typeof value !== 'string' || !isHex(value.trim(), 32)) {
    throw invalidInput('txHash must be 64 hex characters', { txHash: value });
  }
  return value.trim().toLowerCase();
}

const TX_INFO_FLAGS = {
  _inputs: false,
  _metadata: false,
  _assets: false,
  _withdrawals: false,
  _certs: false,
  _scripts: false,
  _bytecode: false,
  _governance: false,
};

export function createTransactionsApi(ctx: Ctx): TransactionsApi {
  return {
    async get(txHash) {
      const hash = parseTxHash(txHash);
      const { rows } = await ctx.http.post<TxInfoRow>(
        'tx_info',
        { _tx_hashes: [hash], ...TX_INFO_FLAGS },
        undefined,
        { select: 'tx_hash,block_height,epoch_no,absolute_slot,tx_timestamp' },
      );
      const row = rows.find((r) => r.tx_hash === hash);
      const state: TransactionState = row
        ? {
            txHash: hash,
            onChain: true,
            includedAt: {
              epoch: row.epoch_no,
              slot: row.absolute_slot,
              ...(row.block_height === null ? {} : { block: row.block_height }),
              time: toIso(row.tx_timestamp),
            },
          }
        : { txHash: hash, onChain: false };
      return ctx.envelope(state);
    },
  };
}
