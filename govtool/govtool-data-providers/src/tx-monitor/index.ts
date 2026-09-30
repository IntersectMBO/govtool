/**
 * Transaction Monitor v1 — progress for a transaction GovTool just helped
 * construct. Optional (SPEC.md §10).
 *
 * The only PUSH interface in this system; everything else is request/response.
 * It exists because MEMPOOL VISIBILITY IS SOMETHING NO LEDGER-DERIVED SOURCE
 * HAS — a transaction in the mempool is not on chain yet, so chain data cannot
 * see it at all.
 */

import type { Hex } from '../chain-data/common';

export interface TxUpdate {
  txHash: Hex;
  state: 'mempool' | 'confirmed';
  /** Counted to 5; beyond that a transaction is settled for GovTool's purposes. */
  confirmations?: number;
  /** Where a user can go to see it. */
  explorerUrl?: string;
}

export interface TransactionMonitorV1 {
  /**
   * Watch a transaction. The callback is invoked on each change and feeds a
   * websocket or an internal listener.
   */
  add(txHash: string, callback: (update: TxUpdate) => void): void;
}
