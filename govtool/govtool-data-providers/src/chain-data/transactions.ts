/**
 * Chain Data API — `/transactions/*`
 *
 * Post-submission confirmation. The required surface is whether the transaction
 * is on chain — nothing more. Richer progress (mempool presence, confirmation
 * depth) is the optional transaction-monitoring component, which sees things a
 * ledger-derived source cannot.
 */

import type { EpochStamp, Envelope, Hex } from './common';

export interface TransactionState {
  txHash: Hex;
  /** Whether the transaction is on chain. */
  onChain: boolean;
  includedAt?: EpochStamp;
}

export interface TransactionsApi {
  get(txHash: string): Promise<Envelope<TransactionState>>;
}
