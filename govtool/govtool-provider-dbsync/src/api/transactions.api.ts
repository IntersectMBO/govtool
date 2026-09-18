import type {
  Envelope,
  TransactionsApi,
  TransactionState,
} from '@govtool/data-providers/chain-data';

import { assertHexText } from '../common/hex';
import { envelope } from '../common/meta';
import { runSql } from '../db/run';
import type { Queryable } from '../db/queryable';
import { mapTransactionStatusRow } from '../mappers/account.mapper';
import type { TransactionStatusRow } from '../rows';

export class DbSyncTransactionsApi implements TransactionsApi {
  constructor(private readonly db: Queryable) {}

  /**
   * `status` is `confirmed` or `unknown`: db-sync only knows transactions it
   * has indexed, so "not found" cannot be told apart from "still in the
   * mempool". `effects` is not classified — the raw voting procedures are
   * returned on `votingProcedures` instead.
   */
  async get(txHash: string): Promise<Envelope<TransactionState>> {
    assertHexText(txHash);
    const rows = await runSql<TransactionStatusRow>(
      this.db,
      'get-transaction-status.sql',
      [txHash, txHash],
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      return envelope({ txHash, status: 'unknown' });
    }
    return envelope(mapTransactionStatusRow(txHash, row));
  }
}
