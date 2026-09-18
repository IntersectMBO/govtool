import type {
  Envelope,
  TransactionsApi,
  TransactionState,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../common/errors';

/**
 * Not implemented.
 *
 * Confirmation status needs `/txs/{hash}`, which this deployment answers with
 * a 500 for every hash — including a transaction taken from the tip block.
 * There is no other route that can tell a submitted transaction apart from an
 * unknown one, and reporting `unknown` for a confirmed transaction would
 * leave GovTool's post-submission screens spinning forever, so this fails
 * loudly instead. The gap is declared at `system.getCapabilities()`.
 */
export class BlockfrostTransactionsApi implements TransactionsApi {
  get(_txHash: string): Promise<Envelope<TransactionState>> {
    return Promise.reject(
      unsupported(
        'transactions.get',
        '/txs/{hash} returns 500 on this deployment for every hash',
      ),
    );
  }
}
