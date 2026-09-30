import { Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { asHttp, required } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';

export type TransactionStatusResponse = {
  transactionConfirmed: boolean;
  votingProcedure: unknown[] | null;
};

@Injectable()
export class TransactionService {
  constructor(@Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1) {}

  async getTransactionStatus(
    transactionId: string,
  ): Promise<TransactionStatusResponse | null> {
    return asHttp(async () => {
      const transactions = required(this.chain.transactions, 'transactions');
      const { data } = await transactions.get(transactionId);
      return {
        transactionConfirmed: data.onChain,
        // A vote is reachable only through its DRep or its governance action,
        // so there is no way to ask what a transaction hash voted on. The
        // legacy field stays in the response as an empty list rather than
        // disappearing from the shape.
        votingProcedure: [],
      };
    });
  }
}
