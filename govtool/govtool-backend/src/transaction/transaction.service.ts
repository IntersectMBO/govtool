import { Inject, Injectable } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { asHttp } from 'src/common/errors';
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
      const { data } = await this.chain.transactions.get(transactionId);
      return {
        transactionConfirmed: data.status === 'confirmed',
        votingProcedure: data.votingProcedures ?? null,
      };
    });
  }
}
