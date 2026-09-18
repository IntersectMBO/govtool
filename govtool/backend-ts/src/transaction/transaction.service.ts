import { Injectable } from '@nestjs/common';

import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';

type TransactionStatus = {
    tx_exists: boolean;
    voting_procedures: unknown[] | null;
};

export type TransactionStatusResponse = {
  transactionConfirmed: boolean;
  votingProcedure: unknown[] | null;
};



@Injectable() 
    export class TransactionService {
        constructor (
            private readonly dbService: DbService,
            private readonly sqlService: SqlService,
        ) {}
   
        async getTransactionStatus(
            transactionId: string,
        ): Promise<TransactionStatusResponse | null > {
            const sql = this.sqlService.load('get-transaction-status.sql');
            const result = await this.dbService.query<TransactionStatus>(sql, [
                transactionId,
                transactionId,
            ]);
           
            if (result.rows.length !== 1) {
                return null;
            }

            const row = result.rows[0];
            
            return {
                transactionConfirmed: row.tx_exists,
                votingProcedure: row.voting_procedures,
            };
        }
   
    }
