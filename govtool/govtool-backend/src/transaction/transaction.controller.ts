import { Controller, Get, Param } from '@nestjs/common';

import { assertHexText } from 'src/common/hex';
import {
  TransactionService,
  TransactionStatusResponse,
} from './transaction.service';

@Controller('transaction')
export class TransactionController {
  constructor(private readonly transactionService: TransactionService) {}

  @Get('status/:transactionId')
  getTransactionStatus(
    @Param('transactionId') transactionId: string,
  ): Promise<TransactionStatusResponse | null> {
    assertHexText(transactionId);
    return this.transactionService.getTransactionStatus(transactionId);
  }
}
