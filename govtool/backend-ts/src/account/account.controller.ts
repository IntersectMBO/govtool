import { Controller, Get, Param } from '@nestjs/common';

import { AccountService } from './account.service';
import { AccountInfoResponse } from './account.type';

@Controller('account')
export class AccountController {
  constructor(private readonly accountService: AccountService) {}

  @Get(':stakeKey')
  getAccountInfo(@Param('stakeKey') stakeKey: string): Promise<AccountInfoResponse> {
    return this.accountService.getAccountInfo(stakeKey);
  }
}
