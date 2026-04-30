import { Controller, Get, Param, Res } from '@nestjs/common';
import type { Response } from 'express';

import { AdaHolderService } from './ada-holder.service';

@Controller('ada-holder')
export class AdaHolderController {
  constructor(private readonly adaHolderService: AdaHolderService) {}

  @Get('get-current-delegation/:stakeKey')
  async getCurrentDelegation(
    @Param('stakeKey') stakeKey: string,
    @Res() response: Response,
  ): Promise<void> {
    const delegation = await this.adaHolderService.getCurrentDelegation(stakeKey);
    response.status(200).json(delegation);
  }

  @Get('get-voting-power/:stakeKey')
  getVotingPower(@Param('stakeKey') stakeKey: string): Promise<number> {
    return this.adaHolderService.getVotingPower(stakeKey);
  }
}
