import { parsePageValue } from 'src/common/pagination';
import { Controller, Get, Param, Query } from '@nestjs/common';

import { DRepService } from './drep.service';
import { DRepInfoResponse, DRepVotingPowerListResponse , DRepListResponse, DRepStatus,VoteResponse} from './drep.type';
import type { DRepListSort} from './drep.type';
import {
  GovernanceActionType,
} from 'src/proposal/proposal.type';
import type {  GovernanceActionSortMode} from 'src/proposal/proposal.type';


@Controller('drep')
export class DRepController {
  constructor(private readonly drepService: DRepService) {}

  @Get('get-voting-power/:drepId')
  getVotingPower(@Param('drepId') drepId: string): Promise<number> {
    return this.drepService.getVotingPower(drepId);
  }

  @Get('voting-power-list')
  getVotingPowerList(
    @Query('identifiers') identifiers?: string | string[],
  ): Promise<DRepVotingPowerListResponse[]> {
    return this.drepService.getVotingPowerList(this.normalizeQueryArray(identifiers));
  }

  @Get('info/:drepId')
  getInfo(@Param('drepId') drepId: string): Promise<DRepInfoResponse> {
    return this.drepService.getInfo(drepId);
  }

  @Get('getVotes/:drepId')
    getVotes(
    @Param('drepId') drepId: string,
    @Query('type') type?: string | string[],
    @Query('type[]') typeArray?: string | string[],
    @Query('sort') sort?: GovernanceActionSortMode,
    @Query('search') search?: string,
    ): Promise<VoteResponse[]> {
    return this.drepService.getVotes(
        drepId,
        [...this.normalizeQueryArray(type), ...this.normalizeQueryArray(typeArray)] as GovernanceActionType[],
        sort,
        search,
    );
}


  @Get('list')
  getList(
  @Query('search') search?: string,
  @Query('status') status?: string | string[],
  @Query('status[]') statusArray?: string | string[],
  @Query('sort') sort?: DRepListSort,
  @Query('page') page?: string,
  @Query('pageSize') pageSize?: string,
  @Query('seed') seed?: string,
    ): Promise<DRepListResponse> {
  return this.drepService.list({
    search,
    status: [
      ...this.normalizeQueryArray(status),
      ...this.normalizeQueryArray(statusArray),
    ] as DRepStatus[],
    sort,
    page: parsePageValue(page, 0, 'page'),
    pageSize: parsePageValue(pageSize, 10, 'pageSize'),
    seed,
  });

    }
  private normalizeQueryArray(value?: string | string[]): string[] {
    if (value === undefined) {
      return [];
    }

    return Array.isArray(value) ? value : [value];
  }
}

