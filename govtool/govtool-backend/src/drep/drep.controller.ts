import { Controller, Get, Param, Query } from '@nestjs/common';

import type { ApiInteger } from 'src/common/integer';
import { MAX_PAGE_SIZE, parsePageValue } from 'src/common/pagination';
import { queryEnum, queryEnums } from 'src/common/query-enum';

import { DRepService } from './drep.service';
import {
  DRepInfoResponse,
  DRepVotingPowerListResponse,
  DRepListResponse,
  VoteResponse,
} from './drep.type';
import { drepListSorts, drepStatuses } from './drep.type';
import type { DRepListSort } from './drep.type';
import {
  governanceActionSortModes,
  governanceActionTypes,
} from 'src/proposal/proposal.type';
import type { GovernanceActionSortMode } from 'src/proposal/proposal.type';

@Controller('drep')
export class DRepController {
  constructor(private readonly drepService: DRepService) {}

  @Get('get-voting-power/:drepId')
  getVotingPower(@Param('drepId') drepId: string): Promise<ApiInteger> {
    return this.drepService.getVotingPower(drepId);
  }

  @Get('voting-power-list')
  getVotingPowerList(
    @Query('identifiers') identifiers?: string | string[],
  ): Promise<DRepVotingPowerListResponse[]> {
    return this.drepService.getVotingPowerList(
      this.normalizeQueryArray(identifiers),
    );
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
      queryEnums(type, typeArray, governanceActionTypes, 'type'),
      queryEnum(sort, governanceActionSortModes, 'sort'),
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
      status: queryEnums(status, statusArray, drepStatuses, 'status'),
      sort: queryEnum(sort, drepListSorts, 'sort'),
      page: parsePageValue(page, 0, 'page'),
      pageSize: parsePageValue(pageSize, 10, 'pageSize', MAX_PAGE_SIZE),
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
