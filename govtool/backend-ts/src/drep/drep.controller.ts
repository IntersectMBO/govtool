import { Controller, Get, Param, Query } from '@nestjs/common';

import { DRepService } from './drep.service';
import { DRepInfoResponse, DRepVotingPowerListResponse , DRepListResponse, DRepStatus,} from './drep.type';
import type { DRepListSort} from './drep.type';

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
  @Get('list')
  getList(
  @Query('search') search?: string,
  @Query('status') status?: string | string[],
  @Query('sort') sort?: DRepListSort,
  @Query('page') page?: string,
  @Query('pageSize') pageSize?: string,
  @Query('seed') seed?: string,
    ): Promise<DRepListResponse> {
  return this.drepService.list({
    search,
    status: this.normalizeQueryArray(status) as DRepStatus[],
    sort,
    page: page === undefined ? 0 : Number(page),
    pageSize: pageSize === undefined ? 10 : Number(pageSize),
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

