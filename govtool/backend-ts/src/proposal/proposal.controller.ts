import { parsePageValue } from 'src/common/pagination';
import { Controller, Get, Param, Query, Res } from '@nestjs/common';
import type { Response } from 'express';

import { ProposalService } from './proposal.service';
import {
  EnactedProposalDetailsResponse,
  GetProposalResponse,
  GovernanceActionType,
  ListProposalsResponse,
} from './proposal.type';
import type {GovernanceActionSortMode} from './proposal.type'
@Controller('proposal')
export class ProposalController {
  constructor(private readonly proposalService: ProposalService) {}

  @Get('list')
  list(
    @Query('type') type?: string | string[],
    @Query('type[]') typeArray?: string | string[],
    @Query('sort') sort?: GovernanceActionSortMode,
    @Query('page') page?: string,
    @Query('pageSize') pageSize?: string,
    @Query('drepId') drepId?: string,
    @Query('search') search?: string,
  ): Promise<ListProposalsResponse> {
    return this.proposalService.list({
      type: [...this.normalizeQueryArray(type), ...this.normalizeQueryArray(typeArray)] as GovernanceActionType[],
      sort,
      page: parsePageValue(page, 0, 'page'),
      pageSize: parsePageValue(pageSize, 10, 'pageSize'),
      drepId,
      search,
    });
  }

  @Get('get/:proposalId')
  get(
    @Param('proposalId') proposalId: string,
    @Query('drepId') drepId?: string,
  ): Promise<GetProposalResponse> {
    return this.proposalService.get(proposalId, drepId);
  }

  @Get('enacted-details')
  async getEnactedDetails(
    @Query('type') type: GovernanceActionType | undefined,
    @Res() response: Response,
  ): Promise<void> {
    const details = await this.proposalService.getEnactedDetails(type);
    response.status(200).json(details);
  }

  private normalizeQueryArray(value?: string | string[]): string[] {
    if (value === undefined) {
      return [];
    }

    return Array.isArray(value) ? value : [value];
  }
}
