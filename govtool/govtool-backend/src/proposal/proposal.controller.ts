import { Controller, Get, Param, Query, Res } from '@nestjs/common';
import type { Response } from 'express';

import { DRepService } from 'src/drep/drep.service';
import type { VoteResponse } from 'src/drep/drep.type';
import { tryLegacyDRepCandidates } from 'src/common/legacy-ids';
import { ProposalService } from './proposal.service';
import {
  GetProposalResponse,
  GovernanceActionType,
  ListProposalsResponse,
} from './proposal.type';
import {
  governanceActionSortModes,
  governanceActionTypes,
} from './proposal.type';
import type { GovernanceActionSortMode } from './proposal.type';
import { MAX_PAGE_SIZE, parsePageValue } from 'src/common/pagination';
import { queryEnum, queryEnums } from 'src/common/query-enum';
@Controller('proposal')
export class ProposalController {
  constructor(
    private readonly proposalService: ProposalService,
    private readonly drepService: DRepService,
  ) {}

  @Get('list')
  async list(
    @Query('type') type?: string | string[],
    @Query('type[]') typeArray?: string | string[],
    @Query('sort') sort?: GovernanceActionSortMode,
    @Query('page') page?: string,
    @Query('pageSize') pageSize?: string,
    @Query('drepId') drepId?: string,
    @Query('search') search?: string,
  ): Promise<ListProposalsResponse> {
    const params = {
      type: queryEnums(type, typeArray, governanceActionTypes, 'type'),
      sort: queryEnum(sort, governanceActionSortModes, 'sort'),
      page: parsePageValue(page, 0, 'page'),
      pageSize: parsePageValue(pageSize, 10, 'pageSize', MAX_PAGE_SIZE),
      drepId,
      search,
    };
    // With a DRep, the list leaves out the actions it has already voted on,
    // as the legacy list did.
    const votes = await this.votesOf(drepId);
    return this.proposalService.list({
      ...params,
      excludedIds: new Set(votes.map(({ proposal }) => govActionId(proposal))),
    });
  }

  @Get('get/:proposalId')
  async get(
    @Param('proposalId') proposalId: string,
    @Query('drepId') drepId?: string,
  ): Promise<GetProposalResponse> {
    const response = await this.proposalService.get(proposalId, drepId);
    const id = govActionId(response.proposal);
    const vote = (await this.votesOf(drepId)).find(
      ({ proposal }) => govActionId(proposal) === id,
    );
    return { ...response, vote: vote?.vote ?? null };
  }

  /**
   * The DRep's votes, or none when `drepId` names no DRep: a disconnected
   * frontend sends the literal text "undefined". Only the voted actions and
   * the votes are read here, so the rows come without their documents.
   */
  private async votesOf(drepId?: string): Promise<VoteResponse[]> {
    if (!drepId || tryLegacyDRepCandidates(drepId) === undefined) {
      return [];
    }
    return this.drepService.getVoteRows(drepId);
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

function govActionId({ txHash, index }: { txHash: string; index: number }) {
  return `${txHash.toLowerCase()}#${index}`;
}
