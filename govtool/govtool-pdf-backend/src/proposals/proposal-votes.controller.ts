import { Controller, Get, HttpCode, Param, Post, Put } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Caller } from '../auth/auth.guard';
import { DataBody, type DataPayload } from '../common/body';
import { RawQuery } from '../query/raw-query';
import { ProposalVotesService } from './proposal-votes.service';

/** §8.4. Every route is authenticated. */
@Controller('proposal-votes')
export class ProposalVotesController {
  constructor(private readonly votes: ProposalVotesService) {}

  @Get()
  findMine(@RawQuery() raw: Record<string, unknown>, @Caller() caller: AuthUser) {
    return this.votes.findMine(raw, caller);
  }

  @Post()
  @HttpCode(200)
  create(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.votes.create(data, caller);
  }

  @Put(':id')
  update(@Param('id') id: string, @DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.votes.update(id, data, caller);
  }
}
