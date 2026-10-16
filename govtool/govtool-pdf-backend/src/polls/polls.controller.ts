import { Controller, Get, HttpCode, Param, Post, Put } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Caller, Public } from '../auth/auth.guard';
import { DataBody, type DataPayload } from '../common/body';
import { RawQuery } from '../query/raw-query';
import { PollsService } from './polls.service';

/** §8.5 and §8.6. */
@Controller()
export class PollsController {
  constructor(private readonly polls: PollsService) {}

  @Public()
  @Get('polls')
  list(@RawQuery() raw: Record<string, unknown>) {
    return this.polls.list(raw);
  }

  @Post('polls')
  @HttpCode(200)
  create(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.polls.create(data, caller);
  }

  @Put('polls/:id')
  close(@Param('id') id: string, @DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.polls.close(id, data, caller);
  }

  @Get('poll-votes')
  listVotes(@RawQuery() raw: Record<string, unknown>, @Caller() caller: AuthUser) {
    return this.polls.listVotes(raw, caller);
  }

  @Post('poll-votes')
  @HttpCode(200)
  createVote(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.polls.createVote(data, caller);
  }

  @Put('poll-votes/:id')
  updateVote(@Param('id') id: string, @DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.polls.updateVote(id, data, caller);
  }
}
