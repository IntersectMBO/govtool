import { Controller, Delete, Get, HttpCode, Param, Post, Put } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Caller, CurrentUser, Public } from '../auth/auth.guard';
import { DataBody, type DataPayload } from '../common/body';
import { RawQuery } from '../query/raw-query';
import { ProposalsService } from './proposals.service';

/** §8.2 and §8.3. */
@Controller()
export class ProposalsController {
  constructor(private readonly proposals: ProposalsService) {}

  @Public()
  @Get('proposals')
  list(@RawQuery() raw: Record<string, unknown>, @CurrentUser() caller: AuthUser | null) {
    return this.proposals.list(raw, caller);
  }

  @Public()
  @Get('proposals/:id')
  findOne(@Param('id') id: string) {
    return this.proposals.findOne(id);
  }

  @Post('proposals')
  @HttpCode(200)
  create(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.proposals.create(data, caller);
  }

  @Delete('proposals/:id')
  remove(@Param('id') id: string, @Caller() caller: AuthUser) {
    return this.proposals.remove(id, caller);
  }

  @Post('proposal-contents')
  @HttpCode(200)
  createContent(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.proposals.createContent(data, caller);
  }

  @Put('proposal-contents/:id')
  updateContent(@Param('id') id: string, @DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.proposals.updateContent(id, data, caller);
  }
}
