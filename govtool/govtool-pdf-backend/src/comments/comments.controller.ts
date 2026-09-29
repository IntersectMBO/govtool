import { Controller, Delete, Get, HttpCode, Param, Post } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Caller, Public } from '../auth/auth.guard';
import { DataBody, type DataPayload } from '../common/body';
import { RawQuery } from '../query/raw-query';
import { CommentsService } from './comments.service';

/**
 * §8.7 and §8.12. There is no list or update route for reports, so pdf-ui's
 * id-less GET/PUT /api/comments-reports/ are 404.
 */
@Controller()
export class CommentsController {
  constructor(private readonly comments: CommentsService) {}

  @Public()
  @Get('comments')
  list(@RawQuery() raw: Record<string, unknown>) {
    return this.comments.list(raw);
  }

  @Post('comments')
  @HttpCode(200)
  create(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.comments.create(data, caller);
  }

  @Post('comments-reports')
  @HttpCode(200)
  report(@DataBody() data: DataPayload, @Caller() caller: AuthUser) {
    return this.comments.report(data, caller);
  }

  @Delete('comments-reports/:id')
  removeReport(@Param('id') id: string, @Caller() caller: AuthUser) {
    return this.comments.removeReport(id, caller);
  }
}
