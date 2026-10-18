import { Controller, Get, HttpCode, Post, Req } from '@nestjs/common';
import type { Request } from 'express';
import { Public } from '../auth/auth.guard';
import { RawBody } from '../common/body';
import { ProxyService } from './proxy.service';

const GOVTOOL_PREFIX = '/api/proxy/govtool/';

/** §9. Raw bodies, not the v4 envelope. */
@Controller('proxy')
export class ProxyController {
  constructor(private readonly proxy: ProxyService) {}

  /** The path is matched on the raw URL, before Express decodes it. */
  @Public()
  @Get('govtool/*path')
  govtool(@Req() req: Request) {
    const url = req.originalUrl ?? req.url;
    const q = url.indexOf('?');
    const path = q < 0 ? url : url.slice(0, q);
    const rawPath = path.startsWith(GOVTOOL_PREFIX) ? path.slice(GOVTOOL_PREFIX.length) : '';
    return this.proxy.govtool(rawPath, q < 0 ? '' : url.slice(q + 1));
  }

  @Post()
  @HttpCode(200)
  fetch(@RawBody() body: Record<string, unknown>) {
    return this.proxy.fetch(body);
  }
}
