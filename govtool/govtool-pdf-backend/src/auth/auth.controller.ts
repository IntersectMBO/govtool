import { Controller, Get, HttpCode, Post, Req, Res } from '@nestjs/common';
import type { Request, Response } from 'express';
import { RawBody } from '../common/body';
import { badRequest } from '../common/errors';
import { RawQuery } from '../query/raw-query';
import { selfProjection } from '../users/self-projection';
import { Public } from './auth.guard';
import { AuthService } from './auth.service';
import { REFRESH_COOKIE, TokensService } from './tokens.service';

/** §7.2, §7.4, §7.6. Every response here is raw (not enveloped). */
@Controller()
export class AuthController {
  constructor(
    private readonly auth: AuthService,
    private readonly tokens: TokensService,
  ) {}

  @Public()
  @Get('auth/challenge')
  async challenge(@RawQuery() query: Record<string, unknown>) {
    return { message: await this.auth.challenge(query.identifier) };
  }

  @Public()
  @Post('auth/local')
  @HttpCode(200)
  async login(
    @RawBody() body: Record<string, unknown>,
    @Req() req: Request,
    @Res({ passthrough: true }) res: Response,
  ) {
    const { user, claims } = await this.auth.login(body, req.headers.authorization);
    const jwt = this.tokens.signAccess(claims);
    this.tokens.setRefreshCookie(res, claims);
    // No refresh token in the body (Δ17).
    return { status: 'Authenticated', jwt, user: selfProjection(user) };
  }

  @Public()
  @Post('token/refresh')
  @HttpCode(200)
  async refresh(@Req() req: Request, @Res({ passthrough: true }) res: Response) {
    const cookies = ((req as { cookies?: unknown }).cookies ?? {}) as Record<string, unknown>;
    const token = cookies[REFRESH_COOKIE];
    if (typeof token !== 'string' || token === '') {
      this.tokens.clearRefreshCookie(res);
      throw badRequest('No Authorization');
    }
    const claims = await this.auth.refresh(token);
    if (!claims) {
      this.tokens.clearRefreshCookie(res);
      throw badRequest('Invalid token.');
    }
    this.tokens.setRefreshCookie(res, claims);
    return { jwt: this.tokens.signAccess(claims) };
  }
}
