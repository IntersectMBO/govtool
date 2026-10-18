import { CanActivate, createParamDecorator, ExecutionContext, Injectable, SetMetadata } from '@nestjs/common';
import { Reflector } from '@nestjs/core';
import type { Request } from 'express';
import { forbidden, internal, notFound, unauthorized } from '../common/errors';
import { AuthUser, RequestWithAuth } from './auth-user';
import { TokensService } from './tokens.service';

export const AUTH_LEVEL = 'pdf:authLevel';
export type AuthLevel = 'public' | 'authenticated';

/**
 * §3.7 public: no token needed; a valid Bearer identifies the caller, an
 * invalid or expired one is ignored (Δ5).
 */
export const Public = () => SetMetadata(AUTH_LEVEL, 'public' satisfies AuthLevel);

/**
 * §3.7 authenticated (the default for every route without @Public): no
 * `Authorization` header gives 403 Forbidden, a bad token or a missing or
 * blocked user gives 401.
 */
export const Authenticated = () => SetMetadata(AUTH_LEVEL, 'authenticated' satisfies AuthLevel);

/** Global guard: resolves the caller once and enforces the route's level. */
@Injectable()
export class AuthGuard implements CanActivate {
  constructor(
    private readonly reflector: Reflector,
    private readonly tokens: TokensService,
  ) {}

  async canActivate(ctx: ExecutionContext): Promise<boolean> {
    const level =
      this.reflector.getAllAndOverride<AuthLevel | undefined>(AUTH_LEVEL, [
        ctx.getHandler(),
        ctx.getClass(),
      ]) ?? 'authenticated';
    const req = ctx.switchToHttp().getRequest<Request & RequestWithAuth>();
    const header = req.headers.authorization;
    if (header === undefined) {
      req.authUser = null;
      if (level === 'public') return true;
      throw forbidden();
    }
    const user = await this.tokens.resolveAuthorization(header);
    req.authUser = user;
    if (user || level === 'public') return true;
    throw unauthorized();
  }
}

/** The caller or null (use on @Public routes). */
export const CurrentUser = createParamDecorator(
  (_: unknown, ctx: ExecutionContext): AuthUser | null =>
    ctx.switchToHttp().getRequest<RequestWithAuth>().authUser ?? null,
);

/** The caller on an authenticated route; the guard guarantees it. */
export const Caller = createParamDecorator((_: unknown, ctx: ExecutionContext): AuthUser => {
  const u = ctx.switchToHttp().getRequest<RequestWithAuth>().authUser;
  if (!u) throw internal(); // @Caller on a @Public route is a programming error
  return u;
});

/**
 * §3.7 owner: the row must exist (else 404 `Not Found`) and belong to the
 * caller (else 403 with the endpoint's message). Returns the row.
 */
export function assertOwner<T>(
  row: T | null | undefined,
  ownerId: (row: T) => number,
  caller: AuthUser,
  forbiddenMessage: string,
): T {
  if (row === null || row === undefined) throw notFound();
  if (ownerId(row) !== caller.id) throw forbidden(forbiddenMessage);
  return row;
}
