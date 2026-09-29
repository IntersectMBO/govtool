import { Inject, Injectable } from '@nestjs/common';
import type { CookieOptions, Response } from 'express';
import * as jwt from 'jsonwebtoken';
import { APP_CONFIG } from '../config/config.module';
import type { AppConfig } from '../config/config';
import { PrismaService } from '../prisma/prisma.service';
import { AccessClaims, AuthUser } from './auth-user';

export const REFRESH_COOKIE = 'refreshToken';
const DREP_RE = /^[0-9a-f]{56}$/;

function claimsFrom(payload: unknown, typ: 'access' | 'refresh'): AccessClaims | null {
  if (typeof payload !== 'object' || payload === null) return null;
  const p = payload as Record<string, unknown>;
  // An access token is never accepted as a refresh token, or the reverse.
  if (typ === 'refresh' ? p.typ !== 'refresh' : p.typ !== undefined) return null;
  if (typeof p.id !== 'number' || !Number.isSafeInteger(p.id) || p.id < 1) return null;
  if (typeof p.stakeKey !== 'string') return null;
  if (p.dRepID !== undefined && (typeof p.dRepID !== 'string' || !DREP_RE.test(p.dRepID))) return null;
  return {
    id: p.id,
    stakeKey: p.stakeKey,
    ...(typeof p.dRepID === 'string' ? { dRepID: p.dRepID } : {}),
  };
}

/** Access JWT, refresh JWT and the refresh cookie (SPEC §7.5). */
@Injectable()
export class TokensService {
  constructor(
    @Inject(APP_CONFIG) private readonly config: AppConfig,
    private readonly prisma: PrismaService,
  ) {}

  signAccess(claims: AccessClaims): string {
    return jwt.sign({ ...claims }, this.config.jwtSecret, {
      algorithm: 'HS256',
      expiresIn: this.config.jwtExpiresSeconds,
    });
  }

  signRefresh(claims: AccessClaims): string {
    return jwt.sign({ ...claims, typ: 'refresh' }, this.config.refreshSecret, {
      algorithm: 'HS256',
      expiresIn: this.config.refreshExpiresSeconds,
    });
  }

  verifyAccess(token: string): AccessClaims | null {
    try {
      return claimsFrom(jwt.verify(token, this.config.jwtSecret, { algorithms: ['HS256'] }), 'access');
    } catch {
      return null;
    }
  }

  verifyRefresh(token: string): AccessClaims | null {
    try {
      return claimsFrom(jwt.verify(token, this.config.refreshSecret, { algorithms: ['HS256'] }), 'refresh');
    } catch {
      return null;
    }
  }

  /**
   * Reload the user a set of claims names. Missing, blocked, or a `stakeKey`
   * that is not the user's username gives null.
   */
  async loadUser(claims: AccessClaims): Promise<AuthUser | null> {
    const row = await this.prisma.user.findUnique({ where: { id: claims.id } });
    if (!row || row.blocked || row.username !== claims.stakeKey) return null;
    return {
      id: row.id,
      username: row.username,
      govtoolUsername: row.govtoolUsername,
      isValidated: row.isValidated,
      blocked: row.blocked,
      createdAt: row.createdAt,
      updatedAt: row.updatedAt,
      dRepID: claims.dRepID ?? null,
      row,
    };
  }

  /** An `Authorization` header value to the caller, or null if not valid. */
  async resolveAuthorization(header: string): Promise<AuthUser | null> {
    const m = /^Bearer\s+(\S+)\s*$/i.exec(header);
    if (!m) return null;
    const claims = this.verifyAccess(m[1]);
    return claims ? this.loadUser(claims) : null;
  }

  private cookieOptions(): CookieOptions {
    return {
      httpOnly: true,
      path: '/',
      sameSite: this.config.refreshCookieSameSite,
      secure: this.config.refreshCookieSecure,
    };
  }

  setRefreshCookie(res: Response, claims: AccessClaims): void {
    res.cookie(REFRESH_COOKIE, this.signRefresh(claims), {
      ...this.cookieOptions(),
      maxAge: this.config.refreshExpiresSeconds * 1000,
    });
  }

  clearRefreshCookie(res: Response): void {
    res.clearCookie(REFRESH_COOKIE, this.cookieOptions());
  }
}
