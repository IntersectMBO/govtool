import { Inject, Injectable } from '@nestjs/common';
import { randomBytes } from 'node:crypto';
import type { AuthChallenge, User } from '@prisma/client';
import { APP_CONFIG } from '../config/config.module';
import type { AppConfig } from '../config/config';
import {
  applicationError,
  badRequest,
  isRecordNotFound,
  unauthorized,
  validationError,
} from '../common/errors';
import { PrismaService } from '../prisma/prisma.service';
import { AccessClaims } from './auth-user';
import { verifyCip8 } from './cip8';
import { parseIdentifier } from './identifier';
import { TokensService } from './tokens.service';

export function challengeMessage(nonce: string, timestamp: number): string {
  return (
    'To proceed, please sign this data to verify your identity. This ensures that the action is secure and confirms your identity.' +
    `\nNonce: ${nonce}\nTimestamp: ${timestamp}`
  );
}

export interface LoginResult {
  user: User;
  claims: AccessClaims;
}

const str = (v: unknown): string | undefined => (typeof v === 'string' && v !== '' ? v : undefined);

@Injectable()
export class AuthService {
  constructor(
    @Inject(APP_CONFIG) private readonly config: AppConfig,
    private readonly prisma: PrismaService,
    private readonly tokens: TokensService,
  ) {}

  /** §7.2 */
  async challenge(rawIdentifier: unknown): Promise<string> {
    if (rawIdentifier === undefined || rawIdentifier === '') throw badRequest('Missing identifier');
    const { identifier } = parseIdentifier(rawIdentifier, this.config.cardanoNetworkId);
    const nonce = randomBytes(16).toString('hex');
    const timestamp = Date.now();
    const message = challengeMessage(nonce, timestamp);
    const now = new Date(timestamp);
    await this.prisma.$transaction(async (tx) => {
      // Expired rows only (Δ14). No per-identifier cap: identifiers are
      // public, so evicting the oldest live challenge would let anyone cancel
      // a victim's pending login.
      await tx.authChallenge.deleteMany({ where: { expiresAt: { lt: now } } });
      await tx.authChallenge.create({
        data: {
          identifier,
          nonce,
          message,
          timestamp: now,
          expiresAt: new Date(timestamp + this.config.challengeTtlSeconds * 1000),
        },
      });
    });
    return message;
  }

  /** §7.4 steps 1–6. `authorization` is the request header (DRep login). */
  async login(body: Record<string, unknown>, authorization: string | undefined): Promise<LoginResult> {
    const rawIdentifier = str(body.identifier);
    if (!rawIdentifier) throw validationError('identifier was not provided');
    const sm = body.signedMessage;
    if (typeof sm !== 'object' || sm === null || Array.isArray(sm)) {
      throw validationError('signData object was not provided');
    }
    const signed = sm as Record<string, unknown>;
    const expectedSignedMessage = str(signed.expectedSignedMessage);
    if (!expectedSignedMessage) throw validationError('Payload was not provided in signData object.');
    if (!str(signed.signature)) throw validationError('Signature was not provided in signData object.');
    if (!str(signed.key)) throw validationError('Key was not provided in signData object.');
    const id = parseIdentifier(rawIdentifier, this.config.cardanoNetworkId);

    const nonce = /Nonce:\s*([0-9a-f]{32})/.exec(expectedSignedMessage)?.[1];
    const ts = /Timestamp:\s*(\d+)/.exec(expectedSignedMessage)?.[1];
    if (!nonce || !ts) throw validationError('Invalid expectedSignedMessage format');

    // One-shot whatever happens next (Δ16).
    let challenge: AuthChallenge;
    try {
      challenge = await this.prisma.authChallenge.delete({ where: { nonce, identifier: id.identifier } });
    } catch (e) {
      if (isRecordNotFound(e)) throw applicationError('Challenge not found');
      throw e;
    }
    if (challenge.expiresAt.getTime() < Date.now()) throw validationError('Challenge expired');
    if (expectedSignedMessage !== challenge.message) {
      throw validationError('expectedSignedMessage does not match original challenge message');
    }

    const ok = await verifyCip8({
      signature: signed.signature,
      key: signed.key,
      message: challenge.message,
      expectedKeyHash: id.keyHash,
    });
    if (!ok) throw applicationError('Verification failed');

    if (id.kind === 'stake') {
      const user = await this.prisma.user.upsert({
        where: { username: id.identifier },
        create: { username: id.identifier },
        update: {},
      });
      if (user.blocked) throw applicationError('Your account has been blocked by an administrator');
      return { user, claims: { id: user.id, stakeKey: user.username } };
    }

    // DRep login: the caller must already hold a stake session.
    const caller = authorization ? await this.tokens.resolveAuthorization(authorization) : null;
    if (!caller) throw unauthorized();
    return {
      user: caller.row,
      claims: { id: caller.id, stakeKey: caller.username, dRepID: id.identifier },
    };
  }

  /** §7.6: claims for a new token pair, or null (the caller clears the cookie). */
  async refresh(cookie: string): Promise<AccessClaims | null> {
    const claims = this.tokens.verifyRefresh(cookie);
    if (!claims) return null;
    const user = await this.tokens.loadUser(claims);
    return user ? claims : null;
  }
}
