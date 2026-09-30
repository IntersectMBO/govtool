import type { User } from '@prisma/client';

/**
 * The self projection (§3.4), only ever sent to that user: no email,
 * password, tokens or role, ever (Δ1).
 */
export function selfProjection(u: User) {
  return {
    id: u.id,
    username: u.username,
    provider: 'local' as const,
    confirmed: true as const,
    blocked: u.blocked,
    govtool_username: u.govtoolUsername,
    is_validated: u.isValidated,
    createdAt: u.createdAt.toISOString(),
    updatedAt: u.updatedAt.toISOString(),
  };
}

/** Strapi's schema rule for govtool_username (§7.7). */
export const USERNAME_RE = /^(?![._])[a-z0-9._]{1,30}$/;

export function isValidUsername(v: unknown): v is string {
  return typeof v === 'string' && USERNAME_RE.test(v);
}
