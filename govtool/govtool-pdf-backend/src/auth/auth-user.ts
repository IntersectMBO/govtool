import type { User } from '@prisma/client';

/** The caller, reloaded from the database on every request that has a token. */
export interface AuthUser {
  id: number;
  /** Lowercase hex reward address. */
  username: string;
  govtoolUsername: string | null;
  isValidated: boolean;
  blocked: boolean;
  createdAt: Date;
  updatedAt: Date;
  /** DRep key hash from the token, present only after a DRep login. */
  dRepID: string | null;
  /** The user row as loaded. */
  row: User;
}

/** Access-token claims (§7.5). */
export interface AccessClaims {
  id: number;
  stakeKey: string;
  dRepID?: string;
}

export interface RequestWithAuth {
  authUser?: AuthUser | null;
}
