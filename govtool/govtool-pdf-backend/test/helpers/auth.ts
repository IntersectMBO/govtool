// Real CIP-8 logins against a running test app.

import type { Ed25519Key } from 'libcardano';
import type { TestApp } from './app';
import { drepIdentifier, newKey, signCip8, SignOptions, stakeIdentifier } from './cip8-signer';

export interface StakeSession {
  key: Ed25519Key;
  identifier: string;
  jwt: string;
  user: { id: number; username: string; govtool_username: string | null; [k: string]: unknown };
  /** `refreshToken=<jwt>` for a Cookie header. */
  refreshCookie: string;
  /** The raw Set-Cookie header of the login response. */
  setCookie: string;
  /** `{ Authorization: 'Bearer …' }` */
  auth: { Authorization: string };
}

export interface DrepSession {
  key: Ed25519Key;
  /** 56-hex DRep key hash. */
  drepId: string;
  jwt: string;
  auth: { Authorization: string };
}

export function refreshCookieFrom(setCookie: string | string[] | undefined): { raw: string; pair: string } {
  const list = Array.isArray(setCookie) ? setCookie : setCookie ? [setCookie] : [];
  const raw = list.find((c) => c.startsWith('refreshToken=')) ?? '';
  return { raw, pair: raw.split(';')[0] };
}

/** GET /api/auth/challenge and return the message. */
export async function getChallenge(t: TestApp, identifier: string): Promise<string> {
  const res = await t.api().get(`/api/auth/challenge?identifier=${identifier}`).expect(200);
  return res.body.message as string;
}

/** Build the POST /api/auth/local body for a message signed by `key`. */
export async function loginBody(
  key: Ed25519Key,
  identifier: string,
  message: string,
  opts: SignOptions & { signedPayload?: string } = {},
) {
  const signed = await signCip8(key, opts.signedPayload ?? message, opts);
  return { identifier, signedMessage: { ...signed, expectedSignedMessage: message } };
}

/**
 * Stake login with a fresh (or given) key. With `username`, also sets
 * govtool_username through PUT /api/users/edit.
 */
export async function loginStake(
  t: TestApp,
  opts: { key?: Ed25519Key; username?: string; sign?: SignOptions } = {},
): Promise<StakeSession> {
  const key = opts.key ?? newKey();
  const identifier = stakeIdentifier(key);
  const message = await getChallenge(t, identifier);
  const res = await t
    .api()
    .post('/api/auth/local')
    .send(await loginBody(key, identifier, message, opts.sign))
    .expect(200);
  const cookie = refreshCookieFrom(res.headers['set-cookie']);
  const session: StakeSession = {
    key,
    identifier,
    jwt: res.body.jwt,
    user: res.body.user,
    refreshCookie: cookie.pair,
    setCookie: cookie.raw,
    auth: { Authorization: `Bearer ${res.body.jwt}` },
  };
  if (opts.username) {
    const edit = await t
      .api()
      .put('/api/users/edit')
      .set(session.auth)
      .send({ govtoolUsername: opts.username })
      .expect(200);
    session.user = edit.body;
  }
  return session;
}

/** DRep login on top of a stake session: the returned JWT carries dRepID. */
export async function loginDrep(
  t: TestApp,
  stake: StakeSession,
  key: Ed25519Key = newKey(),
): Promise<DrepSession> {
  const drepId = drepIdentifier(key);
  const message = await getChallenge(t, drepId);
  const res = await t
    .api()
    .post('/api/auth/local')
    .set(stake.auth)
    .send(await loginBody(key, drepId, message))
    .expect(200);
  return { key, drepId, jwt: res.body.jwt, auth: { Authorization: `Bearer ${res.body.jwt}` } };
}

/** Decode a JWT payload without verifying (as pdf-ui does). */
export function jwtClaims(jwt: string): Record<string, unknown> {
  return JSON.parse(Buffer.from(jwt.split('.')[1], 'base64url').toString('utf8')) as Record<string, unknown>;
}
