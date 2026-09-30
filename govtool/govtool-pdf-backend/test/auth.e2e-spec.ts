// SPEC §7: challenge, login (stake and DRep), tokens, refresh.

import * as jwt from 'jsonwebtoken';
import { createTestApp, TestApp } from './helpers/app';
import { getChallenge, jwtClaims, loginBody, loginDrep, loginStake, refreshCookieFrom } from './helpers/auth';
import { baseAddress, drepIdentifier, newKey, signCip8, stakeIdentifier } from './helpers/cip8-signer';
import { expectError, expectUnauthorized, expectValidation } from './helpers/envelope';
import { TEST_ENV } from './setup/env';

const MESSAGE_RE =
  /^To proceed, please sign this data to verify your identity\. This ensures that the action is secure and confirms your identity\.\nNonce: [0-9a-f]{32}\nTimestamp: \d+$/;

describe('auth (e2e)', () => {
  let t: TestApp;
  beforeAll(async () => {
    t = await createTestApp();
  });
  afterAll(async () => {
    await t.close();
  });

  describe('GET /api/auth/challenge', () => {
    it('issues the exact ASCII message, raw', async () => {
      const id = stakeIdentifier(newKey());
      const res = await t.api().get(`/api/auth/challenge?identifier=${id}`).expect(200);
      expect(Object.keys(res.body)).toEqual(['message']);
      expect(res.body.message).toMatch(MESSAGE_RE);
      const row = await t.prisma.authChallenge.findFirst({ where: { identifier: id } });
      expect(row?.message).toBe(res.body.message);
      expect(row!.expiresAt.getTime() - row!.timestamp.getTime()).toBe(300_000);
    });

    it('lowercases the identifier', async () => {
      const id = stakeIdentifier(newKey());
      await t.api().get(`/api/auth/challenge?identifier=${id.toUpperCase()}`).expect(200);
      expect(await t.prisma.authChallenge.count({ where: { identifier: id } })).toBe(1);
    });

    it('accepts a DRep key hash', async () => {
      await t
        .api()
        .get(`/api/auth/challenge?identifier=${drepIdentifier(newKey())}`)
        .expect(200);
    });

    it('missing and invalid identifiers', async () => {
      expectError(await t.api().get('/api/auth/challenge'), 400, 'BadRequestError', 'Missing identifier');
      expectValidation(await t.api().get('/api/auth/challenge?identifier=zz'), 'Invalid identifier');
      expectValidation(
        await t.api().get(`/api/auth/challenge?identifier=f0${'ab'.repeat(28)}`),
        'Invalid identifier',
      );
    });

    it('purges expired challenges and keeps every live one (Δ14)', async () => {
      const id = stakeIdentifier(newKey());
      await t.prisma.authChallenge.create({
        data: {
          identifier: 'e0' + '00'.repeat(28),
          nonce: 'f'.repeat(32),
          message: 'old',
          timestamp: new Date(0),
          expiresAt: new Date(1000),
        },
      });
      for (let i = 0; i < 12; i++) await t.api().get(`/api/auth/challenge?identifier=${id}`).expect(200);
      expect(await t.prisma.authChallenge.count({ where: { identifier: id } })).toBe(12);
      expect(await t.prisma.authChallenge.count({ where: { nonce: 'f'.repeat(32) } })).toBe(0);
    });

    it('challenges requested by a third party for the same identifier do not evict a pending one (Δ14)', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const message = await getChallenge(t, id);
      // Anyone can ask for challenges for a public identifier.
      for (let i = 0; i < 15; i++) await t.api().get(`/api/auth/challenge?identifier=${id}`).expect(200);
      await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, message))
        .expect(200);
    });
  });

  describe('POST /api/auth/local (stake)', () => {
    it('first login creates the user with govtool_username null; the self projection has no email', async () => {
      const s = await loginStake(t);
      expect(s.user).toEqual({
        id: expect.any(Number),
        username: s.identifier,
        provider: 'local',
        confirmed: true,
        blocked: false,
        govtool_username: null,
        is_validated: false,
        createdAt: expect.any(String),
        updatedAt: expect.any(String),
      });
    });

    it('returns status, jwt, user and no refresh token in the body (Δ17)', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const msg = await getChallenge(t, id);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, msg))
        .expect(200);
      expect(Object.keys(res.body).sort()).toEqual(['jwt', 'status', 'user']);
      expect(res.body.status).toBe('Authenticated');
      expect(JSON.stringify(res.body)).not.toMatch(/refresh/i);
    });

    it('second login returns the same user id', async () => {
      const key = newKey();
      const a = await loginStake(t, { key });
      const b = await loginStake(t, { key });
      expect(b.user.id).toBe(a.user.id);
      expect(await t.prisma.user.count({ where: { username: a.identifier } })).toBe(1);
    });

    it('JWT claims: id, stakeKey, iat, exp = iat + 1h, HS256, no dRepID', async () => {
      const s = await loginStake(t);
      const c = jwtClaims(s.jwt);
      expect(Object.keys(c).sort()).toEqual(['exp', 'iat', 'id', 'stakeKey']);
      expect(c.id).toBe(s.user.id);
      expect(c.stakeKey).toBe(s.identifier);
      expect((c.exp as number) - (c.iat as number)).toBe(3600);
      expect(() => jwt.verify(s.jwt, TEST_ENV.JWT_SECRET, { algorithms: ['HS256'] })).not.toThrow();
    });

    it('sets the refresh cookie: HttpOnly, Path=/, Max-Age 7d, SameSite=Lax, not Secure', async () => {
      const s = await loginStake(t);
      const attrs = s.setCookie.split(';').map((x) => x.trim());
      expect(attrs[0]).toMatch(/^refreshToken=.+/);
      expect(attrs).toContain('HttpOnly');
      expect(attrs).toContain('Path=/');
      expect(attrs).toContain(`Max-Age=${7 * 86400}`);
      expect(attrs).toContain('SameSite=Lax');
      expect(attrs).not.toContain('Secure');
    });

    it('accepts the base address in the protected header (Playwright wallet)', async () => {
      const key = newKey();
      await loginStake(t, { key, sign: { address: baseAddress(newKey(), key) } });
    });

    it('accepts a hashed: true payload', async () => {
      await loginStake(t, { sign: { hashed: true } });
    });

    it('replaying the same signed body gives Challenge not found', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const body = await loginBody(key, id, await getChallenge(t, id));
      await t.api().post('/api/auth/local').send(body).expect(200);
      expectError(
        await t.api().post('/api/auth/local').send(body),
        400,
        'ApplicationError',
        'Challenge not found',
      );
    });

    it('a signature over a different payload fails and consumes the challenge (Δ15, Δ16)', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const msg = await getChallenge(t, id);
      const bad = await loginBody(key, id, msg, { signedPayload: 'something else' });
      expectError(
        await t.api().post('/api/auth/local').send(bad),
        400,
        'ApplicationError',
        'Verification failed',
      );
      const good = await loginBody(key, id, msg);
      expectError(
        await t.api().post('/api/auth/local').send(good),
        400,
        'ApplicationError',
        'Challenge not found',
      );
    });

    it('an old signature by the same key replayed against a fresh challenge fails', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const first = await getChallenge(t, id);
      const oldSig = await signCip8(key, first);
      const fresh = await getChallenge(t, id);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send({ identifier: id, signedMessage: { ...oldSig, expectedSignedMessage: fresh } });
      expectError(res, 400, 'ApplicationError', 'Verification failed');
    });

    it('a wrong key fails verification', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const msg = await getChallenge(t, id);
      const body = await loginBody(newKey(), id, msg);
      expectError(
        await t.api().post('/api/auth/local').send(body),
        400,
        'ApplicationError',
        'Verification failed',
      );
    });

    it('garbage signature and key fail verification, never 500', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const msg = await getChallenge(t, id);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send({ identifier: id, signedMessage: { signature: 'zz', key: '00', expectedSignedMessage: msg } });
      expectError(res, 400, 'ApplicationError', 'Verification failed');
    });

    it('an expired challenge', async () => {
      const short = await createTestApp({ CHALLENGE_TTL_SECONDS: '1' });
      try {
        const key = newKey();
        const id = stakeIdentifier(key);
        const msg = await getChallenge(short, id);
        await new Promise((r) => setTimeout(r, 1200));
        const res = await short
          .api()
          .post('/api/auth/local')
          .send(await loginBody(key, id, msg));
        expectValidation(res, 'Challenge expired');
      } finally {
        await short.close();
      }
    });

    it('a mismatched expectedSignedMessage (nonce matches, text differs)', async () => {
      const key = newKey();
      const id = stakeIdentifier(key);
      const msg = await getChallenge(t, id);
      const altered = msg.replace('To proceed', 'To continue');
      const res = await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, altered));
      expectValidation(res, 'expectedSignedMessage does not match original challenge message');
    });

    it('a challenge issued for another identifier is not found', async () => {
      const key = newKey();
      const msg = await getChallenge(t, stakeIdentifier(newKey()));
      const id = stakeIdentifier(key);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, msg));
      expectError(res, 400, 'ApplicationError', 'Challenge not found');
    });

    it('body validation messages in order', async () => {
      const post = (b: unknown) =>
        t
          .api()
          .post('/api/auth/local')
          .send(b as object);
      const id = stakeIdentifier(newKey());
      expectValidation(await post({}), 'identifier was not provided');
      expectValidation(await post({ identifier: id }), 'signData object was not provided');
      expectValidation(
        await post({ identifier: id, signedMessage: { signature: 'a', key: 'b' } }),
        'Payload was not provided in signData object.',
      );
      expectValidation(
        await post({ identifier: id, signedMessage: { key: 'b', expectedSignedMessage: 'x' } }),
        'Signature was not provided in signData object.',
      );
      expectValidation(
        await post({ identifier: id, signedMessage: { signature: 'a', expectedSignedMessage: 'x' } }),
        'Key was not provided in signData object.',
      );
      expectValidation(
        await post({
          identifier: 'nothex',
          signedMessage: { signature: 'a', key: 'b', expectedSignedMessage: 'x' },
        }),
        'Invalid identifier',
      );
      expectValidation(
        await post({
          identifier: id,
          signedMessage: { signature: 'a', key: 'b', expectedSignedMessage: 'x' },
        }),
        'Invalid expectedSignedMessage format',
      );
    });

    it('a blocked user cannot log in', async () => {
      const key = newKey();
      const s = await loginStake(t, { key });
      await t.prisma.user.update({ where: { id: s.user.id }, data: { blocked: true } });
      const id = stakeIdentifier(key);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, await getChallenge(t, id)));
      expectError(res, 400, 'ApplicationError', 'Your account has been blocked by an administrator');
    });
  });

  describe('POST /api/auth/local (DRep)', () => {
    it('without a Bearer token gives 401', async () => {
      const key = newKey();
      const id = drepIdentifier(key);
      const res = await t
        .api()
        .post('/api/auth/local')
        .send(await loginBody(key, id, await getChallenge(t, id)));
      expectUnauthorized(res);
    });

    it('with a garbage Bearer token gives 401', async () => {
      const key = newKey();
      const id = drepIdentifier(key);
      const res = await t
        .api()
        .post('/api/auth/local')
        .set('Authorization', 'Bearer nope')
        .send(await loginBody(key, id, await getChallenge(t, id)));
      expectUnauthorized(res);
    });

    it('with the stake JWT yields dRepID for the same user', async () => {
      const s = await loginStake(t);
      const d = await loginDrep(t, s);
      const c = jwtClaims(d.jwt);
      expect(c).toMatchObject({ id: s.user.id, stakeKey: s.identifier, dRepID: d.drepId });
      const me = await t.api().get('/api/users/me').set(d.auth).expect(200);
      expect(me.body.id).toBe(s.user.id);
    });

    it('the DRep key must sign: a stake-key signature for a DRep identifier fails', async () => {
      const s = await loginStake(t);
      const drepKey = newKey();
      const id = drepIdentifier(drepKey);
      const body = await loginBody(s.key, id, await getChallenge(t, id));
      const res = await t.api().post('/api/auth/local').set(s.auth).send(body);
      expectError(res, 400, 'ApplicationError', 'Verification failed');
    });

    it('a stale Bearer does not turn a stake login into a DRep login (Δ13)', async () => {
      const a = await loginStake(t);
      const b = await t.api().post('/api/auth/local').set(a.auth);
      expect(b.status).toBe(400); // validation, not a DRep flow
      const key = newKey();
      const id = stakeIdentifier(key);
      const res = await t
        .api()
        .post('/api/auth/local')
        .set(a.auth)
        .send(await loginBody(key, id, await getChallenge(t, id)))
        .expect(200);
      expect(res.body.user.id).not.toBe(a.user.id);
      expect(jwtClaims(res.body.jwt)).not.toHaveProperty('dRepID');
    });
  });

  describe('POST /api/token/refresh', () => {
    it('no cookie: 400 No Authorization and the cookie is cleared', async () => {
      const res = await t.api().post('/api/token/refresh').send({});
      expectError(res, 400, 'BadRequestError', 'No Authorization');
      expect(refreshCookieFrom(res.headers['set-cookie']).raw).toMatch(/^refreshToken=;/);
    });

    it('returns a new JWT and rotates the cookie with the same claims', async () => {
      const s = await loginStake(t);
      await new Promise((r) => setTimeout(r, 1100)); // new iat, so a new token
      const res = await t
        .api()
        .post('/api/token/refresh')
        .set('Cookie', s.refreshCookie)
        .send({})
        .expect(200);
      expect(Object.keys(res.body)).toEqual(['jwt']);
      expect(res.body.jwt).not.toBe(s.jwt);
      expect(jwtClaims(res.body.jwt)).toMatchObject({ id: s.user.id, stakeKey: s.identifier });
      const rotated = refreshCookieFrom(res.headers['set-cookie']);
      expect(rotated.pair).toMatch(/^refreshToken=.+/);
      expect(rotated.pair).not.toBe(s.refreshCookie);
      await t.api().get('/api/users/me').set('Authorization', `Bearer ${res.body.jwt}`).expect(200);
    });

    it('keeps dRepID through refresh', async () => {
      const s = await loginStake(t);
      const key = newKey();
      const id = drepIdentifier(key);
      const login = await t
        .api()
        .post('/api/auth/local')
        .set(s.auth)
        .send(await loginBody(key, id, await getChallenge(t, id)))
        .expect(200);
      const cookie = refreshCookieFrom(login.headers['set-cookie']).pair;
      const res = await t.api().post('/api/token/refresh').set('Cookie', cookie).send({}).expect(200);
      expect(jwtClaims(res.body.jwt)).toMatchObject({ dRepID: id });
    });

    it('an access token as the refresh cookie is rejected', async () => {
      const s = await loginStake(t);
      const res = await t.api().post('/api/token/refresh').set('Cookie', `refreshToken=${s.jwt}`).send({});
      expectError(res, 400, 'BadRequestError', 'Invalid token.');
      expect(refreshCookieFrom(res.headers['set-cookie']).raw).toMatch(/^refreshToken=;/);
    });

    it('a refresh token is not accepted as an access token', async () => {
      const s = await loginStake(t);
      const refresh = s.refreshCookie.slice('refreshToken='.length);
      expectUnauthorized(await t.api().get('/api/users/me').set('Authorization', `Bearer ${refresh}`));
    });

    it('garbage, expired and wrongly signed cookies give Invalid token.', async () => {
      const s = await loginStake(t);
      const claims = { id: s.user.id, stakeKey: s.identifier, typ: 'refresh' };
      const expired = jwt.sign(
        { ...claims, exp: Math.floor(Date.now() / 1000) - 10 },
        TEST_ENV.REFRESH_SECRET,
      );
      const wrongKey = jwt.sign(claims, 'x'.repeat(40));
      for (const v of ['garbage', expired, wrongKey]) {
        const res = await t.api().post('/api/token/refresh').set('Cookie', `refreshToken=${v}`).send({});
        expectError(res, 400, 'BadRequestError', 'Invalid token.');
      }
    });

    it('a blocked user cannot refresh', async () => {
      const s = await loginStake(t);
      await t.prisma.user.update({ where: { id: s.user.id }, data: { blocked: true } });
      const res = await t.api().post('/api/token/refresh').set('Cookie', s.refreshCookie).send({});
      expectError(res, 400, 'BadRequestError', 'Invalid token.');
    });
  });

  describe('token checks on every request', () => {
    it('an expired, wrongly signed or alg-none access token is 401 on authenticated routes', async () => {
      const s = await loginStake(t);
      const claims = { id: s.user.id, stakeKey: s.identifier };
      const expired = jwt.sign({ ...claims, exp: Math.floor(Date.now() / 1000) - 10 }, TEST_ENV.JWT_SECRET);
      const wrong = jwt.sign(claims, 'y'.repeat(40));
      const none = `${Buffer.from('{"alg":"none","typ":"JWT"}').toString('base64url')}.${Buffer.from(
        JSON.stringify(claims),
      ).toString('base64url')}.`;
      for (const tok of [expired, wrong, none]) {
        expectUnauthorized(await t.api().get('/api/users/me').set('Authorization', `Bearer ${tok}`));
      }
    });

    it('a token whose stakeKey is not the user is 401', async () => {
      const s = await loginStake(t);
      const forged = jwt.sign({ id: s.user.id, stakeKey: stakeIdentifier(newKey()) }, TEST_ENV.JWT_SECRET);
      expectUnauthorized(await t.api().get('/api/users/me').set('Authorization', `Bearer ${forged}`));
    });

    it('a token for a user that no longer exists is 401', async () => {
      const forged = jwt.sign({ id: 999999, stakeKey: stakeIdentifier(newKey()) }, TEST_ENV.JWT_SECRET);
      expectUnauthorized(await t.api().get('/api/users/me').set('Authorization', `Bearer ${forged}`));
    });
  });
});
