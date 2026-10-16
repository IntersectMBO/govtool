// SPEC §7.7: /api/users/me and /api/users/edit.

import { createTestApp, TestApp } from './helpers/app';
import { loginStake } from './helpers/auth';
import { expectError, expectForbidden, expectUnauthorized } from './helpers/envelope';

const RULE = 'Failed to update user: govtool_username must match the following: "^(?![._])[a-z0-9._]{1,30}$"';

describe('users (e2e)', () => {
  let t: TestApp;
  beforeAll(async () => {
    t = await createTestApp();
  });
  afterAll(async () => {
    await t.close();
  });

  describe('anonymous and bad tokens', () => {
    it.each([
      ['get', '/api/users/me'],
      ['put', '/api/users/edit'],
    ] as const)('%s %s: no header 403, garbage Bearer 401', async (method, path) => {
      expectForbidden(await t.api()[method](path));
      expectUnauthorized(await t.api()[method](path).set('Authorization', 'Bearer garbage'));
      expectUnauthorized(await t.api()[method](path).set('Authorization', 'Basic abc'));
      expectUnauthorized(await t.api()[method](path).set('Authorization', ''));
    });
  });

  describe('GET /api/users/me', () => {
    it('returns the raw self projection', async () => {
      const s = await loginStake(t);
      const res = await t.api().get('/api/users/me').set(s.auth).expect(200);
      expect(res.body).toEqual(s.user);
      expect(res.body).not.toHaveProperty('email');
      expect(res.body).not.toHaveProperty('data');
    });

    it('a blocked user gets 401', async () => {
      const s = await loginStake(t);
      await t.prisma.user.update({ where: { id: s.user.id }, data: { blocked: true } });
      expectUnauthorized(await t.api().get('/api/users/me').set(s.auth));
    });
  });

  describe('PUT /api/users/edit', () => {
    it('sets and changes govtool_username', async () => {
      const s = await loginStake(t);
      const a = await t
        .api()
        .put('/api/users/edit')
        .set(s.auth)
        .send({ govtoolUsername: 'alice.1' })
        .expect(200);
      expect(a.body).toMatchObject({ id: s.user.id, govtool_username: 'alice.1', username: s.identifier });
      const b = await t
        .api()
        .put('/api/users/edit')
        .set(s.auth)
        .send({ govtoolUsername: 'alice_2' })
        .expect(200);
      expect(b.body.govtool_username).toBe('alice_2');
      const me = await t.api().get('/api/users/me').set(s.auth).expect(200);
      expect(me.body.govtool_username).toBe('alice_2');
    });

    it('missing parameters', async () => {
      const s = await loginStake(t);
      for (const body of [{}, { govtool_username: 'x' }, { govtoolUsername: null }]) {
        const res = await t.api().put('/api/users/edit').set(s.auth).send(body);
        expectError(res, 400, 'BadRequestError', 'Missing parameters for user update.');
      }
      const empty = await t.api().put('/api/users/edit').set(s.auth);
      expectError(empty, 400, 'BadRequestError', 'Missing parameters for user update.');
    });

    it.each(['.lead', '_lead', 'x'.repeat(31), 'Upper', 'a-b', 'a#b', 'a b', '', 42, true])(
      'rejects %p with the rule message',
      async (name) => {
        const s = await loginStake(t);
        const res = await t.api().put('/api/users/edit').set(s.auth).send({ govtoolUsername: name });
        expectError(res, 400, 'BadRequestError', RULE);
      },
    );

    it('accepts numeric names (server rule) and 30 chars', async () => {
      const s = await loginStake(t);
      await t.api().put('/api/users/edit').set(s.auth).send({ govtoolUsername: '12345' }).expect(200);
      await t
        .api()
        .put('/api/users/edit')
        .set(s.auth)
        .send({ govtoolUsername: 'z'.repeat(30) })
        .expect(200);
    });

    it('names are unique across users', async () => {
      await loginStake(t, { username: 'taken_name' });
      const b = await loginStake(t);
      const res = await t.api().put('/api/users/edit').set(b.auth).send({ govtoolUsername: 'taken_name' });
      expectError(res, 400, 'BadRequestError', 'Failed to update user: This attribute must be unique');
    });

    it('updates only the caller, whatever the body says', async () => {
      const a = await loginStake(t);
      const b = await loginStake(t);
      await t
        .api()
        .put('/api/users/edit')
        .set(b.auth)
        .send({ govtoolUsername: 'bob_only', id: a.user.id, blocked: true, is_validated: true })
        .expect(200);
      const rowA = await t.prisma.user.findUniqueOrThrow({ where: { id: a.user.id } });
      const rowB = await t.prisma.user.findUniqueOrThrow({ where: { id: b.user.id } });
      expect(rowA.govtoolUsername).toBeNull();
      expect(rowB).toMatchObject({ govtoolUsername: 'bob_only', blocked: false, isValidated: false });
    });
  });
});
