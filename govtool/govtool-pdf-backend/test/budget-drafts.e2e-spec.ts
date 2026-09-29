// SPEC §8.10 BD drafts: scoped to the caller, draft_data round-trips.

import { createTestApp, TestApp } from './helpers/app';
import { loginStake, StakeSession } from './helpers/auth';
import { expectList, expectNotFound, expectSingle, expectValidation } from './helpers/envelope';
import { bdPayload } from './budget-fixtures';

const DRAFTS_QUERY = 'pagination[pageSize]=1000&populate=creator';
const UPDATE_MISSING = "Resource not found or you don't have permission to update it";
const DELETE_MISSING = "Resource not found or you don't have permission to delete it";

describe('bd-drafts (e2e)', () => {
  let t: TestApp;
  let alice: StakeSession;
  let bob: StakeSession;

  beforeAll(async () => {
    t = await createTestApp();
    alice = await loginStake(t, { username: 'alice' });
    bob = await loginStake(t, { username: 'bob' });
  });
  afterAll(async () => {
    await t.close();
  });

  const create = (s: StakeSession, draftData: unknown) =>
    t
      .api()
      .post('/api/bd-drafts')
      .set(s.auth)
      .send({ data: { draft_data: draftData } });

  it('POST returns {data: {id, attributes}} and draft_data round-trips exactly', async () => {
    // The whole wizard state, with numbers, booleans, nulls and nested arrays.
    const state = {
      ...bdPayload('Draft one'),
      master_id: undefined,
      extra: { n: 1.5, list: [null, true, 'x'] },
    };
    const res = await create(alice, state);
    const data = expectSingle(res)!;
    expect(Object.keys(data.attributes).sort()).toEqual(['createdAt', 'draft_data', 'updatedAt']);
    expect(data.attributes.draft_data).toEqual(JSON.parse(JSON.stringify(state)));
    const again = expectList(await t.api().get(`/api/bd-drafts?${DRAFTS_QUERY}`).set(alice.auth));
    const mine = again.find((d) => d.id === data.id)!;
    expect(mine.attributes.draft_data).toEqual(JSON.parse(JSON.stringify(state)));
    expect(mine.attributes.creator).toEqual({
      data: { id: alice.user.id, attributes: { govtool_username: 'alice' } },
    });
  });

  it('forces the creator (Δ41) and validates draft_data', async () => {
    const res = await t
      .api()
      .post('/api/bd-drafts')
      .set(alice.auth)
      .send({ data: { draft_data: { a: 1 }, creator: bob.user.id } });
    const id = expectSingle(res)!.id;
    const row = await t.prisma.bdDraft.findUniqueOrThrow({ where: { id } });
    expect(row.creatorId).toBe(alice.user.id);
    for (const bad of [null, 'text', 5, [1, 2]]) {
      expectValidation(await create(alice, bad), 'draft_data is invalid');
    }
    expectValidation(
      await t.api().post('/api/bd-drafts').set(alice.auth).send({ data: {} }),
      'draft_data is invalid',
    );
  });

  it('lists only the caller’s drafts; meta.pagination.total counts them', async () => {
    await create(bob, { who: 'bob' }).expect(200);
    const aliceTotal = await t.prisma.bdDraft.count({ where: { creatorId: alice.user.id } });
    const a = await t.api().get(`/api/bd-drafts?${DRAFTS_QUERY}`).set(alice.auth);
    const data = expectList(a, { total: aliceTotal });
    expect(data.every((d) => (d.attributes as any).creator.data.id === alice.user.id)).toBe(true);
    const b = expectList(await t.api().get(`/api/bd-drafts?${DRAFTS_QUERY}`).set(bob.auth), { total: 1 });
    expect(b[0].attributes.draft_data).toEqual({ who: 'bob' });

    // A fresh user sees none (BudgetDiscussionInfo hides the card on total 0).
    const carol = await loginStake(t);
    expectList(await t.api().get(`/api/bd-drafts?${DRAFTS_QUERY}`).set(carol.auth), { total: 0, length: 0 });
  });

  it('a creator filter cannot widen the scope', async () => {
    expectValidation(
      await t.api().get(`/api/bd-drafts?filters[creator]=${bob.user.id}`).set(alice.auth),
      'Invalid key creator.id',
    );
    const bobDraft = await t.prisma.bdDraft.findFirstOrThrow({ where: { creatorId: bob.user.id } });
    expectList(await t.api().get(`/api/bd-drafts?filters[id]=${bobDraft.id}`).set(alice.auth), { total: 0 });
  });

  it('PUT updates the caller’s draft; another user’s is indistinguishable from missing', async () => {
    const id = expectSingle(await create(alice, { v: 1 }))!.id;
    const res = await t
      .api()
      .put(`/api/bd-drafts/${id}`)
      .set(alice.auth)
      .send({ data: { draft_data: { v: 2 } } });
    expect(expectSingle(res)!.attributes.draft_data).toEqual({ v: 2 });

    expectNotFound(
      await t
        .api()
        .put(`/api/bd-drafts/${id}`)
        .set(bob.auth)
        .send({ data: { draft_data: { v: 'mallory' } } }),
      UPDATE_MISSING,
    );
    expectNotFound(
      await t
        .api()
        .put('/api/bd-drafts/999999')
        .set(bob.auth)
        .send({ data: { draft_data: {} } }),
      UPDATE_MISSING,
    );
    expectNotFound(
      await t
        .api()
        .put('/api/bd-drafts/abc')
        .set(bob.auth)
        .send({ data: { draft_data: {} } }),
      UPDATE_MISSING,
    );
    const row = await t.prisma.bdDraft.findUniqueOrThrow({ where: { id } });
    expect(row.draftData).toEqual({ v: 2 });
  });

  it('DELETE removes the caller’s draft and returns it; others get 404', async () => {
    const id = expectSingle(await create(alice, { gone: true }))!.id;
    expectNotFound(await t.api().delete(`/api/bd-drafts/${id}`).set(bob.auth), DELETE_MISSING);
    const res = await t.api().delete(`/api/bd-drafts/${id}`).set(alice.auth);
    const data = expectSingle(res)!;
    expect(data).toEqual({ id, attributes: expect.objectContaining({ draft_data: { gone: true } }) });
    expectNotFound(await t.api().delete(`/api/bd-drafts/${id}`).set(alice.auth), DELETE_MISSING);
    expect(await t.prisma.bdDraft.count({ where: { id } })).toBe(0);
  });
});
