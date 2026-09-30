// SPEC §8.11 BD polls and BD poll votes: DRep-only votes with user_id and
// drep_id from the token, counters, toggling, the submission lock.

import { createTestApp, TestApp } from './helpers/app';
import { DrepSession, loginDrep, loginStake, StakeSession } from './helpers/auth';
import { newKey } from './helpers/cip8-signer';
import {
  expectError,
  expectForbidden,
  expectList,
  expectNotFound,
  expectSingle,
  expectValidation,
} from './helpers/envelope';
import { createBd } from './budget-fixtures';

const pollQuery = (masterId: string) =>
  `filters[$and][0][bd_proposal_id][$eq]=${masterId}&filters[$and][1][is_poll_active]=true&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
const userVoteQuery = (pollId: number, userId: number) =>
  `filters[$and][0][bd_poll_id][$eq]=${pollId}&filters[$and][1][user_id][$eq]=${userId}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
const votersQuery = (pollId: number, vote: boolean) =>
  `fields[0]=drep_id&fields[1]=createdAt&filters[$and][0][vote_result][$eq]=${vote}&filters[$and][1][bd_poll_id][$eq]=${pollId}&pagination[page]=1&pagination[pageSize]=1000`;

const bad = (res: Parameters<typeof expectError>[0], msg: string) =>
  expectError(res, 400, 'BadRequestError', msg);

describe('bd-polls and bd-poll-votes (e2e)', () => {
  let t: TestApp;
  let owner: StakeSession;
  let voterStake: StakeSession;
  let voter: DrepSession;

  beforeAll(async () => {
    t = await createTestApp();
    owner = await loginStake(t, { username: 'owner' });
    voterStake = await loginStake(t, { username: 'voter' });
    voter = await loginDrep(t, voterStake);
  });
  afterAll(async () => {
    await t.close();
  });

  async function freshPoll() {
    const bd = await createBd(t, owner, 'Pollable');
    const polls = expectList(await t.api().get(`/api/bd-polls?${pollQuery(bd.master_id)}`), { total: 1 });
    return { bd, pollId: polls[0].id };
  }
  const vote = (auth: { Authorization: string }, data: Record<string, unknown>) =>
    t.api().post('/api/bd-poll-votes').set(auth).send({ data });
  const counts = async (pollId: number) => {
    const p = await t.prisma.bdPoll.findUniqueOrThrow({ where: { id: pollId } });
    return [p.yes, p.no];
  };

  it('a DRep votes; user_id and drep_id come from the token, not the body', async () => {
    const { pollId } = await freshPoll();
    const res = await vote(voter.auth, {
      bd_poll_id: `${pollId}`,
      vote_result: true,
      drep_voting_power: 1234,
      user_id: owner.user.id,
      drep_id: 'f'.repeat(56),
    });
    const data = expectSingle(res)!;
    expect(data.attributes).toEqual({
      bd_poll_id: String(pollId),
      user_id: String(voterStake.user.id),
      vote_result: true,
      drep_id: voter.drepId,
      drep_voting_power: '1234',
      createdAt: expect.any(String),
      updatedAt: expect.any(String),
    });
    expect(await counts(pollId)).toEqual([1, 0]);

    // pdf-ui's reads.
    const mine = expectList(
      await t.api().get(`/api/bd-poll-votes?${userVoteQuery(pollId, voterStake.user.id)}`),
      {
        total: 1,
      },
    );
    expect(mine[0]).toMatchObject({ id: data.id, attributes: { vote_result: true } });
    const yes = expectList(await t.api().get(`/api/bd-poll-votes?${votersQuery(pollId, true)}`), {
      total: 1,
    });
    expect(yes).toEqual([
      { id: data.id, attributes: { drep_id: voter.drepId, createdAt: expect.any(String) } },
    ]);
    expectList(await t.api().get(`/api/bd-poll-votes?${votersQuery(pollId, false)}`), { total: 0 });
    const poll = expectList(await t.api().get(`/api/bd-polls?filters[id]=${pollId}`))[0];
    expect(poll.attributes).toMatchObject({ poll_yes: 1, poll_no: 0, is_poll_active: true });
  });

  it('non-DRep callers are refused with Missing dRepID', async () => {
    const { pollId } = await freshPoll();
    bad(await vote(voterStake.auth, { bd_poll_id: pollId, vote_result: true }), 'Missing dRepID');
    bad(await vote(owner.auth, { bd_poll_id: pollId, vote_result: false }), 'Missing dRepID');
    expect(await counts(pollId)).toEqual([0, 0]);
  });

  it('input errors', async () => {
    const { pollId } = await freshPoll();
    bad(await vote(voter.auth, { bd_poll_id: pollId }), 'Vote result is required');
    bad(await vote(voter.auth, { bd_poll_id: pollId, vote_result: 'true' }), 'Vote result is required');
    bad(await vote(voter.auth, { vote_result: true }), 'Poll ID is required');
    bad(await vote(voter.auth, { bd_poll_id: 999999, vote_result: true }), 'Poll not found');
    expectValidation(
      await t.api().post('/api/bd-poll-votes').set(voter.auth).send({ nope: 1 }),
      'Missing "data" payload in the request body',
    );
  });

  it('one vote per user and one per DRep, even under two stake keys (Δ12)', async () => {
    const { pollId } = await freshPoll();
    await vote(voter.auth, { bd_poll_id: pollId, vote_result: true, drep_voting_power: '' }).expect(200);
    bad(
      await vote(voter.auth, { bd_poll_id: pollId, vote_result: false }),
      'Poll vote for this user already exist',
    );
    // The same DRep key logged in under another stake key.
    const otherStake = await loginStake(t);
    const sameDrep = await loginDrep(t, otherStake, voter.key);
    bad(
      await vote(sameDrep.auth, { bd_poll_id: pollId, vote_result: true }),
      'Poll vote for this user already exist',
    );
    expect(await counts(pollId)).toEqual([1, 0]);
    const row = await t.prisma.bdPollVote.findFirstOrThrow({ where: { bdPollId: pollId } });
    expect(row.drepVotingPower).toBe('0');
  });

  it('toggling flips both counters; the same value is refused', async () => {
    const { pollId } = await freshPoll();
    const other = await loginDrep(t, await loginStake(t), newKey());
    const v = expectSingle(await vote(voter.auth, { bd_poll_id: pollId, vote_result: true }))!;
    await vote(other.auth, { bd_poll_id: pollId, vote_result: true }).expect(200);
    expect(await counts(pollId)).toEqual([2, 0]);

    const put = (auth: { Authorization: string }, id: number, data: Record<string, unknown>) =>
      t.api().put(`/api/bd-poll-votes/${id}`).set(auth).send({ data });

    const flipped = expectSingle(await put(voter.auth, v.id, { vote_result: false }))!;
    expect(flipped).toMatchObject({ id: v.id, attributes: { vote_result: false } });
    expect(await counts(pollId)).toEqual([1, 1]);
    bad(await put(voter.auth, v.id, { vote_result: false }), 'Poll vote already updated');
    bad(await put(voter.auth, v.id, { vote_result: 'no' }), 'Vote result is required');
    expectSingle(await put(voter.auth, v.id, { vote_result: true }));
    expect(await counts(pollId)).toEqual([2, 0]);
    // Owner check on the vote row; the stake-only token of the same user may flip it.
    expectSingle(await put(voterStake.auth, v.id, { vote_result: false }));
    expect(await counts(pollId)).toEqual([1, 1]);
    expectForbidden(await put(owner.auth, v.id, { vote_result: true }), "You can't access this entry");
    expectNotFound(await put(voter.auth, 999999, { vote_result: true }));
    expect(await counts(pollId)).toEqual([1, 1]);
  });

  it('counters never go negative', async () => {
    const { pollId } = await freshPoll();
    const v = expectSingle(await vote(voter.auth, { bd_poll_id: pollId, vote_result: true }))!;
    await t.prisma.bdPoll.update({ where: { id: pollId }, data: { yes: 0 } });
    await t
      .api()
      .put(`/api/bd-poll-votes/${v.id}`)
      .set(voter.auth)
      .send({ data: { vote_result: false } })
      .expect(200);
    expect(await counts(pollId)).toEqual([0, 1]);
  });

  it('concurrent votes by 10 DReps yield exactly 10', async () => {
    const { pollId } = await freshPoll();
    const dreps = await Promise.all(
      Array.from({ length: 10 }, async () => loginDrep(t, await loginStake(t), newKey())),
    );
    const res = await Promise.all(dreps.map((d) => vote(d.auth, { bd_poll_id: pollId, vote_result: true })));
    expect(res.every((r) => r.status === 200)).toBe(true);
    expect(await counts(pollId)).toEqual([10, 0]);
  });

  it('votes only on active polls (Δ32)', async () => {
    const { pollId } = await freshPoll();
    const v = expectSingle(await vote(voter.auth, { bd_poll_id: pollId, vote_result: true }))!;
    await t.prisma.bdPoll.update({ where: { id: pollId }, data: { isActive: false } });
    const other = await loginDrep(t, await loginStake(t), newKey());
    bad(await vote(other.auth, { bd_poll_id: pollId, vote_result: true }), 'Poll is not active');
    bad(
      await t
        .api()
        .put(`/api/bd-poll-votes/${v.id}`)
        .set(voter.auth)
        .send({ data: { vote_result: false } }),
      'Poll is not active',
    );
    expect(await counts(pollId)).toEqual([1, 0]);
  });

  it('submission lock on creating and modifying votes', async () => {
    const { bd, pollId } = await freshPoll();
    const v = expectSingle(await vote(voter.auth, { bd_poll_id: pollId, vote_result: true }))!;
    await t.prisma.bd.update({ where: { id: bd.id }, data: { submittedForVote: new Date() } });
    const other = await loginDrep(t, await loginStake(t), newKey());
    expectValidation(
      await vote(other.auth, { bd_poll_id: pollId, vote_result: false }),
      'Creating poll votes is not allowed after the proposal has been submitted for voting.',
    );
    expectValidation(
      await t
        .api()
        .put(`/api/bd-poll-votes/${v.id}`)
        .set(voter.auth)
        .send({ data: { vote_result: false } }),
      'Modifying poll votes is not allowed after the proposal has been submitted for voting.',
    );
    expect(await counts(pollId)).toEqual([1, 0]);
  });

  it('lists are public and reject private or unknown paths', async () => {
    expectList(await t.api().get('/api/bd-polls'));
    expectList(await t.api().get('/api/bd-poll-votes'));
    expectValidation(await t.api().get('/api/bd-poll-votes?filters[user][username]=x'), 'Invalid key user');
    expectValidation(await t.api().get('/api/bd-polls?populate=bd_proposal'), 'Invalid populate bd_proposal');
    // No create or update route for polls.
    expectNotFound(await t.api().post('/api/bd-polls').set(owner.auth).send({ data: {} }));
  });
});
