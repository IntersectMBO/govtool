// SPEC §8.5 polls and §8.6 poll votes.

import { createTestApp, TestApp } from './helpers/app';
import { loginStake, StakeSession } from './helpers/auth';
import {
  expectBadRequestDetails,
  expectForbidden,
  expectList,
  expectNotFound,
  expectSingle,
  expectUnauthorized,
  expectValidation,
} from './helpers/envelope';
import { createPoll, createProposal } from './helpers/proposals';

const ISO_MS = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;

describe('polls (e2e)', () => {
  let t: TestApp;
  let owner: StakeSession;
  let voter: StakeSession;

  beforeAll(async () => {
    t = await createTestApp();
    owner = await loginStake(t, { username: 'poll_owner' });
    voter = await loginStake(t, { username: 'poll_voter' });
  });
  afterAll(async () => {
    await t.close();
  });

  const activeQuery = (proposalId: number, active: boolean) =>
    `/api/polls?filters[$and][0][proposal_id][$eq]=${proposalId}&filters[$and][1][is_poll_active]=${active}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
  const voteQuery = (pollId: number) =>
    `/api/poll-votes?filters[poll_id][$eq]=${pollId}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;

  describe('anonymous and bad tokens', () => {
    it('GET /api/polls is public, also with a garbage Bearer', async () => {
      expectList(await t.api().get('/api/polls'));
      expectList(await t.api().get('/api/polls').set('Authorization', 'Bearer garbage'));
    });

    it('boolean filters take $in/$notIn and refuse ordering operators (never a 500)', async () => {
      const { proposalId } = await createProposal(t, owner);
      const pollId = await createPoll(t, owner, proposalId);
      const ids = async (q: string) =>
        expectList(await t.api().get(`/api/polls?filters[proposal_id]=${proposalId}&${q}`)).map((p) => p.id);
      expect(await ids('filters[is_poll_active][$in][0]=true')).toEqual([pollId]);
      expect(await ids('filters[is_poll_active][$in]=true,false')).toEqual([pollId]);
      expect(await ids('filters[is_poll_active][$notIn][0]=true')).toEqual([]);
      expect(await ids('filters[is_poll_active][$in]=')).toEqual([]);
      // A constant FALSE (`$null` on a non-null column) must stay FALSE when nested.
      expect(await ids('filters[createdAt][$null]=true')).toEqual([]);
      expect(await ids('filters[$or][0][createdAt][$null]=true&filters[$or][1][id]=0')).toEqual([]);
      expect(await ids('filters[$or][0][createdAt][$notNull]=true&filters[$or][1][id]=0')).toEqual([pollId]);
      for (const op of ['$lt', '$lte', '$gt', '$gte']) {
        expectValidation(
          await t.api().get(`/api/polls?filters[is_poll_active][${op}]=true`),
          `Invalid operator ${op}`,
        );
      }
    });

    it.each([
      ['post', '/api/polls'],
      ['put', '/api/polls/1'],
      ['get', '/api/poll-votes'],
      ['post', '/api/poll-votes'],
      ['put', '/api/poll-votes/1'],
    ] as const)('%s %s: no header 403, garbage Bearer 401', async (method, path) => {
      expectForbidden(await t.api()[method](path).send({ data: {} }));
      expectUnauthorized(
        await t.api()[method](path).set('Authorization', 'Bearer garbage').send({ data: {} }),
      );
    });
  });

  describe('POST /api/polls', () => {
    it('forces every field but the proposal (Δ30); one active poll per proposal', async () => {
      const { proposalId } = await createProposal(t, owner);
      const before = Date.now();
      const res = await t
        .api()
        .post('/api/polls')
        .set(owner.auth)
        .send({
          data: {
            proposal_id: String(proposalId),
            poll_yes: 50,
            poll_no: 7,
            is_poll_active: false,
            poll_start_dt: '2000-01-01T00:00:00.000Z',
          },
        });
      const poll = expectSingle(res)!;
      expect(poll.attributes).toEqual({
        proposal_id: String(proposalId),
        poll_yes: 0,
        poll_no: 0,
        poll_start_dt: expect.stringMatching(ISO_MS),
        is_poll_active: true,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
      });
      expect(Date.parse(poll.attributes.poll_start_dt as string)).toBeGreaterThanOrEqual(before - 1000);
      expectBadRequestDetails(
        await t
          .api()
          .post('/api/polls')
          .set(owner.auth)
          .send({ data: { proposal_id: proposalId } }),
        'There is already an active pool for this proposal',
      );
    });

    it('errors: missing proposal, not the owner', async () => {
      const { proposalId } = await createProposal(t, owner);
      const post = (s: StakeSession, data: Record<string, unknown>) =>
        t.api().post('/api/polls').set(s.auth).send({ data });
      expectBadRequestDetails(await post(owner, {}), 'Proposal not found');
      expectBadRequestDetails(await post(owner, { proposal_id: 999999 }), 'Proposal not found');
      expectForbidden(await post(voter, { proposal_id: proposalId }), 'User is not owner of this proposal');
    });

    it('concurrent creates leave exactly one active poll', async () => {
      const { proposalId } = await createProposal(t, owner);
      const res = await Promise.all(
        Array.from({ length: 5 }, () =>
          t
            .api()
            .post('/api/polls')
            .set(owner.auth)
            .send({ data: { proposal_id: proposalId } }),
        ),
      );
      expect(res.filter((r) => r.status === 200)).toHaveLength(1);
      for (const r of res.filter((x) => x.status !== 200)) {
        expectBadRequestDetails(r, 'There is already an active pool for this proposal');
      }
      expect(await t.prisma.poll.count({ where: { proposalId, isActive: true } })).toBe(1);
    });
  });

  describe('PUT /api/polls/:id', () => {
    it('closes only (Δ31), owner only; a new poll can then open', async () => {
      const { proposalId } = await createProposal(t, owner);
      const pollId = await createPoll(t, owner, proposalId);
      const put = (s: StakeSession, id: number | string, data: Record<string, unknown>) =>
        t.api().put(`/api/polls/${id}`).set(s.auth).send({ data });
      expectBadRequestDetails(await put(owner, 999999, { is_poll_active: false }), 'Poll not found');
      expectBadRequestDetails(await put(owner, 'abc', { is_poll_active: false }), 'Poll not found');
      expectForbidden(
        await put(voter, pollId, { is_poll_active: false }),
        'User is not authorized to update this Poll.',
      );
      expectValidation(
        await put(owner, pollId, { is_poll_active: true }),
        'Only closing a poll is supported',
      );
      expectValidation(await put(owner, pollId, {}), 'Only closing a poll is supported');

      const closed = expectSingle(await put(owner, pollId, { is_poll_active: false, poll_yes: 9 }))!;
      expect(closed.attributes).toMatchObject({ is_poll_active: false, poll_yes: 0 });
      expectValidation(
        await put(owner, pollId, { is_poll_active: true }),
        'Only closing a poll is supported',
      );

      const second = await createPoll(t, owner, proposalId);
      const active = expectList(await t.api().get(activeQuery(proposalId, true)), { pageSize: 1, total: 1 });
      expect(active[0].id).toBe(second);
      const inactive = expectList(await t.api().get(activeQuery(proposalId, false)), {
        pageSize: 1,
        total: 1,
      });
      expect(inactive[0].id).toBe(pollId);
    });
  });

  describe('poll votes', () => {
    let proposalId: number;
    let pollId: number;
    beforeAll(async () => {
      ({ proposalId } = await createProposal(t, owner));
      pollId = await createPoll(t, owner, proposalId);
    });
    const counts = async (id = pollId) => {
      const p = await t.prisma.poll.findUniqueOrThrow({ where: { id } });
      return [p.yes, p.no];
    };

    it('POST errors in order; forced user_id; yes/no counters', async () => {
      const post = (data: Record<string, unknown>) =>
        t.api().post('/api/poll-votes').set(voter.auth).send({ data });
      expectBadRequestDetails(await post({ poll_id: pollId }), 'Vote result is required');
      expectBadRequestDetails(await post({ vote_result: true }), 'Poll ID is required');
      expectBadRequestDetails(await post({ poll_id: 999999, vote_result: true }), 'Poll not found');

      const vote = expectSingle(
        await post({ poll_id: `${pollId}`, vote_result: true, user_id: owner.user.id }),
      )!;
      expect(vote.attributes).toEqual({
        poll_id: String(pollId),
        user_id: String(voter.user.id),
        vote_result: true,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
      });
      expect(await counts()).toEqual([1, 0]);
      expectBadRequestDetails(
        await post({ poll_id: pollId, vote_result: false }),
        'Poll vote for this user already exist',
      );

      // A second voter says no.
      const other = await loginStake(t);
      await t
        .api()
        .post('/api/poll-votes')
        .set(other.auth)
        .send({ data: { poll_id: pollId, vote_result: false } })
        .expect(200);
      expect(await counts()).toEqual([1, 1]);
    });

    it('GET is scoped to the caller; Appendix A string returns the vote as an array', async () => {
      const mine = expectList(await t.api().get(voteQuery(pollId)).set(voter.auth), {
        pageSize: 1,
        total: 1,
      });
      expect(mine[0].attributes).toMatchObject({ user_id: String(voter.user.id), vote_result: true });
      expectList(await t.api().get(voteQuery(pollId)).set(owner.auth), { total: 0, length: 0 });
      // A client user_id is replaced by the caller.
      const spoof = expectList(
        await t.api().get(`/api/poll-votes?filters[user_id]=${voter.user.id}`).set(owner.auth),
        { total: 0 },
      );
      expect(spoof).toEqual([]);
    });

    it('PUT flips counters; owner only; already updated; never negative', async () => {
      const vote = await t.prisma.pollVote.findFirstOrThrow({ where: { pollId, userId: voter.user.id } });
      const put = (s: StakeSession, data: Record<string, unknown>, id: number | string = vote.id) =>
        t.api().put(`/api/poll-votes/${id}`).set(s.auth).send({ data });
      expectForbidden(await put(owner, { vote_result: false }), "You can't access this entry");
      expectNotFound(await put(voter, { vote_result: false }, 999999));
      expectBadRequestDetails(await put(voter, { vote_result: 'no' }), 'Vote result is required');
      expectBadRequestDetails(await put(voter, { vote_result: true }), 'Poll vote already updated');

      const [yes, no] = await counts();
      expect(expectSingle(await put(voter, { vote_result: false }))!.attributes.vote_result).toBe(false);
      expect(await counts()).toEqual([yes - 1, no + 1]);

      await t.prisma.poll.update({ where: { id: pollId }, data: { yes: 0, no: 0 } });
      await put(voter, { vote_result: true }).expect(200);
      expect(await counts()).toEqual([1, 0]);
    });

    it('closed polls take no votes and no changes (Δ32)', async () => {
      const { proposalId: pid } = await createProposal(t, owner);
      const pid2 = await createPoll(t, owner, pid);
      const v = expectSingle(
        await t
          .api()
          .post('/api/poll-votes')
          .set(voter.auth)
          .send({ data: { poll_id: pid2, vote_result: true } }),
      )!;
      await t
        .api()
        .put(`/api/polls/${pid2}`)
        .set(owner.auth)
        .send({ data: { is_poll_active: false } })
        .expect(200);
      const other = await loginStake(t);
      expectBadRequestDetails(
        await t
          .api()
          .post('/api/poll-votes')
          .set(other.auth)
          .send({ data: { poll_id: pid2, vote_result: true } }),
        'Poll is not active',
      );
      expectBadRequestDetails(
        await t
          .api()
          .put(`/api/poll-votes/${v.id}`)
          .set(voter.auth)
          .send({ data: { vote_result: false } }),
        'Poll is not active',
      );
      // Rolled back: the vote and the counters are unchanged.
      expect((await t.prisma.pollVote.findUniqueOrThrow({ where: { id: v.id } })).voteResult).toBe(true);
      expect(await counts(pid2)).toEqual([1, 0]);
    });

    it('concurrent votes by many users count exactly', async () => {
      const { proposalId: pid } = await createProposal(t, owner);
      const id = await createPoll(t, owner, pid);
      const users = await Promise.all(Array.from({ length: 8 }, () => loginStake(t)));
      const res = await Promise.all(
        users.map((u, i) =>
          t
            .api()
            .post('/api/poll-votes')
            .set(u.auth)
            .send({ data: { poll_id: id, vote_result: i % 2 === 0 } }),
        ),
      );
      expect(res.map((r) => r.status)).toEqual(Array(8).fill(200));
      expect(await counts(id)).toEqual([4, 4]);
    });
  });
});
