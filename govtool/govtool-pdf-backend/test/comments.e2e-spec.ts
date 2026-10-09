// SPEC §8.7 comments and §8.12 reports.

import { createTestApp, TestApp } from './helpers/app';
import { loginDrep, loginStake, StakeSession } from './helpers/auth';
import {
  expectBadRequestDetails,
  expectError,
  expectForbidden,
  expectList,
  expectNoKeyDeep,
  expectNotFound,
  expectSingle,
  expectUnauthorized,
  expectValidation,
} from './helpers/envelope';
import { createProposal } from './helpers/proposals';

const ISO_MS = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;
const REPORTS_POPULATE =
  'populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username';

type Item = { id: number; attributes: Record<string, any> };

describe('comments (e2e)', () => {
  let t: TestApp;
  let a: StakeSession;
  let b: StakeSession;
  let anon: StakeSession;

  beforeAll(async () => {
    t = await createTestApp();
    a = await loginStake(t, { username: 'comment_alice' });
    b = await loginStake(t, { username: 'comment_bob' });
    anon = await loginStake(t); // no govtool_username
  });
  afterAll(async () => {
    await t.close();
  });

  const comment = (s: StakeSession, data: Record<string, unknown>) =>
    t.api().post('/api/comments').set(s.auth).send({ data });
  const topLevel = (id: number, dir = 'desc') =>
    `/api/comments?filters[$and][0][proposal_id]=${id}&filters[$and][1][comment_parent_id][$null]=true&sort[createdAt]=${dir}&pagination[page]=1&pagination[pageSize]=25&${REPORTS_POPULATE}`;
  const replies = (parentId: number) =>
    `/api/comments?filters[comment_parent_id]=${parentId}&pagination[page]=1&pagination[pageSize]=3&sort[createdAt]=desc&${REPORTS_POPULATE}`;

  describe('anonymous and bad tokens', () => {
    it('GET /api/comments is public and needs no filters (Δ33)', async () => {
      expectList(await t.api().get('/api/comments'));
      expectList(await t.api().get('/api/comments').set('Authorization', 'Bearer garbage'));
    });

    it.each([
      ['post', '/api/comments'],
      ['post', '/api/comments-reports'],
      ['delete', '/api/comments-reports/1'],
    ] as const)('%s %s: no header 403, garbage Bearer 401', async (method, path) => {
      expectForbidden(await t.api()[method](path).send({ data: {} }));
      expectUnauthorized(
        await t.api()[method](path).set('Authorization', 'Bearer garbage').send({ data: {} }),
      );
    });

    it('id-less GET/PUT /api/comments-reports/ are 404', async () => {
      expectNotFound(await t.api().get('/api/comments-reports/'));
      expectNotFound(await t.api().put('/api/comments-reports/').set(a.auth));
      expectNotFound(await t.api().get('/api/comments-reports/1').set(a.auth));
    });
  });

  describe('POST /api/comments', () => {
    let proposalId: number;
    beforeAll(async () => {
      ({ proposalId } = await createProposal(t, a));
    });

    it('forces user_id and drep_id; the response has no computed attributes', async () => {
      const res = await comment(b, {
        proposal_id: String(proposalId),
        comment_text: 'Hello',
        drep_id: 'ab'.repeat(28),
        user_id: a.user.id,
        subcommens_number: 5,
      });
      const c = expectSingle(res)!;
      expect(c.attributes).toEqual({
        proposal_id: String(proposalId),
        comment_parent_id: null,
        user_id: String(b.user.id),
        comment_text: 'Hello',
        drep_id: null,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
      });
      // A DRep token sets drep_id from the token.
      const drep = await loginDrep(t, b);
      const d = expectSingle(
        await t
          .api()
          .post('/api/comments')
          .set(drep.auth)
          .send({ data: { proposal_id: proposalId, comment_text: 'As DRep', drep_id: '' } }),
      )!;
      expect(d.attributes.drep_id).toBe(drep.drepId);
    });

    it('validation errors', async () => {
      expectValidation(
        await t.api().post('/api/comments').set(a.auth).send({}),
        'Missing "data" payload in the request body',
      );
      expectBadRequestDetails(await comment(a, { proposal_id: proposalId }), 'Comment text is required');
      expectBadRequestDetails(
        await comment(a, { proposal_id: proposalId, comment_text: '' }),
        'Comment text is required',
      );
      expectBadRequestDetails(
        await comment(a, { proposal_id: proposalId, comment_text: 7 }),
        'Comment text is required',
      );
      expectBadRequestDetails(
        await comment(a, { proposal_id: proposalId, comment_text: 'x'.repeat(15001) }),
        'Comment text is required',
      );
      expect((await comment(a, { proposal_id: proposalId, comment_text: 'x'.repeat(15000) })).status).toBe(
        200,
      );
      expectBadRequestDetails(await comment(a, { comment_text: 'x' }), 'Proposal ID is required');
      // Budget discussions are gone (D167): a BD target is no target.
      expectBadRequestDetails(
        await comment(a, { comment_text: 'x', bd_proposal_id: '1' }),
        'Proposal ID is required',
      );
      expectBadRequestDetails(
        await comment(a, { comment_text: 'x', proposal_id: 999999 }),
        'Proposal not found',
      );
      expectValidation(await comment(a, { comment_text: 'x', proposal_id: 'abc' }), 'proposal_id is invalid');
    });

    it('a reply must sit on the same target (Δ34)', async () => {
      const other = await createProposal(t, a);
      const parent = expectSingle(await comment(a, { proposal_id: other.proposalId, comment_text: 'p' }))!;
      expectBadRequestDetails(
        await comment(b, {
          proposal_id: proposalId,
          comment_parent_id: String(parent.id),
          comment_text: 'r',
        }),
        'Parent comment not found',
      );
      expectBadRequestDetails(
        await comment(b, { proposal_id: proposalId, comment_parent_id: 999999, comment_text: 'r' }),
        'Parent comment not found',
      );
      const reply = expectSingle(
        await comment(b, {
          proposal_id: String(other.proposalId),
          comment_parent_id: String(parent.id),
          comment_text: 'r',
        }),
      )!;
      expect(reply.attributes.comment_parent_id).toBe(String(parent.id));
    });

    it('comments and replies increment prop_comments_number; 20 concurrent yield 20', async () => {
      const { proposalId: pid } = await createProposal(t, a);
      const top = expectSingle(await comment(b, { proposal_id: pid, comment_text: 'top' }))!;
      await comment(a, { proposal_id: pid, comment_parent_id: top.id, comment_text: 'reply' }).expect(200);
      const count = async () =>
        (await t.prisma.proposal.findUniqueOrThrow({ where: { id: pid } })).commentsNumber;
      expect(await count()).toBe(2);
      const res = await Promise.all(
        Array.from({ length: 20 }, (_, i) => comment(b, { proposal_id: pid, comment_text: `c${i}` })),
      );
      expect(res.map((r) => r.status)).toEqual(Array(20).fill(200));
      expect(await count()).toBe(22);
      // The single route reads the counter.
      expect((await t.api().get(`/api/proposals/${pid}`)).body.data.attributes.prop_comments_number).toBe(22);
    });
  });

  describe('GET /api/comments', () => {
    let proposalId: number;
    let first: number;
    let hash: string;
    beforeAll(async () => {
      ({ proposalId } = await createProposal(t, a));
      first = expectSingle(await comment(anon, { proposal_id: proposalId, comment_text: 'first' }))!.id;
      await comment(b, { proposal_id: proposalId, comment_text: 'second' }).expect(200);
      for (let i = 0; i < 4; i++) {
        await comment(a, {
          proposal_id: proposalId,
          comment_parent_id: first,
          comment_text: `reply ${i}`,
        }).expect(200);
      }
      await t.prisma.user.update({ where: { id: b.user.id }, data: { isValidated: true } });
      await t
        .api()
        .post('/api/comments-reports')
        .set(b.auth)
        .send({ data: { comment: first } })
        .expect(200);
      hash = (await t.prisma.commentsReport.findFirstOrThrow({ where: { commentId: first } })).hash;
    });

    it('Appendix A top-level query: computed attributes, reports with reporter, no hash (Δ10)', async () => {
      const res = await t.api().get(topLevel(proposalId));
      const data = expectList(res, { total: 2, pageSize: 25 }) as Item[];
      expectNoKeyDeep(res.body, 'hash');
      expect(data.map((d) => d.attributes.comment_text)).toEqual(['second', 'first']);
      const [second, firstItem] = data;
      expect(Object.keys(firstItem.attributes).sort()).toEqual(
        [
          'proposal_id',
          'comment_parent_id',
          'user_id',
          'comment_text',
          'drep_id',
          'createdAt',
          'updatedAt',
          'comments_reports',
          'user_govtool_username',
          'user_is_validated',
          'subcommens_number',
        ].sort(),
      );
      expect(firstItem.attributes).toMatchObject({
        user_govtool_username: 'Anonymous',
        user_is_validated: false,
        subcommens_number: 4,
      });
      expect(second.attributes).toMatchObject({
        user_govtool_username: 'comment_bob',
        user_is_validated: true,
        subcommens_number: 0,
        comments_reports: { data: [] },
      });
      expect(firstItem.attributes.comments_reports).toEqual({
        data: [
          {
            id: expect.any(Number),
            attributes: {
              moderation_status: null,
              createdAt: expect.stringMatching(ISO_MS),
              updatedAt: expect.stringMatching(ISO_MS),
              publishedAt: expect.stringMatching(ISO_MS),
              // fields[0]=username on a user relation yields nothing (Δ2).
              reporter: { data: { id: b.user.id, attributes: {} } },
            },
          },
        ],
      });
      const asc = expectList(await t.api().get(topLevel(proposalId, 'asc'))) as Item[];
      expect(asc.map((d) => d.attributes.comment_text)).toEqual(['first', 'second']);
    });

    it('Appendix A replies query: pageSize 3, total is the reply count', async () => {
      const data = expectList(await t.api().get(replies(first)), {
        pageSize: 3,
        total: 4,
        length: 3,
      }) as Item[];
      expect(data.map((d) => d.attributes.comment_text)).toEqual(['reply 3', 'reply 2', 'reply 1']);
      expectList(await t.api().get(replies(first).replace('pagination[page]=1', 'pagination[page]=2')), {
        page: 2,
        total: 4,
        length: 1,
      });
    });

    it('Appendix A review query by report hash ($eq only)', async () => {
      const res = await t
        .api()
        .get(
          `/api/comments?filters[comments_reports][hash][$eq]=${hash}&populate[comments_reports][populate][reporter]=*`,
        );
      const data = expectList(res, { total: 1 }) as Item[];
      expectNoKeyDeep(res.body, 'hash');
      expect(data[0].id).toBe(first);
      expect(data[0].attributes.comments_reports.data[0].attributes.reporter).toEqual({
        data: { id: b.user.id, attributes: { govtool_username: 'comment_bob' } },
      });
      expectList(await t.api().get(`/api/comments?filters[comments_reports][hash][$eq]=${'Z'.repeat(89)}`), {
        total: 0,
      });
      for (const op of ['$containsi', '$startsWith', '$ne', '$in']) {
        expectValidation(
          await t.api().get(`/api/comments?filters[comments_reports][hash][${op}]=${hash.slice(0, 3)}`),
          `Invalid operator ${op}`,
        );
      }
      expectValidation(
        await t.api().get('/api/comments?sort[comments_reports][hash]=asc'),
        'Invalid key comments_reports.hash',
      );
      expectValidation(
        await t.api().get('/api/comments?populate=comments_reports.moderator'),
        'Invalid populate comments_reports.moderator',
      );
      expectValidation(await t.api().get('/api/comments?filters[user][username]=x'), 'Invalid key user');
      expectValidation(
        await t.api().get('/api/comments?filters[bd_proposal_id]=1'),
        'Invalid key bd_proposal_id',
      );
    });

    it('filters on moderation_status', async () => {
      await t.prisma.commentsReport.updateMany({
        where: { commentId: first },
        data: { moderationStatus: true },
      });
      const data = expectList(
        await t
          .api()
          .get(
            `/api/comments?filters[comments_reports][moderation_status]=true&filters[proposal_id]=${proposalId}`,
          ),
        { total: 1 },
      ) as Item[];
      expect(data[0].id).toBe(first);
    });
  });

  describe('comment reports (§8.12)', () => {
    let commentId: number;
    beforeAll(async () => {
      const { proposalId } = await createProposal(t, a);
      commentId = expectSingle(await comment(a, { proposal_id: proposalId, comment_text: 'report me' }))!.id;
    });
    const report = (s: StakeSession, data: Record<string, unknown>) =>
      t.api().post('/api/comments-reports').set(s.auth).send({ data });

    it('forces reporter, moderator, status and hash (Δ42); no hash on the wire', async () => {
      const res = await report(b, {
        comment: commentId,
        reporter: a.user.id,
        moderator: a.user.id,
        moderation_status: true,
        hash: 'x',
      });
      const r = expectSingle(res)!;
      expect(r.attributes).toEqual({
        moderation_status: null,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
        publishedAt: expect.stringMatching(ISO_MS),
      });
      const row = await t.prisma.commentsReport.findUniqueOrThrow({ where: { id: r.id } });
      expect(row).toMatchObject({ reporterId: b.user.id, moderatorId: null, moderationStatus: null });
      expect(row.hash).toMatch(/^[A-Za-z0-9]{89}$/);
    });

    it('errors: mandatory, not found, already reported', async () => {
      expectError(await report(a, {}), 400, 'BadRequestError', 'Comment is mandatory.');
      expectError(await report(a, { comment: 999999 }), 400, 'BadRequestError', 'Comment not found');
      expectError(
        await report(b, { comment: String(commentId) }),
        400,
        'BadRequestError',
        'Comment already reported',
      );
    });

    it('DELETE: only the reporter (Δ43)', async () => {
      const r = expectSingle(await report(anon, { comment: commentId }))!;
      expectForbidden(
        await t.api().delete(`/api/comments-reports/${r.id}`).set(a.auth),
        "You can't access this entry",
      );
      expectNotFound(await t.api().delete('/api/comments-reports/999999').set(anon.auth));
      const del = expectSingle(await t.api().delete(`/api/comments-reports/${r.id}`).set(anon.auth))!;
      expect(del.id).toBe(r.id);
      expect(del.attributes).not.toHaveProperty('hash');
      expect(await t.prisma.commentsReport.count({ where: { id: r.id } })).toBe(0);
    });
  });
});
