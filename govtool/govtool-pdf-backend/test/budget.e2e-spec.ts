// SPEC §8.8 budget discussions: create, versions, delete, lock, list and
// single reads, plus the anonymous matrix of every budget route.

import { createTestApp, TestApp } from './helpers/app';
import { loginStake, StakeSession } from './helpers/auth';
import {
  expectError,
  expectForbidden,
  expectList,
  expectNoKeyDeep,
  expectNotFound,
  expectSingle,
  expectUnauthorized,
  expectValidation,
} from './helpers/envelope';
import { bdPayload, createBd, DETAIL_QUERY, editFlowBody } from './budget-fixtures';

const listQuery = (typeId: number, search: string, sort: string, creator?: number, page = 1) =>
  `filters[$and][0][is_active]=true&filters[$and][1][bd_psapb][type_name][id]=${typeId}&filters[$and][2][bd_proposal_detail][proposal_name][$containsi]=${search}${creator !== undefined ? `&filters[$and][3][creator]=${creator}` : ''}&pagination[page]=${page}&pagination[pageSize]=25&${sort}&populate[0]=bd_costing&populate[1]=bd_psapb.type_name&populate[2]=bd_proposal_detail&populate[3]=creator`;

const ISO_MS = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;

describe('budget discussions (e2e)', () => {
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

  describe('anonymous matrix (every budget route)', () => {
    const PUBLIC = ['/api/bds', '/api/bds/1', '/api/bd/versions/1', '/api/bd-polls', '/api/bd-poll-votes'];
    const AUTHENTICATED = [
      ['post', '/api/bds'],
      ['delete', '/api/bds/1'],
      ['get', '/api/bd-drafts'],
      ['post', '/api/bd-drafts'],
      ['put', '/api/bd-drafts/1'],
      ['delete', '/api/bd-drafts/1'],
      ['post', '/api/bd-poll-votes'],
      ['put', '/api/bd-poll-votes/1'],
    ] as const;

    let masterId: string;
    beforeAll(async () => {
      masterId = (await createBd(t, alice, 'Anonymous matrix')).master_id;
    });

    it.each(PUBLIC)('GET %s is public, also with a garbage Bearer (Δ5)', async (path) => {
      const p = path.replace('/1', `/${masterId}`);
      for (const auth of [undefined, 'Bearer garbage']) {
        const req = t.api().get(p);
        const res = await (auth ? req.set('Authorization', auth) : req);
        expect(res.status).toBe(200);
        if (p.startsWith('/api/bds/')) expectSingle(res);
        else if (p.startsWith('/api/bd/versions/')) expect(res.body.data).toHaveLength(1);
        else expectList(res);
      }
    });

    it.each(AUTHENTICATED)('%s %s: no header 403, garbage Bearer 401', async (method, path) => {
      expectForbidden(await t.api()[method](path).send({ data: {} }));
      expectUnauthorized(
        await t.api()[method](path).set('Authorization', 'Bearer garbage').send({ data: {} }),
      );
    });
  });

  describe('POST /api/bds (new BD)', () => {
    it('returns the raw create shape with a top-level master_id', async () => {
      const res = await t
        .api()
        .post('/api/bds')
        .set(alice.auth)
        .send({
          data: bdPayload('Shape', {
            bd_contact_information: {
              be_email: 'secret@example.com',
              be_full_name: 'Secret',
              be_nationality: 2,
            },
          }),
        })
        .expect(200);
      const b = res.body;
      expect(b).not.toHaveProperty('data');
      expect(Object.keys(b).sort()).toEqual(
        [
          'id',
          'master_id',
          'privacy_policy',
          'intersect_named_administrator',
          'intersect_admin_further_text',
          'prop_comments_number',
          'is_active',
          'submitted_for_vote',
          'createdAt',
          'updatedAt',
          'bd_proposal_ownership',
          'bd_psapb',
          'bd_proposal_detail',
          'bd_costing',
          'bd_further_information',
          'creator',
        ].sort(),
      );
      expect(b).toMatchObject({
        id: expect.any(Number),
        master_id: String(b.id),
        privacy_policy: true,
        intersect_named_administrator: false,
        intersect_admin_further_text: 'Further',
        prop_comments_number: 0,
        is_active: true,
        submitted_for_vote: null,
        creator: { id: alice.user.id, govtool_username: 'alice' },
      });
      expect(Object.keys(b.creator).sort()).toEqual(['govtool_username', 'id']);
      expect(b.createdAt).toMatch(ISO_MS);
      // Sections: plain objects of their scalars, no lookup relations.
      expect(b.bd_costing).toMatchObject({
        id: expect.any(Number),
        ada_amount: '1000',
        usd_to_ada_conversion_rate: '0,5',
        amount_in_preferred_currency: '500',
        ada_amount_clone: 1000,
        usd_to_ada_conversion_rate_clone: 0.5,
        amount_in_preferred_currency_clone: 500,
      });
      expect(b.bd_costing).not.toHaveProperty('preferred_currency');
      expect(b.bd_psapb).not.toHaveProperty('type_name');
      expect(b.bd_proposal_detail.proposal_name).toBe('Shape');
      expect(b.bd_further_information).toMatchObject({
        id: expect.any(Number),
        proposal_links: [{ id: expect.any(Number), prop_link: 'https://example.com/a', prop_link_text: 'A' }],
      });
      for (const key of ['bd_contact_information', 'email', 'username', 'be_email']) expectNoKeyDeep(b, key);
      // Stored all the same (legacy drafts), just never returned (Δ11).
      const row = await t.prisma.bd.findUniqueOrThrow({
        where: { id: b.id },
        include: { contactInformation: true },
      });
      expect(row.contactInformation).toMatchObject({ beEmail: 'secret@example.com', beNationalityId: 2 });
    });

    it('auto-creates an active bd-poll on the master id (Playwright 11K)', async () => {
      const bd = await createBd(t, alice, 'Poll auto-create');
      const q = `filters[$and][0][bd_proposal_id][$eq]=${bd.master_id}&filters[$and][1][is_poll_active]=true&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`;
      const polls = expectList(await t.api().get(`/api/bd-polls?${q}`), { total: 1, length: 1 });
      expect(polls[0].attributes).toEqual({
        bd_proposal_id: bd.master_id,
        poll_yes: 0,
        poll_no: 0,
        is_poll_active: true,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
      });
    });

    it('forces creator, counters and submission state; ignores mass-assigned fields (Δ3)', async () => {
      const bd = await createBd(t, alice, 'Forced', {
        creator: bob.user.id,
        is_active: false,
        prop_comments_number: 50,
        submitted_for_vote: '2026-01-01T00:00:00.000Z',
        id: 12345,
      });
      expect(bd).toMatchObject({
        creator: { id: alice.user.id },
        is_active: true,
        prop_comments_number: 0,
        submitted_for_vote: null,
      });
      expect(bd.id).not.toBe(12345);
    });

    it('privacy policy, required sections and lookup ids', async () => {
      const post = (data: Record<string, unknown>) => t.api().post('/api/bds').set(alice.auth).send({ data });
      expectError(
        await post(bdPayload('x', { privacy_policy: false })),
        400,
        'BadRequestError',
        'Privacy policy must be accepted',
      );
      const noPsapb = bdPayload('x');
      delete noPsapb.bd_psapb;
      expectValidation(await post(noPsapb), 'bd_psapb is required');
      expectValidation(
        await post(bdPayload('x', { bd_further_information: null })),
        'bd_further_information is required',
      );
      expectValidation(
        await t.api().post('/api/bds').set(alice.auth).send({}),
        'Missing "data" payload in the request body',
      );

      const before = await t.prisma.bdCosting.count();
      const bad = bdPayload('x');
      (bad.bd_psapb as Record<string, unknown>).type_name = 999;
      expectValidation(await post(bad), 'type_name is invalid');
      const badCurrency = bdPayload('x');
      (badCurrency.bd_costing as Record<string, unknown>).preferred_currency = 77;
      expectValidation(await post(badCurrency), 'preferred_currency is invalid');
      // Nothing half-written.
      expect(await t.prisma.bdCosting.count()).toBe(before);
    });

    it('rejects a javascript: link and writes nothing (Δ46)', async () => {
      const before = await t.prisma.bd.count();
      const p = bdPayload('XSS', {
        bd_further_information: {
          proposal_links: [
            { prop_link: '' },
            { prop_link: 'javascript:alert(document.cookie)', prop_link_text: 'x' },
          ],
        },
      });
      expectValidation(
        await t.api().post('/api/bds').set(alice.auth).send({ data: p }),
        'prop_link is invalid',
      );
      expect(await t.prisma.bd.count()).toBe(before);
    });

    it('accepts numeric amounts and stores them as strings; null lookups are fine', async () => {
      const p = bdPayload('Numbers');
      p.bd_costing = {
        ada_amount: 12.5,
        usd_to_ada_conversion_rate: 3,
        amount_in_preferred_currency: '',
        preferred_currency: null,
      };
      (p.bd_proposal_ownership as Record<string, unknown>).be_country = null;
      const res = await t.api().post('/api/bds').set(alice.auth).send({ data: p }).expect(200);
      expect(res.body.bd_costing).toMatchObject({
        ada_amount: '12.5',
        usd_to_ada_conversion_rate: '3',
        amount_in_preferred_currency: '',
        amount_in_preferred_currency_clone: 0,
      });
    });
  });

  describe('GET /api/bds/:id (master id)', () => {
    it('answers the active version with pdf-ui’s detail populate', async () => {
      const bd = await createBd(t, alice, 'Detail');
      const res = await t.api().get(`/api/bds/${bd.master_id}?${DETAIL_QUERY}`);
      const data = expectSingle(res)!;
      const a = data.attributes as Record<string, any>;
      expect(data.id).toBe(bd.id);
      expect(a).toMatchObject({
        master_id: bd.master_id,
        is_active: true,
        submitted_for_vote: null,
        prop_comments_number: 0,
        user_govtool_username: 'alice',
        master_proposal_created_at: bd.createdAt,
      });
      expect(a).toHaveProperty('submitted_for_vote', null);
      expect(a.creator).toEqual({ data: { id: alice.user.id, attributes: { govtool_username: 'alice' } } });
      expect(a.bd_psapb.data.attributes.type_name.data).toMatchObject({
        id: 1,
        attributes: { type_name: 'Core' },
      });
      expect(a.bd_psapb.data.attributes.roadmap_name.data.id).toBe(10);
      expect(a.bd_psapb.data.attributes.committee_name.data.id).toBe(2);
      expect(a.bd_proposal_detail.data.attributes.contract_type_name.data.id).toBe(1);
      expect(a.bd_proposal_ownership.data.attributes.be_country.data).toMatchObject({
        id: 1,
        attributes: { country_name: 'Nepal' },
      });
      expect(a.bd_costing.data.attributes).toMatchObject({
        ada_amount: '1000',
        usd_to_ada_conversion_rate: '0,5',
      });
      expect(a.bd_costing.data.attributes.preferred_currency.data.attributes.currency_name).toBe(
        'United States Dollar',
      );
      // Component inline, not a {data} relation.
      expect(a.bd_further_information.data.attributes.proposal_links).toEqual([
        { id: expect.any(Number), prop_link: 'https://example.com/a', prop_link_text: 'A' },
      ]);
      for (const key of ['bd_contact_information', 'email', 'username']) expectNoKeyDeep(res.body, key);
    });

    it('unpopulated relations never appear; computed attributes always do', async () => {
      const bd = await createBd(t, alice, 'Bare');
      const a = expectSingle(await t.api().get(`/api/bds/${bd.master_id}`))!.attributes;
      expect(Object.keys(a).sort()).toEqual(
        [
          'privacy_policy',
          'intersect_named_administrator',
          'intersect_admin_further_text',
          'prop_comments_number',
          'is_active',
          'master_id',
          'submitted_for_vote',
          'createdAt',
          'updatedAt',
          'master_proposal_created_at',
          'user_govtool_username',
        ].sort(),
      );
    });

    it('404 "Not Found" for an unknown or non-numeric id (Δ35)', async () => {
      expectNotFound(await t.api().get('/api/bds/999999'));
      expectNotFound(await t.api().get(`/api/bds/abc?${DETAIL_QUERY}`));
      // A non-master row id is not a master id.
      const bd = await createBd(t, alice, 'Versioned for 404');
      const v2 = await t
        .api()
        .post('/api/bds')
        .set(alice.auth)
        .send({ data: bdPayload('v2', { master_id: bd.master_id }) })
        .expect(200);
      expectNotFound(await t.api().get(`/api/bds/${v2.body.id}`));
    });

    it('bd_contact_information is not populatable (Δ11)', async () => {
      const bd = await createBd(t, alice, 'Contact');
      expectValidation(
        await t.api().get(`/api/bds/${bd.master_id}?populate=bd_contact_information`),
        'Invalid populate bd_contact_information',
      );
      expectValidation(
        await t.api().get('/api/bds?filters[creator][username]=x'),
        'Invalid key creator.username',
      );
    });

    it('Anonymous when the creator has no govtool_username', async () => {
      const anon = await loginStake(t);
      const bd = await createBd(t, anon, 'Nameless');
      const a = expectSingle(await t.api().get(`/api/bds/${bd.master_id}`))!.attributes;
      expect(a.user_govtool_username).toBe('Anonymous');
    });
  });

  describe('edit flow: POST with master_id', () => {
    it('creates a new version from pdf-ui’s cleaned echo and deactivates the old one', async () => {
      const bd = await createBd(t, alice, 'Edit me');
      // Comments counted on the live version are copied onto the new one.
      await t.prisma.bd.update({ where: { id: bd.id }, data: { commentsNumber: 3 } });

      const got = expectSingle(await t.api().get(`/api/bds/${bd.master_id}?${DETAIL_QUERY}`))!;
      const body = editFlowBody(got, bd.master_id);
      // The echo carries attributes, a flattened creator and computed values.
      expect(body).toMatchObject({
        is_active: true,
        prop_comments_number: 3,
        creator: { govtool_username: 'alice' },
      });
      body.bd_proposal_detail.proposal_name = 'Edited';
      body.prop_comments_number = 99;
      body.is_active = false;
      body.submitted_for_vote = '2026-01-01T00:00:00.000Z';

      const res = await t.api().post('/api/bds').set(alice.auth).send({ data: body }).expect(200);
      const v2 = res.body;
      expect(v2).toMatchObject({
        master_id: bd.master_id,
        is_active: true,
        prop_comments_number: 3,
        submitted_for_vote: null,
        creator: { id: alice.user.id },
      });
      expect(v2.id).not.toBe(bd.id);
      expect(v2.bd_proposal_detail.proposal_name).toBe('Edited');
      expect(v2.bd_costing).toMatchObject({ ada_amount: '1000', usd_to_ada_conversion_rate: '0,5' });
      expect(v2.bd_further_information.proposal_links).toEqual([
        { id: expect.any(Number), prop_link: 'https://example.com/a', prop_link_text: 'A' },
      ]);

      const old = await t.prisma.bd.findUniqueOrThrow({ where: { id: bd.id } });
      expect(old.isActive).toBe(false);

      // GET by master id now answers v2 with the same relation ids.
      const live = expectSingle(await t.api().get(`/api/bds/${bd.master_id}?${DETAIL_QUERY}`))!;
      expect(live.id).toBe(v2.id);
      const la = live.attributes as Record<string, any>;
      expect(la.master_proposal_created_at).toBe(bd.createdAt);
      expect(la.bd_psapb.data.attributes.type_name.data.id).toBe(1);
      expect(la.bd_proposal_ownership.data.attributes.be_country.data.id).toBe(1);

      // Versions: newest first, one live, fixed populate, public creator.
      const vres = await t.api().get(`/api/bd/versions/${bd.master_id}`).expect(200);
      expect(Object.keys(vres.body).sort()).toEqual(['data', 'meta']);
      expect(vres.body.meta).toEqual({});
      const versions = vres.body.data as Array<{ id: number; attributes: Record<string, any> }>;
      expect(versions.map((v) => [v.id, v.attributes.is_active])).toEqual([
        [v2.id, true],
        [bd.id, false],
      ]);
      const va = versions[0].attributes;
      expect(va.creator.data).toEqual({ id: alice.user.id, attributes: { govtool_username: 'alice' } });
      expect(va.bd_costing.data.attributes.preferred_currency.data.id).toBe(1);
      expect(va.bd_proposal_detail.data.attributes.contract_type_name.data.id).toBe(1);
      expect(va.bd_psapb.data.attributes.committee_name.data.id).toBe(2);
      expect(va.bd_further_information.data.attributes.proposal_links).toHaveLength(1);
      expectNoKeyDeep(vres.body, 'email');
      expectNoKeyDeep(vres.body, 'bd_contact_information');

      // The poll stays on the master id: still exactly one.
      expect(await t.prisma.bdPoll.count({ where: { bdMasterId: Number(bd.master_id) } })).toBe(1);
    });

    it('versions of an unknown or non-numeric id are []', async () => {
      expect((await t.api().get('/api/bd/versions/999999').expect(200)).body).toEqual({ data: [], meta: {} });
      expect((await t.api().get('/api/bd/versions/abc').expect(200)).body).toEqual({ data: [], meta: {} });
    });

    it('owner only: another user gets 403 Unauthorized; unknown chain 404', async () => {
      const bd = await createBd(t, alice, 'Not yours');
      expectForbidden(
        await t
          .api()
          .post('/api/bds')
          .set(bob.auth)
          .send({ data: bdPayload('Hijack', { master_id: bd.master_id }) }),
        'Unauthorized',
      );
      expectNotFound(
        await t
          .api()
          .post('/api/bds')
          .set(alice.auth)
          .send({ data: bdPayload('Ghost', { master_id: 999999 }) }),
      );
      const live = await t.prisma.bd.findFirstOrThrow({
        where: { masterId: Number(bd.master_id), isActive: true },
      });
      expect(live.id).toBe(bd.id);
    });

    it('keeps exactly one active version under concurrent posts (Δ38)', async () => {
      const bd = await createBd(t, alice, 'Race');
      const results = await Promise.all(
        Array.from({ length: 5 }, (_, i) =>
          t
            .api()
            .post('/api/bds')
            .set(alice.auth)
            .send({ data: bdPayload(`Race ${i}`, { master_id: bd.master_id }) }),
        ),
      );
      expect(results.map((r) => r.status)).toEqual([200, 200, 200, 200, 200]);
      const versions = await t.prisma.bd.findMany({ where: { masterId: Number(bd.master_id) } });
      expect(versions).toHaveLength(6);
      expect(versions.filter((v) => v.isActive)).toHaveLength(1);
    });
  });

  describe('submission lock', () => {
    it('blocks a new version and deletion once any version is submitted', async () => {
      const bd = await createBd(t, alice, 'Locked');
      await t.prisma.bd.update({ where: { id: bd.id }, data: { submittedForVote: new Date() } });
      expectValidation(
        await t
          .api()
          .post('/api/bds')
          .set(alice.auth)
          .send({ data: bdPayload('v2', { master_id: bd.master_id }) }),
        'Update is not allowed because this entry has already been submitted for voting.',
      );
      expectValidation(
        await t.api().delete(`/api/bds/${bd.id}`).set(alice.auth),
        'Deletion is not allowed because this entry has already been submitted for voting.',
      );
      const a = expectSingle(await t.api().get(`/api/bds/${bd.master_id}`))!.attributes;
      expect(a.submitted_for_vote).toMatch(ISO_MS);
    });

    it('applies when an older version carries the flag', async () => {
      const bd = await createBd(t, alice, 'Old flag');
      await t
        .api()
        .post('/api/bds')
        .set(alice.auth)
        .send({ data: bdPayload('v2', { master_id: bd.master_id }) })
        .expect(200);
      await t.prisma.bd.update({ where: { id: bd.id }, data: { submittedForVote: new Date() } });
      expectValidation(
        await t
          .api()
          .post('/api/bds')
          .set(alice.auth)
          .send({ data: bdPayload('v3', { master_id: bd.master_id }) }),
        'Update is not allowed because this entry has already been submitted for voting.',
      );
    });
  });

  describe('DELETE /api/bds/:id (row id)', () => {
    it('owner only, 404 for a missing row', async () => {
      const bd = await createBd(t, alice, 'Keep');
      expectForbidden(
        await t.api().delete(`/api/bds/${bd.id}`).set(bob.auth),
        "You can't delete this proposal.",
      );
      expectNotFound(await t.api().delete('/api/bds/999999').set(alice.auth));
      expectNotFound(await t.api().delete('/api/bds/abc').set(alice.auth));
      expect(await t.prisma.bd.count({ where: { id: bd.id } })).toBe(1);
    });

    it('removes the whole chain: versions, sections, links, poll, votes, comments (Δ40)', async () => {
      const bd = await createBd(t, alice, 'Doomed');
      const master = Number(bd.master_id);
      const v2 = (
        await t
          .api()
          .post('/api/bds')
          .set(alice.auth)
          .send({ data: bdPayload('Doomed v2', { master_id: bd.master_id }) })
          .expect(200)
      ).body;
      const poll = await t.prisma.bdPoll.findFirstOrThrow({ where: { bdMasterId: master } });
      await t.prisma.bdPollVote.create({
        data: {
          bdPollId: poll.id,
          userId: bob.user.id,
          voteResult: true,
          drepId: 'a'.repeat(56),
          drepVotingPower: '1',
        },
      });
      const top = await t.prisma.comment.create({
        data: { bdMasterId: master, userId: bob.user.id, text: 'hi' },
      });
      await t.prisma.comment.create({
        data: { bdMasterId: master, userId: bob.user.id, text: 'reply', parentId: top.id },
      });
      const sections = await t.prisma.bd.findMany({ where: { masterId: master } });
      const other = await createBd(t, alice, 'Bystander');

      const res = await t.api().delete(`/api/bds/${v2.id}`).set(alice.auth);
      const data = expectSingle(res)!;
      expect(data.id).toBe(v2.id);
      expect(data.attributes).toMatchObject({ master_id: bd.master_id, is_active: true });
      expect(data.attributes).not.toHaveProperty('bd_costing');

      expect(await t.prisma.bd.count({ where: { masterId: master } })).toBe(0);
      expect(await t.prisma.bdPoll.count({ where: { bdMasterId: master } })).toBe(0);
      expect(await t.prisma.bdPollVote.count({ where: { bdPollId: poll.id } })).toBe(0);
      expect(await t.prisma.comment.count({ where: { bdMasterId: master } })).toBe(0);
      const ids = (
        k: 'costingId' | 'psapbId' | 'proposalDetailId' | 'proposalOwnershipId' | 'furtherInformationId',
      ) => sections.map((s) => s[k]!).filter((x) => x !== null);
      expect(await t.prisma.bdCosting.count({ where: { id: { in: ids('costingId') } } })).toBe(0);
      expect(await t.prisma.bdPsapb.count({ where: { id: { in: ids('psapbId') } } })).toBe(0);
      expect(await t.prisma.bdProposalDetail.count({ where: { id: { in: ids('proposalDetailId') } } })).toBe(
        0,
      );
      expect(
        await t.prisma.bdProposalOwnership.count({ where: { id: { in: ids('proposalOwnershipId') } } }),
      ).toBe(0);
      expect(
        await t.prisma.bdFurtherInformation.count({ where: { id: { in: ids('furtherInformationId') } } }),
      ).toBe(0);
      expect(
        await t.prisma.bdLink.count({ where: { furtherInformationId: { in: ids('furtherInformationId') } } }),
      ).toBe(0);
      // Other chains untouched.
      expectSingle(await t.api().get(`/api/bds/${other.master_id}`));
      expectNotFound(await t.api().get(`/api/bds/${bd.master_id}`));
    });
  });

  describe('GET /api/bds: the pdf-ui list query', () => {
    let carol: StakeSession;
    const made: Record<string, number> = {};

    beforeAll(async () => {
      carol = await loginStake(t, { username: 'carol' });
      const typed = (typeId: number) => ({
        bd_psapb: { ...(bdPayload().bd_psapb as object), type_name: typeId },
      });
      // Research (type 2) is used only here.
      for (const [who, name] of [
        [bob, 'Zeta Research'],
        [alice, 'Alpha research'],
        [carol, 'Mid RESEARCH'],
        [alice, 'Unrelated'],
      ] as const) {
        made[name] = (await createBd(t, who, name, typed(2))).id;
      }
      // An older version must not show up in the list.
      const chain = await createBd(t, bob, 'Research old name', typed(2));
      const v2 = await t
        .api()
        .post('/api/bds')
        .set(bob.auth)
        .send({ data: bdPayload('Research new name', { ...typed(2), master_id: chain.master_id }) })
        .expect(200);
      made['Research new name'] = v2.body.id;
      await t.prisma.bd.update({ where: { id: made['Zeta Research'] }, data: { commentsNumber: 5 } });
      await t.prisma.bd.update({ where: { id: made['Alpha research'] }, data: { commentsNumber: 1 } });
      await t.prisma.bd.update({ where: { id: made['Mid RESEARCH'] }, data: { commentsNumber: 3 } });
      await t.prisma.bd.update({ where: { id: v2.body.id }, data: { commentsNumber: 2 } });
    });

    const names = (data: Array<{ attributes: Record<string, any> }>) =>
      data.map((d) => d.attributes.bd_proposal_detail.data.attributes.proposal_name as string);

    it('search is case-insensitive, only active versions, with populate shapes', async () => {
      const res = await t.api().get(`/api/bds?${listQuery(2, 'research', 'sort[createdAt]=DESC')}`);
      const data = expectList(res, { page: 1, pageSize: 25, total: 4, length: 4 });
      expect(names(data)).toEqual(['Research new name', 'Mid RESEARCH', 'Alpha research', 'Zeta Research']);
      const a = data[0].attributes as Record<string, any>;
      expect(a.creator.data).toEqual({ id: bob.user.id, attributes: { govtool_username: 'bob' } });
      expect(a.bd_psapb.data.attributes.type_name.data.attributes.type_name).toBe('Research');
      expect(a.bd_costing.data.attributes.ada_amount).toBe('1000');
      expect(a.bd_proposal_detail.data.attributes).not.toHaveProperty('contract_type_name');
      expect(a.submitted_for_vote).toBeNull();
      expect(a).toHaveProperty('master_proposal_created_at');
      // The old version's createdAt carries over as the master's.
      const chainMaster = await t.prisma.bd.findUniqueOrThrow({ where: { id: Number(a.master_id) } });
      expect(a.master_proposal_created_at).toBe(chainMaster.createdAt.toISOString());
      expect(a).not.toHaveProperty('bd_further_information');
      expectNoKeyDeep(res.body, 'email');
    });

    it('an empty search matches every row of the type', async () => {
      expectList(await t.api().get(`/api/bds?${listQuery(2, '', 'sort[createdAt]=DESC')}`), { total: 5 });
      expectList(await t.api().get(`/api/bds?${listQuery(4, '', 'sort[createdAt]=DESC')}`), { total: 0 });
    });

    it('My Proposals: filters[$and][3][creator]=<userId>', async () => {
      const data = expectList(
        await t.api().get(`/api/bds?${listQuery(2, '', 'sort[createdAt]=ASC', alice.user.id)}`),
      );
      expect(names(data)).toEqual(['Alpha research', 'Unrelated']);
    });

    it.each([
      ['sort[createdAt]=DESC', ['Research new name', 'Mid RESEARCH', 'Alpha research', 'Zeta Research']],
      ['sort[createdAt]=ASC', ['Zeta Research', 'Alpha research', 'Mid RESEARCH', 'Research new name']],
      [
        'sort[prop_comments_number]=DESC',
        ['Zeta Research', 'Mid RESEARCH', 'Research new name', 'Alpha research'],
      ],
      [
        'sort[prop_comments_number]=ASC',
        ['Alpha research', 'Research new name', 'Mid RESEARCH', 'Zeta Research'],
      ],
      [
        'sort[bd_proposal_detail][proposal_name]=ASC',
        ['Alpha research', 'Mid RESEARCH', 'Research new name', 'Zeta Research'],
      ],
      [
        'sort[bd_proposal_detail][proposal_name]=DESC',
        ['Zeta Research', 'Research new name', 'Mid RESEARCH', 'Alpha research'],
      ],
    ])('%s orders the cards (11B_3)', async (sort, expected) => {
      const data = expectList(await t.api().get(`/api/bds?${listQuery(2, 'research', sort)}`));
      // Names differ in their first letter, so the order holds under any collation.
      expect(names(data)).toEqual(expected);
    });

    it.each(['ASC', 'DESC'])('sort[creator][govtool_username]=%s', async (dir) => {
      const data = expectList(
        await t.api().get(`/api/bds?${listQuery(2, 'research', `sort[creator][govtool_username]=${dir}`)}`),
      );
      const users = data.map((d) => (d.attributes as any).creator.data.attributes.govtool_username as string);
      const sorted = [...users].sort();
      expect(users).toEqual(dir === 'ASC' ? sorted : sorted.reverse());
      expect(new Set(users)).toEqual(new Set(['alice', 'bob', 'carol']));
    });

    it('pagination meta drives the infinite scroll', async () => {
      const res = await t
        .api()
        .get(
          `/api/bds?filters[$and][0][is_active]=true&filters[$and][1][bd_psapb][type_name][id]=2&pagination[page]=2&pagination[pageSize]=2`,
        );
      expectList(res, { page: 2, pageSize: 2, total: 5, length: 2 });
      expect(res.body.meta.pagination.pageCount).toBe(3);
    });

    it('without is_active the list shows every version (the server does not add it)', async () => {
      expectList(await t.api().get('/api/bds?filters[bd_psapb][type_name][id]=2'), { total: 6 });
    });
  });
});
