// SPEC §8.2 proposals, §8.3 proposal contents, §8.4 proposal votes.

import { StakeAddress } from 'libcardano';
import { createTestApp, TestApp } from './helpers/app';
import { loginStake, StakeSession } from './helpers/auth';
import { CORPUS, PROPOSAL_SORTS } from './helpers/pdf-ui-corpus';
import {
  expectBadRequestDetails,
  expectError,
  expectForbidden,
  expectList,
  expectNotFound,
  expectSingle,
  expectUnauthorized,
  expectValidation,
} from './helpers/envelope';
import { createPoll, createProposal, proposalBody, STAKE_TEST_ADDRESS } from './helpers/proposals';

const ISO_MS = /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/;
const TX = 'ab'.repeat(32);
const POPULATE =
  'populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal';

type Item = { id: number; attributes: Record<string, any> };

describe('proposals (e2e)', () => {
  let t: TestApp;
  let a: StakeSession;
  let b: StakeSession;

  beforeAll(async () => {
    t = await createTestApp();
    a = await loginStake(t, { username: 'prop_alice' });
    b = await loginStake(t, { username: 'prop_bob' });
  });
  afterAll(async () => {
    await t.close();
  });

  const get = (path: string, s?: StakeSession) => (s ? t.api().get(path).set(s.auth) : t.api().get(path));

  describe('anonymous and bad tokens', () => {
    let proposalId: number;
    beforeAll(async () => {
      ({ proposalId } = await createProposal(t, a));
    });

    it('public reads answer 200 with no token and with a garbage Bearer (Δ5)', async () => {
      for (const auth of [undefined, 'Bearer garbage']) {
        const list = t.api().get('/api/proposals');
        const one = t.api().get(`/api/proposals/${proposalId}`);
        if (auth) {
          list.set('Authorization', auth);
          one.set('Authorization', auth);
        }
        expectList(await list);
        expectSingle(await one);
      }
    });

    it('draft listing needs a user: 400 BD `User is required`', async () => {
      expectBadRequestDetails(
        await t.api().get('/api/proposals?filters[$and][0][is_draft]=true'),
        'User is required',
      );
      expectBadRequestDetails(
        await t.api().get('/api/proposals?filters[is_draft]=false').set('Authorization', 'Bearer garbage'),
        'User is required',
      );
    });

    it.each([
      ['post', '/api/proposals'],
      ['delete', '/api/proposals/1'],
      ['post', '/api/proposal-contents'],
      ['put', '/api/proposal-contents/1'],
      ['get', '/api/proposal-votes?filters[proposal_id][$eq]=1'],
      ['post', '/api/proposal-votes'],
      ['put', '/api/proposal-votes/1'],
    ] as const)('%s %s: no header 403, garbage Bearer 401', async (method, path) => {
      expectForbidden(await t.api()[method](path).send({ data: {} }));
      expectUnauthorized(
        await t.api()[method](path).set('Authorization', 'Bearer garbage').send({ data: {} }),
      );
    });
  });

  describe('POST /api/proposals', () => {
    it('returns data.attributes {proposal_id, proposal_content_id} as numbers and no data.id', async () => {
      const res = await t.api().post('/api/proposals').set(a.auth).send({ data: proposalBody() });
      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        data: { attributes: { proposal_id: expect.any(Number), proposal_content_id: expect.any(Number) } },
        meta: {},
      });
    });

    it('forces owner, counters and submission state whatever the client sends (Δ3)', async () => {
      const { proposalId } = await createProposal(t, a, {
        user_id: String(b.user.id),
        prop_likes: 99,
        prop_dislikes: 98,
        prop_comments_number: 97,
        prop_submitted: true,
        prop_submission_tx_hash: TX,
        prop_submission_date: '2026-01-01',
        is_locked: true,
        prop_rev_active: false,
        proposal_id: 9999,
        id: 12345,
      });
      const item = expectSingle(await get(`/api/proposals/${proposalId}`)) as Item;
      expect(item.id).toBe(proposalId);
      expect(item.attributes).toMatchObject({
        user_id: String(a.user.id),
        prop_likes: 0,
        prop_dislikes: 0,
        prop_comments_number: 0,
      });
      expect(item.attributes.content.attributes).toMatchObject({
        user_id: String(a.user.id),
        proposal_id: String(proposalId),
        prop_submitted: false,
        prop_submission_tx_hash: null,
        prop_submission_date: null,
        is_locked: false,
        prop_rev_active: true,
      });
    });

    it('validates body, type and length', async () => {
      const post = (data: unknown) =>
        t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send(data as object);
      expectValidation(await post({}), 'Missing "data" payload in the request body');
      expectValidation(
        await post({ data: proposalBody({ gov_action_type_id: 5 }) }),
        'gov_action_type_id is invalid',
      );
      expectValidation(
        await post({ data: proposalBody({ gov_action_type_id: 'x' }) }),
        'gov_action_type_id is invalid',
      );
      expectValidation(
        await post({ data: proposalBody({ prop_name: 'x'.repeat(81) }) }),
        'prop_name is too long',
      );
      expectValidation(await post({ data: proposalBody({ prop_name: '' }) }), 'prop_name is required');
      expectValidation(
        await post({ data: proposalBody({ prop_abstract: 'x'.repeat(2501) }) }),
        'prop_abstract is too long',
      );
      expectValidation(await post({ data: proposalBody({ is_draft: 'yes' }) }), 'is_draft is invalid');
      expectValidation(
        await post({ data: proposalBody({ proposal_links: Array(26).fill({ prop_link: 'https://x.io' }) }) }),
        'proposal_links is too long',
      );
    });

    it('treasury: withdrawals required and valid unless a draft', async () => {
      const post = (o: Record<string, unknown>) =>
        t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send({ data: proposalBody({ gov_action_type_id: 2, ...o }) });
      expectBadRequestDetails(await post({ proposal_withdrawals: [] }), 'Withdrawal parametars not exist');
      expectBadRequestDetails(
        await post({ proposal_withdrawals: undefined }),
        'Withdrawal parametars not exist',
      );
      const bad = 'Withdrawal addrress or amount parametars not valid';
      expectBadRequestDetails(
        await post({
          proposal_withdrawals: [{ prop_receiving_address: 'addr_test1xyz', prop_amount: '10' }],
        }),
        bad,
      );
      expectBadRequestDetails(
        await post({
          proposal_withdrawals: [{ prop_receiving_address: STAKE_TEST_ADDRESS, prop_amount: '0' }],
        }),
        bad,
      );
      const ok = await post({
        proposal_withdrawals: [{ prop_receiving_address: STAKE_TEST_ADDRESS, prop_amount: '12.5' }],
      });
      expect(ok.status).toBe(200);
      const item = expectSingle(await get(`/api/proposals/${ok.body.data.attributes.proposal_id}`)) as Item;
      expect(item.attributes.content.attributes.proposal_withdrawals).toEqual([
        { id: expect.any(Number), prop_receiving_address: STAKE_TEST_ADDRESS, prop_amount: 12.5 },
      ]);
      // Drafts skip the checks; an amount that does not parse is stored as null.
      const draft = await post({
        is_draft: true,
        proposal_withdrawals: [{ prop_receiving_address: 'nope', prop_amount: 'abc' }],
      });
      expect(draft.status).toBe(200);
      const w = await t.prisma.proposalWithdrawal.findMany({
        where: { contentId: draft.body.data.attributes.proposal_content_id },
      });
      expect(w.map((x) => [x.receivingAddress, x.amount])).toEqual([['nope', null]]);
    });

    it('treasury: CARDANO_NETWORK_ID rejects an address on the other network', async () => {
      const t1 = await createTestApp({ CARDANO_NETWORK_ID: '0' });
      const test = StakeAddress.fromBech32(STAKE_TEST_ADDRESS);
      const mainnet = new StakeAddress(1, test.credential).toBech32();
      try {
        const s = await loginStake(t1);
        const res = await t1
          .api()
          .post('/api/proposals')
          .set(s.auth)
          .send({
            data: proposalBody({
              gov_action_type_id: 2,
              proposal_withdrawals: [{ prop_receiving_address: mainnet, prop_amount: 1 }],
            }),
          });
        expectBadRequestDetails(res, 'Withdrawal addrress or amount parametars not valid');
      } finally {
        await t1.close();
      }
    });

    it('constitution: object, URL scheme (Δ24) and guardrails rules', async () => {
      const post = (c: unknown) =>
        t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send({ data: proposalBody({ gov_action_type_id: 3, proposal_constitution_content: c }) });
      expectBadRequestDetails(
        await post(undefined),
        'proposal_constitution_content is required for Constitution action',
      );
      const urlMsg = 'prop_constitution_url is required and must be a valid URL (IPFS is allowed)';
      expectBadRequestDetails(await post({}), urlMsg);
      expectBadRequestDetails(await post({ prop_constitution_url: 'javascript:alert(1)' }), urlMsg);
      expectBadRequestDetails(
        await post({
          prop_constitution_url: 'https://c.io/x',
          prop_have_guardrails_script: true,
          prop_guardrails_script_hash: 'h',
        }),
        'prop_guardrails_script_url is required and must be a valid URL when prop_have_guardrails_script is true',
      );
      expectBadRequestDetails(
        await post({
          prop_constitution_url: 'https://c.io/x',
          prop_have_guardrails_script: true,
          prop_guardrails_script_url: 'ipfs://abc',
        }),
        'prop_guardrails_script_hash is required when prop_have_guardrails_script is true',
      );
      expectBadRequestDetails(
        await post({
          prop_constitution_url: 'https://c.io/x',
          prop_have_guardrails_script: false,
          prop_guardrails_script_url: 'https://g.io',
        }),
        'prop_guardrails_script_url and prop_guardrails_script_hash must not be provided when prop_have_guardrails_script is false or null',
      );
      // pdf-ui's draft-restore echo: {data: {id, attributes}} is unwrapped.
      const ok = await post({
        data: {
          id: 77,
          attributes: {
            prop_constitution_url: 'ipfs://bafyconstitution',
            prop_have_guardrails_script: false,
            prop_guardrails_script_url: '',
            prop_guardrails_script_hash: '',
          },
        },
      });
      expect(ok.status).toBe(200);
      const item = expectSingle(await get(`/api/proposals/${ok.body.data.attributes.proposal_id}`)) as Item;
      const cc = item.attributes.content.attributes.proposal_constitution_content;
      expect(cc).toEqual({
        data: {
          id: expect.any(Number),
          attributes: {
            prop_constitution_url: 'ipfs://bafyconstitution',
            prop_have_guardrails_script: false,
            prop_guardrails_script_url: null,
            prop_guardrails_script_hash: null,
            createdAt: expect.stringMatching(ISO_MS),
            updatedAt: expect.stringMatching(ISO_MS),
          },
        },
      });
      expect(cc.data.id).not.toBe(77);
    });

    it('hard fork: row created when any field is non-empty, wrapped on the wire', async () => {
      const { proposalId } = await createProposal(t, a, {
        gov_action_type_id: 6,
        proposal_hard_fork_content: {
          previous_ga_hash: 'cd'.repeat(32),
          previous_ga_id: 0,
          major: 11,
          minor: '0',
        },
      });
      const item = expectSingle(await get(`/api/proposals/${proposalId}`)) as Item;
      expect(item.attributes.content.attributes.proposal_hard_fork_content.data.attributes).toMatchObject({
        previous_ga_hash: 'cd'.repeat(32),
        previous_ga_id: '0',
        major: '11',
        minor: '0',
      });
      const none = await createProposal(t, a, { gov_action_type_id: 6, proposal_hard_fork_content: {} });
      const item2 = expectSingle(await get(`/api/proposals/${none.proposalId}`)) as Item;
      expect(item2.attributes.content.attributes.proposal_hard_fork_content).toEqual({ data: null });
      // Other types never store a constitution or hard-fork row.
      const info = await createProposal(t, a, {
        proposal_hard_fork_content: { major: '1' },
        proposal_constitution_content: { prop_constitution_url: 'https://x.io' },
      });
      const c = await t.prisma.proposalContent.findUniqueOrThrow({
        where: { id: info.contentId },
        include: { constitutionContent: true },
      });
      expect([c.hardForkContentId, c.constitutionContent]).toEqual([null, null]);
    });

    it('is one transaction: a failing link rolls back the proposal (Δ25)', async () => {
      // A test-only trigger fails the link insert inside the create
      // transaction, after the proposal, hard-fork and content rows.
      await t.prisma.$executeRawUnsafe(`
        CREATE OR REPLACE FUNCTION e2e_fail_link() RETURNS trigger AS $$
        BEGIN RAISE EXCEPTION 'e2e link failure'; END; $$ LANGUAGE plpgsql`);
      await t.prisma.$executeRawUnsafe(`
        CREATE TRIGGER e2e_fail_link BEFORE INSERT ON proposal_links
        FOR EACH ROW WHEN (NEW.link = 'https://fail.test/') EXECUTE FUNCTION e2e_fail_link()`);
      try {
        const before = await t.prisma.proposal.count();
        const res = await t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send({
            data: proposalBody({
              gov_action_type_id: 6,
              proposal_hard_fork_content: { major: '10' },
              proposal_links: [{ prop_link: 'https://ok.io' }, { prop_link: 'https://fail.test/' }],
            }),
          });
        expectError(res, 500, 'InternalServerError', 'Internal Server Error');
        expect(await t.prisma.proposal.count()).toBe(before);
        expect(await t.prisma.proposalHardForkContent.count({ where: { major: '10' } })).toBe(0);
        expect(await t.prisma.proposalLink.count({ where: { link: 'https://ok.io' } })).toBe(0);
      } finally {
        await t.prisma.$executeRawUnsafe('DROP TRIGGER IF EXISTS e2e_fail_link ON proposal_links');
        await t.prisma.$executeRawUnsafe('DROP FUNCTION IF EXISTS e2e_fail_link()');
      }
    });

    it('links: http, https or ipfs only (Δ47); blank rows dropped', async () => {
      const post = (links: unknown[], extra: Record<string, unknown> = {}) =>
        t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send({ data: proposalBody({ proposal_links: links, ...extra }) });
      for (const link of [
        'javascript:alert(1)',
        'data:text/html,<b>x</b>',
        'ftp://x.io/a',
        'example.com',
        'vbscript:x',
      ]) {
        expectValidation(await post([{ prop_link: link, prop_link_text: 't' }]), 'prop_link is invalid');
        expectValidation(await post([{ prop_link: link }], { is_draft: true }), 'prop_link is invalid');
      }
      const ok = await post([
        { prop_link: 'https://a.io/x', prop_link_text: 'A' },
        { prop_link: '', prop_link_text: 'blank' },
        { prop_link: '   ' },
        { prop_link: 'ipfs://bafylink' },
        { prop_link: 'http://b.io' },
      ]);
      expect(ok.status).toBe(200);
      const links = await t.prisma.proposalLink.findMany({
        where: { contentId: ok.body.data.attributes.proposal_content_id },
        orderBy: { position: 'asc' },
      });
      expect(links.map((l) => [l.position, l.link])).toEqual([
        [0, 'https://a.io/x'],
        [1, 'ipfs://bafylink'],
        [2, 'http://b.io'],
      ]);
      // The same rule on a revision.
      expectValidation(
        await t
          .api()
          .post('/api/proposal-contents')
          .set(a.auth)
          .send({
            data: proposalBody({
              proposal_id: ok.body.data.attributes.proposal_id,
              proposal_links: [{ prop_link: 'javascript:alert(1)' }],
            }),
          }),
        'prop_link is invalid',
      );
    });

    it('a NUL byte in any string is V, not a 500', async () => {
      const post = (o: Record<string, unknown>) =>
        t
          .api()
          .post('/api/proposals')
          .set(a.auth)
          .send({ data: proposalBody(o) });
      expectValidation(await post({ prop_name: 'a\u0000b' }), 'prop_name is invalid');
      expectValidation(await post({ prop_abstract: '\u0000' }), 'prop_abstract is invalid');
      expectValidation(
        await post({ proposal_links: [{ prop_link: 'https://x.io', prop_link_text: 'nul\u0000' }] }),
        'prop_link_text is invalid',
      );
      expectValidation(
        await post({
          gov_action_type_id: 3,
          proposal_constitution_content: { prop_constitution_url: 'https://c.io/\u0000' },
        }),
        'prop_constitution_url is invalid',
      );
      expectValidation(
        await get('/api/proposals?filters[prop_name][$containsi]=a%00b'),
        'Invalid value for prop_name',
      );
      expectValidation(
        await get('/api/proposals?filters[prop_name][$in][0]=%00'),
        'Invalid value for prop_name',
      );
    });
  });

  describe('GET /api/proposals list', () => {
    let ids: number[];
    beforeAll(async () => {
      // Distinct names, likes, dislikes and comment counts for the sort checks.
      const specs = [
        // Mixed case on purpose: text sorts linguistically (und-x-icu), as
        // Playwright's localeCompare expects, not by byte.
        { name: 'Sort Bravo', likes: 3, dislikes: 1, comments: 2 },
        { name: 'sort alpha', likes: 1, dislikes: 3, comments: 0 },
        { name: 'SORT Charlie', likes: 2, dislikes: 2, comments: 5 },
      ];
      ids = [];
      for (const s of specs) {
        const { proposalId } = await createProposal(t, a, { gov_action_type_id: 4, prop_name: s.name });
        await t.prisma.proposal.update({
          where: { id: proposalId },
          data: { likes: s.likes, dislikes: s.dislikes, commentsNumber: s.comments },
        });
        ids.push(proposalId);
      }
      // One submitted, one draft (never listed without is_draft).
      const submitted = await t.prisma.proposalContent.findFirstOrThrow({ where: { proposalId: ids[2] } });
      await t.prisma.proposalContent.update({ where: { id: submitted.id }, data: { submitted: true } });
      await createProposal(t, a, { gov_action_type_id: 4, prop_name: 'Sort draft', is_draft: true });
    });

    const listQuery = (sort: string, extra = '', search = 'sort') =>
      `/api/proposals?filters[$and][0][gov_action_type_id]=4&filters[$and][1][prop_name][$containsi]=${search}${extra}&pagination[page]=1&pagination[pageSize]=25&${sort}&${POPULATE}`;

    it('item shape: proposal wrapping its content, relations wrapped, content/gov_action_type not', async () => {
      const data = expectList(await get(listQuery('sort[createdAt]=DESC')), { total: 3 }) as Item[];
      const item = data[0];
      expect(Object.keys(item.attributes).sort()).toEqual(
        [
          'content',
          'createdAt',
          'prop_comments_number',
          'prop_dislikes',
          'prop_likes',
          'updatedAt',
          'user_govtool_username',
          'user_id',
        ].sort(),
      );
      expect(item.attributes.user_govtool_username).toBe('prop_alice');
      const content = item.attributes.content;
      expect(Object.keys(content).sort()).toEqual(['attributes', 'id']);
      expect(Object.keys(content.attributes).sort()).toEqual(
        [
          'proposal_id',
          'prop_rev_active',
          'prop_abstract',
          'prop_motivation',
          'prop_rationale',
          'gov_action_type_id',
          'prop_name',
          'is_draft',
          'user_id',
          'prop_submitted',
          'prop_submission_tx_hash',
          'prop_submission_date',
          'is_locked',
          'createdAt',
          'updatedAt',
          'proposal_links',
          'proposal_withdrawals',
          'proposal_constitution_content',
          'proposal_hard_fork_content',
          'gov_action_type',
        ].sort(),
      );
      expect(content.attributes.gov_action_type_id).toBe('4');
      expect(content.attributes.gov_action_type).toEqual({
        id: 4,
        attributes: {
          gov_action_type_name: 'Motion of No Confidence',
          createdAt: expect.stringMatching(ISO_MS),
          updatedAt: expect.stringMatching(ISO_MS),
          publishedAt: expect.stringMatching(ISO_MS),
        },
      });
      expect(content.attributes.proposal_links).toEqual([
        { id: expect.any(Number), prop_link: 'https://example.com', prop_link_text: 'Example' },
      ]);
      expect(content.attributes.proposal_constitution_content).toEqual({ data: null });
      expect(content.attributes.proposal_hard_fork_content).toEqual({ data: null });
    });

    it.each([
      ['sort[createdAt]=DESC', (x: Item) => x.attributes.createdAt as string, 'desc'],
      ['sort[createdAt]=ASC', (x: Item) => x.attributes.createdAt as string, 'asc'],
      ['sort[proposal][prop_likes]=DESC', (x: Item) => x.attributes.prop_likes as number, 'desc'],
      ['sort[proposal][prop_likes]=ASC', (x: Item) => x.attributes.prop_likes as number, 'asc'],
      ['sort[proposal][prop_dislikes]=DESC', (x: Item) => x.attributes.prop_dislikes as number, 'desc'],
      ['sort[proposal][prop_dislikes]=ASC', (x: Item) => x.attributes.prop_dislikes as number, 'asc'],
      [
        'sort[proposal][prop_comments_number]=DESC',
        (x: Item) => x.attributes.prop_comments_number as number,
        'desc',
      ],
      [
        'sort[proposal][prop_comments_number]=ASC',
        (x: Item) => x.attributes.prop_comments_number as number,
        'asc',
      ],
      ['sort[prop_name]=ASC', (x: Item) => x.attributes.content.attributes.prop_name as string, 'asc'],
      ['sort[prop_name]=DESC', (x: Item) => x.attributes.content.attributes.prop_name as string, 'desc'],
    ] as const)('Appendix A %s orders the list (8B_2)', async (sort, key, dir) => {
      expect(PROPOSAL_SORTS).toContain(sort);
      const data = expectList(await get(listQuery(sort)), { total: 3, length: 3 }) as Item[];
      const keys: Array<string | number> = data.map((x) => (key as (i: Item) => string | number)(x));
      const sorted = [...keys].sort((x, y) =>
        typeof x === 'string' && typeof y === 'string'
          ? x.replace(/ /g, '').localeCompare(y.replace(/ /g, ''))
          : x < y
            ? -1
            : x > y
              ? 1
              : 0,
      );
      expect(keys).toEqual(dir === 'asc' ? sorted : sorted.reverse());
      expect(new Set(keys).size).toBe(3);
    });

    it('$containsi is case-insensitive; prop_submitted filters', async () => {
      expectList(await get(listQuery('sort[createdAt]=DESC', '', 'CHARLIE')), { total: 1 });
      const sub = expectList(
        await get(listQuery('sort[createdAt]=DESC', '&filters[$and][2][prop_submitted]=true')),
        { total: 1 },
      ) as Item[];
      expect(sub[0].id).toBe(ids[2]);
      expectList(await get(listQuery('sort[createdAt]=DESC', '&filters[$and][2][prop_submitted]=false')), {
        total: 2,
      });
    });

    it('pagination: pageCount/total, pageSize 1000, 5000 clamped', async () => {
      const q = '/api/proposals?filters[gov_action_type_id]=4';
      expectList(await get(`${q}&pagination[page]=2&pagination[pageSize]=2`), {
        page: 2,
        pageSize: 2,
        total: 3,
        length: 1,
      });
      expectList(await get(`${q}&pagination[pageSize]=1000`), { pageSize: 1000, total: 3 });
      expectList(await get(`${q}&pagination[pageSize]=5000`), { pageSize: 1000, total: 3 });
    });

    it('fields gives V; unknown keys give V', async () => {
      expectValidation(await get('/api/proposals?fields[0]=prop_name'), 'Invalid query parameter: fields');
      expectValidation(await get('/api/proposals?filters[user][username]=x'), 'Invalid key user');
      expectValidation(await get('/api/proposals?filters[$or][0][prop_id]=1'), 'Invalid key prop_id');
    });
  });

  describe('drafts are caller-scoped (Δ22)', () => {
    let aDraft: number;
    let bDraft: number;
    beforeAll(async () => {
      ({ proposalId: aDraft } = await createProposal(t, a, {
        prop_name: 'A draft',
        is_draft: true,
        gov_action_type_id: 1,
      }));
      ({ proposalId: bDraft } = await createProposal(t, b, {
        prop_name: '',
        is_draft: true,
        gov_action_type_id: 1,
      }));
    });

    const drafts = (extra = '') =>
      `/api/proposals?filters[$and][2][is_draft]=true${extra}&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content`;

    it("B's drafts query with user_id=A returns only B's drafts", async () => {
      const data = expectList(await get(drafts(`&filters[$and][3][user_id]=${a.user.id}`), b)) as Item[];
      expect(data.map((d) => d.id)).toEqual([bDraft]);
      expect(data[0].attributes.content.attributes.prop_name).toBe('');
      const mine = expectList(await get(drafts(), a)) as Item[];
      expect(mine.map((d) => d.id)).toContain(aDraft);
      expect(mine.map((d) => d.id)).not.toContain(bDraft);
      for (const d of mine) expect(d.attributes.content.attributes.is_draft).toBe(true);
    });

    it('Appendix A draft probe and the unreachable prop_submitted variant', async () => {
      expectList(
        await get(
          '/api/proposals?filters[$and][0][is_draft]=true&pagination[page]=1&pagination[pageSize]=1',
          b,
        ),
        {
          total: 1,
          pageSize: 1,
        },
      );
      expectList(await get(drafts('&filters[$and][3][prop_submitted]=false'), b), { total: 1 });
    });

    it('drafts never appear in the public list; draft-only single gives the draft BD', async () => {
      const all = expectList(await get('/api/proposals?pagination[pageSize]=1000', a)) as Item[];
      expect(all.map((d) => d.id)).not.toContain(aDraft);
      expectBadRequestDetails(
        await get(`/api/proposals/${aDraft}`, a),
        'You can not access draft proposal details.',
      );
    });
  });

  describe('GET /api/proposals/:id', () => {
    it('by id; unknown, malformed and tx-hash misses all give BD `Proposal not found` (Δ23)', async () => {
      const { proposalId } = await createProposal(t, a);
      expect((expectSingle(await get(`/api/proposals/${proposalId}`)) as Item).id).toBe(proposalId);
      for (const id of ['999999', 'abc', '-1', 'ef'.repeat(32), '1e3']) {
        expectBadRequestDetails(await get(`/api/proposals/${id}`), 'Proposal not found');
      }
    });

    it('by tx hash once submitted; content null when no active content exists', async () => {
      const { proposalId, contentId } = await createProposal(t, a);
      await t
        .api()
        .put(`/api/proposal-contents/${contentId}`)
        .set(a.auth)
        .send({
          data: {
            prop_submitted: true,
            prop_submission_date: '2026-09-26T10:11:12.000Z',
            prop_submission_tx_hash: TX.toUpperCase(),
          },
        })
        .expect(200);
      const item = expectSingle(await get(`/api/proposals/${TX}`)) as Item;
      expect(item.id).toBe(proposalId);
      expect(item.attributes.content.attributes).toMatchObject({
        prop_submitted: true,
        prop_submission_tx_hash: TX,
        prop_submission_date: '2026-09-26',
      });
      await t.prisma.proposalContent.update({ where: { id: contentId }, data: { revActive: false } });
      expect((expectSingle(await get(`/api/proposals/${proposalId}`)) as Item).attributes.content).toBeNull();
    });
  });

  describe('proposal contents (§8.3)', () => {
    it('owner only: B gets F; missing id and unknown proposal give BD', async () => {
      const { proposalId } = await createProposal(t, a);
      const post = (s: StakeSession, data: Record<string, unknown>) =>
        t.api().post('/api/proposal-contents').set(s.auth).send({ data });
      expectForbidden(
        await post(b, proposalBody({ proposal_id: proposalId })),
        "You can't access this entry",
      );
      expectBadRequestDetails(await post(a, proposalBody()), 'Proposal ID is required');
      expectBadRequestDetails(await post(a, proposalBody({ proposal_id: 999999 })), 'Proposal not found');
    });

    it('non-draft revision deactivates the others, draft revision does not; version history', async () => {
      const { proposalId, contentId: first } = await createProposal(t, a, { prop_name: 'v1' });
      const post = (o: Record<string, unknown>) =>
        t
          .api()
          .post('/api/proposal-contents')
          .set(a.auth)
          .send({
            data: {
              ...proposalBody(o),
              proposal_id: proposalId,
              prop_rev_active: false,
              user_id: b.user.id,
              publish: true,
            },
          });

      const draft = await post({ prop_name: 'v2 draft', is_draft: true });
      const d = expectSingle(draft)!;
      // Scalars only: no components, no relations.
      expect(d.attributes).not.toHaveProperty('proposal_links');
      expect(d.attributes).toMatchObject({
        is_draft: true,
        prop_rev_active: true,
        user_id: String(a.user.id),
      });
      const live = await t.prisma.proposalContent.findUniqueOrThrow({ where: { id: first } });
      expect(live.revActive).toBe(true);
      // The single route prefers the live non-draft content.
      expect((expectSingle(await get(`/api/proposals/${proposalId}`)) as Item).attributes.content.id).toBe(
        first,
      );

      const v3 = expectSingle(await post({ prop_name: 'v3', proposal_hard_fork_content: {} }))!;
      const rows = await t.prisma.proposalContent.findMany({ where: { proposalId }, orderBy: { id: 'asc' } });
      expect(rows.map((r) => [r.name, r.revActive])).toEqual([
        ['v1', false],
        ['v2 draft', false],
        ['v3', true],
      ]);

      const history = expectList(
        await get(
          `/api/proposals?filters[$and][0][prop_id]=${proposalId}&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals`,
        ),
        { total: 2 },
      ) as Item[];
      expect(history.map((h) => h.id)).toEqual([proposalId, proposalId]);
      expect(
        history.map((h) => [h.attributes.content.id, h.attributes.content.attributes.prop_rev_active]),
      ).toEqual([
        [v3.id, true],
        [first, false],
      ]);
    });

    it('PUT: submission bookkeeping only, owner only, once (Δ27, Δ28)', async () => {
      const { proposalId, contentId } = await createProposal(t, a);
      const put = (s: StakeSession, id: number | string, data: Record<string, unknown>) =>
        t.api().put(`/api/proposal-contents/${id}`).set(s.auth).send({ data });
      const good = {
        prop_submitted: true,
        prop_submission_date: new Date().toISOString(),
        prop_submission_tx_hash: 'cd'.repeat(32),
      };
      expectForbidden(await put(b, contentId, good), "You can't access this entry");
      expectNotFound(await put(a, 999999, good));
      expectNotFound(await put(a, 'abc', good));
      expectValidation(
        await put(a, contentId, { ...good, prop_submitted: false }),
        'prop_submitted is invalid',
      );
      expectValidation(
        await put(a, contentId, { ...good, prop_submission_tx_hash: 'xyz' }),
        'prop_submission_tx_hash is invalid',
      );
      expectValidation(
        await put(a, contentId, { ...good, prop_submission_date: 'soon' }),
        'prop_submission_date is invalid',
      );

      const res = await put(a, contentId, {
        ...good,
        prop_name: 'hacked',
        user_id: b.user.id,
        is_draft: true,
        prop_rev_active: false,
      });
      const c = expectSingle(res)!;
      expect(c.attributes).toMatchObject({
        prop_submitted: true,
        prop_name: 'Test proposal',
        user_id: String(a.user.id),
        is_draft: false,
        prop_rev_active: true,
      });
      expect((await t.prisma.proposal.findUniqueOrThrow({ where: { id: proposalId } })).userId).toBe(
        a.user.id,
      );

      const already = "Proposal can't be updated, it has been already submited";
      expectBadRequestDetails(await put(a, contentId, good), already);
      expectBadRequestDetails(
        await t
          .api()
          .post('/api/proposal-contents')
          .set(a.auth)
          .send({ data: proposalBody({ proposal_id: proposalId }) }),
        already,
      );
      // The tx hash is unique across contents.
      const other = await createProposal(t, a);
      expectValidation(await put(a, other.contentId, good), 'This attribute must be unique');
    });
  });

  describe('DELETE /api/proposals/:id', () => {
    it('owner only; removes everything under it, hard-fork rows included (Δ8)', async () => {
      const { proposalId, contentId } = await createProposal(t, a, {
        gov_action_type_id: 6,
        proposal_hard_fork_content: { previous_ga_id: '1', major: '12' },
      });
      const hf = (await t.prisma.proposalContent.findUniqueOrThrow({ where: { id: contentId } }))
        .hardForkContentId!;
      await t
        .api()
        .post('/api/proposal-votes')
        .set(b.auth)
        .send({ data: { proposal_id: String(proposalId), vote_result: true } })
        .expect(200);
      const pollId = await createPoll(t, a, proposalId);
      await t
        .api()
        .post('/api/poll-votes')
        .set(b.auth)
        .send({ data: { poll_id: String(pollId), vote_result: true } })
        .expect(200);
      const c = await t
        .api()
        .post('/api/comments')
        .set(b.auth)
        .send({ data: { proposal_id: String(proposalId), comment_text: 'x' } })
        .expect(200);
      await t
        .api()
        .post('/api/comments-reports')
        .set(a.auth)
        .send({ data: { comment: c.body.data.id } })
        .expect(200);
      await t
        .api()
        .put(`/api/proposal-contents/${contentId}`)
        .set(a.auth)
        .send({ data: { prop_submitted: true, prop_submission_tx_hash: 'ee'.repeat(32) } })
        .expect(200);

      expectForbidden(
        await t.api().delete(`/api/proposals/${proposalId}`).set(b.auth),
        "You can't access this entry",
      );
      expectNotFound(await t.api().delete('/api/proposals/999999').set(a.auth));
      expectNotFound(await t.api().delete('/api/proposals/abc').set(a.auth));

      const res = await t.api().delete(`/api/proposals/${proposalId}`).set(a.auth);
      const d = expectSingle(res)!;
      expect(d).toEqual({
        id: proposalId,
        attributes: {
          user_id: String(a.user.id),
          prop_likes: 1,
          prop_dislikes: 0,
          prop_comments_number: 1,
          createdAt: expect.stringMatching(ISO_MS),
          updatedAt: expect.stringMatching(ISO_MS),
        },
      });
      const counts = await Promise.all([
        t.prisma.proposal.count({ where: { id: proposalId } }),
        t.prisma.proposalContent.count({ where: { proposalId } }),
        t.prisma.proposalHardForkContent.count({ where: { id: hf } }),
        t.prisma.proposalLink.count({ where: { contentId } }),
        t.prisma.proposalVote.count({ where: { proposalId } }),
        t.prisma.poll.count({ where: { proposalId } }),
        t.prisma.pollVote.count({ where: { pollId } }),
        t.prisma.comment.count({ where: { proposalId } }),
        t.prisma.commentsReport.count({ where: { commentId: c.body.data.id } }),
      ]);
      expect(counts).toEqual([0, 0, 0, 0, 0, 0, 0, 0, 0]);
    });
  });

  describe('proposal votes (§8.4)', () => {
    let proposalId: number;
    beforeAll(async () => {
      ({ proposalId } = await createProposal(t, a));
    });
    const counters = async () => {
      const p = await t.prisma.proposal.findUniqueOrThrow({ where: { id: proposalId } });
      return [p.likes, p.dislikes];
    };

    it('GET is a single object from a list route, or null; always the caller (Δ29)', async () => {
      const q = `/api/proposal-votes?filters[proposal_id][$eq]=${proposalId}`;
      expect((await get(q, b)).body).toEqual({ data: null, meta: {} });
      const created = await t
        .api()
        .post('/api/proposal-votes')
        .set(a.auth)
        .send({ data: { proposal_id: String(proposalId), vote_result: false } });
      const vote = expectSingle(created)!;
      expect(vote.attributes).toEqual({
        proposal_id: String(proposalId),
        user_id: String(a.user.id),
        vote_result: false,
        createdAt: expect.stringMatching(ISO_MS),
        updatedAt: expect.stringMatching(ISO_MS),
      });
      expect(expectSingle(await get(q, a))).toEqual(vote);
      // B asking for A's vote gets B's own (none).
      expect((await get(`${q}&filters[user_id]=${a.user.id}`, b)).body).toEqual({ data: null, meta: {} });
      expect((await get(`/api/proposal-votes?filters[user_id][$eq]=${a.user.id}`, b)).body.data).toBeNull();
    });

    it('like, dislike, flip; errors; counters never negative', async () => {
      const post = (data: Record<string, unknown>) =>
        t.api().post('/api/proposal-votes').set(b.auth).send({ data });
      expectBadRequestDetails(await post({ proposal_id: proposalId }), 'Vote result is required');
      expectBadRequestDetails(
        await post({ proposal_id: proposalId, vote_result: 'true' }),
        'Vote result is required',
      );
      expectBadRequestDetails(await post({ vote_result: true }), 'Proposal ID is required');
      expectBadRequestDetails(await post({ proposal_id: 999999, vote_result: true }), 'Proposal not found');

      const [likes0, dislikes0] = await counters();
      const v = expectSingle(
        await post({ proposal_id: String(proposalId), vote_result: true, user_id: a.user.id }),
      )!;
      expect(v.attributes.user_id).toBe(String(b.user.id));
      expect(await counters()).toEqual([likes0 + 1, dislikes0]);
      expectBadRequestDetails(
        await post({ proposal_id: proposalId, vote_result: false }),
        'Proposal vote for this user already exist',
      );

      const put = (s: StakeSession, data: Record<string, unknown>) =>
        t.api().put(`/api/proposal-votes/${v.id}`).set(s.auth).send({ data });
      expectForbidden(await put(a, { vote_result: false }), "You can't access this entry");
      expectNotFound(
        await t
          .api()
          .put('/api/proposal-votes/999999')
          .set(b.auth)
          .send({ data: { vote_result: false } }),
      );
      expectBadRequestDetails(await put(b, { vote_result: null }), 'Vote result is required');
      expectBadRequestDetails(await put(b, { vote_result: true }), 'Proposal vote already updated');

      const flipped = expectSingle(await put(b, { vote_result: false }))!;
      expect(flipped.attributes.vote_result).toBe(false);
      expect(await counters()).toEqual([likes0, dislikes0 + 1]);

      // −1 never goes below zero.
      await t.prisma.proposal.update({ where: { id: proposalId }, data: { likes: 0, dislikes: 0 } });
      await put(b, { vote_result: true }).expect(200);
      expect(await counters()).toEqual([1, 0]);
    });

    it('10 concurrent likes by 10 users yield 10', async () => {
      const { proposalId: pid } = await createProposal(t, a);
      const users = await Promise.all(Array.from({ length: 10 }, () => loginStake(t)));
      const res = await Promise.all(
        users.map((u) =>
          t
            .api()
            .post('/api/proposal-votes')
            .set(u.auth)
            .send({ data: { proposal_id: pid, vote_result: true } }),
        ),
      );
      expect(res.map((r) => r.status)).toEqual(Array(10).fill(200));
      const p = await t.prisma.proposal.findUniqueOrThrow({ where: { id: pid } });
      expect([p.likes, p.dislikes]).toEqual([10, 0]);
      // Same user, concurrently: one vote, one like.
      const u = users[0];
      const { proposalId: pid2 } = await createProposal(t, a);
      const dup = await Promise.all(
        [true, true, false].map((vr) =>
          t
            .api()
            .post('/api/proposal-votes')
            .set(u.auth)
            .send({ data: { proposal_id: pid2, vote_result: vr } }),
        ),
      );
      expect(dup.filter((r) => r.status === 200)).toHaveLength(1);
      const p2 = await t.prisma.proposal.findUniqueOrThrow({ where: { id: pid2 } });
      expect(p2.likes + p2.dislikes).toBe(1);
    });
  });

  it('every Appendix A proposal/vote/poll string parses and answers 200', async () => {
    const routes = new Set(['proposals', 'proposal-votes', 'polls', 'poll-votes']);
    for (const e of CORPUS.filter((c) => routes.has(c.route))) {
      const res = await get(`/api/${e.route}?${e.query}`, a);
      expect({ q: e.query, status: res.status }).toEqual({ q: e.query, status: 200 });
    }
  });
});
