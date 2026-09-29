// SPEC §8.1, §8.9 (the worked example of a list route), plus the shared
// wire behaviour every route inherits: query parsing, errors, CORS, body
// handling, unknown routes, /health.

import { GOVERNANCE_ACTION_TYPES } from '../src/seed/lookups.data';
import { createTestApp, TestApp } from './helpers/app';
import { loginStake } from './helpers/auth';
import { CORPUS } from './helpers/pdf-ui-corpus';
import {
  expectError,
  expectList,
  expectNotFound,
  expectTimestamps,
  expectValidation,
} from './helpers/envelope';

const LOOKUPS: Array<[string, number, string[]]> = [
  ['bd-types', 5, ['type_name']],
  ['bd-road-maps', 11, ['roadmap_name']],
  ['bd-intersect-committees', 9, ['committee_name']],
  ['bd-contract-types', 6, ['contract_type_name']],
  ['bd-currency-lists', 5, ['currency_name', 'currency_letter_code', 'currency_number_code']],
  ['country-lists', 10, ['country_name', 'alfa_2_code', 'alfa_3_code']],
];

describe('lookups (e2e)', () => {
  let t: TestApp;
  beforeAll(async () => {
    t = await createTestApp();
  });
  afterAll(async () => {
    await t.close();
  });

  describe('GET /api/governance-action-types', () => {
    it('lists the seeded rows in the v4 envelope, id asc', async () => {
      const res = await t.api().get('/api/governance-action-types');
      const data = expectList(res, { page: 1, pageSize: 25, total: 5, length: 5 });
      expect(data.map((d) => [d.id, d.attributes.gov_action_type_name])).toEqual(GOVERNANCE_ACTION_TYPES);
      for (const d of data) {
        expect(Object.keys(d.attributes).sort()).toEqual([
          'createdAt',
          'gov_action_type_name',
          'publishedAt',
          'updatedAt',
        ]);
        expectTimestamps(d.attributes, true);
      }
    });

    it('is public even with a garbage or expired Bearer (Δ5)', async () => {
      await t.api().get('/api/governance-action-types').set('Authorization', 'Bearer garbage').expect(200);
      const s = await loginStake(t);
      await t.api().get('/api/governance-action-types').set(s.auth).expect(200);
    });
  });

  describe.each(LOOKUPS)('GET /api/%s', (route, count, fields) => {
    it('lists every seeded row with pageSize=1000', async () => {
      const data = expectList(await t.api().get(`/api/${route}?pagination[pageSize]=1000`), {
        pageSize: 1000,
        total: count,
        length: count,
      });
      expect(data.map((d) => d.id)).toEqual(Array.from({ length: count }, (_, i) => i + 1));
      for (const d of data) {
        expect(Object.keys(d.attributes).sort()).toEqual(
          [...fields, 'createdAt', 'publishedAt', 'updatedAt'].sort(),
        );
      }
    });
  });

  it('currency number codes are strings with leading zeros', async () => {
    const data = expectList(await t.api().get('/api/bd-currency-lists?filters[currency_letter_code]=AUD'));
    expect(data[0].attributes.currency_number_code).toBe('036');
  });

  it('pdf-ui magic names are exact', async () => {
    const types = expectList(await t.api().get('/api/bd-types'));
    expect(types[4].attributes.type_name).toBe('None of these');
    const road = expectList(await t.api().get('/api/bd-road-maps?filters[id]=10'));
    expect(road[0].attributes.roadmap_name).toBe('It supports the product roadmap');
    const ct = expectList(await t.api().get('/api/bd-contract-types?filters[id]=4'));
    expect(ct[0].attributes.contract_type_name).toBe('Other');
  });

  describe('the query subset over a real table', () => {
    it('$containsi is case-insensitive and treats %/_ literally', async () => {
      const a = expectList(await t.api().get('/api/country-lists?filters[country_name][$containsi]=NE'));
      expect(a.map((d) => d.attributes.country_name)).toEqual(['Nepal', 'Netherlands']);
      expectList(await t.api().get('/api/country-lists?filters[country_name][$containsi]=%25'), { total: 0 });
      expectList(await t.api().get('/api/country-lists?filters[country_name][$containsi]=_'), { total: 0 });
      expectList(await t.api().get('/api/country-lists?filters[country_name][$containsi]='), { total: 10 });
    });

    it('+ decodes to a space', async () => {
      const a = expectList(
        await t.api().get('/api/country-lists?filters[country_name][$containsi]=south+kor'),
      );
      expect(a.map((d) => d.attributes.country_name)).toEqual(['South Korea']);
    });

    it('sort forms and case-insensitive direction', async () => {
      const names = async (q: string) =>
        expectList(await t.api().get(`/api/country-lists?${q}`)).map((d) => d.attributes.country_name);
      const asc = [...(await names(''))].sort();
      expect(await names('sort=country_name:asc')).toEqual(asc);
      expect(await names('sort[country_name]=ASC')).toEqual(asc);
      expect(await names('sort[0]=country_name:DESC')).toEqual([...asc].reverse());
      expect(await names('sort[0][country_name]=desc')).toEqual([...asc].reverse());
    });

    it('pagination: pageCount/total, clamping, start/limit, withCount', async () => {
      expectList(await t.api().get('/api/country-lists?pagination[page]=2&pagination[pageSize]=3'), {
        page: 2,
        pageSize: 3,
        total: 10,
        length: 3,
      });
      const clamped = await t.api().get('/api/country-lists?pagination[pageSize]=5000');
      expectList(clamped, { pageSize: 1000, total: 10, length: 10 });
      const off = await t.api().get('/api/country-lists?pagination[start]=8&pagination[limit]=5').expect(200);
      expect(off.body.meta.pagination).toEqual({ start: 8, limit: 5, total: 10 });
      expect(off.body.data).toHaveLength(2);
      const noCount = await t.api().get('/api/country-lists?pagination[withCount]=false').expect(200);
      expect(noCount.body.meta.pagination).toEqual({ page: 1, pageSize: 25 });
    });

    it('fields', async () => {
      const a = expectList(await t.api().get('/api/country-lists?fields[0]=alfa_2_code&filters[id]=1'));
      expect(a).toEqual([{ id: 1, attributes: { alfa_2_code: 'NP' } }]);
    });

    it('validation errors (Δ7)', async () => {
      expectValidation(
        await t.api().get('/api/bd-types?publicationState=live'),
        'Invalid query parameter: publicationState',
      );
      expectValidation(await t.api().get('/api/bd-types?filters[nope]=1'), 'Invalid key nope');
      expectValidation(await t.api().get('/api/bd-types?filters[id]=x'), 'Invalid value for id');
      expectValidation(
        await t.api().get('/api/bd-types?filters[type_name][$regex]=x'),
        'Invalid operator $regex',
      );
      expectValidation(await t.api().get('/api/bd-types?sort=type_name:up'), 'Invalid order direction');
      expectValidation(await t.api().get('/api/bd-types?populate=psapbs'), 'Invalid populate psapbs');
      expectValidation(await t.api().get('/api/bd-types?pagination[page]=0'), 'Invalid pagination');
    });

    it('an unencoded & in search text truncates it (client bug, not repaired)', async () => {
      expectValidation(
        await t.api().get('/api/country-lists?filters[country_name][$containsi]=a&b'),
        'Invalid query parameter: b',
      );
    });
  });

  describe('pdf-ui lookup corpus, unencoded', () => {
    const lookupRoutes = new Set(['governance-action-types', ...LOOKUPS.map(([r]) => r)]);
    it.each(CORPUS.filter((c) => lookupRoutes.has(c.route)).map((c) => [`${c.route}?${c.query}`] as const))(
      '%s',
      async (path) => {
        expectList(await t.api().get(`/api/${path}`));
      },
    );
  });

  describe('shared wire behaviour', () => {
    it('unknown routes are 404 NotFoundError', async () => {
      expectNotFound(await t.api().get('/api/nope'));
      expectNotFound(await t.api().get('/api/comments-reports/'));
      expectNotFound(await t.api().post('/api/bd-types').send({ data: {} }));
    });

    it('GET /health is raw and outside /api', async () => {
      const res = await t.api().get('/health').expect(200);
      expect(res.body).toEqual({ status: 'ok' });
      expectNotFound(await t.api().get('/api/health'));
    });

    it('X-Powered-By is off; Vary: Origin always', async () => {
      const res = await t.api().get('/api/bd-types');
      expect(res.headers['x-powered-by']).toBeUndefined();
      expect(res.headers.vary).toMatch(/Origin/);
    });

    it('malformed JSON is 400 Invalid JSON; an oversized body is 413', async () => {
      const s = await loginStake(t);
      const bad = await t
        .api()
        .put('/api/users/edit')
        .set(s.auth)
        .set('Content-Type', 'application/json')
        .send('{bad');
      expectError(bad, 400, 'BadRequestError', 'Invalid JSON');
      const big = await t
        .api()
        .put('/api/users/edit')
        .set(s.auth)
        .send({ govtoolUsername: 'x', pad: 'y'.repeat(1024 * 1024 + 10) });
      expectError(big, 413, 'PayloadTooLargeError', 'Payload Too Large');
    });
  });

  describe('CORS (§3.8)', () => {
    it('reflects any origin with credentials under *', async () => {
      const pre = await t
        .api()
        .options('/api/bds')
        .set('Origin', 'http://anything.example')
        .set('Access-Control-Request-Method', 'POST')
        .expect(204);
      expect(pre.headers['access-control-allow-origin']).toBe('http://anything.example');
      expect(pre.headers['access-control-allow-credentials']).toBe('true');
      expect(pre.headers['access-control-allow-methods']).toBe('GET, POST, PUT, DELETE, OPTIONS');
      expect(pre.headers['access-control-allow-headers']).toBe('Authorization, Content-Type');
      expect(pre.headers['access-control-max-age']).toBe('600');
      const get = await t.api().get('/api/bd-types').set('Origin', 'http://anything.example').expect(200);
      expect(get.headers['access-control-allow-origin']).toBe('http://anything.example');
    });

    it('with an explicit list, a foreign origin gets no CORS headers', async () => {
      const listed = await createTestApp({ CORS_ORIGINS: 'http://localhost:8080' });
      try {
        const ok = await listed
          .api()
          .options('/api/bd-types')
          .set('Origin', 'http://localhost:8080')
          .expect(204);
        expect(ok.headers['access-control-allow-origin']).toBe('http://localhost:8080');
        const foreign = await listed
          .api()
          .options('/api/bd-types')
          .set('Origin', 'http://evil.example')
          .expect(204);
        expect(foreign.headers['access-control-allow-origin']).toBeUndefined();
        expect(foreign.headers['access-control-allow-credentials']).toBeUndefined();
        expect(foreign.headers['access-control-allow-methods']).toBeUndefined();
        const get = await listed.api().get('/api/bd-types').set('Origin', 'http://evil.example').expect(200);
        expect(get.headers['access-control-allow-origin']).toBeUndefined();
      } finally {
        await listed.close();
      }
    });
  });
});
