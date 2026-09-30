/**
 * The public metadata routes (spec §2.8, backend half): resolve, retry and
 * the two report reads, driven over HTTP against a stubbed metadata service.
 */
import { INestApplication } from '@nestjs/common';
import { Test } from '@nestjs/testing';
import request from 'supertest';
import { App } from 'supertest/types';
import type {
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataReportSummary,
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { ConfigService } from '../src/config/config.service';
import { MetadataGatewayService } from '../src/metadata/metadata-gateway.service';
import { MetadataController } from '../src/metadata/metadata.controller';
import { MetadataService } from '../src/metadata/metadata.service';
import { METADATA } from '../src/providers/providers.module';

const HASH = 'ab'.repeat(32);
const URL_ = 'https://example.org/drep.jsonld';
const AT = '2026-09-24T10:00:00.000Z';
const INTERNAL = 'http://metadata.internal:3000';

const FAILURE: MetadataResult = {
  ok: false,
  code: 'FETCH_ERROR',
  category: 'NETWORK',
  message: 'connection refused',
  reportId: 'rep-1',
  checkedAt: AT,
};

const REPORT: MetadataReport = {
  id: 'rep-1',
  hash: HASH,
  url: URL_,
  effectiveUrl: URL_,
  startedAt: AT,
  finishedAt: AT,
  hops: [
    {
      url: URL_,
      dns: { error: { code: 'ENOTFOUND', message: 'no such host' } },
      attempts: [],
    },
  ],
  result: {
    code: 'FETCH_ERROR',
    category: 'NETWORK',
    message: 'no such host',
    issues: [],
  },
};

const SUMMARIES: MetadataReportSummary[] = [
  {
    id: 'rep-1',
    startedAt: AT,
    code: 'FETCH_ERROR',
    category: 'NETWORK',
    message: 'no such host',
  },
];

type Call = { method: keyof MetadataServiceV1; args: unknown[] };

/**
 * A complete, typed `MetadataServiceV1`. Every method not overridden rejects,
 * so a test that reaches an unexpected method fails loudly. No cast: the
 * stub has to be a valid instance of the contract, or this does not compile.
 */
function stubMetadata(
  calls: Call[],
  overrides: Partial<MetadataServiceV1> = {},
): MetadataServiceV1 {
  const unstubbed = (method: keyof MetadataServiceV1) =>
    Promise.reject(new Error(`${method} not stubbed`));
  const base: MetadataServiceV1 = {
    getMetadata: () => unstubbed('getMetadata'),
    getCipMetadata: <TBody = unknown>(): Promise<MetadataResult<TBody>> =>
      unstubbed('getCipMetadata'),
    refresh: () => unstubbed('refresh'),
    getReport: () => unstubbed('getReport'),
    listReports: () => unstubbed('listReports'),
  };
  const service: MetadataServiceV1 = { ...base, ...overrides };
  return {
    getMetadata: (...args) => {
      calls.push({ method: 'getMetadata', args });
      return service.getMetadata(...args);
    },
    getCipMetadata: <TBody = unknown>(
      cip: number,
      hash: string,
      url?: string,
    ) => {
      calls.push({ method: 'getCipMetadata', args: [cip, hash, url] });
      return service.getCipMetadata<TBody>(cip, hash, url);
    },
    refresh: (...args) => {
      calls.push({ method: 'refresh', args });
      return service.refresh(...args);
    },
    getReport: (...args) => {
      calls.push({ method: 'getReport', args });
      return service.getReport(...args);
    },
    listReports: (...args) => {
      calls.push({ method: 'listReports', args });
      return service.listReports(...args);
    },
  };
}

async function boot(
  metadata: MetadataServiceV1 | null,
): Promise<INestApplication<App>> {
  const moduleRef = await Test.createTestingModule({
    controllers: [MetadataController],
    providers: [
      MetadataService,
      MetadataGatewayService,
      {
        provide: ConfigService,
        useValue: { get: () => ({ ipfsGateway: '', ipfsProjectId: '' }) },
      },
      { provide: METADATA, useValue: metadata },
    ],
  }).compile();
  const app = moduleRef.createNestApplication<INestApplication<App>>({
    logger: false,
  });
  await app.init();
  return app;
}

describe('metadata routes', () => {
  let app: INestApplication<App>;
  let calls: Call[];

  async function start(overrides: Partial<MetadataServiceV1>) {
    calls = [];
    app = await boot(stubMetadata(calls, overrides));
  }

  afterEach(async () => {
    await app?.close();
  });

  describe('GET /metadata/resolve', () => {
    it('answers 200 with a success, normalising the hash', async () => {
      const success: MetadataResult = {
        ok: true,
        hash: HASH,
        body: { body: { givenName: 'Alice' } },
        fetchedAt: AT,
      };
      await start({ getMetadata: () => Promise.resolve(success) });
      const res = await request(app.getHttpServer())
        .get('/metadata/resolve')
        .query({ hash: HASH.toUpperCase(), url: URL_ })
        .expect(200);
      expect(res.body).toEqual(success);
      expect(calls).toEqual([{ method: 'getMetadata', args: [HASH, URL_] }]);
      expect(res.headers['cache-control']).toBeUndefined();
    });

    it('answers 200 with a failure too', async () => {
      await start({ getMetadata: () => Promise.resolve(FAILURE) });
      const res = await request(app.getHttpServer())
        .get('/metadata/resolve')
        .query({ hash: HASH, url: 'ipfs://bafyexample' })
        .expect(200);
      expect(res.body).toEqual(FAILURE);
    });

    it.each([
      ['a short hash', { hash: 'ab', url: URL_ }],
      ['a non-hex hash', { hash: 'zz'.repeat(32), url: URL_ }],
      ['no url', { hash: HASH }],
      ['a file url', { hash: HASH, url: 'file:///etc/passwd' }],
      ['a scheme-less url', { hash: HASH, url: 'example.org/x' }],
      ['a cache parameter', { hash: HASH, url: URL_, invalidate: 'true' }],
    ])('rejects %s with 400 before calling the service', async (_, query) => {
      await start({ getMetadata: () => Promise.resolve(FAILURE) });
      await request(app.getHttpServer())
        .get('/metadata/resolve')
        .query(query)
        .expect(400);
      expect(calls).toEqual([]);
    });

    it('maps a thrown infrastructure error to a generic 502', async () => {
      await start({
        getMetadata: () =>
          Promise.reject(new Error(`connect ECONNREFUSED ${INTERNAL}`)),
      });
      const res = await request(app.getHttpServer())
        .get('/metadata/resolve')
        .query({ hash: HASH, url: URL_ })
        .expect(502);
      expect(JSON.stringify(res.body)).not.toContain('metadata.internal');
      expect(res.body).toEqual({
        errorType: 'MetadataServiceError',
        message: 'The metadata service is unavailable',
      });
    });
  });

  describe('POST /metadata/retry', () => {
    it('answers 200 and mirrors retryAfterSeconds in Retry-After', async () => {
      const outcome: MetadataRefreshOutcome = {
        refetched: false,
        retryAfterSeconds: 37,
        result: FAILURE,
      };
      await start({ refresh: () => Promise.resolve(outcome) });
      const res = await request(app.getHttpServer())
        .post('/metadata/retry')
        .send({ hash: HASH, url: URL_ })
        .expect(200);
      expect(res.body).toEqual(outcome);
      expect(res.headers['retry-after']).toBe('37');
      expect(calls).toEqual([{ method: 'refresh', args: [HASH, URL_] }]);
    });

    it('sets no Retry-After when a real fetch happened', async () => {
      const outcome: MetadataRefreshOutcome = {
        refetched: true,
        result: { ok: true, hash: HASH, body: {}, fetchedAt: AT },
      };
      await start({ refresh: () => Promise.resolve(outcome) });
      const res = await request(app.getHttpServer())
        .post('/metadata/retry')
        .send({ hash: HASH, url: URL_ })
        .expect(200);
      expect(res.body).toEqual(outcome);
      expect(res.headers['retry-after']).toBeUndefined();
    });

    it.each([
      ['no url', { hash: HASH }],
      ['a bad hash', { hash: 'x', url: URL_ }],
      ['an extra field', { hash: HASH, url: URL_, ttl: 0 }],
    ])('rejects %s with 400', async (_, body) => {
      await start({
        refresh: () => Promise.resolve({ refetched: true, result: FAILURE }),
      });
      await request(app.getHttpServer())
        .post('/metadata/retry')
        .send(body)
        .expect(400);
      expect(calls).toEqual([]);
    });

    it('maps a thrown infrastructure error to 502', async () => {
      await start({ refresh: () => Promise.reject(new Error(INTERNAL)) });
      const res = await request(app.getHttpServer())
        .post('/metadata/retry')
        .send({ hash: HASH, url: URL_ })
        .expect(502);
      expect(JSON.stringify(res.body)).not.toContain('metadata.internal');
    });
  });

  describe('GET /metadata/reports/:id', () => {
    it('answers 200 with the report', async () => {
      await start({ getReport: () => Promise.resolve(REPORT) });
      const res = await request(app.getHttpServer())
        .get('/metadata/reports/rep-1')
        .expect(200);
      expect(res.body).toEqual(REPORT);
      expect(calls).toEqual([{ method: 'getReport', args: ['rep-1'] }]);
    });

    it('answers 404 when there is no such report', async () => {
      await start({ getReport: () => Promise.resolve(null) });
      await request(app.getHttpServer())
        .get('/metadata/reports/missing')
        .expect(404);
    });

    it.each(['a.b', 'a_b', 'x'.repeat(65), 'a%2Fb'])(
      'rejects id %s with 400',
      async (id) => {
        await start({ getReport: () => Promise.resolve(REPORT) });
        await request(app.getHttpServer())
          .get(`/metadata/reports/${id}`)
          .expect(400);
        expect(calls).toEqual([]);
      },
    );
  });

  describe('GET /metadata/reports', () => {
    it('answers 200 with the history', async () => {
      await start({ listReports: () => Promise.resolve(SUMMARIES) });
      const res = await request(app.getHttpServer())
        .get('/metadata/reports')
        .query({ hash: HASH, url: URL_ })
        .expect(200);
      expect(res.body).toEqual(SUMMARIES);
      expect(calls).toEqual([{ method: 'listReports', args: [HASH, URL_] }]);
    });

    it('answers 200 with an empty history', async () => {
      await start({ listReports: () => Promise.resolve([]) });
      const res = await request(app.getHttpServer())
        .get('/metadata/reports')
        .query({ hash: HASH, url: URL_ })
        .expect(200);
      expect(res.body).toEqual([]);
    });

    it('rejects a missing hash with 400', async () => {
      await start({ listReports: () => Promise.resolve([]) });
      await request(app.getHttpServer())
        .get('/metadata/reports')
        .query({ url: URL_ })
        .expect(400);
    });
  });
});

describe('metadata routes without a configured service', () => {
  let app: INestApplication<App>;

  beforeAll(async () => {
    app = await boot(null);
  });

  afterAll(async () => {
    await app.close();
  });

  it.each([
    [
      'GET /metadata/resolve',
      () =>
        request(app.getHttpServer())
          .get('/metadata/resolve')
          .query({ hash: HASH, url: URL_ }),
    ],
    [
      'POST /metadata/retry',
      () =>
        request(app.getHttpServer())
          .post('/metadata/retry')
          .send({ hash: HASH, url: URL_ }),
    ],
    [
      'GET /metadata/reports/:id',
      () => request(app.getHttpServer()).get('/metadata/reports/rep-1'),
    ],
    [
      'GET /metadata/reports',
      () =>
        request(app.getHttpServer())
          .get('/metadata/reports')
          .query({ hash: HASH, url: URL_ }),
    ],
  ])('%s answers 503', async (_, send) => {
    const res = await send().expect(503);
    expect(res.body).toEqual({
      errorType: 'MetadataUnconfiguredError',
      message: 'Backend is not configured for metadata resolution',
    });
  });

  it('accepts a CIP100 vote rationale on POST /metadata/validate', async () => {
    await request(app.getHttpServer())
      .post('/metadata/validate')
      .send({ hash: HASH, url: 'file:///etc/passwd', standard: 'CIP100' })
      .expect(201)
      .expect((res) => {
        expect(res.body).toEqual({ status: 'URL_BLOCKED', valid: false });
      });
  });

  it('leaves POST /metadata/validate working', async () => {
    await request(app.getHttpServer())
      .post('/metadata/validate')
      .send({ hash: HASH, url: 'file:///etc/passwd' })
      .expect(201)
      .expect((res) => {
        expect(res.body).toEqual({ status: 'URL_BLOCKED', valid: false });
      });
  });
});
