/**
 * Drives the client against a real local HTTP server that answers in the
 * service's wire format (spec §2.8), so the request line, headers and every
 * response mapping are exercised end to end.
 */
import assert from 'node:assert/strict';
import { createServer, type IncomingHttpHeaders, type Server } from 'node:http';
import type { AddressInfo } from 'node:net';
import { after, before, beforeEach, describe, it } from 'node:test';

import type {
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataReportSummary,
} from '@govtool/data-providers/metadata';

import { createHttpMetadataService, MetadataHttpError } from '../src/index';

const HASH = 'ab'.repeat(32);
const SERVED = 'cd'.repeat(32);
const URL_ = 'https://example.org/drep.jsonld';
const FETCHED_AT = '2026-09-24T10:00:00.000Z';

interface Seen {
  method: string;
  path: string;
  query: URLSearchParams;
  headers: IncomingHttpHeaders;
}

interface Reply {
  status: number;
  body?: unknown;
  raw?: string;
  headers?: Record<string, string>;
  delayMs?: number;
}

let server: Server;
let baseUrl: string;
let seen: Seen[] = [];
let reply: (req: Seen) => Reply = () => ({ status: 500 });

before(async () => {
  server = createServer((req, res) => {
    const url = new URL(req.url ?? '/', 'http://stub');
    const entry: Seen = {
      method: req.method ?? '',
      path: url.pathname,
      query: url.searchParams,
      headers: req.headers,
    };
    seen.push(entry);
    const r = reply(entry);
    const send = () => {
      res.writeHead(r.status, {
        'Content-Type': 'application/json',
        ...r.headers,
      });
      res.end(r.raw ?? (r.body === undefined ? '' : JSON.stringify(r.body)));
    };
    if (r.delayMs) setTimeout(send, r.delayMs);
    else send();
  });
  await new Promise<void>((resolve) => server.listen(0, '127.0.0.1', resolve));
  const { port } = server.address() as AddressInfo;
  baseUrl = `http://127.0.0.1:${port}/`;
});

after(async () => {
  server.closeAllConnections();
  await new Promise<void>((resolve) => server.close(() => resolve()));
});

beforeEach(() => {
  seen = [];
  reply = () => ({ status: 500 });
});

function last(): Seen {
  const entry = seen.at(-1);
  assert.ok(entry, 'no request reached the stub');
  return entry;
}

function service(timeoutMs?: number) {
  return createHttpMetadataService({ baseUrl, timeoutMs });
}

describe('getMetadata', () => {
  it('maps the legacy success shape and sends hash and url', async () => {
    reply = () => ({
      status: 200,
      body: {
        hash: HASH,
        fetchedAt: FETCHED_AT,
        url: URL_,
        metadata: { body: { givenName: 'Alice' } },
      },
    });
    const result = await service().getMetadata(HASH, URL_);
    assert.deepEqual(result, {
      ok: true,
      hash: HASH,
      body: { body: { givenName: 'Alice' } },
      fetchedAt: FETCHED_AT,
    });
    const req = last();
    assert.equal(req.method, 'GET');
    assert.equal(req.path, '/api/metadata');
    assert.equal(req.query.get('hash'), HASH);
    assert.equal(req.query.get('url'), URL_);
    assert.equal(req.query.has('cip'), false);
  });

  it('omits url when none is given', async () => {
    reply = () => ({
      status: 200,
      body: { hash: HASH, fetchedAt: FETCHED_AT, url: URL_, metadata: {} },
    });
    await service().getMetadata(HASH);
    assert.equal(last().query.has('url'), false);
  });

  it('maps a legacy failure with reportId and servedHash', async () => {
    reply = () => ({
      status: 409,
      body: {
        code: 'HASH_MISMATCH',
        category: 'INVALID_CONTENT',
        message: 'served content hashes to another value',
        url: URL_,
        fetchedAt: FETCHED_AT,
        expectedHash: HASH,
        servedHash: SERVED,
        reportId: 'rep-1',
      },
    });
    const result = await service().getMetadata(HASH, URL_);
    assert.deepEqual(result, {
      ok: false,
      code: 'HASH_MISMATCH',
      category: 'INVALID_CONTENT',
      message: 'served content hashes to another value',
      servedHash: SERVED,
      reportId: 'rep-1',
      checkedAt: FETCHED_AT,
    });
  });

  for (const [status, code, category] of [
    [502, 'FETCH_ERROR', 'NETWORK'],
    [504, 'FETCH_ERROR', 'NETWORK'],
    [413, 'EXCEEDS_LIMIT', 'INVALID_CONTENT'],
    [422, 'JSON_PARSE_ERROR', 'INVALID_CONTENT'],
    [422, 'SCHEMA_INVALID', 'SCHEMA_INVALID'],
  ] as const) {
    it(`derives category ${category} for ${status} ${code} when absent`, async () => {
      reply = () => ({
        status,
        body: {
          code,
          message: 'x',
          url: URL_,
          fetchedAt: FETCHED_AT,
          reportId: 'r',
        },
      });
      const result = await service().getMetadata(HASH, URL_);
      assert.equal(result.ok, false);
      if (result.ok) return;
      assert.equal(result.code, code);
      assert.equal(result.category, category);
      assert.equal(result.reportId, 'r');
      assert.equal('servedHash' in result, false);
    });
  }

  it('throws on 400, a caller error', async () => {
    reply = () => ({
      status: 400,
      body: { code: 'FETCH_ERROR', message: 'url is required on a miss' },
    });
    await assert.rejects(service().getMetadata(HASH), (error: unknown) => {
      assert.ok(error instanceof MetadataHttpError);
      assert.equal(error.status, 400);
      return true;
    });
  });

  it('throws on a status with no recognisable failure body', async () => {
    reply = () => ({ status: 500, raw: 'Internal Server Error' });
    await assert.rejects(service().getMetadata(HASH, URL_), MetadataHttpError);
  });

  it('throws when the service cannot be reached', async () => {
    const down = createHttpMetadataService({ baseUrl: 'http://127.0.0.1:1' });
    await assert.rejects(down.getMetadata(HASH, URL_), MetadataHttpError);
  });

  it('throws when the service does not answer in time', async () => {
    reply = () => ({ status: 200, body: {}, delayMs: 500 });
    await assert.rejects(
      service(50).getMetadata(HASH, URL_),
      /timed out after 50 ms/,
    );
  });
});

describe('getCipMetadata', () => {
  it('sends cip and throws on 501', async () => {
    reply = () => ({ status: 501, body: { message: 'not implemented' } });
    await assert.rejects(
      service().getCipMetadata(119, HASH, URL_),
      /CIP validation is not available/,
    );
    assert.equal(last().query.get('cip'), '119');
  });

  it('maps a success once the service supports it', async () => {
    reply = () => ({
      status: 200,
      body: {
        hash: HASH,
        fetchedAt: FETCHED_AT,
        url: URL_,
        metadata: { a: 1 },
      },
    });
    const result = await service().getCipMetadata<{ a: number }>(
      108,
      HASH,
      URL_,
    );
    assert.ok(result.ok);
    assert.equal(result.body.a, 1);
  });
});

describe('refresh', () => {
  it('POSTs to the refresh route and passes the outcome through', async () => {
    const outcome: MetadataRefreshOutcome = {
      refetched: false,
      retryAfterSeconds: 42,
      result: {
        ok: false,
        code: 'FETCH_ERROR',
        category: 'NETWORK',
        message: 'connection refused',
        reportId: 'rep-2',
        checkedAt: FETCHED_AT,
      },
    };
    reply = () => ({
      status: 200,
      body: outcome,
      headers: { 'Retry-After': '42' },
    });
    assert.deepEqual(await service().refresh(HASH, URL_), outcome);
    const req = last();
    assert.equal(req.method, 'POST');
    assert.equal(req.path, `/api/metadata/${HASH}/refresh`);
    assert.equal(req.query.get('url'), URL_);
  });

  it('passes a successful refetch through', async () => {
    const outcome: MetadataRefreshOutcome = {
      refetched: true,
      result: { ok: true, hash: HASH, body: { x: 1 }, fetchedAt: FETCHED_AT },
    };
    reply = () => ({ status: 200, body: outcome });
    assert.deepEqual(await service().refresh(HASH, URL_), outcome);
  });

  it('encodes the hash into the path', async () => {
    reply = () => ({ status: 404 });
    await assert.rejects(service().refresh('a/b', URL_), MetadataHttpError);
    assert.equal(last().path, '/api/metadata/a%2Fb/refresh');
  });

  it('throws on a body outside the contract', async () => {
    reply = () => ({ status: 200, body: { refetched: 'yes' } });
    await assert.rejects(service().refresh(HASH, URL_), MetadataHttpError);
  });
});

describe('reports', () => {
  const report: MetadataReport = {
    id: 'rep-1',
    hash: HASH,
    url: URL_,
    effectiveUrl: URL_,
    startedAt: FETCHED_AT,
    finishedAt: FETCHED_AT,
    hops: [
      {
        url: URL_,
        dns: { addresses: [{ address: '93.184.216.34', family: 4 }] },
        attempts: [
          {
            address: '93.184.216.34',
            family: 4,
            outcome: 'refused',
            errorCode: 'ECONNREFUSED',
            timings: { connectMs: 3 },
          },
        ],
      },
    ],
    result: {
      code: 'FETCH_ERROR',
      category: 'NETWORK',
      message: 'connection refused',
      issues: [],
    },
  };

  it('getReport returns the report', async () => {
    reply = () => ({ status: 200, body: report });
    assert.deepEqual(await service().getReport('rep-1'), report);
    assert.equal(last().path, '/api/metadata/reports/rep-1');
  });

  it('getReport encodes the id', async () => {
    reply = () => ({ status: 404 });
    await service().getReport('../x?y');
    assert.equal(last().path, '/api/metadata/reports/..%2Fx%3Fy');
  });

  it('getReport maps 404 to null', async () => {
    reply = () => ({ status: 404, body: { message: 'not found' } });
    assert.equal(await service().getReport('missing'), null);
  });

  it('getReport throws on other statuses', async () => {
    reply = () => ({ status: 503 });
    await assert.rejects(service().getReport('rep-1'), MetadataHttpError);
  });

  it('listReports sends hash and url and returns the list', async () => {
    const rows: MetadataReportSummary[] = [
      {
        id: 'rep-2',
        startedAt: FETCHED_AT,
        code: 'FETCH_ERROR',
        category: 'NETWORK',
        message: 'timeout',
      },
    ];
    reply = () => ({ status: 200, body: rows });
    assert.deepEqual(await service().listReports(HASH, URL_), rows);
    const req = last();
    assert.equal(req.path, '/api/metadata/reports');
    assert.equal(req.query.get('hash'), HASH);
    assert.equal(req.query.get('url'), URL_);
  });

  it('listReports throws when the body is not an array', async () => {
    reply = () => ({ status: 200, body: { rows: [] } });
    await assert.rejects(service().listReports(HASH, URL_), MetadataHttpError);
  });
});

describe('request hygiene', () => {
  it('never sends a Cache-Control header on any route', async () => {
    reply = (req) =>
      req.path === '/api/metadata'
        ? {
            status: 200,
            body: {
              hash: HASH,
              fetchedAt: FETCHED_AT,
              url: URL_,
              metadata: {},
            },
          }
        : req.path.endsWith('/refresh')
          ? {
              status: 200,
              body: {
                refetched: true,
                result: {
                  ok: true,
                  hash: HASH,
                  body: {},
                  fetchedAt: FETCHED_AT,
                },
              },
            }
          : req.path === '/api/metadata/reports'
            ? { status: 200, body: [] }
            : { status: 404 };
    const s = service();
    await s.getMetadata(HASH, URL_);
    await s.refresh(HASH, URL_);
    await s.getReport('rep-1');
    await s.listReports(HASH, URL_);
    assert.equal(seen.length, 4);
    for (const req of seen) {
      assert.equal(req.headers['cache-control'], undefined);
      assert.equal(req.headers['pragma'], undefined);
    }
  });

  it('refuses a baseUrl that is not http(s)', () => {
    assert.throws(() => createHttpMetadataService({ baseUrl: 'file:///x' }));
    assert.throws(() => createHttpMetadataService({ baseUrl: 'not a url' }));
    assert.throws(() =>
      createHttpMetadataService({ baseUrl: 'http://u:p@host' }),
    );
  });
});
