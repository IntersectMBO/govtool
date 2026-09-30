import { describe, it, expect, beforeAll, afterAll, beforeEach, vi } from 'vitest';
import request from 'supertest';
import { Server } from 'http';
import express, { Express } from 'express';
import { server as appServer } from '../src/index';
import { prisma } from '../src/config/db';
import { blake2b } from 'libcardano';
import { allowAddressesForTesting } from '../src/helpers/addressGuard';
import { setIpfsGatewaysForTesting } from '../src/helpers/ipfs';

let server: Server;
const json5Body = '{ name: "Lenient", /* comment */ list: [1, 2,], }';
const badAlgBody = '{\n  "hashAlgorithm": "sha256",\n  "body": {}\n}';
let mockApi: Express;
let mockServer: Server;

const waitFor = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

const testMetadata = { name: 'Test Meta', version: '1.0' };
const wrongMetadata = { name: 'Wrong Meta' };
const hashOf = (value: unknown) => blake2b.hash32(Buffer.from(JSON.stringify(value))).toString('hex');
// What /mutable serves. Tests change it to model a url whose content changes.
let mutableContent: unknown = wrongMetadata;
let fetchCount = 0;

// Mock IPFS gateways: each serves CIDS by id, behaving as `gatewayMode` says.
type GatewayMode = 'ok' | 'wrong' | 429 | 503 | 500;
const CIDS: Record<string, unknown> = {
  ['bafkrei' + 'a'.repeat(52)]: { doc: 'a' },
  ['bafkrei' + 'b'.repeat(52)]: { doc: 'b' },
  ['bafkrei' + 'c'.repeat(52)]: { doc: 'c' },
  ['Qm' + 'x'.repeat(44)]: { doc: 'q' },
};
const [CID_A, CID_B, CID_C, CID_Q] = Object.keys(CIDS);
const gatewayMode: Record<string, GatewayMode> = {};
const gatewayHits: Record<string, number> = {};
let elsewhereHits = 0;
const testMetadataHash = blake2b.hash32(Buffer.from(JSON.stringify(testMetadata))).toString('hex');

beforeAll(async () => {
  // Start the main application server
  await prisma.$connect();
  // The mock server is on loopback, which the address guard refuses.
  allowAddressesForTesting(['127.0.0.1', '::1']);
  server = appServer.listen(0); // Listen on a random free port

  // Setup and start the mock server
  mockApi = express();
  mockApi.get('/valid-meta', (req, res) => {
    res.json(testMetadata);
  });
  mockApi.get('/invalid-json', (req, res) => {
    res.send('this is not json');
  });
  mockApi.get('/not-found', (req, res) => {
    res.status(404).send('Not Found');
  });
  mockApi.get('/server-error', (req, res) => {
    res.status(500).send('Internal Server Error');
  });
  mockApi.get('/timeout', (req, res) => {
    setTimeout(() => {
      res.json(testMetadata);
    }, 5000); // 5 second delay
  });
  mockApi.get('/wrong-hash', (req, res) => {
    res.json(wrongMetadata);
  });
  mockApi.get('/mutable', (req, res) => {
    fetchCount++;
    res.json(mutableContent);
  });
  mockApi.get('/too-large', (req, res) => {
    res.type('application/json').send(`"${'a'.repeat(2 * 1024 * 1024 + 10)}"`);
  });
  mockApi.get('/bad-alg', (req, res) => {
    res.type('application/json').send(badAlgBody);
  });
  mockApi.get('/redirect-relative', (req, res) => {
    res.redirect(302, '/valid-meta');
  });
  mockApi.get('/redirect-private', (req, res) => {
    res.redirect(302, 'http://10.1.2.3/meta');
  });
  // A gateway named by an anchor, which the service must not contact.
  mockApi.get('/ipfs/:cid', (req, res) => {
    elsewhereHits++;
    res.status(404).send('not here');
  });
  mockApi.get('/:gw/ipfs/:cid', (req, res) => {
    const gw = req.params.gw;
    gatewayHits[gw] = (gatewayHits[gw] ?? 0) + 1;
    const mode = gatewayMode[gw] ?? 'ok';
    if (typeof mode === 'number') {
      res.status(mode).send(`gateway ${gw} says ${mode}`);
      return;
    }
    const doc = CIDS[req.params.cid];
    if (doc === undefined) {
      res.status(404).send('unknown cid');
      return;
    }
    res.json(mode === 'wrong' ? { tampered: true } : doc);
  });
  mockApi.get('/json5', (req, res) => {
    res.type('application/json').send(json5Body);
  });
  mockServer = mockApi.listen(0); // Listen on another random free port
});

afterAll(async () => {
  await new Promise<void>((resolve) => server.close(() => resolve()));
  await new Promise<void>((resolve) => mockServer.close(() => resolve()));
  await prisma.$disconnect();
});

beforeEach(async () => {
  // Let the previous test's deferred cache writes land, then clear the table.
  await waitFor(100);
  await prisma.metadata.deleteMany({});
  await prisma.fetch_report.deleteMany({});
  await prisma.fetch_body.deleteMany({});
  mutableContent = wrongMetadata;
  fetchCount = 0;
  vi.resetModules();
});

describe('GET /api/metadata', () => {
  it('should fetch metadata successfully when not in cache', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/valid-meta`;
    
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(200);
    expect(response.body.hash).toBe(testMetadataHash);
    expect(response.body.metadata).toEqual(testMetadata);

    // Verify it's in the database now
    await waitFor(100); // Wait for setImmediate to run
    const dbEntry = await prisma.metadata.findFirst({ where: { hash: Buffer.from(testMetadataHash, 'hex') } });
    expect(dbEntry).not.toBeNull();
    expect(dbEntry?.url).toBe(mockUrl);
  });

  it('should return metadata from cache if available', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/valid-meta`;
    
    // First, populate the cache
    await prisma.metadata.create({
      data: {
        hash: Buffer.from(testMetadataHash, 'hex'),
        url: mockUrl,
        data: Buffer.from(JSON.stringify(testMetadata)),
        fetchedAt: new Date(),
      },
    });

    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(200);
    expect(response.body.hash).toBe(testMetadataHash);
    expect(response.body.metadata).toEqual(testMetadata);
  });

  it('should return 400 for an invalid hash', async () => {
    const response = await request(server)
      .get('/api/metadata?hash=invalidhash');
    
    expect(response.status).toBe(400);
    expect(response.body.message).toBe('Invalid hash provided in request');
  });

  it('should return 400 if URL is not provided and metadata is not in cache', async () => {
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}`);

    expect(response.status).toBe(400);
    expect(response.body.message).toBe('Url not provided, Cached value not available');
  });

  it('should handle invalid JSON from the source URL', async () => {
    const invalidJsonHash = blake2b.hash32(Buffer.from('this is not json')).toString('hex');
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/invalid-json`;

    const response = await request(server)
      .get(`/api/metadata?hash=${invalidJsonHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(422);
    expect(response.body.message).toBe('Unable to parse data into JSON');
    expect(response.body.code).toBe('JSON_PARSE_ERROR');
    expect(response.body.metadata).toBeUndefined();
  });

  it('should return 400 for an invalid URL format', async () => {
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=invalid-url`);

    expect(response.status).toBe(400);
    expect(response.body.message).toContain('Invalid url in request');
  });

  it('should handle 404 from the source URL', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/not-found`;
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(502);
    expect(response.body.code).toBe('FETCH_ERROR');
    expect(response.body.message).toContain('Unexpected Status code: 404');
  });

  it('should handle 500 from the source URL', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/server-error`;
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(502);
    expect(response.body.code).toBe('FETCH_ERROR');
    expect(response.body.message).toContain('Unexpected Status code: 500');
  });

  it('should return HASH_MISMATCH with both hashes', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/wrong-hash`;
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(409);
    expect(response.body.code).toBe('HASH_MISMATCH');
    expect(response.body.message).toContain('Hash of fetched data does not match');
    expect(response.body.expectedHash).toBe(testMetadataHash);
    expect(response.body.servedHash).toBe(hashOf(wrongMetadata));
  });

  it('should cache mismatched content permanently under the hash it was served with', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/wrong-hash`;
    await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);
    await waitFor(100);

    // No url needed: the served hash is now a cache hit on its own.
    const response = await request(server).get(`/api/metadata?hash=${hashOf(wrongMetadata)}`);
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(wrongMetadata);
  });

  it('should replay a recent mismatch without refetching', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/mutable`;
    const path = `/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`;
    await request(server).get(path);
    await waitFor(100);

    const response = await request(server).get(path);
    expect(response.status).toBe(409);
    expect(response.body.code).toBe('HASH_MISMATCH');
    expect(response.body.servedHash).toBe(hashOf(wrongMetadata));
    expect(fetchCount).toBe(1);
  });

  it('should refetch once a mismatch expires, because the url may now serve the right content', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/mutable`;
    const path = `/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`;
    expect((await request(server).get(path)).status).toBe(409);
    await waitFor(100);

    // Age the mismatch past the error TTL, and let the publisher fix the url.
    await prisma.metadata.updateMany({
      where: { code: 'HASH_MISMATCH' },
      data: { fetchedAt: new Date(Date.now() - 2 * 60 * 1000) },
    });
    mutableContent = testMetadata;

    const response = await request(server).get(path);
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(testMetadata);
    expect(fetchCount).toBe(2);
  });

  it('should not let Cache-Control invalidate refetch inside the refresh window', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/mutable`;
    const path = `/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`;
    expect((await request(server).get(path)).status).toBe(409);
    mutableContent = testMetadata;

    const response = await request(server).get(path).set('cache-control', 'invalidate');
    expect(response.status).toBe(409);
    expect(fetchCount).toBe(1);
  });

  it('should never serve content cached for the url under a different hash', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/valid-meta`;
    await prisma.metadata.create({
      data: {
        hash: Buffer.from(hashOf(wrongMetadata), 'hex'),
        url: mockUrl,
        data: Buffer.from(JSON.stringify(wrongMetadata)),
        fetchedAt: new Date(),
      },
    });

    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(testMetadata);
  });

  it('should serve cached content by hash even with Cache-Control invalidate', async () => {
    await prisma.metadata.create({
      data: {
        hash: Buffer.from(testMetadataHash, 'hex'),
        url: 'http://unreachable.invalid/meta',
        data: Buffer.from(JSON.stringify(testMetadata)),
        fetchedAt: new Date(),
      },
    });

    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent('http://unreachable.invalid/meta')}`)
      .set('cache-control', 'invalidate');
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(testMetadata);
  });

  it('should replay a recent fetch error, and refetch once the window has passed', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/valid-meta`;
    await prisma.metadata.create({
      data: {
        hash: Buffer.from(testMetadataHash, 'hex'),
        url: mockUrl,
        code: 'FETCH_ERROR',
        error: 'connect ECONNREFUSED',
        fetchedAt: new Date(),
      },
    });
    const path = `/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`;

    const cached = await request(server).get(path);
    expect(cached.status).toBe(502);
    expect(cached.body.code).toBe('FETCH_ERROR');
    expect(cached.body.category).toBe('NETWORK');
    expect(cached.body.message).toBe('connect ECONNREFUSED');

    await prisma.metadata.updateMany({ data: { fetchedAt: new Date(Date.now() - 2 * 60 * 1000) } });
    const refreshed = await request(server).get(path).set('cache-control', 'invalidate');
    expect(refreshed.status).toBe(200);
    expect(refreshed.body.metadata).toEqual(testMetadata);
  });

  it('should return EXCEEDS_LIMIT for an oversized document', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/too-large`;
    const response = await request(server)
      .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`);

    expect(response.status).toBe(413);
    expect(response.body.code).toBe('EXCEEDS_LIMIT');
  });

  it('should cache the exact bytes served, so lenient JSON5 still hashes and re-reads', async () => {
    const hash = blake2b.hash32(Buffer.from(json5Body)).toString('hex');
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/json5`;
    const fresh = await request(server)
      .get(`/api/metadata?hash=${hash}&url=${encodeURIComponent(mockUrl)}`);
    expect(fresh.status).toBe(200);
    await waitFor(100);

    const row = await prisma.metadata.findFirst({ where: { hash: Buffer.from(hash, 'hex') } });
    expect(Buffer.from(row!.data!).toString('utf-8')).toBe(json5Body);

    const cached = await request(server).get(`/api/metadata?hash=${hash}`);
    expect(cached.status).toBe(200);
    expect(cached.body.metadata).toEqual({ name: 'Lenient', list: [1, 2] });
  });

  it('should handle timeouts', async () => {
    const mockUrl = `http://localhost:${(mockServer.address() as any).port}/timeout`;
    process.env.METADATA_REQUEST_TIMEOUT_MS = '100';
    let response;
    try {
      response = await request(server)
        .get(`/api/metadata?hash=${testMetadataHash}&url=${encodeURIComponent(mockUrl)}`)
        .set('cache-control', 'invalidate');
    } finally {
      delete process.env.METADATA_REQUEST_TIMEOUT_MS;
    }

    expect(response!.status).toBe(504);
    expect(response!.body.code).toBe('FETCH_ERROR');
    expect(response!.body.message).toContain('Timeout');
  }, 6000);
});

const port = () => (mockServer.address() as any).port;
const resolvePath = (hash: string, url: string) =>
  `/api/metadata?hash=${hash}&url=${encodeURIComponent(url)}`;
const reportOf = async (reportId: string) => {
  const response = await request(server).get(`/api/metadata/reports/${reportId}`);
  expect(response.status).toBe(200);
  return response.body;
};

describe('fetch reports', () => {
  it('should give every failure a report id and a category', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/not-found`));
    expect(response.status).toBe(502);
    expect(response.body.category).toBe('NETWORK');
    expect(response.body.reportId).toMatch(/^[0-9a-f-]{36}$/);

    const report = await reportOf(response.body.reportId);
    expect(report.hash).toBe(testMetadataHash);
    expect(report.result).toMatchObject({ code: 'FETCH_ERROR', category: 'NETWORK' });
    expect(report.hops).toHaveLength(1);
    expect(report.hops[0].dns.addresses.length).toBeGreaterThan(0);
    expect(report.hops[0].attempts[0].outcome).toBe('connected');
    expect(report.hops[0].response.status).toBe(404);
    expect(report.body).toMatchObject({ encoding: 'utf8', data: 'Not Found', truncated: false });
  });

  it('should replay the same report id while the failure is cached', async () => {
    const path = resolvePath(testMetadataHash, `http://localhost:${port()}/server-error`);
    const first = await request(server).get(path);
    const second = await request(server).get(path);
    expect(second.body.reportId).toBe(first.body.reportId);
  });

  it('should refuse a non-public address without connecting, and report why', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, 'http://169.254.169.254/latest/meta-data'));
    expect(response.status).toBe(502);
    expect(response.body.message).toContain('non-public');

    const report = await reportOf(response.body.reportId);
    expect(report.hops[0].attempts).toEqual([
      { address: '169.254.169.254', family: 4, outcome: 'blocked', blockedRange: 'linkLocal', timings: {} },
    ]);
    expect(report.body).toBeUndefined();
  });

  it('should refuse a redirect into a private range at that hop', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/redirect-private`));
    expect(response.status).toBe(502);
    const report = await reportOf(response.body.reportId);
    expect(report.hops).toHaveLength(2);
    expect(report.hops[0].redirectTo).toBe('http://10.1.2.3/meta');
    expect(report.hops[1].attempts[0]).toMatchObject({ outcome: 'blocked', blockedRange: 'private' });
  });

  it('should follow a relative redirect', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/redirect-relative`));
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(testMetadata);
  });

  it('should report every address tried when the connection is refused', async () => {
    const closed = express().listen(0);
    const closedPort = (closed.address() as any).port;
    await new Promise<void>((resolve) => closed.close(() => resolve()));

    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${closedPort}/meta`));
    expect(response.status).toBe(502);
    const report = await reportOf(response.body.reportId);
    const attempts = report.hops[0].attempts;
    expect(attempts.length).toBe(report.hops[0].dns.addresses.length);
    for (const attempt of attempts) {
      expect(attempt).toMatchObject({ outcome: 'refused', errorCode: 'ECONNREFUSED' });
    }
  });

  it('should report a DNS failure', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, 'http://does-not-exist.invalid/meta'));
    expect(response.status).toBe(502);
    expect(response.body.message).toContain('DNS lookup failed');
    const report = await reportOf(response.body.reportId);
    expect(report.hops[0].dns.error.code).toMatch(/^E/);
    expect(report.hops[0].attempts).toEqual([]);
  });

  it('should record the stage a timeout happened in', async () => {
    process.env.METADATA_REQUEST_TIMEOUT_MS = '100';
    let response;
    try {
      response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/timeout`));
    } finally {
      delete process.env.METADATA_REQUEST_TIMEOUT_MS;
    }
    expect(response.status).toBe(504);
    const report = await reportOf(response.body.reportId);
    expect(report.hops[0].attempts[0]).toMatchObject({ outcome: 'timeout', timeoutStage: 'first_byte' });
  }, 6000);

  it('should point a parse error at the character where parsing stopped', async () => {
    const hash = blake2b.hash32(Buffer.from('this is not json')).toString('hex');
    const response = await request(server).get(resolvePath(hash, `http://localhost:${port()}/invalid-json`));
    const report = await reportOf(response.body.reportId);
    const [issue] = report.result.issues;
    expect(issue.reason).toContain('invalid character');
    expect(issue.range.start).toMatchObject({ line: 1, column: 2, offset: 1, byteOffset: 1 });
    expect(report.body.data.slice(issue.range.start.offset, issue.range.end.offset)).toBe('h');
  });

  it('should point a schema issue at the offending value', async () => {
    const hash = blake2b.hash32(Buffer.from(badAlgBody)).toString('hex');
    const response = await request(server).get(resolvePath(hash, `http://localhost:${port()}/bad-alg`));
    expect(response.status).toBe(422);
    expect(response.body).toMatchObject({ code: 'SCHEMA_INVALID', category: 'SCHEMA_INVALID' });

    const report = await reportOf(response.body.reportId);
    const [issue] = report.result.issues;
    expect(issue.field).toBe('hashAlgorithm');
    expect(report.body.data.slice(issue.range.start.offset, issue.range.end.offset)).toBe('"sha256"');
    expect(issue.range.start).toMatchObject({ line: 2, column: 20 });
  });

  it('should keep the first 2 MB of an oversized body and mark it truncated', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/too-large`));
    expect(response.body.category).toBe('INVALID_CONTENT');
    const report = await reportOf(response.body.reportId);
    expect(report.body.truncated).toBe(true);
    expect(report.body.size).toBe(2 * 1024 * 1024);
    expect(report.body.data.length).toBe(2 * 1024 * 1024);
  });

  it('should serve a mismatch body from the content cache, not a second copy', async () => {
    const response = await request(server).get(resolvePath(testMetadataHash, `http://localhost:${port()}/wrong-hash`));
    const report = await reportOf(response.body.reportId);
    expect(report.result.servedHash).toBe(hashOf(wrongMetadata));
    expect(report.body.hash).toBe(hashOf(wrongMetadata));
    expect(JSON.parse(report.body.data)).toEqual(wrongMetadata);
    expect(await prisma.fetch_body.count()).toBe(0);
  });

  it('should store an identical failing body once however often it is fetched', async () => {
    const path = resolvePath(testMetadataHash, `http://localhost:${port()}/not-found`);
    await request(server).get(path);
    await prisma.metadata.updateMany({ data: { fetchedAt: new Date(Date.now() - 2 * 60 * 1000) } });
    await request(server).get(path);
    expect(await prisma.fetch_report.count()).toBe(2);
    expect(await prisma.fetch_body.count()).toBe(1);
  });

  it('should list every report for an anchor, newest first', async () => {
    const url = `http://localhost:${port()}/mutable`;
    const first = await request(server).get(resolvePath(testMetadataHash, url));
    await prisma.metadata.updateMany({ data: { fetchedAt: new Date(Date.now() - 2 * 60 * 1000) } });
    await prisma.fetch_report.updateMany({ data: { startedAt: new Date(Date.now() - 2 * 60 * 1000) } });
    const second = await request(server).get(resolvePath(testMetadataHash, url));

    const list = await request(server).get(`/api/metadata/reports?hash=${testMetadataHash}&url=${encodeURIComponent(url)}`);
    expect(list.status).toBe(200);
    expect(list.body.map((r: any) => r.id)).toEqual([second.body.reportId, first.body.reportId]);
    expect(list.body[0]).toMatchObject({ code: 'HASH_MISMATCH', category: 'INVALID_CONTENT' });
  });

  it('should 404 an unknown or malformed report id', async () => {
    expect((await request(server).get('/api/metadata/reports/00000000-0000-0000-0000-000000000000')).status).toBe(404);
    expect((await request(server).get('/api/metadata/reports/not-a-uuid')).status).toBe(404);
  });

  it('should answer 501 for cip validation until it exists', async () => {
    const response = await request(server)
      .get(`${resolvePath(testMetadataHash, `http://localhost:${port()}/valid-meta`)}&cip=119`);
    expect(response.status).toBe(501);
  });
});

describe('POST /api/metadata/:hash/refresh', () => {
  const refreshPath = (hash: string, url: string) =>
    `/api/metadata/${hash}/refresh?url=${encodeURIComponent(url)}`;

  it('should return cached content without refetching', async () => {
    const url = `http://localhost:${port()}/valid-meta`;
    await request(server).get(resolvePath(testMetadataHash, url));
    const response = await request(server).post(refreshPath(testMetadataHash, url));
    expect(response.status).toBe(200);
    expect(response.body).toMatchObject({ refetched: false, result: { ok: true, hash: testMetadataHash } });
    expect(response.body.retryAfterSeconds).toBeUndefined();
  });

  it('should refuse a second fetch inside the window and say how long to wait', async () => {
    const url = `http://localhost:${port()}/mutable`;
    await request(server).get(resolvePath(testMetadataHash, url));
    mutableContent = testMetadata;

    const response = await request(server).post(refreshPath(testMetadataHash, url));
    expect(response.status).toBe(200);
    expect(response.body.refetched).toBe(false);
    expect(response.body.retryAfterSeconds).toBeGreaterThanOrEqual(1);
    expect(response.body.retryAfterSeconds).toBeLessThanOrEqual(60);
    expect(response.headers['retry-after']).toBe(String(response.body.retryAfterSeconds));
    expect(response.body.result).toMatchObject({ ok: false, code: 'HASH_MISMATCH', category: 'INVALID_CONTENT' });
    expect(response.body.result.reportId).toBeTruthy();
    expect(fetchCount).toBe(1);
  });

  it('should refetch once the window has passed', async () => {
    const url = `http://localhost:${port()}/mutable`;
    await request(server).get(resolvePath(testMetadataHash, url));
    await prisma.metadata.updateMany({ data: { fetchedAt: new Date(Date.now() - 2 * 60 * 1000) } });
    mutableContent = testMetadata;

    const response = await request(server).post(refreshPath(testMetadataHash, url));
    expect(response.body).toMatchObject({ refetched: true, result: { ok: true, body: testMetadata } });
  });

  it('should fetch on the first retry when nothing was cached', async () => {
    const url = `http://localhost:${port()}/not-found`;
    const response = await request(server).post(refreshPath(testMetadataHash, url));
    expect(response.body).toMatchObject({ refetched: true, result: { ok: false, code: 'FETCH_ERROR' } });
  });

  it('should reject a missing url or bad hash', async () => {
    expect((await request(server).post(`/api/metadata/${testMetadataHash}/refresh`)).status).toBe(400);
    expect((await request(server).post(`/api/metadata/nothex/refresh?url=http%3A%2F%2Fx.test`)).status).toBe(400);
  });
});

describe('IPFS gateways', () => {
  const base = () => `http://localhost:${port()}`;
  const gw = (name: string) => `${base()}/${name}`;
  const ipfsPath = (cid: string, url = `ipfs://${cid}`) => resolvePath(hashOf(CIDS[cid]), url);

  beforeEach(() => {
    setIpfsGatewaysForTesting([gw('gwA'), gw('gwB'), gw('gwC')]);
    delete process.env.IPFS_PRIMARY_GATEWAY;
    for (const k of Object.keys(gatewayMode)) delete gatewayMode[k];
    for (const k of Object.keys(gatewayHits)) delete gatewayHits[k];
    elsewhereHits = 0;
  });

  afterAll(() => {
    setIpfsGatewaysForTesting(undefined);
    delete process.env.IPFS_PRIMARY_GATEWAY;
  });

  it('should try the primary gateway first when one is set', async () => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwB');
    const response = await request(server).get(ipfsPath(CID_A));
    expect(response.status).toBe(200);
    expect(response.body.metadata).toEqual(CIDS[CID_A]);
    expect(gatewayHits).toEqual({ gwB: 1 });
  });

  it('should fail over in configured order after the primary, and not blacklist on a 500', async () => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwC');
    gatewayMode.gwC = 500;
    gatewayMode.gwA = 500;
    expect((await request(server).get(ipfsPath(CID_A))).status).toBe(200);
    expect(gatewayHits).toEqual({ gwC: 1, gwA: 1, gwB: 1 });

    expect((await request(server).get(ipfsPath(CID_B))).status).toBe(200);
    expect(gatewayHits).toEqual({ gwC: 2, gwA: 2, gwB: 2 });
  });

  it.each([429, 503] as const)('should blacklist a gateway for 3 minutes after a %s', async (status) => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwA');
    gatewayMode.gwA = status;
    expect((await request(server).get(ipfsPath(CID_A))).status).toBe(200);
    expect(gatewayHits).toEqual({ gwA: 1, gwB: 1 });

    // Inside the 3 minutes the primary is not even asked.
    expect((await request(server).get(ipfsPath(CID_B))).status).toBe(200);
    expect(gatewayHits).toEqual({ gwA: 1, gwB: 2 });

    const now = Date.now();
    const clock = vi.spyOn(Date, 'now').mockReturnValue(now + 3 * 60 * 1000 + 1000);
    try {
      gatewayMode.gwA = 'ok';
      expect((await request(server).get(ipfsPath(CID_C))).status).toBe(200);
      expect(gatewayHits).toEqual({ gwA: 2, gwB: 2 });
    } finally {
      clock.mockRestore();
    }
  });

  it('should take a gateway answer as final, even when the hash does not match', async () => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwA');
    gatewayMode.gwA = 'wrong';
    const response = await request(server).get(ipfsPath(CID_A));
    expect(response.status).toBe(409);
    expect(response.body.code).toBe('HASH_MISMATCH');
    expect(gatewayHits).toEqual({ gwA: 1 });
  });

  it('should fetch an anchor that names another gateway through our gateways', async () => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwA');
    const pathForm = await request(server).get(ipfsPath(CID_A, `http://127.0.0.1:${port()}/ipfs/${CID_A}`));
    expect(pathForm.status).toBe(200);
    const subdomainForm = await request(server).get(ipfsPath(CID_Q, `https://${CID_Q.toLowerCase()}.ipfs.gateway.invalid/`));
    // A CIDv0 is case-sensitive, so a lowercased subdomain is not a valid CID and is fetched as written.
    expect(subdomainForm.status).toBe(502);
    const v1Subdomain = await request(server).get(ipfsPath(CID_B, `https://${CID_B}.ipfs.gateway.invalid/`));
    expect(v1Subdomain.status).toBe(200);
    expect(elsewhereHits).toBe(0);
    expect(gatewayHits).toEqual({ gwA: 2 });
  });

  it('should try every gateway in a random order when no primary is set', async () => {
    gatewayMode.gwA = 500;
    gatewayMode.gwB = 500;
    gatewayMode.gwC = 500;
    const order = (hops: any[]) => hops.map((h) => new URL(h.url).pathname.split('/')[1]);

    const low = vi.spyOn(Math, 'random').mockReturnValue(0);
    const first = await request(server).get(ipfsPath(CID_A));
    low.mockRestore();
    const high = vi.spyOn(Math, 'random').mockReturnValue(0.99);
    const second = await request(server).get(ipfsPath(CID_B));
    high.mockRestore();

    const firstOrder = order((await reportOf(first.body.reportId)).hops);
    const secondOrder = order((await reportOf(second.body.reportId)).hops);
    expect([...firstOrder].sort()).toEqual(['gwA', 'gwB', 'gwC']);
    expect([...secondOrder].sort()).toEqual(['gwA', 'gwB', 'gwC']);
    expect(firstOrder).not.toEqual(secondOrder);
  });

  it('should list every gateway tried in the report, and say why each failed', async () => {
    process.env.IPFS_PRIMARY_GATEWAY = gw('gwA');
    gatewayMode.gwA = 500;
    gatewayMode.gwB = 500;
    gatewayMode.gwC = 500;
    const response = await request(server).get(ipfsPath(CID_A));
    expect(response.status).toBe(502);
    expect(response.body.message).toMatch(/^No IPFS gateway served the content \(localhost:\d+: HTTP 500; .*\)$/);
    const report = await reportOf(response.body.reportId);
    expect(report.effectiveUrl).toBe(`${gw('gwA')}/ipfs/${CID_A}`);
    expect(report.hops.map((h: any) => h.url)).toEqual(
      ['gwA', 'gwB', 'gwC'].map((g) => `${gw(g)}/ipfs/${CID_A}`),
    );
    expect(report.hops.map((h: any) => h.response.status)).toEqual([500, 500, 500]);
  });

  it('should fail fast when every gateway is blacklisted', async () => {
    gatewayMode.gwA = 429;
    gatewayMode.gwB = 429;
    gatewayMode.gwC = 503;
    expect((await request(server).get(ipfsPath(CID_A))).status).toBe(502);
    const hits = { ...gatewayHits };

    const response = await request(server).get(ipfsPath(CID_B));
    expect(response.status).toBe(502);
    expect(response.body.message).toContain('All IPFS gateways are temporarily unavailable');
    expect(gatewayHits).toEqual(hits);
  });
});
