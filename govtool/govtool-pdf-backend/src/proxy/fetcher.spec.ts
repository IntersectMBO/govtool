import * as http from 'node:http';
import type { AddressInfo } from 'node:net';
import { FetchError, FetchOptions, decodeBody, safeGet } from './fetcher';

describe('safeGet', () => {
  let server: http.Server;
  let port: number;
  const hosts: string[] = [];

  beforeAll(async () => {
    server = http.createServer((req, res) => {
      hosts.push(req.headers.host ?? '');
      const path = req.url ?? '';
      if (path === '/ok') {
        res.writeHead(200, { 'content-type': 'application/json' });
        return res.end('{"a":1}');
      }
      if (path === '/to-blocked') {
        res.writeHead(301, { location: `http://127.0.0.2:${port}/ok` });
        return res.end();
      }
      if (path === '/to-name') {
        res.writeHead(307, { location: `http://evil.test:${port}/ok` });
        return res.end();
      }
      if (path === '/to-ftp') {
        res.writeHead(302, { location: 'ftp://127.0.0.1/x' });
        return res.end();
      }
      if (path === '/relative') {
        res.writeHead(302, { location: '/ok' });
        return res.end();
      }
      if (path === '/declared-big') {
        res.writeHead(200, { 'content-length': '999999' });
        return res.end('x'.repeat(999999));
      }
      res.writeHead(404);
      res.end();
    });
    await new Promise<void>((r) => server.listen(0, '127.0.0.1', r));
    port = (server.address() as AddressInfo).port;
  });
  afterAll(() => new Promise<void>((r) => server.close(() => r())));

  // Only 127.0.0.1 is "public" here, so the stub is reachable and anything
  // else loopback stands in for a private destination.
  const opts = (o: Partial<FetchOptions> = {}): FetchOptions => ({
    timeoutMs: 2000,
    maxBytes: 10000,
    maxRedirects: 3,
    headers: { 'User-Agent': 'test' },
    checkAddress: true,
    isBlocked: (a) => a !== '127.0.0.1',
    resolve: (host) =>
      Promise.resolve(
        host === 'public.test'
          ? [{ address: '127.0.0.1', family: 4 }]
          : host === 'evil.test'
            ? [{ address: '10.0.0.1', family: 4 }]
            : host === 'mixed.test'
              ? [
                  { address: '127.0.0.1', family: 4 },
                  { address: '10.0.0.1', family: 4 },
                ]
              : [],
      ),
    ...o,
  });

  const kind = async (p: Promise<unknown>) => {
    try {
      await p;
      return 'ok';
    } catch (e) {
      return e instanceof FetchError ? e.kind : 'other';
    }
  };

  it('connects to the resolved, validated address with the original Host header (pinning)', async () => {
    hosts.length = 0;
    const r = await safeGet(`http://public.test:${port}/ok`, opts());
    expect(r.status).toBe(200);
    expect(decodeBody(r)).toEqual({ a: 1 });
    expect(hosts).toEqual([`public.test:${port}`]);
  });

  it('refuses a host when any of its addresses is blocked', async () => {
    expect(await kind(safeGet(`http://mixed.test:${port}/ok`, opts()))).toBe('blocked');
    expect(await kind(safeGet(`http://unknown.test:${port}/ok`, opts()))).toBe('upstream');
  });

  it('re-validates every redirect target', async () => {
    expect(await kind(safeGet(`http://127.0.0.1:${port}/to-blocked`, opts()))).toBe('blocked');
    expect(await kind(safeGet(`http://127.0.0.1:${port}/to-name`, opts()))).toBe('blocked');
    expect(await kind(safeGet(`http://127.0.0.1:${port}/to-ftp`, opts()))).toBe('invalid-url');
    expect((await safeGet(`http://127.0.0.1:${port}/relative`, opts())).status).toBe(200);
    expect(await kind(safeGet(`http://127.0.0.1:${port}/relative`, opts({ maxRedirects: 0 })))).toBe(
      'upstream',
    );
  });

  it('enforces the declared and streamed size limit', async () => {
    expect(await kind(safeGet(`http://127.0.0.1:${port}/declared-big`, opts()))).toBe('upstream');
  });

  it('connects to a PDF_PROXY_HOST_REWRITES target, keeping URL and Host, on every hop', async () => {
    hosts.length = 0;
    const connectTo = new Map([['bucket.test:3001', { host: 'public.test', port }]]);
    const r = await safeGet('http://bucket.test:3001/ok', opts({ checkAddress: false, connectTo }));
    expect(r.status).toBe(200);
    expect(hosts).toEqual(['bucket.test:3001']);
    // An IP-literal source, which Node would otherwise connect to directly.
    hosts.length = 0;
    const literal = new Map([['127.0.0.2:3001', { host: 'public.test', port }]]);
    expect(
      (await safeGet('http://127.0.0.2:3001/ok', opts({ checkAddress: false, connectTo: literal }))).status,
    ).toBe(200);
    expect(hosts).toEqual(['127.0.0.2:3001']);
    // A redirect back to the rewritten origin is rewritten again.
    const back = new Map([[`127.0.0.1:${port}`, { host: '127.0.0.1', port }]]);
    expect((await safeGet(`http://127.0.0.1:${port}/relative`, opts({ connectTo: back }))).status).toBe(200);
    // Unmatched hosts are untouched.
    expect(await kind(safeGet('http://other.test:3001/ok', opts({ checkAddress: false, connectTo })))).toBe(
      'upstream',
    );
  });

  it('still applies the address guard to a rewrite target', async () => {
    const connectTo = new Map([['bucket.test:3001', { host: 'evil.test', port }]]);
    expect(await kind(safeGet('http://bucket.test:3001/ok', opts({ connectTo })))).toBe('blocked');
  });

  it('skips the address check when told to', async () => {
    const r = await safeGet(
      `http://127.0.0.1:${port}/ok`,
      opts({ checkAddress: false, isBlocked: () => true }),
    );
    expect(r.status).toBe(200);
  });
});

describe('decodeBody', () => {
  const r = (contentType: string | null, body: string) => ({
    status: 200,
    contentType,
    body: Buffer.from(body),
  });
  it('parses JSON content types only', () => {
    expect(decodeBody(r('application/json; charset=utf-8', '{"x":1}'))).toEqual({ x: 1 });
    expect(decodeBody(r('application/ld+json', '[1]'))).toEqual([1]);
    expect(decodeBody(r('text/plain', '{"x":1}'))).toBe('{"x":1}');
    expect(decodeBody(r(null, 'abc'))).toBe('abc');
    expect(decodeBody(r('application/json', 'not json'))).toBe('not json');
  });
});
