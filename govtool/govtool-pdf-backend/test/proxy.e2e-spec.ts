// SPEC §9: the GovTool proxy and the safe fetcher, against local stub servers.

import * as http from 'node:http';
import type { AddressInfo } from 'node:net';
import { createTestApp, TestApp } from './helpers/app';
import { loginStake, StakeSession } from './helpers/auth';
import { expectForbidden, expectNotFound, expectUnauthorized } from './helpers/envelope';

interface Seen {
  url: string;
  headers: http.IncomingHttpHeaders;
}

/** A stub upstream: routes by path, records every request. */
async function stub(): Promise<{ base: string; seen: Seen[]; close: () => Promise<void> }> {
  const seen: Seen[] = [];
  const server = http.createServer((req, res) => {
    seen.push({ url: req.url ?? '', headers: req.headers });
    const path = (req.url ?? '').split('?')[0];
    const json = (status: number, body: unknown) => {
      res.writeHead(status, { 'content-type': 'application/json; charset=utf-8' });
      res.end(JSON.stringify(body));
    };
    if (path === '/proposal/enacted-details')
      return json(200, { hash: 'ab'.repeat(32), id: 0, query: req.url });
    if (path === '/other/allowed') return json(200, { ok: true });
    if (path === '/fail') return json(500, { message: 'boom' });
    if (path === '/missing') return json(404, { message: 'nope' });
    if (path === '/json') return json(200, { constitution: true });
    if (path === '/text') {
      res.writeHead(200, { 'content-type': 'text/plain' });
      return res.end('plain constitution text');
    }
    if (path === '/big') {
      res.writeHead(200, { 'content-type': 'text/plain' });
      return res.end('x'.repeat(5000));
    }
    if (path === '/slow') {
      setTimeout(() => json(200, { late: true }), 1500);
      return;
    }
    const redirect = /^\/redirect\/(\d+)$/.exec(path);
    if (redirect) {
      const n = Number(redirect[1]);
      res.writeHead(302, { location: n <= 1 ? '/json' : `/redirect/${n - 1}` });
      return res.end();
    }
    if (path === '/redirect-file') {
      res.writeHead(302, { location: 'file:///etc/passwd' });
      return res.end();
    }
    if (path.startsWith('/ipfs/')) return json(200, { ipfs: path });
    res.writeHead(418);
    res.end();
  });
  await new Promise<void>((r) => server.listen(0, '127.0.0.1', r));
  const { port } = server.address() as AddressInfo;
  return {
    base: `http://127.0.0.1:${port}`,
    seen,
    close: () => new Promise<void>((r) => server.close(() => r())),
  };
}

/** A raw GET that keeps the path exactly as written (no client normalisation). */
function rawGet(t: TestApp, path: string): Promise<{ status: number; body: any }> {
  const server = t.app.getHttpServer() as http.Server;
  const { port } = server.address() as AddressInfo;
  return new Promise((resolve, reject) => {
    const req = http.request({ host: '127.0.0.1', port, path, method: 'GET' }, (res) => {
      let s = '';
      res.on('data', (c: Buffer) => (s += c.toString()));
      res.on('end', () => resolve({ status: res.statusCode ?? 0, body: s ? JSON.parse(s) : null }));
    });
    req.on('error', reject);
    req.end();
  });
}

describe('proxy (e2e)', () => {
  let up: Awaited<ReturnType<typeof stub>>;
  beforeAll(async () => {
    up = await stub();
  });
  afterAll(async () => {
    await up.close();
  });

  describe('GET /api/proxy/govtool/<path> (§9.1)', () => {
    let t: TestApp;
    beforeAll(async () => {
      t = await createTestApp({
        GOVTOOL_API_BASE_URL: up.base,
        GOVTOOL_PROXY_ALLOWED_PATHS: 'proposal/enacted-details,other/allowed',
        PROXY_TIMEOUT_MS: '500',
      });
    });
    afterAll(async () => {
      await t.close();
    });

    it('forwards an allowlisted path with its query; public; no client headers', async () => {
      up.seen.length = 0;
      const res = await t
        .api()
        .get('/api/proxy/govtool/proposal/enacted-details?type=HardForkInitiation')
        .set('Authorization', 'Bearer garbage')
        .set('Cookie', 'refreshToken=secret')
        .set('X-Custom', 'leak');
      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        status: 200,
        data: { hash: 'ab'.repeat(32), id: 0, query: '/proposal/enacted-details?type=HardForkInitiation' },
      });
      const h = up.seen[0].headers;
      expect(h['user-agent']).toBe('govtool-pdf-proxy');
      expect(h.accept).toBe('application/json');
      expect(h.authorization).toBeUndefined();
      expect(h.cookie).toBeUndefined();
      expect(h['x-custom']).toBeUndefined();
      expect((await t.api().get('/api/proxy/govtool/other/allowed')).body).toEqual({
        status: 200,
        data: { ok: true },
      });
    });

    it('refuses anything not allowlisted, and traversal in any spelling (Δ44)', async () => {
      up.seen.length = 0;
      for (const p of [
        '/api/proxy/govtool/fail',
        '/api/proxy/govtool/proposal/enacted-details/extra',
        '/api/proxy/govtool/proposal/../fail',
        '/api/proxy/govtool/proposal/%2e%2e/fail',
        '/api/proxy/govtool/proposal%2Fenacted-details',
        '/api/proxy/govtool/proposal//enacted-details',
        '/api/proxy/govtool/proposal\\enacted-details',
      ]) {
        const res = await rawGet(t, p);
        expect({ p, status: res.status }).toEqual({ p, status: 404 });
        if (res.body) expect(res.body.error.name).toBe('NotFoundError');
      }
      expect(up.seen.filter((s) => s.url.startsWith('/fail'))).toEqual([]);
      // Matching is exact after percent-decoding.
      expect((await rawGet(t, '/api/proxy/govtool/%70roposal/enacted-details')).status).toBe(200);
    });

    it('upstream non-2xx, timeout and redirect', async () => {
      const t2 = await createTestApp({
        GOVTOOL_API_BASE_URL: up.base,
        GOVTOOL_PROXY_ALLOWED_PATHS: 'fail,slow,redirect/1',
        PROXY_TIMEOUT_MS: '300',
      });
      try {
        const fail = await t2.api().get('/api/proxy/govtool/fail');
        expect([fail.status, fail.body]).toEqual([
          500,
          { error: 'Request failed with status code 500', details: { message: 'boom' } },
        ]);
        const slow = await t2.api().get('/api/proxy/govtool/slow');
        expect([slow.status, slow.body]).toEqual([502, { error: 'Upstream request failed', details: null }]);
        const redirect = await t2.api().get('/api/proxy/govtool/redirect/1');
        expect([redirect.status, redirect.body]).toEqual([
          502,
          { error: 'Upstream request failed', details: null },
        ]);
      } finally {
        await t2.close();
      }
    });

    it('503 when GOVTOOL_API_BASE_URL is unset', async () => {
      const t2 = await createTestApp({ GOVTOOL_API_BASE_URL: undefined });
      try {
        const res = await t2.api().get('/api/proxy/govtool/proposal/enacted-details?type=HardForkInitiation');
        expect([res.status, res.body]).toEqual([
          503,
          { error: 'GOVTOOL_API_BASE_URL is not configured', details: null },
        ]);
        expectNotFound(await t2.api().get('/api/proxy/govtool/elsewhere'));
      } finally {
        await t2.close();
      }
    });
  });

  describe('POST /api/proxy (§9.2)', () => {
    describe('with PDF_ALLOW_PRIVATE_URLS=true (stub upstream)', () => {
      let t: TestApp;
      let s: StakeSession;
      beforeAll(async () => {
        t = await createTestApp({
          PDF_ALLOW_PRIVATE_URLS: 'true',
          PROXY_TIMEOUT_MS: '500',
          PROXY_MAX_BYTES: '1000',
          IPFS_GATEWAY_URL: `${up.base}/ipfs`,
        });
        s = await loginStake(t);
      });
      afterAll(async () => {
        await t.close();
      });
      const fetch = (body: Record<string, unknown>) => t.api().post('/api/proxy').set(s.auth).send(body);

      it('is authenticated (Δ45)', async () => {
        expectForbidden(
          await t
            .api()
            .post('/api/proxy')
            .send({ url: `${up.base}/json` }),
        );
        expectUnauthorized(
          await t
            .api()
            .post('/api/proxy')
            .set('Authorization', 'Bearer garbage')
            .send({ url: `${up.base}/json` }),
        );
      });

      it('JSON is parsed, text is a string; only safe headers go out', async () => {
        up.seen.length = 0;
        const json = await fetch({
          url: `${up.base}/json`,
          method: 'GET',
          headers: { 'X-Leak': '1' },
          data: 'x',
        });
        expect([json.status, json.body]).toEqual([200, { status: 200, data: { constitution: true } }]);
        expect(up.seen[0].headers['x-leak']).toBeUndefined();
        expect(up.seen[0].headers['user-agent']).toBe('govtool-pdf-proxy');
        expect(up.seen[0].headers.accept).toBe('*/*');
        expect(up.seen[0].headers.authorization).toBeUndefined();
        const text = await fetch({ url: `${up.base}/text` });
        expect(text.body).toEqual({ status: 200, data: 'plain constitution text' });
      });

      it('ipfs:// goes through IPFS_GATEWAY_URL', async () => {
        const res = await fetch({ url: 'ipfs://bafyCID/doc.json', method: 'GET' });
        expect(res.body).toEqual({ status: 200, data: { ipfs: '/ipfs/bafyCID/doc.json' } });
      });

      it('follows up to 3 redirects; more is 502; a non-http target is refused', async () => {
        expect((await fetch({ url: `${up.base}/redirect/3` })).body).toEqual({
          status: 200,
          data: { constitution: true },
        });
        const four = await fetch({ url: `${up.base}/redirect/4` });
        expect([four.status, four.body]).toEqual([502, { error: 'Upstream request failed', details: null }]);
        const file = await fetch({ url: `${up.base}/redirect-file` });
        expect([file.status, file.body]).toEqual([400, { error: 'Invalid URL', details: null }]);
      });

      it('oversize, timeout and upstream errors', async () => {
        const big = await fetch({ url: `${up.base}/big` });
        expect([big.status, big.body]).toEqual([502, { error: 'Upstream request failed', details: null }]);
        const slow = await fetch({ url: `${up.base}/slow` });
        expect([slow.status, slow.body]).toEqual([502, { error: 'Upstream request failed', details: null }]);
        const missing = await fetch({ url: `${up.base}/missing` });
        expect([missing.status, missing.body]).toEqual([
          404,
          { error: 'Request failed with status code 404', details: { message: 'nope' } },
        ]);
      });

      it('non-GET and malformed URLs', async () => {
        for (const method of ['POST', 'DELETE', 'put', 1]) {
          const r = await fetch({ url: `${up.base}/json`, method });
          expect([r.status, r.body]).toEqual([400, { error: 'Only GET is supported', details: null }]);
        }
        expect((await fetch({ url: `${up.base}/json`, method: 'get' })).status).toBe(200);
        for (const url of [
          undefined,
          '',
          'not a url',
          'ftp://example.com/x',
          'file:///etc/passwd',
          'javascript:alert(1)',
          `http://user:pass@${up.base.slice(7)}/json`,
          'ipfs://',
        ]) {
          const r = await fetch({ url });
          expect({ url, status: r.status, body: r.body }).toEqual({
            url,
            status: 400,
            body: { error: 'Invalid URL', details: null },
          });
        }
      });
    });

    describe('with the switch off (default)', () => {
      let t: TestApp;
      let s: StakeSession;
      beforeAll(async () => {
        t = await createTestApp();
        s = await loginStake(t);
      });
      afterAll(async () => {
        await t.close();
      });

      it('refuses loopback, private, link-local and mapped destinations before connecting', async () => {
        up.seen.length = 0;
        const port = up.base.split(':')[2];
        for (const url of [
          `${up.base}/json`,
          `http://localhost:${port}/json`,
          `http://[::1]:${port}/json`,
          `http://[::ffff:127.0.0.1]:${port}/json`,
          `http://0x7f.1:${port}/json`,
          `http://2130706433:${port}/json`,
          'http://169.254.169.254/latest/meta-data/',
          'http://10.1.2.3/',
          'http://192.168.1.1/',
          'http://172.16.0.1/',
          'http://100.64.0.1/',
          'http://0.0.0.0/',
          'http://[fd00::1]/',
          'http://[fe80::1]/',
          'https://127.0.0.1/',
        ]) {
          const r = await t.api().post('/api/proxy').set(s.auth).send({ url, method: 'GET' });
          expect({ url, status: r.status, body: r.body }).toEqual({
            url,
            status: 400,
            body: { error: 'Destination not allowed', details: null },
          });
        }
        expect(up.seen).toEqual([]);
      });
    });
  });
});
