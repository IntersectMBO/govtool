/**
 * The transport: error mapping, retries, the token header, row counts,
 * big-integer JSON and body chunking. Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { chunkIds, createKoiosHttp, inList, parseContentRange, parseJson } from '../dist/http.js';
import { createKoiosProvider } from '../dist/index.js';
import { chainRoutes, fakeKoios, provider, rejectsWith } from './helpers.mjs';

const http = (fetch, extra = {}) => createKoiosHttp({ network: 'mainnet', fetch, maxRetries: 0, ...extra });

test('404 maps to NOT_FOUND', async () => {
  const { fetch } = fakeKoios({ tip: { status: 404, body: { message: 'x' } } });
  await rejectsWith(http(fetch).get('tip'), 'NOT_FOUND');
});

test('429 maps to PROVIDER_RATE_LIMITED with retryAfterSeconds', async () => {
  const { fetch } = fakeKoios({ tip: { status: 429, headers: { 'retry-after': '7' } } });
  await assert.rejects(http(fetch).get('tip'), (e) => {
    assert.equal(e.code, 'PROVIDER_RATE_LIMITED');
    assert.equal(e.retryAfterSeconds, 7);
    assert.equal(e.retryable, true);
    return true;
  });
});

test('a 429 whose Retry-After exceeds the wait cap is surfaced without waiting', async () => {
  const { fetch, calls } = fakeKoios({ tip: { status: 429, headers: { 'retry-after': '120' } } });
  const started = Date.now();
  await rejectsWith(http(fetch, { maxRetries: 3 }).get('tip'), 'PROVIDER_RATE_LIMITED');
  assert.equal(calls.length, 1);
  assert.ok(Date.now() - started < 1000);
});

test('5xx maps to PROVIDER_UNAVAILABLE and is retried', async () => {
  let n = 0;
  const { fetch, calls } = fakeKoios({ tip: () => (n++ < 1 ? { status: 503 } : chainRoutes.tip) });
  const res = await http(fetch, { maxRetries: 2 }).get('tip');
  assert.equal(res.rows[0].epoch_no, 657);
  assert.equal(calls.length, 2);
  const { fetch: always } = fakeKoios({ tip: { status: 502 } });
  await rejectsWith(http(always).get('tip'), 'PROVIDER_UNAVAILABLE');
});

test('a timeout maps to PROVIDER_TIMEOUT', async () => {
  // AbortSignal.timeout's timer does not hold the event loop open; this one does.
  const hang = (_url, init) =>
    new Promise((_resolve, reject) => {
      const keepAlive = setTimeout(() => {}, 5000);
      init.signal.addEventListener('abort', () => {
        clearTimeout(keepAlive);
        reject(init.signal.reason);
      });
    });
  await rejectsWith(http(hang, { timeoutMs: 20 }).get('tip'), 'PROVIDER_TIMEOUT');
});

test('a network failure maps to PROVIDER_UNAVAILABLE', async () => {
  const broken = async () => {
    throw new TypeError('fetch failed');
  };
  await rejectsWith(http(broken).get('tip'), 'PROVIDER_UNAVAILABLE');
});

test('400 and 413 are this provider’s fault: INTERNAL, body not forwarded', async () => {
  const { fetch } = fakeKoios({ tip: { status: 400, body: { message: 'column record.secret_col does not exist' } } });
  await assert.rejects(http(fetch).get('tip'), (e) => {
    assert.equal(e.code, 'INTERNAL');
    assert.doesNotMatch(e.message + JSON.stringify(e.details), /secret_col/);
    return true;
  });
});

test('401/403 map to PROVIDER_UNAVAILABLE and never echo the token', async () => {
  const { fetch, calls } = fakeKoios({ tip: { status: 401 } });
  await assert.rejects(http(fetch, { token: 'sekret-token' }).get('tip'), (e) => {
    assert.equal(e.code, 'PROVIDER_UNAVAILABLE');
    assert.doesNotMatch(`${e.message} ${JSON.stringify(e.details)} ${e.stack}`, /sekret-token/);
    return true;
  });
  assert.equal(calls[0].headers.get('authorization'), 'Bearer sekret-token');
});

test('no token means no Authorization header', async () => {
  const { fetch, calls } = fakeKoios({ tip: chainRoutes.tip });
  await http(fetch).get('tip');
  assert.equal(calls[0].headers.get('authorization'), null);
});

test('base URLs: network default, explicit override, and bad values refused', () => {
  assert.equal(createKoiosHttp({ network: 'preprod' }).baseUrl, 'https://preprod.koios.rest/api/v1');
  assert.equal(createKoiosHttp({ network: 'mainnet', baseUrl: 'https://k.example/api/v1/' }).baseUrl, 'https://k.example/api/v1');
  assert.throws(() => createKoiosHttp({ network: 'sanchonet' }), /baseUrl/);
  assert.throws(() => createKoiosHttp({ network: 'constructor' }), /baseUrl/);
  assert.throws(() => createKoiosHttp({ network: 'mainnet', baseUrl: 'ftp://x' }), /http/);
  assert.throws(() => createKoiosHttp({ network: 'mainnet', baseUrl: 'https://user:pw@x/api/v1' }), /credentials/);
  assert.throws(() => createKoiosProvider({ network: 'mainnet', baseUrl: 'not a url' }), /valid URL/);
});

test('query values are URL-encoded and filters refuse unvalidated values', async () => {
  const { fetch, calls } = fakeKoios({ drep_list: [] });
  await http(fetch).get('drep_list', { drep_id: 'eq.a&b=c' });
  assert.equal(calls[0].url.searchParams.get('drep_id'), 'eq.a&b=c');
  assert.equal([...calls[0].url.searchParams.keys()].length, 1);
  assert.throws(() => inList(['ok', 'x),or(y']), /unvalidated/);
  assert.equal(inList(['a1', 'b2']), 'in.(a1,b2)');
  assert.throws(() => http(fetch).get('../tip'), /endpoint/);
});

test('Content-Range gives the total', () => {
  assert.equal(parseContentRange('0-999/1685'), 1685);
  assert.equal(parseContentRange('*/0'), 0);
  assert.equal(parseContentRange('0-0/*'), undefined);
  assert.equal(parseContentRange(null), undefined);
});

test('JSON keeps integers beyond 2^53 exact', () => {
  const parsed = parseJson('{"a": 45000000000000000123, "b": 0.67, "c": 12}');
  assert.equal(parsed.a, '45000000000000000123');
  assert.equal(parsed.b, 0.67);
  assert.equal(parsed.c, 12);
});

test('bulk POST bodies stay under the 5,120-byte limit', async () => {
  const ids = Array.from({ length: 400 }, (_, i) => `drep1${'x'.repeat(53)}${String(i).padStart(4, '0')}`);
  const chunks = chunkIds('_drep_ids', ids);
  assert.ok(chunks.length > 1);
  for (const chunk of chunks) assert.ok(Buffer.byteLength(JSON.stringify({ _drep_ids: chunk })) <= 4600);
  assert.deepEqual(chunks.flat(), ids);
  const { fetch, calls } = fakeKoios({ drep_info: (_url, init) => init.body._drep_ids.map((drep_id) => ({ drep_id })) });
  const rows = await http(fetch).postChunked('drep_info', '_drep_ids', ids);
  assert.equal(rows.length, 400);
  for (const call of calls) assert.ok(Buffer.byteLength(JSON.stringify(call.body)) <= 5120);
});

test('getWindow serves a full window at any offset, with the exact total', async () => {
  const rows = Array.from({ length: 2500 }, (_, i) => ({ n: i }));
  const { fetch, calls } = fakeKoios({ drep_list: rows });
  const res = await http(fetch).getWindow('drep_list', {}, { offset: 700, limit: 1000 });
  assert.equal(res.total, 2500);
  assert.equal(res.rows.length, 1000);
  assert.equal(res.rows[0].n, 700);
  assert.equal(res.rows[999].n, 1699);
  assert.equal(calls[0].headers.get('prefer'), 'count=exact');
  const end = await http(fetch).getWindow('drep_list', {}, { offset: 3000, limit: 10 });
  assert.deepEqual(end, { rows: [], total: 2500 });
});

test('a provider method never throws synchronously', async () => {
  const { chainData } = provider({});
  await rejectsWith(chainData.network.getProtocolParams({ epoch: 'x' }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.get(123), 'INVALID_INPUT');
});
