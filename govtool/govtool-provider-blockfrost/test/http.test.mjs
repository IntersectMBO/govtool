/**
 * The HTTP client: the project id header, error mapping, retries, 429
 * handling, pacing, and that no error ever carries the project id.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { BlockfrostHttp, TokenBucket, retryAfterSeconds } from '../dist/http.js';
import { EPOCH, PROJECT_ID, fakeFetch, provider, rejectsWith } from './fake.mjs';

const tx = 'ab'.repeat(32);

test('sends the project id as the project_id header to the hosted URL', async () => {
  const { chainData, fetch } = provider({ '/epochs/latest': EPOCH, [`/txs/${tx}`]: { hash: tx, block_height: 1, block_time: EPOCH.start_time, slot: 1, index: 0 } });
  await chainData.transactions.get(tx);
  assert.equal(fetch.calls[0].headers.project_id, PROJECT_ID);
  assert.match(fetch.calls[0].url, /^https:\/\/cardano-mainnet\.blockfrost\.io\/api\/v0\//);
});

test('a preprod provider talks to the preprod host; baseUrl overrides it', async () => {
  const f = fakeFetch({ '/health': { is_healthy: true } });
  await new BlockfrostHttp({ network: 'preprod', fetch: f, rateLimit: null }).get('/health');
  assert.match(f.calls[0].url, /^https:\/\/cardano-preprod\.blockfrost\.io\/api\/v0\/health$/);
  const g = fakeFetch({ '/health': { is_healthy: true } });
  await new BlockfrostHttp({ network: 'mainnet', baseUrl: 'https://ryo.example/api/v0/', fetch: g }).get('/health');
  assert.equal(g.calls[0].url, 'https://ryo.example/api/v0/health');
  assert.equal(g.calls[0].headers.project_id, undefined, 'no header without a project id');
  assert.throws(() => new BlockfrostHttp({ network: 'sanchonet' }), /pass baseUrl/);
});

test('404 is NOT_FOUND; 400 is INVALID_INPUT with only Blockfrost\'s own message', async () => {
  const { chainData } = provider({
    '/governance/committee': { status: 400, body: { status_code: 400, error: 'Bad Request', message: 'Invalid path.' } },
  });
  const e = await rejectsWith(chainData.governance.committee.getCommittee(), 'INVALID_INPUT');
  assert.deepEqual(e.details, { path: '/governance/committee', status: 400, message: 'Invalid path.' });
  const { chainData: c2 } = provider({});
  await rejectsWith(c2.governance.committee.getCommittee(), 'NOT_FOUND');
});

test('403 (bad key) is PROVIDER_UNAVAILABLE and the error never contains the project id', async () => {
  const body = { status_code: 403, error: 'Forbidden', message: `Invalid project token. ${PROJECT_ID}` };
  const { chainData } = provider({ '/governance/committee': { status: 403, body } });
  const e = await rejectsWith(chainData.governance.committee.getCommittee(), 'PROVIDER_UNAVAILABLE');
  assert.equal(e.message, 'Blockfrost refused the project credentials');
  assert.equal(e.cause, undefined);
  const everything = JSON.stringify({ message: e.message, details: e.details, stack: e.stack, e });
  assert.ok(!everything.includes(PROJECT_ID), 'project id leaked into the error');
});

test('no error path carries the project id: transport failure, timeout, 5xx', async () => {
  const leaky = new Error(`connect ECONNREFUSED headers=${PROJECT_ID}`);
  for (const [fetch, code] of [
    [async () => { throw leaky; }, 'PROVIDER_UNAVAILABLE'],
    [async () => { throw Object.assign(new Error('timed out'), { name: 'TimeoutError' }); }, 'PROVIDER_TIMEOUT'],
    [async () => new Response(`upstream ${PROJECT_ID}`, { status: 502 }), 'PROVIDER_UNAVAILABLE'],
    [async () => new Response('{}', { status: 504 }), 'PROVIDER_TIMEOUT'],
  ]) {
    const http = new BlockfrostHttp({ network: 'mainnet', projectId: PROJECT_ID, fetch, sleep: async () => {}, rateLimit: null, maxRetries: 1 });
    const e = await rejectsWith(http.get('/health'), code);
    assert.equal(e.cause, undefined);
    assert.ok(!JSON.stringify({ m: e.message, d: e.details, s: e.stack }).includes(PROJECT_ID), `${code} leaked the project id`);
  }
});

test('429 honours Retry-After, then succeeds', async () => {
  let n = 0;
  const waits = [];
  const fetch = async () =>
    ++n === 1
      ? new Response('{"status_code":429,"error":"Project Over Limit","message":"Usage is over limit."}', { status: 429, headers: { 'retry-after': '2' } })
      : new Response('{"is_healthy":true}', { status: 200 });
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async (ms) => void waits.push(ms), rateLimit: null });
  assert.deepEqual(await http.get('/health'), { is_healthy: true });
  assert.deepEqual(waits, [2000]);
});

test('429 exhausted is PROVIDER_RATE_LIMITED, retryable, with retryAfterSeconds', async () => {
  let n = 0;
  const fetch = async () => (n++, new Response('{}', { status: 429, headers: { 'retry-after': '5' } }));
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async () => {}, rateLimit: null, maxRetries: 2 });
  const e = await rejectsWith(http.get('/health'), 'PROVIDER_RATE_LIMITED');
  assert.equal(e.retryable, true);
  assert.equal(e.retryAfterSeconds, 5);
  assert.equal(n, 3, 'one try plus two retries');
});

test('a Retry-After longer than the cap is thrown at once, not slept on', async () => {
  let n = 0;
  const fetch = async () => (n++, new Response('{}', { status: 429, headers: { 'retry-after': '3600' } }));
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async () => assert.fail('slept'), rateLimit: null });
  const e = await rejectsWith(http.get('/health'), 'PROVIDER_RATE_LIMITED');
  assert.equal(e.retryAfterSeconds, 3600);
  assert.equal(n, 1);
});

test('402 (daily quota spent) and 418 (banned) are rate limits and not retried', async () => {
  for (const status of [402, 418]) {
    let n = 0;
    const fetch = async () => (n++, new Response('{}', { status }));
    const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async () => {}, rateLimit: null });
    const e = await rejectsWith(http.get('/health'), 'PROVIDER_RATE_LIMITED');
    assert.equal(e.retryable, false);
    assert.equal(n, 1);
  }
});

test('5xx is retried with backoff, then PROVIDER_UNAVAILABLE', async () => {
  let n = 0;
  const waits = [];
  const fetch = async () => (n++, new Response('{}', { status: 500 }));
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async (ms) => void waits.push(ms), rateLimit: null, maxRetries: 3 });
  const e = await rejectsWith(http.get('/health'), 'PROVIDER_UNAVAILABLE');
  assert.equal(e.retryable, true);
  assert.equal(n, 4);
  assert.deepEqual(waits, [250, 500, 1000]);
});

test('a timeout is PROVIDER_TIMEOUT (the request is aborted by the signal)', async () => {
  const fetch = (_url, init) =>
    new Promise((_resolve, reject) => init.signal.addEventListener('abort', () => reject(init.signal.reason)));
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, sleep: async () => {}, rateLimit: null, timeoutMs: 20, maxRetries: 0 });
  // AbortSignal.timeout's timer does not hold the event loop open; a real socket would.
  const keepAlive = setTimeout(() => {}, 5000);
  const e = await rejectsWith(http.get('/health'), 'PROVIDER_TIMEOUT');
  clearTimeout(keepAlive);
  assert.match(e.message, /20 ms/);
});

test('a body that is not JSON is INTERNAL', async () => {
  const http = new BlockfrostHttp({ network: 'mainnet', fetch: async () => new Response('<html>', { status: 200 }), rateLimit: null });
  await rejectsWith(http.get('/health'), 'INTERNAL');
});

test('never more than maxConcurrency requests in flight', async () => {
  let inFlight = 0;
  let peak = 0;
  const fetch = async () => {
    peak = Math.max(peak, ++inFlight);
    await new Promise((r) => setTimeout(r, 5));
    inFlight--;
    return new Response('[]', { status: 200 });
  };
  const http = new BlockfrostHttp({ network: 'mainnet', fetch, rateLimit: null, maxConcurrency: 3 });
  await Promise.all(Array.from({ length: 12 }, () => http.get('/x')));
  assert.equal(peak, 3);
});

test('getAll reads until a short page and no further than the batch', async () => {
  const rows = Array.from({ length: 250 }, (_, i) => ({ i }));
  const f = fakeFetch({ '/rows': (url) => rows.slice((Number(url.searchParams.get('page')) - 1) * 100, Number(url.searchParams.get('page')) * 100) });
  const http = new BlockfrostHttp({ network: 'mainnet', fetch: f, rateLimit: null });
  const all = await http.getAll('/rows', { batch: 2 });
  assert.equal(all.length, 250);
  assert.deepEqual(all.map((r) => r.i), rows.map((r) => r.i));
  assert.equal(f.calls.length, 4, 'pages 1-2, then 3-4 (4 is the one wasted read)');
  assert.ok(f.calls.every((c) => c.url.includes('count=100')));
});

test('Retry-After parses seconds and HTTP dates', () => {
  assert.equal(retryAfterSeconds('7'), 7);
  assert.equal(retryAfterSeconds(new Date(10_000).toUTCString(), 0), 10);
  assert.equal(retryAfterSeconds(null), undefined);
  assert.equal(retryAfterSeconds('soon'), undefined);
});

test('the token bucket bursts, then paces at its rate, and a 429 drains it', async () => {
  let now = 0;
  const waits = [];
  const bucket = new TokenBucket(10, 3, () => now, async (ms) => {
    waits.push(ms);
    now += ms;
  });
  for (let i = 0; i < 3; i++) await bucket.take();
  assert.deepEqual(waits, [], 'the burst is free');
  await bucket.take();
  assert.deepEqual(waits, [100], 'then one token per 100 ms');
  now += 1000;
  bucket.drain();
  await bucket.take();
  assert.equal(waits.length, 2, 'after a drain the next request waits');
});
