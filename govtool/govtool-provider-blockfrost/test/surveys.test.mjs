/**
 * surveys.getDefinition over `/txs/{hash}/metadata/cbor`: input validation,
 * missing versus failing, and that the label-17 value comes back as the
 * singleton map `{17: payload}` whichever form Blockfrost served, with CBOR
 * types and 64-bit integers intact. Results are decoded here by a reader
 * independent of the provider and of CSL.
 */
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { test } from 'node:test';

import { CMap, bytes, cbor, provider, rejectsWith } from './fake.mjs';

const FIX = JSON.parse(readFileSync(new URL('./fixtures/cip179.json', import.meta.url), 'utf8'));
const tx = 'ab'.repeat(32);
const path = `/txs/${tx}/metadata/cbor`;
const row = (metadata, label = '17') => ({ label, metadata, cbor_metadata: '\\xnot-cbor-never-read' });
const serve = (rows) => provider({ [path]: rows });
const definition = async (rows) => (await serve(rows).chainData.surveys.getDefinition(tx)).data;

/* -- an independent CBOR reader: integers as bigint, maps as entry lists ------ */

function decode(hex) {
  const buf = Buffer.from(hex, 'hex');
  let pos = 0;
  const arg = (info) => {
    if (info < 24) return BigInt(info);
    const n = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
    if (n === undefined) throw new Error(`unsupported additional info ${info}`);
    let v = 0n;
    for (let i = 0; i < n; i++) v = (v << 8n) | BigInt(buf[pos++]);
    return v;
  };
  const item = () => {
    const h = buf[pos++];
    const major = h >> 5;
    const info = h & 31;
    if (info === 31) {
      if (major !== 4 && major !== 5) throw new Error('unsupported indefinite item');
      const out = [];
      while (buf[pos] !== 0xff) out.push(major === 4 ? item() : [item(), item()]);
      pos++;
      return major === 4 ? out : { map: out };
    }
    const n = arg(info);
    switch (major) {
      case 0:
        return n;
      case 1:
        return -1n - n;
      case 2:
      case 3: {
        const raw = buf.subarray(pos, pos + Number(n));
        pos += Number(n);
        return major === 2 ? { bytes: raw.toString('hex') } : raw.toString('utf8');
      }
      case 4:
        return Array.from({ length: Number(n) }, item);
      case 5:
        return { map: Array.from({ length: Number(n) }, () => [item(), item()]) };
      default:
        throw new Error(`unsupported major type ${major}`);
    }
  };
  const value = item();
  assert.equal(pos, buf.length, 'trailing bytes in the result');
  return value;
}

/** The decoded `{17: payload}`, asserting it is exactly that. */
function label17(hex) {
  const top = decode(hex);
  assert.ok(top.map, 'result is a map');
  assert.equal(top.map.length, 1, 'result is a singleton map');
  assert.equal(top.map[0][0], 17n, 'its key is the integer 17');
  return top.map[0][1];
}

/* -- validation --------------------------------------------------------------- */

test('a valid hash reads /txs/{hash}/metadata/cbor and answers the label-17 map', async () => {
  const { chainData, fetch } = serve([row('a10001', '0'), row(FIX.dbSyncRowCborHex)]);
  const { data, meta } = await chainData.surveys.getDefinition(tx);
  assert.deepEqual(data, { txHash: tx, metadataLabel: 17, payloadCborHex: FIX.dbSyncRowCborHex });
  assert.deepEqual(meta, { provider: 'blockfrost', network: 'mainnet' });
  assert.deepEqual(fetch.calls.map((c) => c.path), [path]);
});

test('an uppercase, padded hash is matched and reported lowercase', async () => {
  const { chainData, fetch } = serve([row(FIX.dbSyncRowCborHex)]);
  const { data } = await chainData.surveys.getDefinition(`  ${tx.toUpperCase()} `);
  assert.equal(data.txHash, tx);
  assert.equal(fetch.calls[0].path, path);
});

test('a malformed hash rejects asynchronously with INVALID_INPUT and reads nothing', async () => {
  const { chainData, fetch } = serve([row(FIX.dbSyncRowCborHex)]);
  for (const bad of ['', 'xyz', 'ab'.repeat(31), 'ab'.repeat(33), 'zz'.repeat(32), undefined, null, 42, {}]) {
    await rejectsWith(chainData.surveys.getDefinition(bad), 'INVALID_INPUT');
  }
  assert.equal(fetch.calls.length, 0);
});

/* -- missing is null ---------------------------------------------------------- */

test('an unknown transaction (404) is null', async () => {
  const { chainData } = provider({});
  assert.equal((await chainData.surveys.getDefinition(tx)).data, null);
});

test('no metadata rows is null', async () => {
  assert.equal(await definition([]), null);
});

test('metadata without label 17 is null', async () => {
  assert.equal(await definition([row('a10001', '0'), row('a1186f02', '111'), row(FIX.dbSyncRowCborHex.replace(/^a111/, 'a112'), '18')]), null);
});

/* -- normalization ------------------------------------------------------------ */

test('the bare payload is wrapped into {17: payload}', async () => {
  const data = await definition([row(FIX.payloadOnlyCborHex)]);
  assert.deepEqual(label17(data.payloadCborHex), decode(FIX.payloadOnlyCborHex));
  assert.equal(data.payloadCborHex, FIX.dbSyncRowCborHex);
});

test('an already wrapped map is not wrapped again', async () => {
  const data = await definition([row(FIX.dbSyncRowCborHex)]);
  const value = label17(data.payloadCborHex);
  assert.ok(Array.isArray(value), 'the value under 17 is the payload, not another map');
  assert.deepEqual(value, decode(FIX.payloadOnlyCborHex));
});

test('inner and wrapped inputs decode to the same value', async () => {
  const inner = await definition([row(FIX.payloadOnlyCborHex)]);
  const wrapped = await definition([row(FIX.dbSyncRowCborHex)]);
  assert.deepEqual(decode(inner.payloadCborHex), decode(wrapped.payloadCborHex));
});

test('a batch keeps every definition, not one selected index', async () => {
  const data = await definition([row(FIX.batchedDbSyncRowCborHex)]);
  const value = label17(data.payloadCborHex);
  assert.deepEqual(value, label17(FIX.batchedDbSyncRowCborHex));
  assert.equal(value[1].length, 2, 'two definitions');
});

test('byte strings, text and integer keys of the fixture survive', async () => {
  const [version, [first]] = label17((await definition([row(FIX.payloadOnlyCborHex)])).payloadCborHex);
  assert.equal(version, 0n);
  const fields = new Map(first.map.map(([k, v]) => [k, v]));
  assert.ok([...fields.keys()].every((k) => typeof k === 'bigint'), 'integer map keys');
  assert.deepEqual(fields.get(1n), [0n, { bytes: '22'.repeat(28) }]);
  assert.equal(fields.get(2n), 'Audit');
  assert.equal(fields.get(3n), 'Independent fixture');
  assert.equal(fields.get(5n), 500n);
});

test('64-bit signed integers, bytes and text keep their CBOR types, inner or wrapped', async () => {
  const payload = [
    5,
    new CMap([
      [0, bytes('00ff10')],
      [1, 'text'],
      [2, -18446744073709551616n],
      [3, 18446744073709551615n],
      [4, -9007199254740993n],
      ['k', -1],
    ]),
  ];
  const inner = cbor(payload).toString('hex');
  const wrapped = cbor(new CMap([[17, payload]])).toString('hex');
  const a = (await definition([row(inner)])).payloadCborHex;
  const b = (await definition([row(wrapped)])).payloadCborHex;
  assert.deepEqual(decode(a), decode(b));
  const [five, { map }] = label17(a);
  assert.equal(five, 5n);
  assert.deepEqual(map, [
    [0n, { bytes: '00ff10' }],
    [1n, 'text'],
    [2n, -18446744073709551616n],
    [3n, 18446744073709551615n],
    [4n, -9007199254740993n],
    ['k', -1n],
  ]);
});

/* -- corrupt source data ------------------------------------------------------- */

test('null metadata bytes are PROVIDER_UNAVAILABLE, retryable, never null', async () => {
  const e = await rejectsWith(serve([row(null)]).chainData.surveys.getDefinition(tx), 'PROVIDER_UNAVAILABLE');
  assert.equal(e.retryable, true);
});

test('malformed CBOR is INTERNAL', async () => {
  for (const bad of [
    '',
    'zz',
    'a11',
    FIX.payloadOnlyCborHex.slice(0, -2), // truncated
    `${FIX.payloadOnlyCborHex}00`, // trailing bytes
    `${FIX.dbSyncRowCborHex}ff`,
    'f93c00', // a float: CBOR, but not metadata
    'a1f93c0001', // a map keyed by a float
  ]) {
    await rejectsWith(serve([row(bad)]).chainData.surveys.getDefinition(tx), 'INTERNAL');
  }
});

test('metadata over 1 MiB is INTERNAL', async () => {
  const big = `5a00100001${'00'.repeat(1024 * 1024 + 1)}`;
  await rejectsWith(serve([row(big)]).chainData.surveys.getDefinition(tx), 'INTERNAL');
});

test('more than one label-17 row is INTERNAL', async () => {
  await rejectsWith(serve([row(FIX.dbSyncRowCborHex), row(FIX.dbSyncRowCborHex)]).chainData.surveys.getDefinition(tx), 'INTERNAL');
});

test('a metadata map keyed by any other label set is INTERNAL', async () => {
  const value = FIX.payloadOnlyCborHex;
  for (const map of [`a112${value}`, `a211${value}1201`, 'a0']) {
    await rejectsWith(serve([row(map)]).chainData.surveys.getDefinition(tx), 'INTERNAL');
  }
});

test('a body that is not a list is INTERNAL', async () => {
  await rejectsWith(serve({ label: '17', metadata: FIX.dbSyncRowCborHex }).chainData.surveys.getDefinition(tx), 'INTERNAL');
});

/* -- transport failures keep their codes ----------------------------------------- */

test('authentication, quota, rate limit, 5xx and timeout are errors, not null', async () => {
  for (const [answer, code] of [
    [{ status: 403, body: { status_code: 403, error: 'Forbidden', message: 'Invalid project token.' } }, 'PROVIDER_UNAVAILABLE'],
    [{ status: 402, body: { status_code: 402, error: 'Project Over Limit', message: 'Usage is over limit.' } }, 'PROVIDER_RATE_LIMITED'],
    [{ status: 429, body: { status_code: 429, error: 'Project Over Limit', message: 'Usage is over limit.' } }, 'PROVIDER_RATE_LIMITED'],
    [{ status: 418, body: { status_code: 418, error: 'Requesting IP auto-banned' } }, 'PROVIDER_RATE_LIMITED'],
    [{ status: 500, body: { status_code: 500, error: 'Internal Server Error' } }, 'PROVIDER_UNAVAILABLE'],
    [{ status: 504, body: '' }, 'PROVIDER_TIMEOUT'],
  ]) {
    const { chainData } = provider({ [path]: answer }, { maxRetries: 1 });
    await rejectsWith(chainData.surveys.getDefinition(tx), code);
  }
  const timeout = async () => {
    throw Object.assign(new Error('timed out'), { name: 'TimeoutError' });
  };
  const { chainData } = provider({}, { fetch: timeout, maxRetries: 0 });
  await rejectsWith(chainData.surveys.getDefinition(tx), 'PROVIDER_TIMEOUT');
});
