/**
 * SurveysApi (CIP-179 definitions, SPEC.md §5.6), driven by a fake db.
 * Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import { MAX_PAYLOAD_HEX_LENGTH, SURVEY_DEFINITION_SQL } from '../dist/surveys.js';

/** A fake db answering by SQL text; records every call. */
function fakeDb(routes) {
  const calls = [];
  return {
    calls,
    async query(sql, params = []) {
      calls.push({ sql, params });
      for (const [match, answer] of routes) {
        if (sql === match) return typeof answer === 'function' ? answer(params) : answer;
      }
      throw new Error(`unexpected SQL: ${sql.slice(0, 80)}`);
    },
  };
}
const provider = (rows) => {
  const db = fakeDb([[SURVEY_DEFINITION_SQL, rows]]);
  return { db, surveys: createDbSyncProvider({ network: 'preview', db }).chainData.surveys };
};
async function rejectsWith(fn, code) {
  const p = fn();
  assert.ok(p instanceof Promise, 'must return a promise, never throw synchronously');
  await assert.rejects(p, (e) => e.code === code || assert.fail(`expected ${code}, got ${e.code}: ${e.message}`));
}

/*
 * Real-shape rows, from frontend/src/cip179/fixtures/dbSyncMetadata.json:
 * db-sync's tx_metadata.bytes for label 17, a singleton map {17: payload}.
 * Copied rather than imported so this package stays standalone.
 */
const DB_SYNC_ROW_CBOR_HEX =
  'a111820081a80005018200581c22222222222222222222222222222222222222222222222222222222026541756469740373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201';
const BATCHED_DB_SYNC_ROW_CBOR_HEX =
  'a111820082a80005018200581c22222222222222222222222222222222222222222222222222222222026541756469740373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201a80005018200581c2222222222222222222222222222222222222222222222222222222202665365636f6e640373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201';

const HASH = '1'.repeat(64);
const row = (hex) => ({ payload_cbor_hex: hex });

test('the namespace is served', () => {
  const { surveys } = provider([]);
  assert.equal(typeof surveys?.getDefinition, 'function');
});

test('the statement reads label 17 bytes by decoded hash, and sees duplicates', () => {
  const sql = SURVEY_DEFINITION_SQL.replace(/\s+/g, ' ');
  assert.match(sql, /encode\(tm\.bytes, 'hex'\) AS payload_cbor_hex/);
  assert.match(sql, /FROM tx_metadata tm JOIN tx ON tx\.id = tm\.tx_id/);
  assert.match(sql, /tx\.hash = decode\(\$1, 'hex'\)/);
  assert.match(sql, /tm\.key = 17/);
  assert.match(sql, /LIMIT 2$/);
  assert.doesNotMatch(sql, /json/i, 'never rebuilt from the json column');
});

test('getDefinition: a label-17 row is served as stored, with its hash and label', async () => {
  const { surveys, db } = provider([row(DB_SYNC_ROW_CBOR_HEX)]);
  const { data, meta } = await surveys.getDefinition(HASH);
  assert.deepEqual(data, { txHash: HASH, metadataLabel: 17, payloadCborHex: DB_SYNC_ROW_CBOR_HEX });
  assert.deepEqual(meta, { provider: 'dbsync', network: 'preview' });
  assert.deepEqual(db.calls, [{ sql: SURVEY_DEFINITION_SQL, params: [HASH] }]);
});

test('getDefinition: a batch row keeps every definition, not one index', async () => {
  const { surveys } = provider([row(BATCHED_DB_SYNC_ROW_CBOR_HEX)]);
  const { data } = await surveys.getDefinition(HASH);
  assert.equal(data.payloadCborHex, BATCHED_DB_SYNC_ROW_CBOR_HEX);
  // map(1) {17: [0, array(2) ...]}: the definitions array has both entries.
  assert.ok(data.payloadCborHex.startsWith('a111820082'));
});

test('getDefinition: an uppercase, padded hash is matched and reported lowercase', async () => {
  const hash = 'AbCd'.repeat(16);
  const { surveys, db } = provider([row(DB_SYNC_ROW_CBOR_HEX.toUpperCase())]);
  const { data } = await surveys.getDefinition(`  ${hash}\n`);
  assert.equal(data.txHash, hash.toLowerCase());
  assert.equal(data.payloadCborHex, DB_SYNC_ROW_CBOR_HEX);
  assert.deepEqual(db.calls[0].params, [hash.toLowerCase()]);
});

test('getDefinition: no row is null, not NOT_FOUND', async () => {
  const { surveys } = provider([]);
  assert.equal((await surveys.getDefinition(HASH)).data, null);
});

test('getDefinition rejects a malformed hash asynchronously, without querying', async () => {
  const { surveys, db } = provider([]);
  for (const bad of ['abcd', 'z'.repeat(64), `${HASH}00`, '', undefined, null, 42, "'; DROP TABLE tx; --"]) {
    await rejectsWith(() => surveys.getDefinition(bad), 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});

test('getDefinition: two label-17 rows for one transaction are INTERNAL', async () => {
  const { surveys } = provider([row(DB_SYNC_ROW_CBOR_HEX), row(DB_SYNC_ROW_CBOR_HEX)]);
  await rejectsWith(() => surveys.getDefinition(HASH), 'INTERNAL');
});

test('getDefinition: a row without bytes is INTERNAL, never null', async () => {
  for (const value of [null, undefined, '']) {
    const { surveys } = provider([row(value)]);
    await rejectsWith(() => surveys.getDefinition(HASH), 'INTERNAL');
  }
});

test('getDefinition: stored bytes that are not hex are INTERNAL', async () => {
  for (const value of [`${DB_SYNC_ROW_CBOR_HEX}0`, `${DB_SYNC_ROW_CBOR_HEX.slice(0, -2)}zz`, 'a111 820080', 42]) {
    const { surveys } = provider([row(value)]);
    await rejectsWith(() => surveys.getDefinition(HASH), 'INTERNAL');
  }
});

test('getDefinition: bytes that are not a singleton {17: payload} map are INTERNAL', async () => {
  const notSingleton = [
    DB_SYNC_ROW_CBOR_HEX.slice(4), // the bare payload, no label map
    `a212${DB_SYNC_ROW_CBOR_HEX.slice(4)}`, // map(2)
    `a110${DB_SYNC_ROW_CBOR_HEX.slice(4)}`, // map(1) under label 16
    `a1181d${DB_SYNC_ROW_CBOR_HEX.slice(4)}`, // map(1) under label 29
    'a111', // a key with no value
  ];
  for (const value of notSingleton) {
    const { surveys } = provider([row(value)]);
    await rejectsWith(() => surveys.getDefinition(HASH), 'INTERNAL');
  }
});

test('getDefinition: more than 1 MiB of stored bytes is INTERNAL', async () => {
  assert.equal(MAX_PAYLOAD_HEX_LENGTH, 2 * 1024 * 1024);
  const atLimit = `a11141${'00'.repeat((MAX_PAYLOAD_HEX_LENGTH - 6) / 2)}`;
  assert.equal(atLimit.length, MAX_PAYLOAD_HEX_LENGTH);
  const ok = provider([row(atLimit)]);
  assert.equal((await ok.surveys.getDefinition(HASH)).data.payloadCborHex.length, MAX_PAYLOAD_HEX_LENGTH);
  const over = provider([row(`${atLimit}00`)]);
  await rejectsWith(() => over.surveys.getDefinition(HASH), 'INTERNAL');
});

test('getDefinition: a driver failure surfaces as PROVIDER_UNAVAILABLE through the guard', async () => {
  const db = { query: async () => Promise.reject(Object.assign(new Error('connection refused'), { code: 'ECONNREFUSED' })) };
  const { surveys } = createDbSyncProvider({ network: 'preview', db }).chainData;
  await rejectsWith(() => surveys.getDefinition(HASH), 'PROVIDER_UNAVAILABLE');
});
