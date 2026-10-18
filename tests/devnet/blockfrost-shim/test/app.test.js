'use strict';

const test = require('node:test');
const assert = require('node:assert/strict');
const http = require('node:http');

const bech32 = require('../src/bech32');
const { parseStakeAddress, parseDRepId, cip129DRepId } = require('../src/ids');
const { createHandler } = require('../src/app');

// From tests/govtool-backend/test_data.json: a CIP-105 DRep id and its key hash.
const DREP_CIP105 = 'drep1xadn8r2e4vcr7p74jnahhq6x4fjswp8umlyykxrfwje2707cqh9';
const DREP_HASH = '375b338d59ab303f07d594fb7b8346aa650704fcdfc84b186974b2af';
const STAKE_TEST = bech32.encode('stake_test', Buffer.from('e0' + DREP_HASH, 'hex'));

test('bech32 decodes a CIP-105 DRep id', () => {
  const decoded = bech32.decode(DREP_CIP105);
  assert.equal(decoded.prefix, 'drep');
  assert.equal(decoded.bytes.toString('hex'), DREP_HASH);
  assert.equal(bech32.encode('drep', decoded.bytes), DREP_CIP105);
});

test('bech32 rejects a bad checksum and mixed case', () => {
  assert.equal(bech32.decode(DREP_CIP105.slice(0, -1) + 'q'), null);
  assert.equal(bech32.decode('Drep' + DREP_CIP105.slice(4)), null);
});

test('DRep ids: CIP-105, CIP-129, script, hex', () => {
  assert.deepEqual(parseDRepId(DREP_CIP105), { raw: Buffer.from(DREP_HASH, 'hex'), hasScript: false });
  const cip129 = cip129DRepId(Buffer.from(DREP_HASH, 'hex'), false);
  assert.deepEqual(parseDRepId(cip129), { raw: Buffer.from(DREP_HASH, 'hex'), hasScript: false });
  const script129 = cip129DRepId(Buffer.from(DREP_HASH, 'hex'), true);
  assert.equal(parseDRepId(script129).hasScript, true);
  const script105 = bech32.encode('drep_script', Buffer.from(DREP_HASH, 'hex'));
  assert.equal(parseDRepId(script105).hasScript, true);
  assert.deepEqual(parseDRepId(DREP_HASH), { raw: Buffer.from(DREP_HASH, 'hex'), hasScript: false });
  assert.equal(parseDRepId('drep1nope'), null);
});

test('stake addresses: network must match the prefix', () => {
  assert.equal(parseStakeAddress(STAKE_TEST).hashRaw.toString('hex'), 'e0' + DREP_HASH);
  const wrongNet = bech32.encode('stake_test', Buffer.from('e1' + DREP_HASH, 'hex'));
  assert.equal(parseStakeAddress(wrongNet), null);
  assert.equal(parseStakeAddress(DREP_CIP105), null);
});

function fakeDb(rowsBySql) {
  const calls = [];
  return {
    calls,
    async query(sql, params) {
      calls.push(params);
      for (const [needle, rows] of rowsBySql) {
        if (sql.includes(needle)) return { rows };
      }
      return { rows: [] };
    },
  };
}

async function withServer(deps, fn) {
  const server = http.createServer(createHandler(deps));
  await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
  const base = `http://127.0.0.1:${server.address().port}`;
  try {
    await fn(base);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
}

test('tx submit forwards CBOR to Kuber and answers 200 with the tx id', async () => {
  const txId = 'ab'.repeat(32);
  let forwarded;
  const fetch = async (url, init) => {
    forwarded = { url, init };
    return new Response(JSON.stringify(txId), { status: 202 });
  };
  await withServer({ db: fakeDb([]), kuberUrl: 'http://kuber:8081', fetch }, async (base) => {
    const res = await fetch_(`${base}/api/v0/tx/submit`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/cbor', project_id: 'ignored' },
      body: Buffer.from('84a0a0f5f6', 'hex'),
    });
    assert.equal(res.status, 200);
    assert.equal(await res.json(), txId);
  });
  assert.equal(forwarded.url, 'http://kuber:8081/api/submit/tx');
  assert.equal(forwarded.init.headers['Content-Type'], 'application/cbor');
  assert.equal(Buffer.from(forwarded.init.body).toString('hex'), '84a0a0f5f6');
});

test('tx submit reports a node rejection as 400', async () => {
  const fetch = async () =>
    new Response(JSON.stringify({ message: 'BadInputsUTxO' }), { status: 400 });
  await withServer({ db: fakeDb([]), kuberUrl: 'http://kuber:8081', fetch }, async (base) => {
    const res = await fetch_(`${base}/api/v0/tx/submit`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/cbor' },
      body: Buffer.from('00', 'hex'),
    });
    assert.equal(res.status, 400);
    assert.match((await res.json()).message, /BadInputsUTxO/);
  });
});

test('tx submit requires application/cbor', async () => {
  await withServer({ db: fakeDb([]), kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/v0/tx/submit`, { method: 'POST', body: 'x' });
    assert.equal(res.status, 415);
  });
});

test('accounts: registered, unknown and malformed', async () => {
  const db = fakeDb([
    ['FROM stake_address', [{
      stake_address: STAKE_TEST, registered: true, registered_epoch: 3,
      pool_id: null, active_epoch: null,
      drep_raw: Buffer.from(DREP_HASH, 'hex'), drep_view: DREP_CIP105, drep_has_script: false,
    }]],
  ]);
  await withServer({ db, kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/api/v0/accounts/${STAKE_TEST}`);
    assert.equal(res.status, 200);
    const body = await res.json();
    assert.equal(body.registered, true);
    assert.equal(body.active, false);
    assert.equal(body.drep_id, cip129DRepId(Buffer.from(DREP_HASH, 'hex'), false));
    assert.equal(db.calls[0][0].toString('hex'), 'e0' + DREP_HASH);

    const bad = await fetch_(`${base}/api/v0/accounts/stake_test1xyz`);
    assert.equal(bad.status, 400);
  });
  await withServer({ db: fakeDb([]), kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/api/v0/accounts/${STAKE_TEST}`);
    assert.equal(res.status, 404);
    assert.equal((await res.json()).status_code, 404);
  });
});

test('dreps: retired flag and 404', async () => {
  const db = fakeDb([
    ['FROM drep_hash', [{
      raw: Buffer.from(DREP_HASH, 'hex'), has_script: false, active_epoch: 2,
      retired: true, amount: '5000000', active_until: 30, current_epoch: 10,
    }]],
  ]);
  await withServer({ db, kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/api/v0/governance/dreps/${DREP_CIP105}`);
    assert.equal(res.status, 200);
    const body = await res.json();
    assert.equal(body.retired, true);
    assert.equal(body.expired, false);
    assert.equal(body.hex, DREP_HASH);
    assert.deepEqual(db.calls[0], [Buffer.from(DREP_HASH, 'hex'), false]);
  });
  await withServer({ db: fakeDb([]), kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/api/v0/governance/dreps/${DREP_CIP105}`);
    assert.equal(res.status, 404);
  });
});

test('unknown routes answer 404 JSON; database errors do not leak', async () => {
  const db = { async query() { throw new Error('password authentication failed'); } };
  await withServer({ db, kuberUrl: 'http://kuber:8081' }, async (base) => {
    const res = await fetch_(`${base}/api/v0/blocks/latest`);
    assert.equal(res.status, 404);
    assert.equal((await res.json()).error, 'Not Found');

    const broken = await fetch_(`${base}/api/v0/accounts/${STAKE_TEST}`);
    assert.equal(broken.status, 500);
    assert.doesNotMatch(await broken.text(), /password/);
  });
});

// The global fetch, kept apart from the stubbed one passed to the handler.
const fetch_ = globalThis.fetch;
