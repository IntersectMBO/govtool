import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const { createDbSyncProvider, capabilities } = require('../dist/index.js');
const { DREP_SORTS, DREP_FILTERS, DREP_SEARCH } = require('../dist/governance/dreps.js');
const { encodeDRepId, encodeGovActionId } = require('../dist/ids.js');
const { bech32 } = require('bech32');

const KEY_HASH = 'aa'.repeat(28);
const SCRIPT_HASH = 'bb'.repeat(28);
const TX = 'cd'.repeat(32);
const PROPOSAL_TX = 'ef'.repeat(32);
const CIP129 = encodeDRepId(KEY_HASH, false);
const CIP129_SCRIPT = encodeDRepId(SCRIPT_HASH, true);
/** Same credential, CIP-105 form: no header byte. */
const CIP105 = bech32.encode('drep', bech32.toWords(Buffer.from(KEY_HASH, 'hex')), 1023);

const stamp = (epoch, block) => ({ epoch_no: epoch, slot_no: String(epoch * 1000), block_no: String(block), time: new Date(Date.UTC(2025, 0, 1, 12)) });

const listRow = (over = {}) => ({
  id: '7',
  hash: KEY_HASH,
  has_script: false,
  status: 'active',
  expiry_known: true,
  active_until: 120,
  amount: '123456789012345678901',
  snap_epoch: 100,
  total_count: '42',
  ...over,
});

const detail = (event, over = {}) => ({
  event,
  drep_hash_id: '7',
  tx_hash: TX,
  cert_index: 1,
  deposit: event === 'registration' ? '500000000' : event === 'retirement' ? '-500000000' : null,
  anchor_url: 'https://example.com/drep.json',
  anchor_hash: '11'.repeat(32),
  current_anchor_url: 'https://example.com/current.json',
  current_anchor_hash: '22'.repeat(32),
  has_current_anchor: true,
  ...stamp(90, 5000),
  ...over,
});

/** A fake Db that answers by the statement's leading tag and records every call. */
function fakeDb(routes) {
  const calls = [];
  return {
    calls,
    query: async (sql, params = []) => {
      const tag = /\/\* (dreps:[a-z-]+) \*\//.exec(sql)?.[1];
      calls.push({ tag, sql, params });
      const route = routes[tag];
      if (route === undefined) throw new Error(`unexpected statement ${tag}`);
      return typeof route === 'function' ? route(params, sql) : route;
    },
  };
}

const provider = (routes) => {
  const db = fakeDb(routes);
  return { db, dreps: createDbSyncProvider({ network: 'preview', db }).chainData.governance.dreps };
};

const hydrated = (rows, extra = {}) => ({
  'dreps:list': rows,
  'dreps:details': [detail('registration'), detail('update', { tx_hash: 'ab'.repeat(32), ...stamp(95, 6000) })],
  'dreps:delegator-counts': [{ drep_hash_id: '7', delegators: '3' }],
  'dreps:activity': [{ drep_hash_id: '7', votable: '10', voted: '4' }],
  ...extra,
});

async function rejects(promise, code) {
  assert.ok(promise instanceof Promise, 'must return a promise, not throw');
  await assert.rejects(promise, (error) => error.code === code);
}

test('declares exactly what list honours, and system.ts carries it', () => {
  assert.deepEqual(DREP_SORTS, ['votingPower', 'registrationDate', 'random']);
  assert.deepEqual(DREP_FILTERS, ['status', 'kind']);
  assert.deepEqual(DREP_SEARCH, ['exactId']);
  const caps = capabilities();
  assert.deepEqual(caps.sorts.dreps, DREP_SORTS);
  assert.deepEqual(caps.filters.dreps, DREP_FILTERS);
  assert.deepEqual(caps.search, DREP_SEARCH);
});

test('listDelegators is absent: per-delegator active voting power is not in db-sync', () => {
  const { dreps } = provider({});
  assert.equal(dreps.listDelegators, undefined);
  for (const m of ['list', 'get', 'listVotes', 'listUpdateHistory', 'getCounts']) assert.equal(typeof dreps[m], 'function');
});

test('list maps a row into a contract DRep', async () => {
  const { dreps } = provider(hydrated([listRow()]));
  const { data, meta } = await dreps.list({ page: 1, size: 10, sort: 'votingPower' });
  assert.equal(meta.provider, 'dbsync');
  assert.equal(data.total, 42);
  const [d] = data.elements;
  assert.equal(d.id, CIP129);
  assert.equal(d.role, 'drep');
  assert.equal(d.isScriptBased, false);
  assert.equal(d.kind, 'drep');
  assert.deepEqual(d.anchor, { url: 'https://example.com/current.json', dataHash: '22'.repeat(32) });
  assert.equal(d.status, 'active');
  assert.equal(d.expiryEpoch, 120);
  assert.deepEqual(d.votingPower, { amount: '123456789012345678901', basis: 'active', epoch: 100 });
  assert.equal(d.delegatorCount, 3);
  assert.deepEqual(d.activity, { voted: 4, votable: 10 });
  assert.deepEqual(d.registration.latest, {
    txRef: { txHash: TX, index: 1, block: 5000 },
    at: { epoch: 90, slot: 90000, block: 5000, time: '2025-01-01T12:00:00.000Z' },
    anchor: { url: 'https://example.com/drep.json', dataHash: '11'.repeat(32) },
    deposit: '500000000',
  });
  assert.equal(d.registration.latestUpdate.deposit, null);
  assert.equal(d.registration.latestUpdate.at.epoch, 95);
  assert.equal(d.registration.retiredAt, null);
});

test('a DRep with no anchor is anonymous; a script DRep gets a script CIP-129 id', async () => {
  const { dreps } = provider(
    hydrated([listRow({ hash: SCRIPT_HASH, has_script: true })], {
      'dreps:details': [detail('registration', { has_current_anchor: false, current_anchor_url: null, current_anchor_hash: null })],
    }),
  );
  const [d] = (await dreps.list({ page: 1, size: 1, sort: 'votingPower' })).data.elements;
  assert.equal(d.id, CIP129_SCRIPT);
  assert.equal(d.isScriptBased, true);
  assert.equal(d.anchor, null);
  assert.equal(d.kind, 'anonymous');
});

test('expiryEpoch only when db-sync holds the ledger expiry; null power stays null', async () => {
  const { dreps } = provider(hydrated([listRow({ expiry_known: false, active_until: null, amount: null })]));
  const [d] = (await dreps.list({ page: 1, size: 1, sort: 'votingPower' })).data.elements;
  assert.equal('expiryEpoch' in d, false);
  assert.equal(d.votingPower, null);
});

test('a retired DRep carries retiredAt and no delegators', async () => {
  const { dreps } = provider(
    hydrated([listRow({ status: 'retired', expiry_known: false })], {
      'dreps:details': [detail('registration'), detail('retirement', { ...stamp(99, 7000) })],
      'dreps:delegator-counts': [],
    }),
  );
  const [d] = (await dreps.list({ page: 1, size: 1, sort: 'votingPower' })).data.elements;
  assert.equal(d.status, 'retired');
  assert.equal(d.registration.retiredAt.epoch, 99);
  assert.equal(d.delegatorCount, 0);
});

test('filters and sort are bound or whitelisted, never spliced from input', async () => {
  const { db, dreps } = provider(hydrated([listRow()]));
  await dreps.list({ page: 3, size: 5, sort: 'registrationDate', status: ['inactive', 'active'], kind: ['anonymous'] });
  const call = db.calls.find((c) => c.tag === 'dreps:list');
  assert.deepEqual(call.params, [['inactive', 'active'], ['anonymous'], null, null, 5, 10]);
  assert.match(call.sql, /ORDER BY reg_tx DESC, reg_cert DESC, id ASC/);
});

test('list refuses what it does not honour', async () => {
  const { dreps } = provider({});
  await rejects(dreps.list({ page: 1, size: 5, sort: 'activity' }), 'CAPABILITY_UNSUPPORTED');
  await rejects(dreps.list({ page: 1, size: 5, sort: "votingPower; DROP TABLE tx" }), 'INVALID_INPUT');
  await rejects(dreps.list({ page: 1, size: 5, status: ["active' OR 1=1 --"] }), 'INVALID_INPUT');
  await rejects(dreps.list({ page: 1, size: 5, kind: 'drep' }), 'INVALID_INPUT');
  await rejects(dreps.list({ page: 0, size: 5, sort: 'votingPower' }), 'INVALID_INPUT');
  await rejects(dreps.list({ page: 1, size: 5000, sort: 'votingPower' }), 'INVALID_INPUT');
  await rejects(dreps.list(undefined), 'INVALID_INPUT');
});

test('random is the default and is not paged', async () => {
  const { db, dreps } = provider(hydrated([listRow()]));
  await rejects(dreps.list({ page: 2, size: 20 }), 'INVALID_INPUT');
  await rejects(dreps.list({ page: 2, size: 20, sort: 'random' }), 'INVALID_INPUT');
  await dreps.list({ page: 1, size: 20 });
  assert.match(db.calls.find((c) => c.tag === 'dreps:list').sql, /ORDER BY random\(\)/);
});

test('search matches a CIP-129 id only; anything else is no match, not an error', async () => {
  const { db, dreps } = provider(hydrated([listRow()]));
  await dreps.list({ page: 1, size: 5, search: ` ${CIP129.toUpperCase()} ` });
  const params = db.calls.find((c) => c.tag === 'dreps:list').params;
  assert.deepEqual(params[2], Buffer.from(KEY_HASH, 'hex'));
  assert.equal(params[3], false);

  for (const term of [CIP105, 'Some Name', "'; DROP TABLE drep_hash; --"]) {
    const { db: quiet, dreps: d } = provider({});
    const { data } = await d.list({ page: 1, size: 5, search: term });
    assert.deepEqual(data, { elements: [], total: 0 });
    assert.equal(quiet.calls.length, 0);
  }
});

test('an empty page past the end still reports the total', async () => {
  const { db, dreps } = provider({ 'dreps:list': [], 'dreps:count-filtered': [{ total_count: '12' }] });
  const { data } = await dreps.list({ page: 9, size: 5, sort: 'votingPower' });
  assert.deepEqual(data, { elements: [], total: 12 });
  assert.deepEqual(db.calls.at(-1).params, [null, null, null, null]);
});

test('get decodes strictly: CIP-105 is INVALID_INPUT, unknown is NOT_FOUND', async () => {
  const { dreps } = provider({ 'dreps:list': [] });
  await rejects(dreps.get(CIP105), 'INVALID_INPUT');
  await rejects(dreps.get('drep1xyz'), 'INVALID_INPUT');
  await rejects(dreps.get(42), 'INVALID_INPUT');
  await rejects(dreps.get(CIP129), 'NOT_FOUND');
});

test('get returns the same shape as a listing row', async () => {
  const { db, dreps } = provider(hydrated([listRow()]));
  const { data } = await dreps.get(CIP129);
  assert.equal(data.id, CIP129);
  assert.deepEqual(db.calls[0].params.slice(2), [Buffer.from(KEY_HASH, 'hex'), false, 1, 0]);
});

const voteRow = (over = {}) => ({
  type: 'NewCommittee',
  proposal_tx_hash: PROPOSAL_TX,
  proposal_index: 2,
  vote: 'Abstain',
  vote_tx_hash: TX,
  vote_index: 0,
  anchor_url: null,
  anchor_hash: null,
  title: null,
  total_count: '2',
  ...stamp(101, 9000),
  ...over,
});

test('listVotes maps voted and not-voted rows', async () => {
  const notVoted = voteRow({ type: 'InfoAction', vote: null, vote_tx_hash: null, vote_index: null, title: 'Hello', block_no: null, epoch_no: null, slot_no: null, time: null });
  const { db, dreps } = provider({ 'dreps:resolve': [{ id: '7' }], 'dreps:votes': [voteRow(), notVoted] });
  const { data } = await dreps.listVotes(CIP129, { page: 1, size: 10, voted: undefined, sort: 'oldest' });
  assert.equal(data.total, 2);
  assert.deepEqual(data.elements[0], {
    voted: true,
    action: { id: encodeGovActionId(PROPOSAL_TX, 2), type: 'UpdateCommittee' },
    choice: 'abstain',
    anchor: null,
    txRef: { txHash: TX, index: 0, block: 9000 },
    at: { epoch: 101, slot: 101000, block: 9000, time: '2025-01-01T12:00:00.000Z' },
  });
  assert.deepEqual(data.elements[1], {
    voted: false,
    action: { id: encodeGovActionId(PROPOSAL_TX, 2), type: 'InfoAction', title: 'Hello' },
  });
  const call = db.calls.find((c) => c.tag === 'dreps:votes');
  assert.deepEqual(call.params, [['7'], null, 10, 0]);
  assert.match(call.sql, /ASC, r\.proposal_id ASC/);
});

test('listVotes validates, resolves and refuses', async () => {
  const { db, dreps } = provider({ 'dreps:resolve': [], 'dreps:votes': [], 'dreps:votes-count': [{ total_count: '3' }] });
  await rejects(dreps.listVotes(CIP129, { page: 1, size: 5 }), 'NOT_FOUND');
  await rejects(dreps.listVotes(CIP105, { page: 1, size: 5 }), 'INVALID_INPUT');
  await rejects(dreps.listVotes(CIP129, { page: 1, size: 5, sort: 'top' }), 'INVALID_INPUT');
  await rejects(dreps.listVotes(CIP129, { page: 1, size: 5, voted: 'yes' }), 'INVALID_INPUT');
  assert.ok(db.calls.every((c) => c.tag === 'dreps:resolve'));

  const ok = provider({ 'dreps:resolve': [{ id: '7' }], 'dreps:votes': [], 'dreps:votes-count': [{ total_count: '3' }] });
  const { data } = await ok.dreps.listVotes(CIP129, { page: 4, size: 5, voted: false });
  assert.deepEqual(data, { elements: [], total: 3 });
  assert.deepEqual(ok.db.calls.at(-1).params, [['7'], false]);
});

test('listUpdateHistory is a dated anchor feed', async () => {
  const row = { tx_hash: TX, cert_index: 0, deposit: null, anchor_url: 'https://x', anchor_hash: '33'.repeat(32), total_count: 1, ...stamp(80, 100) };
  const { db, dreps } = provider({ 'dreps:resolve': [{ id: '7' }], 'dreps:history': [row] });
  const { data } = await dreps.listUpdateHistory(CIP129, { page: 1, size: 10, sort: 'asc' });
  assert.deepEqual(data, {
    elements: [{
      txRef: { txHash: TX, index: 0, block: 100 },
      at: { epoch: 80, slot: 80000, block: 100, time: '2025-01-01T12:00:00.000Z' },
      anchor: { url: 'https://x', dataHash: '33'.repeat(32) },
      deposit: null,
    }],
    total: 1,
  });
  assert.match(db.calls.at(-1).sql, /ORDER BY r\.tx_id ASC, r\.cert_index ASC/);
  await rejects(dreps.listUpdateHistory(CIP129, { page: 1, size: 10, sort: 'sideways' }), 'INVALID_INPUT');
});

test('getCounts serves all three counters plus anonymous', async () => {
  const { dreps } = provider({ 'dreps:counts': [{ registered: '10', active: '6', inactive: '4', anonymous: '2' }] });
  const { data } = await dreps.getCounts();
  assert.deepEqual(data, { totalRegistered: 10, totalActive: 6, totalInactive: 4, anonymous: 2 });
});

test('database failures surface as ChainDataError rejections', async () => {
  const db = { query: async () => { throw Object.assign(new Error('boom'), { code: '57014' }); } };
  const dreps = createDbSyncProvider({ network: 'preview', db }).chainData.governance.dreps;
  await rejects(dreps.getCounts(), 'PROVIDER_TIMEOUT');
});
