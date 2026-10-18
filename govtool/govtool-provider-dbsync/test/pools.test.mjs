import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import { encodeGovActionId, encodePoolId } from '../dist/ids.js';

const H = (n) => n.toString(16).padStart(2, '0').repeat(28);
const TX = (n) => n.toString(16).padStart(2, '0').repeat(32);

const poolRow = (n, over = {}) => ({
  hash: H(n), pledge: '5000000000', retired: false, meta_url: `https://pool${n}.example/meta.json`, meta_hash: 'cd'.repeat(32),
  snap_epoch: 900, voting_power: '1000000000000', active_stake: '990000000000', active_known: true, total_count: '42', ...over,
});

function setup(routes) {
  const calls = [];
  const db = {
    query: async (sql, params = []) => {
      calls.push({ sql, params });
      for (const [fragment, rows] of routes) if (sql.includes(fragment)) return typeof rows === 'function' ? rows(params, calls) : rows;
      throw new Error(`unrouted SQL: ${sql.slice(0, 80)}`);
    },
  };
  return { calls, pools: createDbSyncProvider({ network: 'preview', db }).chainData.governance.pools };
}

const POOLS = 'FROM pools p';
const POOL_ID = 'SELECT ph.id::text AS id';
const VOTES = 'FROM voting_procedure vp';

test('list: maps rows to SpoVoter, with no ticker or name', async () => {
  const { pools, calls } = setup([[POOLS, [poolRow(1)]]]);
  const res = await pools.list({ page: 2, size: 10 });
  assert.deepEqual(res.meta, { provider: 'dbsync', network: 'preview' });
  assert.deepEqual(res.data, {
    elements: [{
      role: 'spo', id: encodePoolId(H(1)), poolId: encodePoolId(H(1)), isScriptBased: false,
      anchor: { url: 'https://pool1.example/meta.json', dataHash: 'cd'.repeat(32) },
      votingPower: { amount: '1000000000000', basis: 'active', epoch: 900 },
      activeStake: '990000000000',
      pledge: '5000000000',
    }],
    total: 42,
  });
  assert.deepEqual(calls[0].params, [null, 10, 10, false], 'listing excludes retired pools and pages by offset');
});

test('list: absent from the snapshot is null voting power; active stake 0 or omitted by what is known', async () => {
  const { pools } = setup([[POOLS, [
    poolRow(1, { voting_power: null, active_stake: null, active_known: true, meta_url: null, meta_hash: null }),
    poolRow(2, { voting_power: null, active_stake: null, active_known: false }),
  ]]]);
  const [a, b] = (await pools.list({ page: 1, size: 10 })).data.elements;
  assert.equal(a.votingPower, null);
  assert.equal(a.activeStake, '0');
  assert.equal(a.anchor, null);
  assert.ok(!('activeStake' in b), 'unknown active stake is omitted, not zero');
});

test('list: no stake snapshot at all is refused rather than served as no power', async () => {
  const { pools } = setup([[POOLS, [poolRow(1, { snap_epoch: null, voting_power: null })]]]);
  await assert.rejects(pools.list({ page: 1, size: 10 }), (e) => e.code === 'CAPABILITY_UNSUPPORTED');
});

test('list: exact pool1 search binds the hash; other terms match nothing without a query', async () => {
  const { pools, calls } = setup([[POOLS, [poolRow(3, { total_count: '1' })]]]);
  const hit = await pools.list({ page: 1, size: 5, search: `  ${encodePoolId(H(3))} ` });
  assert.equal(hit.data.total, 1);
  assert.equal(calls[0].params[0], H(3));
  const miss = await pools.list({ page: 1, size: 5, search: 'TICKR' });
  assert.deepEqual(miss.data, { elements: [], total: 0 });
  assert.equal(calls.length, 1);
  await pools.list({ page: 1, size: 5, search: '   ' });
  assert.equal(calls[1].params[0], null, 'a blank search is no filter');
});

test('list: a page past the end is empty but keeps the total', async () => {
  const { pools, calls } = setup([[POOLS, (params) => (params[2] === 0 ? [poolRow(1)] : [])]]);
  const res = await pools.list({ page: 9, size: 10 });
  assert.deepEqual(res.data, { elements: [], total: 42 });
  assert.deepEqual(calls[1].params, [null, 1, 0, false]);
});

test('list: an empty first page has total 0', async () => {
  const { pools, calls } = setup([[POOLS, []]]);
  assert.deepEqual((await pools.list({ page: 1, size: 10 })).data, { elements: [], total: 0 });
  assert.equal(calls.length, 1);
});

test('list: bad paging rejects with INVALID_INPUT, never throws', async () => {
  const { pools, calls } = setup([[POOLS, []]]);
  for (const q of [{ page: 0, size: 10 }, { page: 1, size: 0 }, { page: 1, size: 100000 }, { page: 1.5, size: 1 }]) {
    let promise;
    assert.doesNotThrow(() => { promise = pools.list(q); });
    await assert.rejects(promise, (e) => e.code === 'INVALID_INPUT');
  }
  assert.equal(calls.length, 0);
});

test('get: includes retired pools, NOT_FOUND for unknown, INVALID_INPUT for a non-pool1 id', async () => {
  const { pools, calls } = setup([[POOLS, (params) => (params[0] === H(1) ? [poolRow(1, { retired: true, voting_power: null })] : [])]]);
  const got = await pools.get(encodePoolId(H(1)));
  assert.equal(got.data.votingPower, null);
  assert.deepEqual(calls[0].params, [H(1), 1, 0, true]);
  await assert.rejects(pools.get(encodePoolId(H(2))), (e) => e.code === 'NOT_FOUND');
  let promise;
  assert.doesNotThrow(() => { promise = pools.get('drep1qqqq'); });
  await assert.rejects(promise, (e) => e.code === 'INVALID_INPUT');
});

const voteRow = (over = {}) => ({
  vote: 'Yes', tx_hash: TX(9), index: 2, block_no: '100', epoch_no: 900, slot_no: '123456', time: new Date('2026-01-02T03:04:05Z'),
  anchor_url: null, anchor_hash: null, action_tx_hash: TX(1), action_index: 0, action_type: 'NewCommittee', total_count: '3', ...over,
});

test('listVotes: maps votes with the action they were cast on', async () => {
  const { pools, calls } = setup([
    [POOL_ID, [{ id: '77' }]],
    [VOTES, [voteRow(), voteRow({ vote: 'Abstain', action_type: 'InfoAction', anchor_url: 'https://r', anchor_hash: 'ef'.repeat(32) })]],
  ]);
  const id = encodePoolId(H(4));
  const res = await pools.listVotes(id, { page: 1, size: 2 });
  assert.equal(res.data.total, 3);
  const at = { epoch: 900, slot: 123456, block: 100, time: '2026-01-02T03:04:05.000Z' };
  assert.deepEqual(res.data.elements[0], {
    voter: { role: 'spo', id, isScriptBased: false },
    choice: 'yes',
    anchor: null,
    txRef: { txHash: TX(9), index: 2, block: 100 },
    at,
    action: { id: encodeGovActionId(TX(1), 0), type: 'UpdateCommittee' },
  });
  assert.equal(res.data.elements[1].choice, 'abstain');
  assert.equal(res.data.elements[1].action.type, 'InfoAction');
  assert.deepEqual(res.data.elements[1].anchor, { url: 'https://r', dataHash: 'ef'.repeat(32) });
  assert.deepEqual(calls[0].params, [H(4)]);
  assert.deepEqual(calls[1].params, ['77', 2, 0]);
});

test('listVotes: unknown pool is NOT_FOUND; bad id or paging is INVALID_INPUT', async () => {
  const { pools } = setup([[POOL_ID, []], [VOTES, []]]);
  await assert.rejects(pools.listVotes(encodePoolId(H(4)), { page: 1, size: 5 }), (e) => e.code === 'NOT_FOUND');
  await assert.rejects(pools.listVotes('pool1bad', { page: 1, size: 5 }), (e) => e.code === 'INVALID_INPUT');
  await assert.rejects(pools.listVotes(encodePoolId(H(4)), { page: 0, size: 5 }), (e) => e.code === 'INVALID_INPUT');
});

test('listVotes: a pool with no votes is an empty page with total 0', async () => {
  const { pools } = setup([[POOL_ID, [{ id: '77' }]], [VOTES, []]]);
  assert.deepEqual((await pools.listVotes(encodePoolId(H(4)), { page: 1, size: 5 })).data, { elements: [], total: 0 });
});

test('listVotes: an unrecognised db enum is an INTERNAL refusal', async () => {
  const { pools } = setup([[POOL_ID, [{ id: '77' }]], [VOTES, [voteRow({ vote: 'Maybe' })]]]);
  await assert.rejects(pools.listVotes(encodePoolId(H(4)), { page: 1, size: 5 }), (e) => e.code === 'INTERNAL');
});
