/**
 * Mapping logic of the accounts area, driven by a fake db.
 * Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import {
  ACCOUNT_SQL,
  DELEGATION_SQL,
  HISTORY_COUNT_SQL,
  HISTORY_SQL,
  POOL_DELEGATION_SQL,
  VOTING_POWER_SQL,
  resolveAddress,
} from '../dist/accounts.js';
import { decodeDRepId, decodePoolId, encodeStakeAddress } from '../dist/ids.js';

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
const provider = (routes, network = 'preview') => {
  const db = fakeDb(routes);
  return { db, accounts: createDbSyncProvider({ network, db }).chainData.accounts };
};
async function rejectsWith(fn, code) {
  const p = fn();
  assert.ok(p instanceof Promise, 'must return a promise, never throw synchronously');
  await assert.rejects(p, (e) => e.code === code || assert.fail(`expected ${code}, got ${e.code}: ${e.message}`));
}

const KEY_HASH = '11'.repeat(28);
const SCRIPT_HASH = '22'.repeat(28);
const DREP_HASH = '33'.repeat(28);
const POOL_HASH = '44'.repeat(28);
const TX = 'cd'.repeat(32);
const ADDR = encodeStakeAddress(KEY_HASH, false, 'preview');
const SCRIPT_ADDR = encodeStakeAddress(SCRIPT_HASH, true, 'preview');
const block = { epoch_no: 1400, slot_no: '120000000', block_no: '4600000', time: new Date('2026-08-01T00:00:00Z') };

/* -- address ---------------------------------------------------------------- */

test('resolveAddress binds the 29-byte reward address and canonicalises', () => {
  const r = resolveAddress(ADDR.toUpperCase(), 'preview');
  assert.equal(r.stakeAddress, ADDR);
  assert.equal(r.hash, KEY_HASH);
  assert.equal(r.hashRaw, `e0${KEY_HASH}`);
  assert.equal(r.isScript, false);
  const s = resolveAddress(SCRIPT_ADDR, 'preview');
  assert.equal(s.hashRaw, `f0${SCRIPT_HASH}`);
  assert.equal(s.isScript, true);
  const m = resolveAddress(encodeStakeAddress(KEY_HASH, false, 'mainnet'), 'mainnet');
  assert.equal(m.hashRaw, `e1${KEY_HASH}`);
});

test('every method rejects a wrong-network, malformed or non-string address without querying', async () => {
  const { accounts, db } = provider([]);
  const mainnet = encodeStakeAddress(KEY_HASH, false, 'mainnet');
  const drepId = 'drep1yfaaaaa270yjt6tu5skndugekprf5ykv5jshanl0c6gqx5qpstskf';
  for (const bad of [mainnet, 'garbage', '', drepId, 42, undefined, "stake_test1'; DROP TABLE tx;--"]) {
    for (const m of ['get', 'getDelegation', 'getPoolDelegation', 'getVotingPower']) {
      await rejectsWith(() => accounts[m](bad), 'INVALID_INPUT');
    }
    await rejectsWith(() => accounts.listDelegationHistory(bad, { page: 1, size: 5 }), 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});

/* -- get -------------------------------------------------------------------- */

test('get: registered when the newest certificate is a registration', async () => {
  const { accounts, db } = provider([[ACCOUNT_SQL, [{ id: '7', last_event: 'reg' }]]]);
  const { data } = await accounts.get(ADDR);
  assert.deepEqual(data, { stakeAddress: ADDR, stakeKeyHash: KEY_HASH, isRegistered: true, isScriptBased: false });
  assert.deepEqual(db.calls[0].params, [`e0${KEY_HASH}`]);
  assert.ok(!('balance' in data), 'balance is not served');
});

test('get: deregistered and never-seen accounts are known-unregistered, not NOT_FOUND', async () => {
  const { accounts } = provider([[ACCOUNT_SQL, (p) => (p[0].startsWith('e0') ? [{ id: '7', last_event: 'dereg' }] : [])]]);
  assert.equal((await accounts.get(ADDR)).data.isRegistered, false);
  const unseen = await accounts.get(SCRIPT_ADDR);
  assert.deepEqual(unseen.data, { stakeAddress: SCRIPT_ADDR, stakeKeyHash: SCRIPT_HASH, isRegistered: false, isScriptBased: true });
});

/* -- getDelegation ---------------------------------------------------------- */

const delegationRow = (over = {}) => ({
  drep_hash: DREP_HASH,
  drep_view: 'drep1whatever',
  has_script: false,
  tx_hash: TX,
  cert_index: 1,
  ...block,
  account_deregistered: false,
  drep_retired: false,
  ...over,
});

test('getDelegation: a DRep as a VoterRef with a CIP-129 id', async () => {
  const { accounts } = provider([[DELEGATION_SQL, [delegationRow()]]]);
  const { data } = await accounts.getDelegation(ADDR);
  assert.equal(data.target.kind, 'drep');
  assert.equal(data.target.drep.role, 'drep');
  assert.equal(data.target.drep.isScriptBased, false);
  assert.deepEqual(decodeDRepId(data.target.drep.id), { hash: DREP_HASH, isScript: false });
  assert.deepEqual(data.txRef, { txHash: TX, index: 1, block: 4600000 });
  assert.deepEqual(data.since, { epoch: 1400, slot: 120000000, block: 4600000, time: '2026-08-01T00:00:00.000Z' });
});

test('getDelegation: a script DRep carries the script header', async () => {
  const { accounts } = provider([[DELEGATION_SQL, [delegationRow({ has_script: true })]]]);
  const { data } = await accounts.getDelegation(ADDR);
  assert.deepEqual(decodeDRepId(data.target.drep.id), { hash: DREP_HASH, isScript: true });
  assert.equal(data.target.drep.isScriptBased, true);
});

test('getDelegation: the predefined targets are not DReps', async () => {
  for (const [view, target] of [['drep_always_abstain', 'alwaysAbstain'], ['drep_always_no_confidence', 'alwaysNoConfidence']]) {
    const { accounts } = provider([[DELEGATION_SQL, [delegationRow({ drep_hash: null, drep_view: view })]]]);
    const { data } = await accounts.getDelegation(ADDR);
    assert.deepEqual(data.target, { kind: 'predefined', target });
  }
});

test('getDelegation: null when never delegated, deregistered since, or the DRep retired since', async () => {
  for (const rows of [[], [delegationRow({ account_deregistered: true })], [delegationRow({ drep_retired: true })]]) {
    const { accounts } = provider([[DELEGATION_SQL, rows]]);
    assert.equal((await accounts.getDelegation(ADDR)).data, null);
  }
});

test('getDelegation: an unrecognised credential-less DRep row is refused', async () => {
  const { accounts } = provider([[DELEGATION_SQL, [delegationRow({ drep_hash: null, drep_view: 'something_new' })]]]);
  await rejectsWith(() => accounts.getDelegation(ADDR), 'INTERNAL');
});

/* -- getPoolDelegation ------------------------------------------------------ */

test('getPoolDelegation: pool1 id, or null when cleared', async () => {
  const row = { pool_hash: POOL_HASH, tx_hash: TX, cert_index: 0, ...block, account_deregistered: false, pool_retired: false };
  const { accounts } = provider([[POOL_DELEGATION_SQL, [row]]]);
  const { data } = await accounts.getPoolDelegation(ADDR);
  assert.equal(decodePoolId(data.poolId), POOL_HASH);
  assert.deepEqual(data.txRef, { txHash: TX, index: 0, block: 4600000 });
  assert.equal(data.since.epoch, 1400);
  for (const rows of [[], [{ ...row, pool_retired: true }], [{ ...row, account_deregistered: true }]]) {
    const { accounts: a } = provider([[POOL_DELEGATION_SQL, rows]]);
    assert.equal((await a.getPoolDelegation(ADDR)).data, null);
  }
});

/* -- getVotingPower --------------------------------------------------------- */

const vpRow = (over = {}) => ({
  id: '7',
  epoch_no: 1430,
  last_event: 'reg',
  utxo: '9000000000000000001',
  rewards: '500',
  rewards_rest: '25',
  withdrawn: '100',
  proposal_deposits: '0',
  ...over,
});

test('getVotingPower: utxo + reward balance + held deposits, exact beyond 2^53', async () => {
  const { accounts } = provider([[VOTING_POWER_SQL, [vpRow({ proposal_deposits: '1000000000' })]]]);
  const { data } = await accounts.getVotingPower(ADDR);
  assert.deepEqual(data, { amount: '9000000001000000426', basis: 'live', epoch: 1430 });
});

test('getVotingPower: a registered empty account is a real "0"', async () => {
  const { accounts } = provider([[VOTING_POWER_SQL, [vpRow({ utxo: '0', rewards: '0', rewards_rest: '0', withdrawn: '0' })]]]);
  assert.equal((await accounts.getVotingPower(ADDR)).data.amount, '0');
});

test('getVotingPower: null, never 0, when the ledger counts nothing (unregistered or unseen)', async () => {
  for (const row of [vpRow({ last_event: 'dereg' }), vpRow({ last_event: null }), vpRow({ id: null, last_event: null, utxo: '0' })]) {
    const { accounts } = provider([[VOTING_POWER_SQL, [row]]]);
    assert.equal((await accounts.getVotingPower(ADDR)).data, null);
  }
});

test('getVotingPower: incomplete reward history is refused, not clamped', async () => {
  const { accounts } = provider([[VOTING_POWER_SQL, [vpRow({ rewards: '10', rewards_rest: '0', withdrawn: '11' })]]]);
  await rejectsWith(() => accounts.getVotingPower(ADDR), 'INTERNAL');
});

/* -- listDelegationHistory -------------------------------------------------- */

const historyRows = [
  { kind: 'governance', tx_id: '9', cert_index: 1, drep_hash: null, drep_view: 'drep_always_abstain', has_script: false, pool_hash: null, tx_hash: TX, ...block, total_count: '3' },
  { kind: 'pool', tx_id: '9', cert_index: 1, drep_hash: null, drep_view: null, has_script: null, pool_hash: POOL_HASH, tx_hash: TX, ...block, total_count: '3' },
];

test('listDelegationHistory maps both kinds and carries the total', async () => {
  const { accounts, db } = provider([[HISTORY_SQL, historyRows]]);
  const { data } = await accounts.listDelegationHistory(ADDR, { page: 1, size: 2 });
  assert.equal(data.total, 3);
  assert.deepEqual(data.elements[0], {
    kind: 'governance',
    target: { kind: 'predefined', target: 'alwaysAbstain' },
    at: { epoch: 1400, slot: 120000000, block: 4600000, time: '2026-08-01T00:00:00.000Z' },
    txRef: { txHash: TX, index: 1, block: 4600000 },
  });
  assert.equal(data.elements[1].kind, 'pool');
  assert.equal(decodePoolId(data.elements[1].poolId), POOL_HASH);
  assert.ok(!('target' in data.elements[1]));
  assert.deepEqual(db.calls[0].params, [`e0${KEY_HASH}`, null, 2, 0]);
});

test('listDelegationHistory binds kind, limit and offset', async () => {
  const { accounts, db } = provider([[HISTORY_SQL, [historyRows[1]]]]);
  await accounts.listDelegationHistory(ADDR, { page: 3, size: 10, kind: 'pool' });
  assert.deepEqual(db.calls[0].params, [`e0${KEY_HASH}`, 'pool', 10, 20]);
});

test('listDelegationHistory: a page past the end is empty but still totalled', async () => {
  const { accounts, db } = provider([[HISTORY_SQL, []], [HISTORY_COUNT_SQL, [{ total_count: '3' }]]]);
  const { data } = await accounts.listDelegationHistory(ADDR, { page: 9, size: 2 });
  assert.deepEqual(data, { elements: [], total: 3 });
  assert.equal(db.calls.length, 2);
});

test('listDelegationHistory: an empty first page is a known total of 0 with one query', async () => {
  const { accounts, db } = provider([[HISTORY_SQL, []]]);
  assert.deepEqual((await accounts.listDelegationHistory(ADDR, { page: 1, size: 5 })).data, { elements: [], total: 0 });
  assert.equal(db.calls.length, 1);
});

test('listDelegationHistory validates paging and kind', async () => {
  const { accounts, db } = provider([]);
  for (const q of [{ page: 0, size: 5 }, { page: 1, size: 0 }, { page: 1, size: 100000 }, { page: 1.5, size: 5 }, { page: 1, size: 5, kind: 'drep' }, undefined]) {
    await rejectsWith(() => accounts.listDelegationHistory(ADDR, q), 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});
