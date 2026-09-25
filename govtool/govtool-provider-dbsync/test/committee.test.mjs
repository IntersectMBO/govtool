import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import { assembleCommittee } from '../dist/governance/committee/membership.js';
import { assembleConstitution } from '../dist/governance/committee/constitution.js';
import { encodeCommitteeColdId, encodeCommitteeHotId, encodeGovActionId } from '../dist/ids.js';

const H = (n) => n.toString(16).padStart(2, '0').repeat(28);
const TX = (n) => n.toString(16).padStart(2, '0').repeat(32);
const member = (hashId, n, isScript, expiry) => ({ hashId: String(hashId), hash: H(n), isScript, expiry });

const genesis = (members, q = ['2', '3']) => ({
  id: null, type: null, enacted_epoch: null, prev_id: null, tx_hash: null, index: null,
  committee_id: '1', quorum_numerator: q[0], quorum_denominator: q[1], members,
});
const update = (id, prev, epoch, members, q = ['2', '3']) => ({
  id: String(id), type: 'NewCommittee', enacted_epoch: epoch, prev_id: prev === null ? null : String(prev),
  tx_hash: TX(id), index: 0, committee_id: String(100 + id), quorum_numerator: q[0], quorum_denominator: q[1], members,
});
const noConfidence = (id, prev, epoch) => ({
  id: String(id), type: 'NoConfidence', enacted_epoch: epoch, prev_id: prev === null ? null : String(prev),
  tx_hash: TX(id), index: 1, committee_id: null, quorum_numerator: null, quorum_denominator: null, members: null,
});

/** A fake db that answers by matching a fragment of the SQL, and records calls. */
function fakeDb(routes) {
  const calls = [];
  return {
    calls,
    query: async (sql, params = []) => {
      calls.push({ sql, params });
      for (const [fragment, rows] of routes) if (sql.includes(fragment)) return typeof rows === 'function' ? rows(params) : rows;
      throw new Error(`unrouted SQL: ${sql.slice(0, 80)}`);
    },
  };
}

test('genesis only: members, quorum, no enacting action, terms start at Conway', () => {
  const c = assembleCommittee([genesis([member(7, 1, true, 500)])], [], 507);
  assert.deepEqual(c, {
    members: [{
      role: 'cc', coldCredential: encodeCommitteeColdId(H(1), true), hotCredential: null,
      termStartEpoch: 507, termExpiryEpoch: 500, hasResigned: false, isScriptBased: true,
    }],
    quorum: { numerator: 2, denominator: 3 },
    enactedBy: null,
    isDissolved: false,
  });
});

test('the head is found by predecessor links, not row order or epoch alone', () => {
  const rows = [
    update(12, 11, 600, [member(8, 2, false, 700)], ['3', '4']),
    genesis([member(7, 1, true, 500)]),
    update(11, null, 600, [member(7, 1, true, 500), member(8, 2, false, 650)], ['1', '2']),
  ];
  const c = assembleCommittee(rows, [], 507);
  assert.equal(c.enactedBy.id, encodeGovActionId(TX(12), 0));
  assert.deepEqual(c.enactedBy, { id: encodeGovActionId(TX(12), 0), txHash: TX(12), index: 0 });
  assert.deepEqual(c.quorum, { numerator: 3, denominator: 4 });
  assert.deepEqual(c.members.map((m) => [m.coldCredential, m.termExpiryEpoch]), [[encodeCommitteeColdId(H(2), false), 700]]);
});

test('term start survives re-election and restarts after removal', () => {
  const rows = [
    genesis([member(7, 1, true, 500), member(8, 2, true, 500)]),
    update(1, null, 600, [member(8, 2, true, 900)]),                          // 1 removed; 2 re-elected
    update(2, 1, 650, [member(7, 1, true, 950), member(8, 2, true, 900)]), // 1 re-added
  ];
  const byId = Object.fromEntries(assembleCommittee(rows, [], 507).members.map((m) => [m.coldCredential, m.termStartEpoch]));
  assert.equal(byId[encodeCommitteeColdId(H(2), true)], 507);
  assert.equal(byId[encodeCommitteeColdId(H(1), true)], 650);
});

test('a key hash and a script hash with the same bytes are different members', () => {
  const rows = [
    genesis([member(7, 1, true, 500)]),
    update(1, null, 600, [member(9, 1, false, 900)]), // the script member removed, a key member added
  ];
  const certs = [{ cold_key_id: '7', resigned: false, hot_hash: H(5), hot_is_script: false }];
  const [m] = assembleCommittee(rows, certs, 507).members;
  assert.equal(m.coldCredential, encodeCommitteeColdId(H(1), false));
  assert.equal(m.isScriptBased, false);
  assert.equal(m.termStartEpoch, 600, 'the script seat of the same bytes is not this seat');
  assert.equal(m.hotCredential, null, "the script member's hot key is not this member's");
});

test('hot credential from the latest authorisation; a resignation clears it', () => {
  const rows = [genesis([member(7, 1, true, 500), member(8, 2, false, 500), member(9, 3, false, 500)])];
  const certs = [
    { cold_key_id: '7', resigned: false, hot_hash: H(4), hot_is_script: true },
    { cold_key_id: '8', resigned: true, hot_hash: null, hot_is_script: null },
  ];
  const byId = Object.fromEntries(assembleCommittee(rows, certs, null).members.map((m) => [m.coldCredential, m]));
  assert.equal(byId[encodeCommitteeColdId(H(1), true)].hotCredential, encodeCommitteeHotId(H(4), true));
  assert.equal(byId[encodeCommitteeColdId(H(1), true)].hasResigned, false);
  assert.equal(byId[encodeCommitteeColdId(H(2), false)].hotCredential, null);
  assert.equal(byId[encodeCommitteeColdId(H(2), false)].hasResigned, true);
  assert.equal(byId[encodeCommitteeColdId(H(3), false)].hotCredential, null);
  assert.equal(byId[encodeCommitteeColdId(H(3), false)].termStartEpoch, null, 'unknown Conway start stays null');
});

test('NoConfidence dissolves: no members, the removed committee quorum, enacted by it', () => {
  const rows = [genesis([member(7, 1, true, 500)], ['1', '2']), update(1, null, 600, [member(7, 1, true, 900)], ['3', '5']), noConfidence(2, 1, 610)];
  const c = assembleCommittee(rows, [], 507);
  assert.deepEqual(c.members, []);
  assert.equal(c.isDissolved, true);
  assert.deepEqual(c.quorum, { numerator: 3, denominator: 5 });
  assert.deepEqual(c.enactedBy, { id: encodeGovActionId(TX(2), 1), txHash: TX(2), index: 1 });
});

test('after NoConfidence a new committee starts every term afresh', () => {
  const rows = [
    genesis([member(7, 1, true, 500)]),
    noConfidence(1, null, 600),
    update(2, 1, 620, [member(7, 1, true, 900)]),
  ];
  const c = assembleCommittee(rows, [], 507);
  assert.equal(c.isDissolved, false);
  assert.equal(c.members[0].termStartEpoch, 620);
});

test('inconsistent lineage data is refused, not guessed', () => {
  const g = genesis([member(7, 1, true, 500)]);
  const refused = (rows) => assert.throws(() => assembleCommittee(rows, [], 1), (e) => e.code === 'INTERNAL');
  refused([g, update(1, null, 600, []), update(2, null, 600, [])]); // fork
  refused([g, update(1, 99, 600, [])]);                              // orphan
  refused([g, { ...update(1, null, 600, []), committee_id: null }]); // no committee row
  refused([update(1, null, 600, [])]);                               // no genesis
  refused([genesis([], ['1', '0'])]);                                // zero denominator
});

function provider(routes) {
  const db = fakeDb(routes);
  return { db, api: createDbSyncProvider({ network: 'preview', db }).chainData.governance.committee };
}

const committeeRoutes = [
  ['FROM committee_registration', [{ cold_key_id: '7', resigned: false, hot_hash: H(4), hot_is_script: false }]],
  ['FROM epoch_param', [{ epoch: 507 }]],
  ['json_agg', [genesis([member(7, 1, false, 500)])]],
];

test('getCommittee: envelope with the provider meta', async () => {
  const { api } = provider(committeeRoutes);
  const res = await api.getCommittee();
  assert.deepEqual(res.meta, { provider: 'dbsync', network: 'preview' });
  assert.equal(res.data.members[0].hotCredential, encodeCommitteeHotId(H(4), false));
});

test('getMember: found by cold id, NOT_FOUND otherwise', async () => {
  const { api } = provider(committeeRoutes);
  const id = encodeCommitteeColdId(H(1), false);
  assert.equal((await api.getMember(id)).data.coldCredential, id);
  assert.equal((await api.getMember(id.toUpperCase())).data.coldCredential, id);
  await assert.rejects(api.getMember(encodeCommitteeColdId(H(1), true)), (e) => e.code === 'NOT_FOUND');
});

test('getMember: a non-CIP-129 id rejects with INVALID_INPUT, never throws, never queries', async () => {
  const { api, db } = provider(committeeRoutes);
  for (const bad of ['', 'cc_cold1xyz', encodeCommitteeHotId(H(1), false), 'drep1abc']) {
    let promise;
    assert.doesNotThrow(() => { promise = api.getMember(bad); });
    await assert.rejects(promise, (e) => e.code === 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});

test('getCommittee: a database failure rejects with PROVIDER_UNAVAILABLE', async () => {
  const { api } = provider([['', () => { throw new Error('connection refused'); }]]);
  await assert.rejects(api.getCommittee(), (e) => e.code === 'PROVIDER_UNAVAILABLE');
});

/* Constitution */

const constitutionRow = (id, prev, epoch, url, script = null) => ({
  id: id === null ? null : String(id), prev_id: prev === null ? null : String(prev),
  tx_hash: id === null ? null : TX(id), index: id === null ? null : 0, enacted_epoch: epoch,
  epoch_start: epoch === null ? null : new Date(Date.UTC(2025, 0, epoch - 599)),
  url, data_hash: 'ab'.repeat(32), script_hash: script,
});

test('getConstitution: genesis in force has no enacting action', async () => {
  const { api } = provider([['FROM constitution', [constitutionRow(null, null, null, 'ipfs://genesis', 'fa'.repeat(28))]]]);
  assert.deepEqual((await api.getConstitution()).data, {
    anchor: { url: 'ipfs://genesis', dataHash: 'ab'.repeat(32) },
    guardrailsScriptHash: 'fa'.repeat(28),
    enactedBy: null,
    enactedAt: null,
  });
});

test('getConstitution: the enacted head replaces genesis and earlier ones', () => {
  const c = assembleConstitution([
    constitutionRow(null, null, null, 'ipfs://genesis', 'fa'.repeat(28)),
    constitutionRow(6, 5, 600, 'ipfs://second'),
    constitutionRow(5, null, 600, 'ipfs://first', 'db'.repeat(28)),
  ]);
  assert.deepEqual(c, {
    anchor: { url: 'ipfs://second', dataHash: 'ab'.repeat(32) },
    guardrailsScriptHash: null,
    enactedBy: { id: encodeGovActionId(TX(6), 0), txHash: TX(6), index: 0 },
    enactedAt: { epoch: 600, time: '2025-01-01T00:00:00.000Z' },
  });
});

test('getConstitution: no genesis row and nothing enacted is refused', () => {
  assert.throws(() => assembleConstitution([]), (e) => e.code === 'INTERNAL');
});
