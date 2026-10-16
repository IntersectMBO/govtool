/**
 * DReps: directory, filters, sorts, random paging rule, detail with activity,
 * vote listing, update history and counts, over a fake Koios. Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { voteRows } from '../dist/governance/dreps.js';
import {
  EPOCH,
  actionId,
  at,
  chainRoutes,
  cip105,
  drepId,
  filterRows,
  hash,
  provider,
  rejectsWith,
  txHash,
} from './helpers.mjs';

const d1 = drepId(1); // active, named, updated since registering
const d2 = drepId(2, true); // inactive (past expiry), anonymous, script
const d3 = drepId(3); // retired; its last anchor stays
const d4 = drepId(4); // registered this epoch: no snapshot yet

const listRows = [
  { drep_id: d1, hex: hash(1), has_script: false, registered: true },
  { drep_id: d2, hex: hash(2), has_script: true, registered: true },
  { drep_id: d3, hex: hash(3), has_script: false, registered: false },
  { drep_id: d4, hex: hash(4), has_script: false, registered: true },
];

const info = {
  [d1]: { drep_id: d1, hex: hash(1), has_script: false, drep_status: 'registered', deposit: '500000000', active: true, expires_epoch_no: EPOCH + 10, amount: '9000', meta_url: 'https://a', meta_hash: 'aa'.repeat(32), live_delegator_count: 12 },
  [d2]: { drep_id: d2, hex: hash(2), has_script: true, drep_status: 'registered', deposit: '500000000', active: false, expires_epoch_no: EPOCH - 2, amount: '0', meta_url: null, meta_hash: null, live_delegator_count: 0 },
  [d3]: { drep_id: d3, hex: hash(3), has_script: false, drep_status: 'deregistered', deposit: null, active: false, expires_epoch_no: null, amount: '0', meta_url: null, meta_hash: null, live_delegator_count: 0 },
  [d4]: { drep_id: d4, hex: hash(4), has_script: false, drep_status: 'registered', deposit: '500000000', active: true, expires_epoch_no: EPOCH + 20, amount: '0', meta_url: 'https://d', meta_hash: 'dd'.repeat(32), live_delegator_count: 1 },
};

const cert = (drep, byte, epoch, action, extra = {}) => ({
  drep_id: drep,
  hex: listRows.find((r) => r.drep_id === drep).hex,
  has_script: listRows.find((r) => r.drep_id === drep).has_script,
  update_tx_hash: txHash(byte),
  cert_index: 0,
  block_time: at(epoch, byte),
  action,
  deposit: action === 'registered' ? '500000000' : action === 'deregistered' ? '-500000000' : null,
  meta_url: null,
  meta_hash: null,
  ...extra,
});
const updates = [
  cert(d1, 10, 600, 'registered', { meta_url: 'https://old', meta_hash: '01'.repeat(32) }),
  cert(d1, 11, 650, 'updated', { meta_url: 'https://a', meta_hash: 'aa'.repeat(32) }),
  cert(d2, 20, 550, 'registered'),
  cert(d3, 30, 540, 'registered', { meta_url: 'https://c', meta_hash: 'cc'.repeat(32) }),
  cert(d3, 31, 620, 'deregistered'),
  cert(d4, 40, EPOCH, 'registered', { meta_url: 'https://d', meta_hash: 'dd'.repeat(32) }),
];

const prop = (byte, type, proposed, over = {}) => ({
  proposal_id: actionId(byte),
  proposal_tx_hash: txHash(byte),
  proposal_index: 0,
  proposal_type: type,
  block_time: at(proposed, byte),
  proposed_epoch: proposed,
  ratified_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  ...over,
});
const proposals = [
  prop(101, 'InfoAction', 530, { expired_epoch: 536, dropped_epoch: 537 }), // before d1 registered
  prop(102, 'TreasuryWithdrawals', 610, { ratified_epoch: 615 }),
  prop(103, 'InfoAction', 640, { expired_epoch: 646, dropped_epoch: 647 }),
  prop(104, 'ParameterChange', 655), // live
];
const votes = [
  { vote_tx_hash: txHash(201), voter_role: 'DRep', voter_id: d1, proposal_id: actionId(102), proposal_tx_hash: txHash(102), proposal_index: 0, proposal_type: 'TreasuryWithdrawals', epoch_no: 612, block_height: 5, block_time: at(612), vote: 'Yes', meta_url: 'https://r', meta_hash: 'ee'.repeat(32) },
  { vote_tx_hash: txHash(202), voter_role: 'DRep', voter_id: d1, proposal_id: actionId(104), proposal_tx_hash: txHash(104), proposal_index: 0, proposal_type: 'ParameterChange', epoch_no: 656, block_height: 6, block_time: at(656), vote: 'No', meta_url: null, meta_hash: null },
];

function routes(extra = {}) {
  return {
    ...chainRoutes,
    drep_list: (url) => filterRows(listRows, url),
    drep_info: (_url, init) => init.body._drep_ids.map((id) => info[id]).filter(Boolean),
    drep_updates: (url) => filterRows(updates, url),
    proposal_list: proposals,
    vote_list: (url) => filterRows(votes, url),
    epoch_params: [{ epoch_no: 537 }],
    ...extra,
  };
}

test('get assembles a DRep from list, info and certificates', async () => {
  const { chainData } = provider(routes());
  const { data } = await chainData.governance.dreps.get(d1);
  assert.equal(data.id, d1);
  assert.equal(data.role, 'drep');
  assert.equal(data.kind, 'drep');
  assert.deepEqual(data.anchor, { url: 'https://a', dataHash: 'aa'.repeat(32) });
  assert.equal(data.status, 'active');
  assert.equal(data.expiryEpoch, EPOCH + 10);
  assert.deepEqual(data.votingPower, { amount: '9000', basis: 'active', epoch: EPOCH });
  assert.equal(data.delegatorCount, 12);
  assert.equal(data.registration.latest.at.epoch, 600);
  assert.equal(data.registration.latest.deposit, '500000000');
  assert.deepEqual(data.registration.latest.txRef, { txHash: txHash(10), index: 0 });
  assert.equal(data.registration.latestUpdate.at.epoch, 650);
  assert.equal(data.registration.latestUpdate.deposit, null);
  assert.equal(data.registration.retiredAt, null);
  // Votable since registering at 600: 102, 103, 104 (101 closed before). Voted 102 and 104.
  assert.deepEqual(data.activity, { voted: 2, votable: 3 });
});

test('status is read off the expiry; retired keeps its last anchor; a new DRep has no power yet', async () => {
  const { chainData } = provider(routes());
  const inactive = (await chainData.governance.dreps.get(d2)).data;
  assert.equal(inactive.status, 'inactive');
  assert.equal(inactive.kind, 'anonymous');
  assert.equal(inactive.anchor, null);
  assert.equal(inactive.isScriptBased, true);
  const retired = (await chainData.governance.dreps.get(d3)).data;
  assert.equal(retired.status, 'retired');
  assert.equal(retired.registration.retiredAt.epoch, 620);
  assert.equal(retired.kind, 'drep', 'the anchor of the newest registration or update, as db-sync reads it');
  assert.equal(retired.expiryEpoch, undefined);
  const fresh = (await chainData.governance.dreps.get(d4)).data;
  assert.equal(fresh.votingPower, null);
});

test('ids: CIP-105 and junk are INVALID_INPUT, an unknown CIP-129 id NOT_FOUND', async () => {
  const { chainData, calls } = provider(routes());
  await rejectsWith(chainData.governance.dreps.get(cip105(1)), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.get('drep_always_abstain'), 'INVALID_INPUT');
  assert.equal(calls.length, 0, 'rejected before any request');
  await rejectsWith(chainData.governance.dreps.get(drepId(9)), 'NOT_FOUND');
});

test('random is the default, returns size rows with the total, and is not paged', async () => {
  const { chainData } = provider(routes());
  const { data } = await chainData.governance.dreps.list({ page: 1, size: 2 });
  assert.equal(data.elements.length, 2);
  assert.equal(data.total, 4);
  await rejectsWith(chainData.governance.dreps.list({ page: 2, size: 2 }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.list({ page: 2, size: 2, sort: 'random' }), 'INVALID_INPUT');
});

test('sorts: registrationDate newest first, votingPower largest first; activity refused', async () => {
  const { chainData } = provider(routes());
  const byDate = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate' });
  assert.deepEqual(byDate.data.elements.map((d) => d.id), [d4, d1, d2, d3]);
  const p1 = await chainData.governance.dreps.list({ page: 1, size: 2, sort: 'registrationDate' });
  const p2 = await chainData.governance.dreps.list({ page: 2, size: 2, sort: 'registrationDate' });
  assert.deepEqual([...p1.data.elements, ...p2.data.elements].map((d) => d.id), [d4, d1, d2, d3]);
  assert.equal(p2.data.total, 4);
  const byPower = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'votingPower' });
  assert.equal(byPower.data.elements[0].id, d1);
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 10, sort: 'activity' }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 10, sort: 'fame' }), 'INVALID_INPUT');
});

test('filters: status by expiry, retired pushed down, kind from the anchor', async () => {
  const { chainData, calls } = provider(routes());
  const active = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', status: ['active'] });
  assert.deepEqual(active.data.elements.map((d) => d.id), [d4, d1]);
  assert.equal(calls.find((c) => c.endpoint === 'drep_list').url.searchParams.get('registered'), 'eq.true');
  const retired = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', status: ['retired'] });
  assert.deepEqual(retired.data.elements.map((d) => d.id), [d3]);
  const inactiveOrRetired = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', status: ['inactive', 'retired'] });
  assert.deepEqual(inactiveOrRetired.data.elements.map((d) => d.id), [d2, d3]);
  const anon = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', kind: ['anonymous'] });
  assert.deepEqual(anon.data.elements.map((d) => d.id), [d2]);
  const both = await chainData.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', kind: ['anonymous', 'drep'] });
  assert.equal(both.data.total, 4);
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 10, status: ['sleepy'] }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.dreps.list({ page: 1, size: 10, kind: 'drep' }), 'INVALID_INPUT');
});

test('search is exactId only: a CIP-129 id matches itself; CIP-105 and names match nothing', async () => {
  const { chainData } = provider(routes());
  const hit = await chainData.governance.dreps.list({ page: 1, size: 10, search: d2.toUpperCase() });
  assert.deepEqual(hit.data.elements.map((d) => d.id), [d2]);
  assert.deepEqual((await chainData.governance.dreps.list({ page: 1, size: 10, search: cip105(2) })).data, { elements: [], total: 0 });
  assert.deepEqual((await chainData.governance.dreps.list({ page: 1, size: 10, search: 'Alice' })).data, { elements: [], total: 0 });
});

test('listVotes: voted and not-voted rows, filterable and ordered', async () => {
  const { chainData } = provider(routes());
  const all = await chainData.governance.dreps.listVotes(d1, { page: 1, size: 10 });
  assert.equal(all.data.total, 3);
  assert.deepEqual(all.data.elements.map((r) => [r.action.id, r.voted]), [
    [actionId(104), true],
    [actionId(103), false],
    [actionId(102), true],
  ]);
  const notVoted = all.data.elements.find((r) => !r.voted);
  assert.deepEqual(Object.keys(notVoted).sort(), ['action', 'voted'], 'no choice or anchor on a not-voted row');
  const first = all.data.elements[2];
  assert.equal(first.choice, 'yes');
  assert.deepEqual(first.anchor, { url: 'https://r', dataHash: 'ee'.repeat(32) });
  assert.equal(first.action.type, 'TreasuryWithdrawals');
  const oldestVoted = await chainData.governance.dreps.listVotes(d1, { page: 1, size: 10, voted: true, sort: 'oldest' });
  assert.deepEqual(oldestVoted.data.elements.map((r) => r.action.id), [actionId(102), actionId(104)]);
  const onlyNot = await chainData.governance.dreps.listVotes(d1, { page: 1, size: 10, voted: false });
  assert.equal(onlyNot.data.total, 1);
  await rejectsWith(chainData.governance.dreps.listVotes(d1, { page: 1, size: 10, sort: 'random' }), 'INVALID_INPUT');
});

test('vote window: bootstrap-era non-Info actions are not votable', () => {
  const rows = voteRows(
    [prop(1, 'TreasuryWithdrawals', 520, { expired_epoch: 526 }), prop(2, 'InfoAction', 520, { expired_epoch: 526 })],
    [],
    { startEpoch: 515, endEpoch: 657, bootstrapEnd: 537 },
  );
  assert.deepEqual(rows.map((r) => r.proposal.proposal_id), [actionId(2)]);
});

test('listUpdateHistory is the metadata-change feed, both orders', async () => {
  const { chainData } = provider(routes());
  const desc = await chainData.governance.dreps.listUpdateHistory(d3, { page: 1, size: 10 });
  assert.equal(desc.data.total, 1, 'the retirement is not a metadata change');
  const asc = await chainData.governance.dreps.listUpdateHistory(d1, { page: 1, size: 10, sort: 'asc' });
  assert.deepEqual(asc.data.elements.map((e) => e.anchor.url), ['https://old', 'https://a']);
  await rejectsWith(chainData.governance.dreps.listUpdateHistory(d1, { page: 1, size: 10, sort: 'up' }), 'INVALID_INPUT');
});

test('getCounts: all three, plus anonymous', async () => {
  const { chainData } = provider(routes());
  const { data } = await chainData.governance.dreps.getCounts();
  assert.deepEqual(data, { totalRegistered: 3, totalActive: 2, totalInactive: 1, anonymous: 1 });
});

test('listDelegators and liveVotingPower are not served', () => {
  const { chainData } = provider(routes());
  assert.equal(chainData.governance.dreps.listDelegators, undefined);
});
