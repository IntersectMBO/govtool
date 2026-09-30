/**
 * Committee, constitution, pools and accounts over a fake Koios. Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { termStart } from '../dist/governance/committee.js';
import {
  EPOCH,
  actionId,
  at,
  chainRoutes,
  coldId,
  drepId,
  filterRows,
  hash,
  hotId,
  poolId,
  provider,
  rejectsWith,
  stakeAddr,
  txHash,
} from './helpers.mjs';

/* -- committee --------------------------------------------------------------- */

const ucRow = (byte, prev, enacted, removed, added, quorum) => ({
  block_time: at(enacted - 5, byte),
  proposal_id: actionId(byte),
  proposal_tx_hash: txHash(byte),
  proposal_index: 0,
  proposal_type: 'NewCommittee',
  proposal_description: {
    tag: 'UpdateCommittee',
    contents: [prev ? { txId: txHash(prev), govActionIx: 0 } : null, removed.map((b) => ({ keyHash: hash(b) })), Object.fromEntries(added.map(([b, e]) => [`keyHash-${hash(b)}`, e])), quorum],
  },
  previous_gov_action_proposal_id: prev ? actionId(prev) : null,
  deposit: '0',
  return_address: null,
  proposed_epoch: enacted - 5,
  ratified_epoch: enacted - 1,
  enacted_epoch: enacted,
  dropped_epoch: null,
  expired_epoch: null,
  expiration: enacted,
  meta_url: null,
  meta_hash: null,
});
// Genesis had 1, 2, 3. Step A (581) removes 3, adds 4 and re-appoints 2. Step B (602) adds 5.
const stepA = ucRow(50, null, 581, [3], [[2, 700], [4, 700]], { numerator: 2, denominator: 3 });
const stepB = ucRow(51, 50, 602, [], [[5, 800]], { numerator: 3, denominator: 5 });
const constitutionRow = {
  ...ucRow(60, null, 542, [], [], { numerator: 1, denominator: 1 }),
  proposal_type: 'NewConstitution',
  proposal_description: { tag: 'NewConstitution', contents: [null, { anchor: { url: 'ipfs://c', dataHash: 'CC'.repeat(32) }, script: 'fa'.repeat(28) }] },
};

const member = (b, status, expiry, hotByte = b + 100) => ({
  status,
  cc_cold_hex: hash(b),
  cc_cold_has_script: false,
  cc_hot_hex: status === 'resigned' ? null : hash(hotByte),
  cc_hot_has_script: status === 'resigned' ? null : false,
  expiration_epoch: expiry,
});

const committeeRoutes = (extra = {}) => ({
  ...chainRoutes,
  committee_info: [
    {
      proposal_id: stepB.proposal_id,
      proposal_tx_hash: stepB.proposal_tx_hash,
      proposal_index: 0,
      quorum_numerator: 3,
      quorum_denominator: 5,
      members: [member(1, 'authorized', 650), member(2, 'authorized', 700), member(4, 'not_authorized', 700), member(5, 'resigned', 800)],
    },
  ],
  proposal_list: (url) => filterRows([stepA, stepB, constitutionRow], url, ['proposal_type']).filter((r) => (url.searchParams.get('proposal_type') ?? '').includes(r.proposal_type)),
  epoch_params: [{ epoch_no: 507 }],
  // Member 2's current hot key voted at epoch 520, before step A.
  vote_list: (url) => filterRows([{ voter_id: hotId(102), voter_role: 'ConstitutionalCommittee', epoch_no: 520 }], url),
  ...extra,
});

test('committee from committee_info, with lineage-derived term starts', async () => {
  const { chainData } = provider(committeeRoutes());
  const { data } = await chainData.governance.committee.getCommittee();
  assert.deepEqual(data.quorum, { numerator: 3, denominator: 5 });
  assert.equal(data.enactedBy.id, stepB.proposal_id);
  assert.equal(data.isDissolved, false);
  const byCold = Object.fromEntries(data.members.map((m) => [m.coldCredential, m]));
  // 1: never added by any step, so a genesis member: seated since Conway began.
  assert.equal(byCold[coldId(1)].termStartEpoch, 507);
  // 2: re-appointed at 581, but its hot key voted before 581: a genesis member too.
  assert.equal(byCold[coldId(2)].termStartEpoch, 507);
  // 4: added at 581 with no evidence either way about genesis: unknown.
  assert.equal(byCold[coldId(4)].termStartEpoch, null);
  assert.equal(byCold[coldId(4)].hotCredential, null);
  // 5: added at 602 after a step it was not in? It was never removed, so unknown too.
  assert.equal(byCold[coldId(5)].termStartEpoch, null);
  assert.equal(byCold[coldId(5)].hasResigned, true);
  assert.equal(byCold[coldId(5)].hotCredential, null);
  assert.equal(byCold[coldId(1)].hotCredential, hotId(101));
  assert.equal(byCold[coldId(1)].termExpiryEpoch, 650);
  const sorted = [...data.members].map((m) => m.coldCredential);
  assert.deepEqual(sorted, [...sorted].sort());
});

test('termStart: a removal ends a run; re-addition starts a new one', () => {
  const steps = [
    { epoch: 581, dissolves: false, added: new Set(['m']), removed: new Set(), quorum: null },
    { epoch: 600, dissolves: false, added: new Set(), removed: new Set(['m']), quorum: null },
    { epoch: 620, dissolves: false, added: new Set(['m']), removed: new Set(), quorum: null },
    { epoch: 640, dissolves: false, added: new Set(['m']), removed: new Set(), quorum: null },
  ];
  assert.equal(termStart('m', steps, 507), 620);
  const dissolved = [{ epoch: 590, dissolves: true, added: new Set(), removed: new Set() }, { epoch: 600, dissolves: false, added: new Set(['m']), removed: new Set() }];
  assert.equal(termStart('m', dissolved, 507), 600);
});

test('getMember by cold credential; a hot id is INVALID_INPUT; a non-member NOT_FOUND', async () => {
  const { chainData } = provider(committeeRoutes());
  assert.equal((await chainData.governance.committee.getMember(coldId(2))).data.coldCredential, coldId(2));
  await rejectsWith(chainData.governance.committee.getMember(hotId(102)), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.committee.getMember(coldId(3)), 'NOT_FOUND');
});

test('a dissolved committee: no members, the last quorum, isDissolved', async () => {
  const nc = { ...ucRow(52, 51, 610, [], [], null), proposal_type: 'NoConfidence', proposal_description: { tag: 'NoConfidence', contents: [{ txId: txHash(51), govActionIx: 0 }] } };
  const { chainData } = provider(
    committeeRoutes({
      committee_info: [{ proposal_id: nc.proposal_id, proposal_tx_hash: nc.proposal_tx_hash, proposal_index: 0, quorum_numerator: null, quorum_denominator: null, members: null }],
      proposal_list: (url) => filterRows([stepA, stepB, nc], url, ['proposal_type']).filter((r) => (url.searchParams.get('proposal_type') ?? '').includes(r.proposal_type)),
    }),
  );
  const { data } = await chainData.governance.committee.getCommittee();
  assert.deepEqual(data, { members: [], quorum: { numerator: 3, denominator: 5 }, enactedBy: { id: nc.proposal_id, txHash: nc.proposal_tx_hash, index: 0 }, isDissolved: true });
});

test('committee_info and the lineage must agree on the head', async () => {
  const { chainData } = provider(committeeRoutes({ committee_info: [{ proposal_id: stepA.proposal_id, quorum_numerator: 2, quorum_denominator: 3, members: [] }] }));
  await rejectsWith(chainData.governance.committee.getCommittee(), 'INTERNAL');
});

test('constitution is derived from the enacted head of its lineage', async () => {
  const { chainData } = provider(committeeRoutes());
  const { data } = await chainData.governance.committee.getConstitution();
  assert.deepEqual(data.anchor, { url: 'ipfs://c', dataHash: 'cc'.repeat(32) });
  assert.equal(data.guardrailsScriptHash, 'fa'.repeat(28));
  assert.equal(data.enactedBy.id, constitutionRow.proposal_id);
  assert.equal(data.enactedAt.epoch, 542);
  assert.ok(data.enactedAt.time.endsWith('Z'));
  const none = provider(committeeRoutes({ proposal_list: [] }));
  await rejectsWith(none.chainData.governance.committee.getConstitution(), 'CAPABILITY_UNSUPPORTED');
});

/* -- pools ------------------------------------------------------------------- */

const poolRow = (b, status = 'registered') => ({
  pool_id_bech32: poolId(b),
  pool_id_hex: hash(b),
  pledge: '1000',
  meta_url: 'https://p',
  meta_hash: `\\x${'bb'.repeat(32)}`,
  pool_status: status,
  active_stake: '5000',
});
const poolRoutes = {
  ...chainRoutes,
  pool_list: (url) => filterRows([poolRow(1), poolRow(2, 'retiring'), poolRow(3)], url),
  pool_voting_power_history: (url) => filterRows([{ pool_id_bech32: poolId(1), amount: '10' }, { pool_id_bech32: poolId(2), amount: '30' }], url),
  pool_info: (_url, init) => (init.body._pool_bech32_ids[0] === poolId(1) ? [{ ...poolRow(1), meta_hash: 'bb'.repeat(32), live_stake: '6000', voting_power: '10' }] : []),
  vote_list: (url) =>
    filterRows(
      [{ vote_tx_hash: txHash(9), voter_role: 'SPO', voter_id: poolId(1), proposal_id: actionId(7), proposal_tx_hash: txHash(7), proposal_index: 0, proposal_type: 'NewCommittee', epoch_no: 600, block_height: 9, block_time: at(600), vote: 'Abstain', meta_url: null, meta_hash: null }],
      url,
    ),
};

test('pools: ordered by voting power, none last; bytea escape stripped; paged with total', async () => {
  const { chainData } = provider(poolRoutes);
  const { data } = await chainData.governance.pools.list({ page: 1, size: 10 });
  assert.deepEqual(data.elements.map((p) => p.id), [poolId(2), poolId(1), poolId(3)]);
  assert.equal(data.total, 3);
  assert.deepEqual(data.elements[0].votingPower, { amount: '30', basis: 'active', epoch: EPOCH });
  assert.equal(data.elements[2].votingPower, null);
  assert.equal(data.elements[0].anchor.dataHash, 'bb'.repeat(32));
  assert.equal(data.elements[0].activeStake, '5000');
  assert.ok(!('ticker' in data.elements[0]) && !('name' in data.elements[0]));
  const p2 = await chainData.governance.pools.list({ page: 2, size: 2 });
  assert.deepEqual(p2.data.elements.map((p) => p.id), [poolId(3)]);
  assert.equal((await chainData.governance.pools.list({ page: 1, size: 10, search: poolId(3) })).data.total, 1);
  assert.deepEqual((await chainData.governance.pools.list({ page: 1, size: 10, search: 'OCTAS' })).data, { elements: [], total: 0 });
});

test('pools.get and listVotes', async () => {
  const { chainData } = provider(poolRoutes);
  const { data } = await chainData.governance.pools.get(poolId(1));
  assert.equal(data.liveStake, '6000');
  assert.deepEqual(data.votingPower, { amount: '10', basis: 'active', epoch: EPOCH });
  await rejectsWith(chainData.governance.pools.get(poolId(8)), 'NOT_FOUND');
  await rejectsWith(chainData.governance.pools.get('pool1xyz'), 'INVALID_INPUT');
  const votes = await chainData.governance.pools.listVotes(poolId(1), { page: 1, size: 10 });
  assert.equal(votes.data.total, 1);
  assert.deepEqual(votes.data.elements[0].action, { id: actionId(7), type: 'UpdateCommittee' });
  assert.deepEqual(votes.data.elements[0].voter, { role: 'spo', id: poolId(1), isScriptBased: false });
  await rejectsWith(chainData.governance.pools.listVotes(poolId(8), { page: 1, size: 10 }), 'NOT_FOUND');
});

/* -- accounts ---------------------------------------------------------------- */

const addr = stakeAddr(1);
const accountRow = {
  stake_address: addr,
  status: 'registered',
  delegated_drep: drepId(5),
  delegated_pool: poolId(1),
  utxo: '1000',
  rewards: '500',
  withdrawals: '700',
  rewards_available: '-200',
  reserves: '100',
  treasury: '0',
  proposal_refund: '300',
};
const accountRoutes = (extra = {}) => ({
  ...chainRoutes,
  account_info: (_url, init) => (init.body._stake_addresses[0] === addr ? [accountRow] : []),
  account_updates: [
    {
      stake_address: addr,
      updates: [
        { action_type: 'delegation_drep', tx_hash: txHash(1), epoch_no: 600, epoch_slot: 1, absolute_slot: 100, block_time: at(600) },
        { action_type: 'delegation_drep', tx_hash: txHash(2), epoch_no: 640, epoch_slot: 1, absolute_slot: 200, block_time: at(640) },
        { action_type: 'delegation_pool', tx_hash: txHash(3), epoch_no: 500, epoch_slot: 1, absolute_slot: 50, block_time: at(500) },
      ],
    },
  ],
  drep_updates: [{ drep_id: drepId(5), hex: hash(5), has_script: false, update_tx_hash: txHash(4), cert_index: 0, block_time: at(550), action: 'registered', deposit: '1', meta_url: null, meta_hash: null }],
  epoch_params: [{ epoch_no: 537, protocol_major: 10 }],
  pool_info: [{ ...poolRow(1), pool_status: 'registered', retiring_epoch: null }],
  proposal_list: (url) => filterRows([{ return_address: addr, deposit: '100000000000', enacted_epoch: null, dropped_epoch: null, expired_epoch: null }], url),
  ...extra,
});

test('accounts.get: unknown well-formed address is unregistered, not missing', async () => {
  const { chainData } = provider(accountRoutes());
  const known = (await chainData.accounts.get(addr.toUpperCase())).data;
  assert.deepEqual(known, { stakeAddress: addr, stakeKeyHash: hash(1), isRegistered: true, isScriptBased: false });
  assert.equal((await chainData.accounts.get(stakeAddr(2))).data.isRegistered, false);
  assert.equal((await chainData.accounts.getDelegation(stakeAddr(2))).data, null);
  assert.equal((await chainData.accounts.getVotingPower(stakeAddr(2))).data, null);
  await rejectsWith(chainData.accounts.get('stake_test1uqfu74w3wh4gfzu8m6e7j987h4lq9r3t7ef5gaw497uu85qsqfy27'), 'INVALID_INPUT');
  await rejectsWith(chainData.accounts.get(42), 'INVALID_INPUT');
});

test('delegation: the target, dated by the newest certificate', async () => {
  const { chainData } = provider(accountRoutes());
  const { data } = await chainData.accounts.getDelegation(addr);
  assert.deepEqual(data.target, { kind: 'drep', drep: { role: 'drep', id: drepId(5), isScriptBased: false } });
  assert.deepEqual(data.txRef, { txHash: txHash(2) });
  assert.equal(data.since.epoch, 640);
  const abstain = provider(accountRoutes({ account_info: [{ ...accountRow, delegated_drep: 'drep_always_abstain' }] }));
  assert.deepEqual((await abstain.chainData.accounts.getDelegation(addr)).data.target, { kind: 'predefined', target: 'alwaysAbstain' });
});

test('a delegation to a DRep that retired since does not stand (protocol >= 10)', async () => {
  const retiredLater = [
    { drep_id: drepId(5), hex: hash(5), has_script: false, update_tx_hash: txHash(4), cert_index: 0, block_time: at(550), action: 'registered', deposit: '1', meta_url: null, meta_hash: null },
    { drep_id: drepId(5), hex: hash(5), has_script: false, update_tx_hash: txHash(5), cert_index: 0, block_time: at(650), action: 'deregistered', deposit: '-1', meta_url: null, meta_hash: null },
  ];
  const { chainData } = provider(accountRoutes({ drep_updates: retiredLater }));
  assert.equal((await chainData.accounts.getDelegation(addr)).data, null);
});

test('pool delegation, and one to a pool retired since', async () => {
  const { chainData } = provider(accountRoutes());
  const { data } = await chainData.accounts.getPoolDelegation(addr);
  assert.deepEqual(data, { poolId: poolId(1), txRef: { txHash: txHash(3) }, since: { epoch: 500, slot: 50, time: new Date(at(500) * 1000).toISOString().replace('.000Z', 'Z') } });
  const retired = provider(accountRoutes({ pool_info: [{ ...poolRow(1), pool_status: 'retired', retiring_epoch: 600 }] }));
  assert.equal((await retired.chainData.accounts.getPoolDelegation(addr)).data, null);
});

test('voting power: UTxO + rewards incl. non-staking - withdrawals + held proposal deposits', async () => {
  const { chainData } = provider(accountRoutes());
  const { data } = await chainData.accounts.getVotingPower(addr);
  // 1000 + (500 + 100 + 0 + 300 - 700) + 100000000000
  assert.deepEqual(data, { amount: String(1000 + 200 + 100000000000), basis: 'live', epoch: EPOCH });
  const broken = provider(accountRoutes({ account_info: [{ ...accountRow, withdrawals: '99999' }] }));
  await rejectsWith(broken.chainData.accounts.getVotingPower(addr), 'INTERNAL');
});

test('accounts: balance and delegation history are not served', async () => {
  const { chainData } = provider(accountRoutes());
  assert.equal(chainData.accounts.listDelegationHistory, undefined);
  assert.ok(!('balance' in (await chainData.accounts.get(addr)).data));
});
