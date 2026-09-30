/**
 * Proposals: mapping, filters, paging, lineage, voter context and vote
 * aggregates, over a fake Koios. The two voting summaries are recorded from
 * mainnet (a HardForkInitiation tallied at epoch 643, a live ParameterChange at
 * 657). Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { drepFigures, spoFigures } from '../dist/governance/proposals/aggregates.js';
import { decodeBody } from '../dist/governance/proposals/body.js';
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
  paramsRow,
  poolId,
  provider,
  rejectsWith,
  stakeAddr,
  txHash,
} from './helpers.mjs';

const hfSummary = {
  proposal_type: 'HardForkInitiation',
  epoch_no: 643,
  drep_active_yes_vote_power: '4158552295928039',
  drep_yes_vote_power: '4158552295928039',
  drep_active_no_vote_power: '50510940457',
  drep_no_vote_power: '1183461492087298',
  drep_active_abstain_vote_power: '13424965345534',
  drep_always_no_confidence_vote_power: '175901106225800',
  drep_always_abstain_vote_power: '9330880365186717',
  pool_active_yes_vote_power: '10445177677947930',
  pool_yes_vote_power: '10445177677947930',
  pool_active_no_vote_power: '0',
  pool_no_vote_power: '9254034669953667',
  pool_active_abstain_vote_power: '1699130913945099',
  pool_passive_always_abstain_vote_power: '4010227408074541',
  pool_passive_always_no_confidence_vote_power: '52023987145075',
  committee_yes_votes_cast: 7,
  committee_no_votes_cast: 0,
  committee_abstain_votes_cast: 0,
};

const pcSummary = {
  proposal_type: 'ParameterChange',
  epoch_no: EPOCH,
  drep_active_yes_vote_power: '534967421824635',
  drep_yes_vote_power: '534967421824635',
  drep_active_no_vote_power: '490675179099982',
  drep_no_vote_power: '4013327351598878',
  drep_active_abstain_vote_power: '636672345292614',
  drep_always_no_confidence_vote_power: '138316910791856',
  drep_always_abstain_vote_power: '10362551208190278',
  pool_active_yes_vote_power: '0',
  pool_yes_vote_power: '0',
  pool_active_no_vote_power: '0',
  pool_no_vote_power: '0',
  pool_active_abstain_vote_power: '0',
  pool_passive_always_abstain_vote_power: '12101433550466055',
  pool_passive_always_no_confidence_vote_power: '50078195923665',
  committee_yes_votes_cast: 2,
  committee_no_votes_cast: 0,
  committee_abstain_votes_cast: 0,
};

/** Pool figures for a distribution of 1000: yes 300, no 100, abstain 50, passive abstain 40, passive no-confidence 20. */
const pools = (noConfidence = false) => ({
  pool_active_yes_vote_power: '300',
  pool_yes_vote_power: noConfidence ? '320' : '300',
  pool_active_no_vote_power: '100',
  pool_no_vote_power: noConfidence ? '590' : '610',
  pool_active_abstain_vote_power: '50',
  pool_passive_always_abstain_vote_power: '40',
  pool_passive_always_no_confidence_vote_power: '20',
});

const proposal = (byte, over = {}) => ({
  block_time: at(over.proposed_epoch ?? EPOCH - 3, byte),
  proposal_id: actionId(byte),
  proposal_tx_hash: txHash(byte),
  proposal_index: 0,
  proposal_type: 'InfoAction',
  proposal_description: { tag: 'InfoAction' },
  previous_gov_action_proposal_id: null,
  deposit: '100000000000',
  return_address: stakeAddr(byte),
  proposed_epoch: EPOCH - 3,
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  expiration: EPOCH + 4,
  meta_url: 'ipfs://x',
  meta_hash: 'ab'.repeat(32),
  ...over,
});

const hf = proposal(1, {
  proposal_type: 'HardForkInitiation',
  proposal_description: { tag: 'HardForkInitiation', contents: [null, { major: 11, minor: 0 }] },
  proposed_epoch: 637,
  ratified_epoch: 643,
  enacted_epoch: 644,
  expiration: 644,
});
const pc = proposal(2, {
  proposal_type: 'ParameterChange',
  proposal_description: { tag: 'ParameterChange', contents: [null, { minPoolCost: 75000000 }, 'fa'.repeat(28)] },
});
const uc1 = proposal(3, {
  proposal_type: 'NewCommittee',
  proposal_description: {
    tag: 'UpdateCommittee',
    contents: [null, [{ scriptHash: hash(9) }], { [`keyHash-${hash(4)}`]: 799, [`scriptHash-${hash(5)}`]: 726 }, { numerator: 2, denominator: 3 }],
  },
  proposed_epoch: 573,
  ratified_epoch: 580,
  enacted_epoch: 581,
  expiration: 580,
});
const nc = proposal(4, {
  proposal_type: 'NoConfidence',
  proposal_description: { tag: 'NoConfidence', contents: [{ txId: txHash(3), govActionIx: 0 }] },
  previous_gov_action_proposal_id: actionId(3),
  proposed_epoch: 590,
  ratified_epoch: 595,
  enacted_epoch: 596,
});
const uc2 = proposal(6, {
  proposal_type: 'NewCommittee',
  proposal_description: { tag: 'UpdateCommittee', contents: [{ txId: txHash(4), govActionIx: 0 }, [], { [`keyHash-${hash(4)}`]: 900 }, { numerator: 3, denominator: 5 }] },
  previous_gov_action_proposal_id: actionId(4),
  proposed_epoch: 600,
  ratified_epoch: 601,
  enacted_epoch: 602,
});
const tw = proposal(5, {
  proposal_type: 'TreasuryWithdrawals',
  proposal_description: {
    tag: 'TreasuryWithdrawals',
    contents: [[[{ network: 'Mainnet', credential: { keyHash: hash(8) } }, '45000000000000000123']], null],
  },
  proposed_epoch: 620,
  expired_epoch: 627,
  dropped_epoch: 628,
  expiration: 627,
});
const PROPOSALS = [hf, pc, uc1, nc, uc2, tw];

const summaries = {
  [hf.proposal_id]: hfSummary,
  [pc.proposal_id]: pcSummary,
  [uc1.proposal_id]: { ...pcSummary, ...pools(), proposal_type: 'NewCommittee', epoch_no: 580 },
  [nc.proposal_id]: { ...pcSummary, ...pools(true), proposal_type: 'NoConfidence', epoch_no: 595, drep_yes_vote_power: String(534967421824635n + 138316910791856n) },
  [uc2.proposal_id]: { ...pcSummary, ...pools(), proposal_type: 'NewCommittee', epoch_no: 601 },
  [tw.proposal_id]: { ...pcSummary, proposal_type: 'TreasuryWithdrawals', epoch_no: 627 },
};

const committeeInfo = {
  proposal_id: uc2.proposal_id,
  proposal_tx_hash: uc2.proposal_tx_hash,
  proposal_index: 0,
  quorum_numerator: 3,
  quorum_denominator: 5,
  members: [
    { status: 'authorized', cc_cold_hex: hash(4), cc_cold_has_script: false, cc_hot_hex: hash(14), cc_hot_has_script: false, expiration_epoch: 900 },
    { status: 'authorized', cc_cold_hex: hash(5), cc_cold_has_script: true, cc_hot_hex: hash(15), cc_hot_has_script: true, expiration_epoch: 700 },
    { status: 'resigned', cc_cold_hex: hash(6), cc_cold_has_script: false, cc_hot_hex: null, cc_hot_has_script: null, expiration_epoch: 900 },
    { status: 'authorized', cc_cold_hex: hash(7), cc_cold_has_script: false, cc_hot_hex: hash(17), cc_hot_has_script: false, expiration_epoch: EPOCH - 1 },
  ],
};

const vote = (voter_role, voter_id, proposalRow, choice, blockTime, over = {}) => ({
  vote_tx_hash: txHash(blockTime % 250),
  voter_role,
  voter_id,
  proposal_id: proposalRow.proposal_id,
  proposal_tx_hash: proposalRow.proposal_tx_hash,
  proposal_index: 0,
  proposal_type: proposalRow.proposal_type,
  epoch_no: EPOCH,
  block_height: 1000 + (blockTime % 1000),
  block_time: blockTime,
  vote: choice,
  meta_url: null,
  meta_hash: null,
  ...over,
});
const VOTES = [
  vote('ConstitutionalCommittee', hotId(14), pc, 'No', at(EPOCH, 10)),
  // A re-vote: only the later one counts.
  vote('ConstitutionalCommittee', hotId(14), pc, 'Yes', at(EPOCH, 20)),
  vote('ConstitutionalCommittee', hotId(15, true), pc, 'Abstain', at(EPOCH, 30)),
  // Expired member: not eligible, not counted.
  vote('ConstitutionalCommittee', hotId(17), pc, 'Yes', at(EPOCH, 40)),
  vote('DRep', drepId(1), pc, 'Yes', at(EPOCH, 50)),
  vote('SPO', poolId(2), hf, 'Yes', at(643, 60)),
  vote('DRep', drepId(1), hf, 'No', at(643, 70)),
];

function routes(extra = {}) {
  const sortRows = (rows, order) => {
    if (!order) return rows;
    const [col, dir] = order.split(',')[0].split('.');
    return [...rows].sort((a, b) => ((a[col] ?? Infinity) - (b[col] ?? Infinity)) * (dir === 'desc' ? -1 : 1));
  };
  return {
    ...chainRoutes,
    proposal_list: (url) => {
      let rows = filterRows(PROPOSALS, url);
      const or = url.searchParams.get('or');
      if (or) {
        const live = (r) => !r.enacted_epoch && !r.ratified_epoch && !r.expired_epoch && !r.dropped_epoch;
        const want = [];
        if (or.includes('and(enacted_epoch.is.null,ratified_epoch.is.null,expired_epoch.is.null,dropped_epoch.is.null)')) want.push(live);
        if (or.startsWith('(enacted_epoch.not.is.null')) want.push((r) => r.enacted_epoch !== null);
        rows = rows.filter((r) => want.some((f) => f(r)));
      }
      return sortRows(rows, url.searchParams.get('order'));
    },
    proposal_voting_summary: (url) => [summaries[url.searchParams.get('_proposal_id')]],
    epoch_params: (url) => {
      const epochs = url.searchParams.get('epoch_no')?.slice(4, -1).split(',').map(Number) ?? [];
      if (url.searchParams.get('protocol_major') === 'gte.9') return [{ epoch_no: 507 }];
      return epochs.map((e) => paramsRow(e, e < 585 ? 9 : 10));
    },
    committee_info: [committeeInfo],
    vote_list: (url) => filterRows(VOTES, url),
    ...extra,
  };
}

test('aggregate figures re-cut from the recorded HardForkInitiation summary', () => {
  const d = drepFigures('HardForkInitiation', hfSummary);
  assert.deepEqual(d, {
    yes: 4158552295928039n,
    no: 50510940457n + 175901106225800n,
    abstain: 13424965345534n,
    notVoted: 1183461492087298n - 175951617166257n,
  });
  const s = spoFigures('HardForkInitiation', hfSummary, 10);
  assert.deepEqual(s, { yes: 10445177677947930n, no: 0n, abstain: 1699130913945099n, notVoted: 9254034669953667n });
  // The SPO denominator is the whole pool distribution.
  assert.equal(s.yes + s.no + s.abstain + s.notVoted, 10445177677947930n + 9254034669953667n + 1699130913945099n);
});

test('SPO figures: bootstrap abstains the silent, NoConfidence counts passive no-confidence as Yes', () => {
  const info = { ...pcSummary, pool_active_yes_vote_power: '10', pool_yes_vote_power: '10', pool_active_no_vote_power: '5', pool_no_vote_power: '85', pool_active_abstain_vote_power: '3', pool_passive_always_abstain_vote_power: '2', pool_passive_always_no_confidence_vote_power: '4' };
  // total = yes+no (95) + explicit abstain (3) + passive abstain (2) = 100
  assert.deepEqual(spoFigures('InfoAction', info, 9), { yes: 10n, no: 5n, abstain: 85n, notVoted: 0n });
  assert.deepEqual(spoFigures('InfoAction', info, 10), { yes: 10n, no: 9n, abstain: 5n, notVoted: 76n });
  const ncInfo = { ...info, pool_yes_vote_power: '14', pool_no_vote_power: '81' };
  assert.deepEqual(spoFigures('NoConfidence', ncInfo, 10), { yes: 14n, no: 5n, abstain: 5n, notVoted: 76n });
  // Koios zeroed the pool tally (it judged pools not to vote): refused, not read as a unanimous abstain.
  assert.throws(() => spoFigures('InfoAction', { ...pcSummary }, 10), (e) => e.code === 'INTERNAL');
});

test('list maps rows, attaches aggregates, pages with an exact total', async () => {
  const { chainData, calls } = provider(routes());
  const page = await chainData.governance.proposals.list({ page: 1, size: 2 });
  assert.equal(page.data.total, PROPOSALS.length);
  assert.equal(page.data.elements.length, 2);
  const listCall = calls.find((c) => c.endpoint === 'proposal_list');
  assert.equal(listCall.url.searchParams.get('order'), 'block_time.desc,proposal_tx_hash.desc,proposal_index.desc');
  assert.equal(listCall.url.searchParams.get('limit'), '2');
  assert.equal(listCall.headers.get('prefer'), 'count=exact');

  const { data } = await chainData.governance.proposals.get(hf.proposal_id);
  assert.equal(data.type, 'HardForkInitiation');
  assert.deepEqual(data.body, { type: 'HardForkInitiation', protocolVersion: { major: 11, minor: 0 } });
  assert.equal(data.lifecycle.status, 'enacted');
  assert.deepEqual(data.lifecycle.expires, { epoch: 644 });
  assert.equal(data.depositReturnAddress, stakeAddr(1));
  const roles = Object.fromEntries(data.voteAggregates.map((a) => [a.role, a]));
  assert.deepEqual(Object.keys(roles).sort(), ['drep', 'spo'], 'a concluded action carries no cc aggregate');
  assert.equal(roles.drep.representation, 'stake');
  assert.equal(roles.drep.totalEligible, String(4158552295928039n + 1183461492087298n + 13424965345534n));
  assert.deepEqual(roles.drep.threshold, { numerator: 3, denominator: 5 });
  assert.deepEqual(roles.spo.threshold, { numerator: 51, denominator: 100 });
});

test('a live action gets a cc count aggregate from the eligible committee', async () => {
  const { chainData } = provider(routes());
  const { data } = await chainData.governance.proposals.get(pc.proposal_id);
  const cc = data.voteAggregates.find((a) => a.role === 'cc');
  // Eligible: members 4 and 5 (6 resigned, 7 expired). 4 re-voted Yes, 5 abstained.
  assert.deepEqual(cc, {
    role: 'cc',
    representation: 'count',
    yes: '1',
    no: '0',
    abstain: '1',
    notVoted: '0',
    totalEligible: '2',
    threshold: { numerator: 3, denominator: 5 },
  });
  assert.equal(data.voteAggregates.find((a) => a.role === 'spo'), undefined, 'minPoolCost is not a security parameter');
  assert.deepEqual(data.body.changes, { minPoolCost: '75000000' });
  assert.equal(data.body.guardrailsScriptHash, 'fa'.repeat(28));
});

test('bodies: UpdateCommittee (Koios NewCommittee), TreasuryWithdrawals with an exact big amount', async () => {
  const { chainData } = provider(routes());
  const u = (await chainData.governance.proposals.get(uc1.proposal_id)).data;
  assert.equal(u.type, 'UpdateCommittee');
  assert.deepEqual(u.body.removed, [{ coldCredential: coldId(9, true) }]);
  assert.deepEqual(u.body.added, [
    { coldCredential: coldId(4), termExpiryEpoch: 799 },
    { coldCredential: coldId(5, true), termExpiryEpoch: 726 },
  ]);
  const t = (await chainData.governance.proposals.get(tw.proposal_id)).data;
  assert.equal(t.lifecycle.status, 'expired');
  assert.equal(t.lifecycle.droppedAt, null, 'an expired action is not reported dropped');
  assert.deepEqual(t.body.withdrawals, [{ stakeAddress: stakeAddr(8), amount: '45000000000000000123' }]);
  assert.equal(t.body.totalAmount, '45000000000000000123');
});

test('filters: type maps UpdateCommittee to NewCommittee; status becomes a PostgREST or', async () => {
  const { chainData, calls } = provider(routes());
  const byType = await chainData.governance.proposals.list({ page: 1, size: 10, type: ['UpdateCommittee'] });
  assert.equal(byType.data.total, 2);
  assert.equal(calls.find((c) => c.endpoint === 'proposal_list').url.searchParams.get('proposal_type'), 'in.(NewCommittee)');
  const live = await chainData.governance.proposals.list({ page: 1, size: 10, status: ['live'] });
  assert.deepEqual(live.data.elements.map((a) => a.id), [pc.proposal_id]);
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 10, status: ['bogus'] }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 10, sort: 'mostYesVotes' }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 10, sort: 'loudest' }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 1001 }), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.list({ page: 0, size: 10 }), 'INVALID_INPUT');
});

test('search: an action id or a tx hash; anything else matches nothing without a request', async () => {
  const { chainData, calls } = provider(routes());
  const byId = await chainData.governance.proposals.list({ page: 1, size: 10, search: tw.proposal_id });
  assert.deepEqual(byId.data.elements.map((a) => a.id), [tw.proposal_id]);
  const byTx = await chainData.governance.proposals.list({ page: 1, size: 10, search: txHash(5).toUpperCase() });
  assert.equal(byTx.data.total, 1);
  const before = calls.length;
  const none = await chainData.governance.proposals.list({ page: 1, size: 10, search: 'drep lovers' });
  assert.deepEqual(none.data, { elements: [], total: 0 });
  assert.equal(calls.length, before);
});

test('committee-lineage aggregates: NoConfidence yes includes always-no-confidence; UpdateCommittee after it uses the no-confidence threshold', async () => {
  const { chainData } = provider(routes());
  const n = (await chainData.governance.proposals.get(nc.proposal_id)).data;
  const drep = n.voteAggregates.find((a) => a.role === 'drep');
  assert.equal(drep.yes, String(534967421824635n + 138316910791856n));
  assert.equal(drep.no, '490675179099982');
  const spo = n.voteAggregates.find((a) => a.role === 'spo');
  assert.deepEqual([spo.yes, spo.no, spo.abstain, spo.notVoted, spo.totalEligible], ['320', '100', '90', '490', '1000']);
  const u = (await chainData.governance.proposals.get(uc2.proposal_id)).data;
  const t = Object.fromEntries(u.voteAggregates.map((a) => [a.role, a.threshold]));
  assert.deepEqual(t, { drep: { numerator: 3, denominator: 5 }, spo: { numerator: 51, denominator: 100 } });
  const before = (await chainData.governance.proposals.get(uc1.proposal_id)).data;
  // uc1 tallied at 580, before bootstrap ended (protocol 9): DRep threshold 0.
  assert.deepEqual(before.voteAggregates.find((a) => a.role === 'drep').threshold, { numerator: 0, denominator: 1 });
  const bootSpo = before.voteAggregates.find((a) => a.role === 'spo');
  assert.deepEqual([bootSpo.abstain, bootSpo.notVoted], ['600', '0']);
});

test('getEnacted walks the lineage; UpdateCommittee and NoConfidence share the committee one', async () => {
  const { chainData } = provider(routes());
  assert.equal((await chainData.governance.proposals.getEnacted('committee')).data.id, uc2.proposal_id);
  assert.equal((await chainData.governance.proposals.getEnacted('hardFork')).data.id, hf.proposal_id);
  assert.equal((await chainData.governance.proposals.getEnacted('constitution')).data, null, 'genesis case');
  await rejectsWith(chainData.governance.proposals.getEnacted('treasury'), 'INVALID_INPUT');
});

test('voter context: single action, listing annotation, voted filter; cc cold refused', async () => {
  const { chainData } = provider(routes());
  const mine = await chainData.governance.proposals.get(pc.proposal_id, { voterId: drepId(1) });
  assert.equal(mine.data.myVote.choice, 'yes');
  assert.deepEqual(mine.data.myVote.voter, { role: 'drep', id: drepId(1), isScriptBased: false });
  const other = await chainData.governance.proposals.get(pc.proposal_id, { voterId: drepId(3) });
  assert.equal(other.data.myVote, null);
  const hot = await chainData.governance.proposals.get(pc.proposal_id, { voterId: hotId(14) });
  assert.equal(hot.data.myVote.choice, 'yes', 'the later of two votes');
  assert.equal(hot.data.myVote.voter.cold, coldId(4));
  await rejectsWith(chainData.governance.proposals.get(pc.proposal_id, { voterId: coldId(4) }), 'CAPABILITY_UNSUPPORTED');
  await rejectsWith(chainData.governance.proposals.get(pc.proposal_id, { voterId: 'bob' }), 'INVALID_INPUT');
  const voted = await chainData.governance.proposals.list({ page: 1, size: 10, voterId: drepId(1), voted: true });
  assert.deepEqual(voted.data.elements.map((a) => a.id).sort(), [hf.proposal_id, pc.proposal_id].sort());
  assert.ok(voted.data.elements.every((a) => a.myVote !== null));
  const notVoted = await chainData.governance.proposals.list({ page: 1, size: 10, voterId: drepId(1), voted: false });
  assert.equal(notVoted.data.total, PROPOSALS.length - 2);
  assert.ok(notVoted.data.elements.every((a) => a.myVote === null));
  await rejectsWith(chainData.governance.proposals.list({ page: 1, size: 10, voted: true }), 'INVALID_INPUT');
});

test('listVotes: latest per voter, cold resolved, paged with total', async () => {
  const { chainData } = provider(routes());
  const page1 = await chainData.governance.proposals.listVotes(pc.proposal_id, { page: 1, size: 2 });
  const page2 = await chainData.governance.proposals.listVotes(pc.proposal_id, { page: 2, size: 2 });
  assert.equal(page1.data.total, 4, 'the re-vote is collapsed');
  assert.equal(page1.data.elements.length + page2.data.elements.length, 4);
  const all = [...page1.data.elements, ...page2.data.elements];
  const cc = all.find((v) => v.voter.role === 'cc' && v.voter.hot === hotId(14));
  assert.equal(cc.choice, 'yes');
  assert.equal(cc.voter.cold, coldId(4));
  assert.match(cc.txRef.txHash, /^[0-9a-f]{64}$/);
  await rejectsWith(chainData.governance.proposals.listVotes(actionId(99), { page: 1, size: 2 }), 'NOT_FOUND');
});

test('listActivity derives the lifecycle feed', async () => {
  const { chainData } = provider(routes());
  const { data } = await chainData.governance.proposals.listActivity(hf.proposal_id, { page: 1, size: 10 });
  assert.deepEqual(data.elements.map((e) => e.status), ['live', 'ratified', 'enacted']);
  assert.equal(data.total, 3);
});

test('a malformed action id is INVALID_INPUT; an unknown one NOT_FOUND', async () => {
  const { chainData } = provider(routes());
  await rejectsWith(chainData.governance.proposals.get('gov_action1xyz'), 'INVALID_INPUT');
  await rejectsWith(chainData.governance.proposals.get(actionId(77)), 'NOT_FOUND');
});

test('ParameterChange bodies carry every parameter in contract names and shapes', () => {
  // Shapes recorded from mainnet proposal_description (execution units, cost models).
  const update = {
    maxTxExecutionUnits: { steps: 10000000000, memory: 16500000 },
    maxBlockExecutionUnits: { steps: 20000000000, memory: 72000000 },
    costModels: { PlutusV3: [100788, 420, 1, 1, -900] },
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    executionUnitPrices: { priceMemory: 0.0577, priceSteps: { numerator: 721, denominator: 10000000 } },
    txFeePerByte: 44,
    futureParam: { a: 1 },
  };
  const { body, paramKeys } = decodeBody('ParameterChange', { tag: 'ParameterChange', contents: [null, update, null] }, 'mainnet');
  assert.deepEqual(body.changes, {
    maxTxExecutionUnits: { memory: 16500000, steps: 10000000000 },
    maxBlockExecutionUnits: { memory: 72000000, steps: 20000000000 },
    costModels: { PlutusV3: [100788, 420, 1, 1, -900] },
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    executionUnitPrices: { memory: { numerator: 577, denominator: 10000 }, steps: { numerator: 721, denominator: 10000000 } },
    minFeeA: 44,
    futureParam: { a: 1 },
  });
  assert.equal(paramKeys.length, Object.keys(update).length);
  const only = decodeBody('ParameterChange', { tag: 'ParameterChange', contents: [null, { costModels: { PlutusV1: [1, 2] } }, null] }, 'mainnet');
  assert.deepEqual(only.body.changes, { costModels: { PlutusV1: [1, 2] } }, 'a change of only a new parameter is not empty');
  assert.throws(
    () => decodeBody('ParameterChange', { tag: 'ParameterChange', contents: [null, { costModels: { PlutusV1: { 'addInteger-cpu': 1 } } }, null] }, 'mainnet'),
    (e) => e.code === 'INTERNAL',
  );
  assert.throws(
    () => decodeBody('ParameterChange', { tag: 'ParameterChange', contents: [null, { maxTxExecutionUnits: { memory: 1, steps: '90071992547409930' } }, null] }, 'mainnet'),
    (e) => e.code === 'INTERNAL',
  );
});
