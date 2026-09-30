import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider, capabilities } from '../dist/index.js';
import { toRatio, continuedFraction } from '../dist/governance/proposals/ratio.js';
import { decodeBody, paramGroups } from '../dist/governance/proposals/body.js';
import { deriveStatus, toGovAction, toLifecycle } from '../dist/governance/proposals/rows.js';
import {
  assembleAggregates,
  ccFigures,
  drepFigures,
  spoFigures,
  thresholds,
} from '../dist/governance/proposals/aggregates.js';
import { toVoteRecord } from '../dist/governance/proposals/voters.js';
import {
  decodeStakeAddress,
  encodeCommitteeColdId,
  encodeCommitteeHotId,
  encodeDRepId,
  encodeGovActionId,
} from '../dist/ids.js';

const H = (n) => n.toString(16).padStart(2, '0').repeat(28);
const TX = (n) => n.toString(16).padStart(2, '0').repeat(32);

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
const provider = (routes = []) => {
  const db = fakeDb(routes);
  return { db, api: createDbSyncProvider({ network: 'preview', db }).chainData.governance.proposals };
};

/* -- ratios ----------------------------------------------------------------- */

test('thresholds: short decimals are exact, float artefacts go through continued fractions', () => {
  assert.deepEqual(toRatio(0.67), { numerator: 67, denominator: 100 });
  assert.deepEqual(toRatio('0.51'), { numerator: 51, denominator: 100 });
  assert.deepEqual(toRatio(0.6666666666666666), { numerator: 2, denominator: 3 });
  assert.deepEqual(toRatio('0.000050'), { numerator: 1, denominator: 20000 });
  assert.deepEqual(toRatio('0.0010'), { numerator: 1, denominator: 1000 });
  assert.deepEqual(toRatio(1), { numerator: 1, denominator: 1 });
  assert.deepEqual(toRatio({ numerator: 2, denominator: 3 }), { numerator: 2, denominator: 3 });
  assert.deepEqual(toRatio({ numerator: '134', denominator: '200' }), { numerator: 67, denominator: 100 });
  assert.equal(toRatio({ numerator: 1, denominator: 0 }), undefined);
  assert.equal(toRatio('abc'), undefined);
  assert.equal(toRatio(null), undefined);
  assert.equal(toRatio(Number.NaN), undefined);
  assert.deepEqual(continuedFraction(0.333333333333), { numerator: 1, denominator: 3 });
});

/* -- status and lifecycle ----------------------------------------------------- */

const row = (over = {}) => ({
  id: '1', tx_hash: TX(1), index: 0, db_type: 'InfoAction', description: '{"tag":"InfoAction"}',
  deposit: '100000000000', return_address: 'stake_test1uz7ve3j0dwpra3d082hfaa0k3h87svruhhnxnhpw0fk06csj0y8h6',
  expiration: 706, ratified_epoch: null, enacted_epoch: null, dropped_epoch: null, expired_epoch: null,
  tally_epoch: 700, sub_epoch: 675, sub_slot: '58320000', sub_block: '2500000', sub_time: new Date('2024-09-01T00:00:00Z'),
  anchor_url: null, anchor_hash: null, prev_tx_hash: null, prev_index: null, ...over,
});

test('status precedence: enacted > ratified > expired > dropped > live', () => {
  const s = (o) => deriveStatus({ ratified_epoch: null, enacted_epoch: null, dropped_epoch: null, expired_epoch: null, ...o });
  assert.equal(s({}), 'live');
  assert.equal(s({ ratified_epoch: 5, enacted_epoch: 6 }), 'enacted');
  assert.equal(s({ ratified_epoch: 5 }), 'ratified');
  assert.equal(s({ expired_epoch: 706, dropped_epoch: 707 }), 'expired');
  assert.equal(s({ dropped_epoch: 707 }), 'dropped');
});

test('an expired action keeps droppedAt null although db-sync sets dropped_epoch', () => {
  const l = toLifecycle(row({ expired_epoch: 706, dropped_epoch: 707 }));
  assert.equal(l.status, 'expired');
  assert.deepEqual(l.expiredAt, { epoch: 706 });
  assert.equal(l.droppedAt, null);
  const d = toLifecycle(row({ dropped_epoch: 707 }));
  assert.equal(d.status, 'dropped');
  assert.deepEqual(d.droppedAt, { epoch: 707 });
  assert.equal(d.expiredAt, null);
});

test('lifecycle stamps: submitted carries slot, block and UTC time; others are epoch-only', () => {
  const l = toLifecycle(row({ ratified_epoch: 690, enacted_epoch: 691 }));
  assert.deepEqual(l.submitted, { epoch: 675, slot: 58320000, block: 2500000, time: '2024-09-01T00:00:00.000Z' });
  assert.deepEqual(l.submittedTx, { txHash: TX(1), index: 0, block: 2500000, at: l.submitted });
  assert.deepEqual(l.expires, { epoch: 706 });
  assert.deepEqual(l.enactedAt, { epoch: 691 });
});

test('GovAction mapping: CIP-129 ids, explicit nulls, previous action ref', () => {
  const { action } = toGovAction(row({ prev_tx_hash: TX(9), prev_index: '2', anchor_url: 'ipfs://x', anchor_hash: 'ab' }), 'preview');
  assert.equal(action.id, encodeGovActionId(TX(1), 0));
  assert.deepEqual(action.previousAction, { id: encodeGovActionId(TX(9), 2), txHash: TX(9), index: 2 });
  assert.deepEqual(action.anchor, { url: 'ipfs://x', dataHash: 'ab' });
  assert.equal(action.deposit, '100000000000');
  const bare = toGovAction(row(), 'preview').action;
  assert.equal(bare.previousAction, null);
  assert.equal(bare.anchor, null);
  assert.equal('voteAggregates' in bare, false);
});

/* -- bodies ------------------------------------------------------------------- */

test('ParameterChange: contract names, lovelace strings, exact ratios, extras kept', () => {
  const description = JSON.stringify({
    tag: 'ParameterChange',
    contents: [
      null,
      { stakeAddressDeposit: 2000000, txFeePerByte: 44, minPoolCost: 170000000, monetaryExpansion: 0.003, maxBlockExecutionUnits: { steps: 20000000000, memory: 62000000 } },
      'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64',
    ],
  });
  const withThresholds = description.replace(
    '"stakeAddressDeposit"',
    '"dRepVotingThresholds":{"ppGovGroup":0.76,"ppNetworkGroup":0.68,"committeeNormal":0.68,"ppEconomicGroup":0.68,"ppTechnicalGroup":0.68,"hardForkInitiation":0.61,"motionNoConfidence":0.68,"treasuryWithdrawal":0.68,"updateToConstitution":0.76,"committeeNoConfidence":0.61},"stakeAddressDeposit"',
  );
  const { body, paramKeys } = decodeBody('ParameterChange', withThresholds, 'preview');
  assert.equal(body.type, 'ParameterChange');
  assert.equal(body.changes.keyDeposit, '2000000');
  assert.equal(body.changes.minFeeA, 44);
  assert.equal(body.changes.minPoolCost, '170000000');
  assert.deepEqual(body.changes.monetaryExpansion, { numerator: 3, denominator: 1000 });
  assert.deepEqual(body.changes.drepThresholds.ppGovGroup, { numerator: 19, denominator: 25 });
  assert.deepEqual(body.changes.maxBlockExecutionUnits, { steps: 20000000000, memory: 62000000 });
  assert.equal(body.guardrailsScriptHash, 'fa24fb305126805cf2164c161d852a0e7330cf988f1fe558cf7d4a64');
  assert.ok(paramKeys.includes('dRepVotingThresholds') && paramKeys.includes('minPoolCost'));
});

test('ParameterChange: every current parameter decodes to its contract name and shape', () => {
  // Keys and value shapes as they appear in preview's gov_action_proposal.description.
  const update = {
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1900,
    maxTxExecutionUnits: { steps: 14900000000, memory: 40000000 },
    collateralPercentage: 200,
    maxCollateralInputs: 5,
    costModels: { PlutusV3: [100788, 420, 1, 1] },
    executionUnitPrices: { priceSteps: 0.0001389, priceMemory: 0.0577 },
    minFeeRefScriptCostPerByte: 15,
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 250,
    treasuryCut: 0.1,
    poolPledgeInfluence: 0.3,
  };
  const description = JSON.stringify({ tag: 'ParameterChange', contents: [null, update, null] });
  const { body } = decodeBody('ParameterChange', description, 'preview');
  assert.deepEqual(body.changes, {
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1900,
    maxTxExecutionUnits: { memory: 40000000, steps: 14900000000 },
    collateralPercentage: 200,
    maxCollateralInputs: 5,
    costModels: { PlutusV3: [100788, 420, 1, 1] },
    executionUnitPrices: {
      memory: { numerator: 577, denominator: 10000 },
      steps: { numerator: 1389, denominator: 10000000 },
    },
    minFeeRefScriptCostPerByte: { numerator: 15, denominator: 1 },
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 250,
    treasuryCut: { numerator: 1, denominator: 10 },
    poolPledgeInfluence: { numerator: 3, denominator: 10 },
  });
});

test('TreasuryWithdrawals: amounts above 2^53 stay exact; addresses from the credential', () => {
  const huge = '45000000000000001';
  const description = `{"tag":"TreasuryWithdrawals","contents":[[[{"network":"Testnet","credential":{"keyHash":"${H(3)}"}},${huge}],[{"network":"Testnet","credential":{"scriptHash":"${H(4)}"}},5]],null]}`;
  const { body } = decodeBody('TreasuryWithdrawals', description, 'preview');
  assert.equal(body.withdrawals[0].amount, huge);
  assert.equal(body.totalAmount, '45000000000000006');
  assert.deepEqual(decodeStakeAddress(body.withdrawals[0].stakeAddress, 'preview'), { hash: H(3), isScript: false });
  assert.deepEqual(decodeStakeAddress(body.withdrawals[1].stakeAddress, 'preview'), { hash: H(4), isScript: true });
  assert.equal(body.guardrailsScriptHash, null);
});

test('UpdateCommittee: cold credentials in CIP-129, terms, exact quorum', () => {
  const description = `{"tag":"UpdateCommittee","contents":[null,[{"scriptHash":"${H(5)}"}],{"keyHash-${H(6)}":1500},{"numerator":2,"denominator":3}]}`;
  const { body } = decodeBody('NewCommittee', description, 'preview');
  assert.equal(body.type, 'UpdateCommittee');
  assert.deepEqual(body.removed, [{ coldCredential: encodeCommitteeColdId(H(5), true) }]);
  assert.deepEqual(body.added, [{ coldCredential: encodeCommitteeColdId(H(6), false), termExpiryEpoch: 1500 }]);
  assert.deepEqual(body.quorum, { numerator: 2, denominator: 3 });
});

test('NewConstitution, HardForkInitiation, NoConfidence, InfoAction', () => {
  const c = decodeBody('NewConstitution', '{"tag":"NewConstitution","contents":[null,{"anchor":{"url":"https://c","dataHash":"AB"},"script":null}]}', 'preview').body;
  assert.deepEqual(c, { type: 'NewConstitution', anchor: { url: 'https://c', dataHash: 'ab' }, guardrailsScriptHash: null });
  const h = decodeBody('HardForkInitiation', '{"tag":"HardForkInitiation","contents":[null,{"major":10,"minor":0}]}', 'preview').body;
  assert.deepEqual(h, { type: 'HardForkInitiation', protocolVersion: { major: 10, minor: 0 } });
  assert.deepEqual(decodeBody('NoConfidence', '{}', 'preview').body, { type: 'NoConfidence' });
  assert.deepEqual(decodeBody('InfoAction', '{}', 'preview').body, { type: 'InfoAction' });
});

test('an undecodable body is refused, not passed through untyped', () => {
  assert.throws(() => decodeBody('HardForkInitiation', '{"tag":"HardForkInitiation","contents":[null,"x"]}', 'preview'), { code: 'INTERNAL' });
  assert.throws(() => decodeBody('Surveys', '{}', 'preview'), { code: 'INTERNAL' });
});

test('parameter groups decide the thresholds', () => {
  assert.deepEqual(paramGroups(['minPoolCost']), { drep: ['ppEconomicGroup'], security: false });
  assert.deepEqual(paramGroups(['maxTxSize']), { drep: ['ppNetworkGroup'], security: true });
  assert.deepEqual(paramGroups(['govActionDeposit']), { drep: ['ppGovGroup'], security: true });
  const unknown = paramGroups(['someFutureParam']);
  assert.equal(unknown.drep.length, 4);
  assert.equal(unknown.security, true);
});

/* -- figures ------------------------------------------------------------------- */

const drepRow = { id: '1', e: 10, has_distr: true, active: '1000', no_confidence: '50', yes: '300', no: '100', abstain: '200' };

test('DRep figures: always-no-confidence is Yes on NoConfidence, No elsewhere; silence is notVoted', () => {
  assert.deepEqual(drepFigures('InfoAction', drepRow), { yes: 300n, no: 150n, abstain: 200n, notVoted: 400n });
  assert.deepEqual(drepFigures('NoConfidence', drepRow), { yes: 350n, no: 100n, abstain: 200n, notVoted: 400n });
  assert.deepEqual(drepFigures('InfoAction', { ...drepRow, yes: null, no: null, abstain: null }), { yes: 0n, no: 50n, abstain: 0n, notVoted: 1000n });
  assert.throws(() => drepFigures('InfoAction', { ...drepRow, has_distr: false }), { code: 'STALE_DATA' });
});

const spoRow = { id: '1', e: 10, pools: '5', total: '1000', yes: '100', no: '50', abstain: '25', silent: '825', silent_no_confidence: '75', silent_abstain: '150' };

test('SPO figures follow the ledger default for a pool that did not vote', () => {
  assert.deepEqual(spoFigures('ParameterChange', spoRow, 10), { yes: 100n, no: 125n, abstain: 175n, notVoted: 600n });
  assert.deepEqual(spoFigures('NoConfidence', spoRow, 10), { yes: 175n, no: 50n, abstain: 175n, notVoted: 600n });
  assert.deepEqual(spoFigures('HardForkInitiation', spoRow, 10), { yes: 100n, no: 50n, abstain: 25n, notVoted: 825n });
  // Bootstrap: silence abstains, except on a hard fork.
  assert.deepEqual(spoFigures('ParameterChange', spoRow, 9), { yes: 100n, no: 50n, abstain: 850n, notVoted: 0n });
  assert.deepEqual(spoFigures('HardForkInitiation', spoRow, 9), { yes: 100n, no: 50n, abstain: 25n, notVoted: 825n });
  assert.throws(() => spoFigures('ParameterChange', { ...spoRow, pools: 0 }, 10), { code: 'STALE_DATA' });
});

test('committee figures count eligible members only', () => {
  assert.deepEqual(ccFigures({ eligible: '7', yes: '4', no: '1', abstain: '1' }), { yes: 4n, no: 1n, abstain: 1n, notVoted: 1n });
});

/* -- thresholds ----------------------------------------------------------------- */

const params = {
  e: 10, protocol_major: 10,
  dvt_motion_no_confidence: 0.67, dvt_committee_normal: 0.67, dvt_committee_no_confidence: 0.6, dvt_update_to_constitution: 0.75,
  dvt_hard_fork_initiation: 0.6, dvt_p_p_network_group: 0.67, dvt_p_p_economic_group: 0.67, dvt_p_p_technical_group: 0.67,
  dvt_p_p_gov_group: 0.75, dvt_treasury_withdrawal: 0.67,
  pvt_motion_no_confidence: 0.51, pvt_committee_normal: 0.51, pvt_committee_no_confidence: 0.51, pvt_hard_fork_initiation: 0.51,
  pvtpp_security_group: 0.51,
};
const quorum = { numerator: 2, denominator: 3 };
const target = (dbType, paramKeys) => ({ id: '1', dbType, epoch: 10, ...(paramKeys ? { paramKeys } : {}) });
const r = (n, d) => ({ numerator: n, denominator: d });

test('thresholds by type and role, as the ledger assigns them', () => {
  const c = { exists: true, quorum };
  assert.deepEqual(thresholds(target('NoConfidence'), params, c), { drep: r(67, 100), spo: r(51, 100) });
  assert.deepEqual(thresholds(target('NewCommittee'), params, c), { drep: r(67, 100), spo: r(51, 100) });
  assert.deepEqual(thresholds(target('NewCommittee'), params, { exists: false }), { drep: r(3, 5), spo: r(51, 100) });
  assert.deepEqual(thresholds(target('NewConstitution'), params, c), { drep: r(3, 4), cc: quorum });
  assert.deepEqual(thresholds(target('TreasuryWithdrawals'), params, c), { drep: r(67, 100), cc: quorum });
  assert.deepEqual(thresholds(target('HardForkInitiation'), params, c), { drep: r(3, 5), spo: r(51, 100), cc: quorum });
  assert.deepEqual(thresholds(target('InfoAction'), params, c), { drep: r(1, 1), spo: r(1, 1), cc: r(1, 1) });
});

test('ParameterChange: the highest group threshold; pools only on the security group; bootstrap DRep 0', () => {
  const c = { exists: true, quorum };
  assert.deepEqual(thresholds(target('ParameterChange', ['minPoolCost']), params, c), { drep: r(67, 100), cc: quorum });
  assert.deepEqual(thresholds(target('ParameterChange', ['minPoolCost', 'govActionDeposit']), params, c), {
    drep: r(3, 4), spo: r(51, 100), cc: quorum,
  });
  assert.deepEqual(thresholds(target('ParameterChange', ['maxTxSize']), { ...params, protocol_major: 9 }, c), {
    drep: r(0, 1), spo: r(51, 100), cc: quorum,
  });
});

test('assembled aggregates balance, and the committee is left out when none is in force', () => {
  const targets = [target('TreasuryWithdrawals')];
  const cc = { id: '1', e: 10, has_committee: true, quorum_numerator: '2', quorum_denominator: '3', eligible: '3', yes: '2', no: '0', abstain: '0' };
  const out = assembleAggregates(targets, { drep: [drepRow], spo: [], cc: [cc], params: [params] }).get('1');
  assert.deepEqual(out.map((a) => [a.role, a.representation]), [['drep', 'stake'], ['cc', 'count']]);
  for (const a of out) assert.equal(BigInt(a.yes) + BigInt(a.no) + BigInt(a.abstain) + BigInt(a.notVoted), BigInt(a.totalEligible));
  assert.equal(out[0].totalEligible, '1050');
  const none = assembleAggregates(targets, { drep: [drepRow], spo: [], cc: [{ ...cc, has_committee: false }], params: [params] }).get('1');
  assert.deepEqual(none.map((a) => a.role), ['drep']);
  assert.throws(() => assembleAggregates(targets, { drep: [drepRow], spo: [], cc: [cc], params: [] }), { code: 'STALE_DATA' });
});

/* -- votes ---------------------------------------------------------------------- */

const voteRow = (over) => ({
  proposal_id: '1', voter_role: 'DRep', vote: 'Yes', vote_index: 0, vote_tx_hash: TX(7), vote_block_no: '10',
  vote_epoch: 700, vote_slot: '99', vote_time: new Date('2025-01-01T00:00:00Z'),
  drep_raw: null, drep_script: null, pool_raw: null, hot_raw: null, hot_script: null, cold_raw: null, cold_script: null,
  vote_anchor_url: null, vote_anchor_hash: null, ...over,
});

test('vote records: CIP-129 DRep, committee hot with resolved cold, null anchor', () => {
  const d = toVoteRecord(voteRow({ drep_raw: H(1), drep_script: false }));
  assert.deepEqual(d.voter, { role: 'drep', id: encodeDRepId(H(1), false), isScriptBased: false });
  assert.equal(d.anchor, null);
  assert.equal(d.choice, 'yes');
  const c = toVoteRecord(voteRow({ voter_role: 'ConstitutionalCommittee', vote: 'Abstain', hot_raw: H(2), hot_script: true, cold_raw: H(3), cold_script: true, vote_anchor_url: 'u', vote_anchor_hash: 'ff' }));
  assert.deepEqual(c.voter, { role: 'cc', hot: encodeCommitteeHotId(H(2), true), cold: encodeCommitteeColdId(H(3), true), isScriptBased: true });
  assert.deepEqual(c.anchor, { url: 'u', dataHash: 'ff' });
  const unresolved = toVoteRecord(voteRow({ voter_role: 'ConstitutionalCommittee', hot_raw: H(2), hot_script: false }));
  assert.equal('cold' in unresolved.voter, false);
});

/* -- API surface ---------------------------------------------------------------- */

test('declaration: sorts, filters, aggregates and voter context on a listing', () => {
  const caps = capabilities();
  assert.deepEqual(caps.sorts.proposals, ['newest', 'oldest', 'soonestToExpire', 'mostYesVotes', 'highestParticipation']);
  assert.deepEqual(caps.filters.proposals, ['type', 'status']);
  assert.deepEqual(caps.voteAggregate, ['stake', 'count']);
  assert.ok(caps.optionalArguments.includes('proposals.voterContextOnList'));
});

test('txHash#index is INVALID_INPUT and never reaches the database', async () => {
  const { api, db } = provider();
  await assert.rejects(api.get(`${TX(1)}#0`), { code: 'INVALID_INPUT' });
  await assert.rejects(api.listVotes(`${TX(1)}#0`, { page: 1, size: 5 }), { code: 'INVALID_INPUT' });
  assert.equal(db.calls.length, 0);
});

test('a well-formed unknown id is NOT_FOUND', async () => {
  const { api } = provider([['FROM tx t', []]]);
  await assert.rejects(api.get(encodeGovActionId(TX(9), 3)), { code: 'NOT_FOUND' });
});

test('methods reject rather than throw synchronously', () => {
  const { api } = provider();
  for (const call of [() => api.get(42), () => api.list(null), () => api.getEnacted('Committee'), () => api.listActivity('x', { page: 0, size: 1 })]) {
    const p = call();
    assert.ok(p instanceof Promise);
    p.catch(() => {});
  }
});

test('list validation: unknown sort, status, type; voted needs voterId; bad paging', async () => {
  const { api, db } = provider();
  await assert.rejects(api.list({ page: 1, size: 5, sort: 'random' }), { code: 'INVALID_INPUT' });
  await assert.rejects(api.list({ page: 1, size: 5, status: ['pending'] }), { code: 'INVALID_INPUT' });
  await assert.rejects(api.list({ page: 1, size: 5, type: ['NewCommittee'] }), { code: 'INVALID_INPUT' });
  await assert.rejects(api.list({ page: 1, size: 5, voted: true }), { code: 'INVALID_INPUT' });
  await assert.rejects(api.list({ page: 0, size: 5 }), { code: 'INVALID_INPUT' });
  await assert.rejects(api.list({ page: 1, size: 5, voterId: 'drep1xyz' }), { code: 'INVALID_INPUT' });
  assert.equal(db.calls.length, 0);
});

test('an unmatched search term is an empty page, not an error', async () => {
  const { api, db } = provider();
  const r = await api.list({ page: 1, size: 5, search: 'treasury' });
  assert.deepEqual(r.data, { elements: [], total: 0 });
  assert.equal(db.calls.length, 0);
});

test('filter and search values are bound, never interpolated', async () => {
  const { api, db } = provider([['count(*) AS n', [{ n: '0' }]], ['total_count', []]]);
  await api.list({ page: 1, size: 5, type: ['UpdateCommittee'], status: ['live', 'dropped'], search: TX(5), sort: 'soonestToExpire' });
  const [main] = db.calls;
  assert.ok(!main.sql.includes(TX(5)) && !main.sql.includes('NewCommittee'));
  assert.ok(main.params.some((p) => Array.isArray(p) && p.includes('NewCommittee')));
  assert.ok(main.params.includes(TX(5)));
  assert.match(main.sql, /ORDER BY g\.expiration ASC NULLS LAST, g\.id ASC/);
});

test('getEnacted is keyed by lineage: committee covers UpdateCommittee AND NoConfidence', async () => {
  const { api, db } = provider([['enacted_epoch IS NOT NULL', (params) => (params[0].includes('NoConfidence') ? [{ tx_hash: TX(4), index: 1 }] : [])]]);
  const committee = await api.getEnacted('committee');
  assert.deepEqual(db.calls[0].params[0], ['NewCommittee', 'NoConfidence']);
  assert.deepEqual(committee.data, { id: encodeGovActionId(TX(4), 1), txHash: TX(4), index: 1 });
  const genesis = await api.getEnacted('constitution');
  assert.equal(genesis.data, null);
  await assert.rejects(api.getEnacted('NoConfidence'), { code: 'INVALID_INPUT' });
  await assert.rejects(api.getEnacted('__proto__'), { code: 'INVALID_INPUT' });
});

test('listActivity: submission, then each lifecycle step, in order', async () => {
  const { api } = provider([['FROM tx t', [row({ ratified_epoch: 690, enacted_epoch: 691 })]]]);
  const r = await api.listActivity(encodeGovActionId(TX(1), 0), { page: 1, size: 10 });
  assert.deepEqual(r.data.elements.map((e) => [e.status, e.at.epoch]), [['live', 675], ['ratified', 690], ['enacted', 691]]);
  assert.equal(r.data.total, 3);
});
