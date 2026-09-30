/**
 * Network, accounts, transactions and identifiers, over a fake Blockfrost.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { decodeCbor, drepCertAnchorOf, proposalAnchorsOf, votesOf } from '../dist/cbor.js';
import { fromBlockfrostDRepId } from '../dist/ids.js';
import { decodeBody } from '../dist/governance/proposals/body.js';
import { parseExactJson } from '../dist/json.js';
import { eraOf, mapGenesisParams, mapProtocolParams } from '../dist/network.js';
import { toRatio } from '../dist/ratio.js';
import {
  EPOCH,
  GENESIS,
  PARAMS,
  PREDEFINED,
  VOTE,
  VOTER,
  cbor,
  drepCip105,
  drepId,
  drepRow,
  hash28,
  hash32,
  paged,
  poolId,
  provider,
  rejectsWith,
  stakeAddr,
  timeIn,
  txCbor,
} from './fake.mjs';

/* -- network ---------------------------------------------------------------- */

test('protocol parameters: typed, named, thresholds as exact ratios', () => {
  const p = mapProtocolParams(PARAMS);
  assert.equal(p.epoch, 600);
  assert.deepEqual(p.protocolVersion, { major: 10, minor: 0 });
  assert.equal(p.govActionLifetime, 6);
  assert.equal(p.govActionDeposit, '100000000000');
  assert.equal(p.drepActivity, 20);
  assert.equal(p.maxValSize, 5000);
  assert.deepEqual(p.drepThresholds.motionNoConfidence, { numerator: 67, denominator: 100 });
  assert.deepEqual(p.drepThresholds.committeeNoConfidence, { numerator: 3, denominator: 5 });
  assert.deepEqual(p.poolThresholds.ppSecurityGroup, { numerator: 51, denominator: 100 });
  assert.deepEqual(mapProtocolParams({ ...PARAMS, dvt_p_p_gov_group: 0.6666666666666666 }).drepThresholds.ppGovGroup, {
    numerator: 2,
    denominator: 3,
  });
  assert.equal(mapProtocolParams({ ...PARAMS, drep_activity: null }), undefined, 'a pre-Conway row is not params');
});

test('protocol parameters: every current-era parameter, typed', () => {
  const { epoch, protocolVersion, drepThresholds, poolThresholds, ...rest } = mapProtocolParams(PARAMS);
  assert.deepEqual(rest, {
    govActionLifetime: 6,
    govActionDeposit: '100000000000',
    drepDeposit: '500000000',
    drepActivity: 20,
    committeeMinSize: 5,
    committeeMaxTermLength: 146,
    minFeeA: 44,
    minFeeB: 155381,
    minFeeRefScriptCostPerByte: { numerator: 15, denominator: 1 },
    keyDeposit: '2000000',
    poolDeposit: '500000000',
    coinsPerUtxoByte: '4310',
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    maxTxSize: 16384,
    maxValSize: 5000,
    maxTxExecutionUnits: { memory: 16500000, steps: 10000000000 },
    maxBlockExecutionUnits: { memory: 72000000, steps: 20000000000 },
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    executionUnitPrices: { memory: { numerator: 577, denominator: 10000 }, steps: { numerator: 721, denominator: 10000000 } },
    costModels: { PlutusV1: [100788, 420, 1, 1, 1000], PlutusV2: [100788, 420, 1, 1, -900], PlutusV3: [100788, 420, 1, 1, 1000, 173] },
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    poolPledgeInfluence: { numerator: 3, denominator: 10 },
    monetaryExpansion: { numerator: 3, denominator: 1000 },
    treasuryCut: { numerator: 1, denominator: 5 },
    minPoolCost: '170000000',
  });
  assert.deepEqual(toRatio(7.21e-5), { numerator: 721, denominator: 10000000 });
});

test('cost models come from the raw arrays; a missing language is absent; bad input refused', () => {
  assert.deepEqual(mapProtocolParams({ ...PARAMS, cost_models_raw: { PlutusV1: [1], PlutusV3: [2, 3] } }).costModels, { PlutusV1: [1], PlutusV3: [2, 3] });
  assert.equal(mapProtocolParams({ ...PARAMS, cost_models_raw: null }), undefined, 'the named map is never a fallback');
  assert.equal(mapProtocolParams({ ...PARAMS, cost_models_raw: { PlutusV1: [1.5] } }), undefined);
  for (const key of ['max_block_size', 'max_block_ex_steps', 'price_mem', 'a0', 'e_max', 'min_pool_cost']) {
    assert.equal(mapProtocolParams({ ...PARAMS, [key]: null }), undefined, key);
  }
});

test('execution budgets: strings must be safe integers', () => {
  assert.throws(() => mapProtocolParams({ ...PARAMS, max_block_ex_steps: '90071992547409930' }), (e) => e.code === 'INTERNAL');
  assert.throws(() => mapProtocolParams({ ...PARAMS, max_tx_ex_mem: '1.5' }), (e) => e.code === 'INTERNAL');
});

test('genesis parameters: typed, network checked', async () => {
  const { chainData } = provider({ '/genesis': GENESIS });
  const { data, meta } = await chainData.network.getGenesisParams();
  assert.equal(meta.provider, 'blockfrost');
  assert.deepEqual(data, {
    networkMagic: 764824073,
    networkId: 'Mainnet',
    systemStart: '2017-09-23T21:44:51Z',
    epochLength: 432000,
    slotLength: 1,
    activeSlotsCoefficient: { numerator: 1, denominator: 20 },
    securityParam: 2160,
    slotsPerKesPeriod: 129600,
    maxKesEvolutions: 62,
    updateQuorum: 5,
    maxLovelaceSupply: '45000000000000000',
  });
  assert.equal(mapGenesisParams({ ...GENESIS, network_magic: 2 }).networkId, 'Testnet');
  assert.throws(() => mapGenesisParams({ ...GENESIS, security_param: null }), (e) => e.code === 'INTERNAL');
  const { chainData: wrong } = provider({ '/genesis': { ...GENESIS, network_magic: 1 } });
  await rejectsWith(wrong.network.getGenesisParams(), 'INTERNAL');
});

test('ParameterChange bodies carry the new parameters in contract names and shapes', () => {
  const update = {
    maxTxExecutionUnits: { steps: 10000000000, memory: 16500000 },
    maxBlockExecutionUnits: { steps: 20000000000, memory: 72000000 },
    costModels: { PlutusV3: [100788, 420, -900] },
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    executionUnitPrices: { priceMemory: 0.0577, priceSteps: 7.21e-5 },
  };
  const pc = (u) => parseExactJson(JSON.stringify({ tag: 'ParameterChange', contents: [null, u, null] }));
  const { body } = decodeBody('ParameterChange', pc(update), 'mainnet');
  assert.deepEqual(body.changes, {
    maxTxExecutionUnits: { memory: 16500000, steps: 10000000000 },
    maxBlockExecutionUnits: { memory: 72000000, steps: 20000000000 },
    costModels: { PlutusV3: [100788, 420, -900] },
    maxBlockBodySize: 90112,
    maxBlockHeaderSize: 1100,
    collateralPercentage: 150,
    maxCollateralInputs: 3,
    poolRetireMaxEpoch: 18,
    stakePoolTargetNum: 500,
    executionUnitPrices: { memory: { numerator: 577, denominator: 10000 }, steps: { numerator: 721, denominator: 10000000 } },
  });
  const only = decodeBody('ParameterChange', pc({ costModels: { PlutusV1: [1, 2] } }), 'mainnet');
  assert.deepEqual(only.body.changes, { costModels: { PlutusV1: [1, 2] } }, 'a change of only a new parameter is not empty');
  assert.throws(() => decodeBody('ParameterChange', pc({ costModels: { PlutusV1: { 'addInteger-cpu': 1 } } }), 'mainnet'), (e) => e.code === 'INTERNAL');
  assert.throws(
    () => decodeBody('ParameterChange', parseExactJson('{"tag":"ParameterChange","contents":[null,{"maxTxExecutionUnits":{"memory":1,"steps":90071992547409930}},null]}'), 'mainnet'),
    (e) => e.code === 'INTERNAL',
  );
});

test('getProtocolParams: current, past epoch, pre-Conway NOT_FOUND, unknown epoch NOT_FOUND', async () => {
  const { chainData, fetch } = provider({
    '/epochs/latest/parameters': PARAMS,
    '/epochs/550/parameters': { ...PARAMS, epoch: 550 },
    '/epochs/300/parameters': { ...PARAMS, epoch: 300, gov_action_lifetime: null, dvt_motion_no_confidence: null },
  });
  assert.equal((await chainData.network.getProtocolParams()).data.epoch, 600);
  assert.equal((await chainData.network.getProtocolParams({ epoch: 550 })).data.epoch, 550);
  await rejectsWith(chainData.network.getProtocolParams({ epoch: 300 }), 'NOT_FOUND');
  await rejectsWith(chainData.network.getProtocolParams({ epoch: 9999 }), 'NOT_FOUND');
  await rejectsWith(chainData.network.getProtocolParams({ epoch: -1 }), 'INVALID_INPUT');
  assert.deepEqual(
    fetch.calls.map((c) => c.path),
    ['/epochs/latest/parameters', '/epochs/550/parameters', '/epochs/300/parameters', '/epochs/9999/parameters'],
  );
});

test('network info: tip, era from the protocol version, wrong network refused', async () => {
  const routes = {
    '/blocks/latest': { time: EPOCH.start_time + 60, height: 123, slot: 456, epoch: 600 },
    '/epochs/latest/parameters': PARAMS,
    '/genesis': { network_magic: 764824073 },
  };
  const { chainData } = provider(routes);
  const { data } = await chainData.network.getNetworkInfo();
  assert.deepEqual(data, {
    network: 'mainnet',
    era: 'conway',
    tip: { epoch: 600, slot: 456, block: 123, time: '2001-09-09T01:47:40Z' },
    currentEpoch: 600,
  });
  const { chainData: wrong } = provider({ ...routes, '/genesis': { network_magic: 1 } });
  await rejectsWith(wrong.network.getNetworkInfo(), 'INTERNAL');
  assert.equal(eraOf(9), 'conway');
  assert.throws(() => eraOf(40));
});

test('stake distribution: active and live, the DRep breakdown from the directory, predefined totals', async () => {
  const { chainData } = provider({
    '/epochs/latest': EPOCH,
    '/network': { supply: { treasury: '5', reserves: '6' }, stake: { live: '20999999999999999', active: '21000000000000000' } },
    '/governance/dreps': paged([
      drepRow(1, { amount: '300' }),
      drepRow(2, { amount: '200', expired: true }),
      drepRow(3, { amount: '100', retired: true }),
      ...PREDEFINED,
    ]),
  });
  const { data } = await chainData.network.getStakeDistribution();
  assert.deepEqual(data, {
    epoch: 600,
    totalActiveStake: '21000000000000000',
    totalLiveStake: '20999999999999999',
    totalStakeControlledByDReps: '300',
    totalStakeControlledBySPOs: '21000000000000000',
    alwaysAbstainVotingPower: '7000',
    alwaysNoConfidenceVotingPower: '500',
  });
});

test('treasury: the current pots; a past epoch is refused, not answered with today', async () => {
  const { chainData } = provider({ '/epochs/latest': EPOCH, '/network': { supply: { treasury: '5', reserves: '6' }, stake: { live: '1', active: '1' } } });
  assert.deepEqual((await chainData.network.getTreasury()).data, { epoch: 600, balance: '5', reserves: '6' });
  assert.deepEqual((await chainData.network.getTreasury({ epoch: 600 })).data.epoch, 600);
  await rejectsWith(chainData.network.getTreasury({ epoch: 599 }), 'CAPABILITY_UNSUPPORTED');
});

/* -- transactions ------------------------------------------------------------ */

test('transactions: on chain with its block and epoch; a 404 is onChain false, not NOT_FOUND', async () => {
  const tx = hash32(0xab);
  const { chainData } = provider({
    '/epochs/latest': EPOCH,
    [`/txs/${tx}`]: { hash: tx, block_height: 77, block_time: timeIn(598, 5), slot: 88, index: 1 },
  });
  const { data } = await chainData.transactions.get(tx.toUpperCase());
  assert.deepEqual(data, { txHash: tx, onChain: true, includedAt: { epoch: 598, slot: 88, block: 77, time: new Date(timeIn(598, 5) * 1000).toISOString().replace('.000Z', 'Z') } });
  assert.deepEqual((await chainData.transactions.get(hash32(0xcd))).data, { txHash: hash32(0xcd), onChain: false });
  await rejectsWith(chainData.transactions.get('xyz'), 'INVALID_INPUT');
});

/* -- identifiers ------------------------------------------------------------- */

test('Blockfrost DRep ids: CIP-129 and CIP-105 (both prefixes) decode to one credential', () => {
  assert.deepEqual(fromBlockfrostDRepId(drepId(4)), { hash: hash28(4), isScript: false });
  assert.deepEqual(fromBlockfrostDRepId(drepCip105(4)), { hash: hash28(4), isScript: false });
  assert.deepEqual(fromBlockfrostDRepId(drepCip105(4, true)), { hash: hash28(4), isScript: true });
  assert.deepEqual(fromBlockfrostDRepId(drepId(4, true)), { hash: hash28(4), isScript: true });
  assert.equal(fromBlockfrostDRepId('drep_always_abstain'), undefined);
  assert.throws(() => fromBlockfrostDRepId('drep1nonsense'), (e) => e.code === 'INTERNAL');
});

/* -- accounts ---------------------------------------------------------------- */

test('accounts: CIP-105 delegation from /accounts is re-encoded to CIP-129', async () => {
  const addr = stakeAddr(1);
  const { chainData } = provider({
    [`/accounts/${addr}`]: { stake_address: addr, active: true, registered: true, drep_id: drepCip105(7), pool_id: poolId(3) },
    [`/governance/dreps/${drepId(7)}`]: drepRow(7),
  });
  const { data } = await chainData.accounts.getDelegation(addr);
  assert.deepEqual(data, { target: { kind: 'drep', drep: { role: 'drep', id: drepId(7), isScriptBased: false } }, txRef: null });
});

test('accounts: predefined targets, a retired DRep clears it, deregistration clears it', async () => {
  const a = stakeAddr(1);
  const b = stakeAddr(2);
  const c = stakeAddr(3);
  const { chainData } = provider({
    [`/accounts/${a}`]: { stake_address: a, active: true, registered: true, drep_id: 'drep_always_no_confidence', pool_id: null },
    [`/accounts/${b}`]: { stake_address: b, active: true, registered: true, drep_id: drepId(8), pool_id: null },
    [`/governance/dreps/${drepId(8)}`]: drepRow(8, { retired: true }),
    [`/accounts/${c}`]: { stake_address: c, active: false, registered: false, drep_id: drepId(7), pool_id: null },
  });
  assert.deepEqual((await chainData.accounts.getDelegation(a)).data.target, { kind: 'predefined', target: 'alwaysNoConfidence' });
  assert.equal((await chainData.accounts.getDelegation(b)).data, null);
  assert.equal((await chainData.accounts.getDelegation(c)).data, null);
});

test('accounts: an address Blockfrost has never seen is a known "not registered", not NOT_FOUND', async () => {
  const addr = stakeAddr(9, true);
  const { chainData } = provider({});
  assert.deepEqual((await chainData.accounts.get(addr)).data, {
    stakeAddress: addr,
    stakeKeyHash: hash28(9),
    isRegistered: false,
    isScriptBased: true,
  });
  assert.equal((await chainData.accounts.getDelegation(addr)).data, null);
  assert.equal((await chainData.accounts.getPoolDelegation(addr)).data, null);
  await rejectsWith(chainData.accounts.get('stake_test1uqfu74w3wh4gfzu8m6e7j987h4lq9r3t7ef5gaw497uu85qsqfy27'), 'INVALID_INPUT');
});

test('accounts: pool delegation with the certificate that set it', async () => {
  const addr = stakeAddr(1);
  const { chainData } = provider({
    '/epochs/latest': EPOCH,
    [`/accounts/${addr}`]: { stake_address: addr, active: true, registered: true, drep_id: null, pool_id: poolId(3) },
    [`/accounts/${addr}/delegations?count=1&order=desc`]: [
      { active_epoch: 598, tx_hash: hash32(0x11), amount: '5', pool_id: poolId(3), tx_slot: 9, block_time: timeIn(596), block_height: 99 },
    ],
  });
  const { data } = await chainData.accounts.getPoolDelegation(addr);
  assert.equal(data.poolId, poolId(3));
  assert.deepEqual(data.txRef, { txHash: hash32(0x11), block: 99 });
  assert.equal(data.since.epoch, 596);
});

test('accounts: optional methods Blockfrost cannot serve are absent, not stubs', () => {
  const { chainData } = provider({});
  assert.equal(chainData.accounts.getVotingPower, undefined);
  assert.equal(chainData.accounts.listDelegationHistory, undefined);
});

/* -- CBOR -------------------------------------------------------------------- */

test('CBOR: voting procedures, proposal anchors and DRep certificate anchors', () => {
  const action = hash32(0x42);
  const tx = decodeCbor(
    txCbor({
      votes: [
        { kind: VOTER.drepKey, hash: hash28(1), actionTx: action, actionIndex: 0, vote: VOTE.yes, anchor: { url: 'https://r.example/1', hash: hash32(1) } },
        { kind: VOTER.drepKey, hash: hash28(1), actionTx: action, actionIndex: 1, vote: VOTE.no },
        { kind: VOTER.ccHotScript, hash: hash28(2), actionTx: action, actionIndex: 0, vote: VOTE.abstain },
      ],
      proposals: [{ url: 'https://p.example/0', hash: hash32(3) }],
      certs: [[16, [0, Buffer.from(hash28(5), 'hex')], 500000000, ['https://d.example', Buffer.from(hash32(6), 'hex')]], [18, [0, Buffer.from(hash28(5), 'hex')], null]],
    }).cbor,
  );
  assert.deepEqual(votesOf(tx), [
    { voter: { kind: 'drep', hash: hash28(1), isScript: false }, action: { txHash: action, index: 0 }, vote: 'yes', anchor: { url: 'https://r.example/1', dataHash: hash32(1) } },
    { voter: { kind: 'drep', hash: hash28(1), isScript: false }, action: { txHash: action, index: 1 }, vote: 'no', anchor: null },
    { voter: { kind: 'ccHot', hash: hash28(2), isScript: true }, action: { txHash: action, index: 0 }, vote: 'abstain', anchor: null },
  ]);
  assert.deepEqual(proposalAnchorsOf(tx), [{ url: 'https://p.example/0', dataHash: hash32(3) }]);
  assert.deepEqual(drepCertAnchorOf(tx, 0), { url: 'https://d.example', dataHash: hash32(6) });
  assert.equal(drepCertAnchorOf(tx, 1), null);
});

test('CBOR: truncated, trailing or over-deep input is refused, never half-read', () => {
  const ok = cbor([1, 2, 3]).toString('hex');
  assert.throws(() => decodeCbor(ok.slice(0, -2)), (e) => e.code === 'INTERNAL');
  assert.throws(() => decodeCbor(`${ok}00`), (e) => e.code === 'INTERNAL');
  assert.throws(() => decodeCbor('9a7fffffff'), (e) => e.code === 'INTERNAL', 'a huge declared length');
  assert.throws(() => decodeCbor('81'.repeat(100) + '00'), (e) => e.code === 'INTERNAL', 'nesting');
  assert.throws(() => decodeCbor('zz'), (e) => e.code === 'INTERNAL');
});
