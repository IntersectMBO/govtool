/**
 * Mapping logic of the network and transactions areas, driven by a fake db.
 * Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import { PROTOCOL_PARAMS_SQL, STAKE_DISTRIBUTION_SQL, TREASURY_SQL, eraOf, toParamRatio, toCostModels } from '../dist/network.js';

import { TRANSACTION_SQL } from '../dist/transactions.js';

/** A fake db answering by SQL text; records every call. */
function fakeDb(routes) {
  const calls = [];
  return {
    calls,
    async query(sql, params = []) {
      calls.push({ sql, params });
      for (const [match, answer] of routes) {
        if (sql === match || (typeof match === 'function' && match(sql))) {
          return typeof answer === 'function' ? answer(params) : answer;
        }
      }
      throw new Error(`unexpected SQL: ${sql.slice(0, 80)}`);
    },
  };
}

const provider = (routes, network = 'preview') => {
  const db = fakeDb(routes);
  return { db, chainData: createDbSyncProvider({ network, db }).chainData };
};

async function rejectsWith(promiseOrFn, code) {
  const p = typeof promiseOrFn === 'function' ? promiseOrFn() : promiseOrFn;
  assert.ok(p instanceof Promise, 'must return a promise, never throw synchronously');
  await assert.rejects(p, (e) => e.code === code || assert.fail(`expected ${code}, got ${e.code}: ${e.message}`));
}

const tipBlock = {
  epoch_no: 1430,
  slot_no: '123599244',
  block_no: '4692421',
  time: new Date('2026-09-24T13:07:24Z'),
  proto_major: 11,
  proto_minor: 0,
};

const paramRow = {
  tip_epoch: 1430,
  epoch_no: 1430,
  protocol_major: 11,
  protocol_minor: 0,
  gov_action_lifetime: '30',
  gov_action_deposit: '1000000000',
  drep_deposit: '500000000',
  drep_activity: '31',
  committee_min_size: '3',
  committee_max_term_length: '365',
  dvt_motion_no_confidence: 0.67,
  dvt_committee_normal: 0.67,
  dvt_committee_no_confidence: 0.6,
  dvt_update_to_constitution: 0.75,
  dvt_hard_fork_initiation: 0.6,
  dvt_p_p_network_group: 0.67,
  dvt_p_p_economic_group: 0.67,
  dvt_p_p_technical_group: 0.67,
  dvt_p_p_gov_group: 0.75,
  dvt_treasury_withdrawal: 0.67,
  pvt_motion_no_confidence: 0.51,
  pvt_committee_normal: 0.51,
  pvt_committee_no_confidence: 0.51,
  pvt_hard_fork_initiation: 0.51,
  pvtpp_security_group: 2 / 3,
  key_deposit: '2000000',
  pool_deposit: '500000000',
  coins_per_utxo_size: '4310',
  min_fee_a: 44,
  min_fee_b: 155381,
  max_tx_size: 16384,
  max_val_size: '5000',
  min_fee_ref_script_cost_per_byte: 15,
  max_block_size: 90112,
  max_bh_size: 1100,
  max_tx_ex_mem: '17500000',
  max_tx_ex_steps: '10000000000',
  max_block_ex_mem: '77500000',
  max_block_ex_steps: '20000000000',
  collateral_percent: 150,
  max_collateral_inputs: 3,
  price_mem: 0.0577,
  price_step: 0.0000721,
  max_epoch: 18,
  optimal_pool_count: 500,
  influence: 0.3,
  monetary_expand_rate: 0.003,
  treasury_growth_rate: 0.2,
  min_pool_cost: '75000000',
  cost_models: { PlutusV1: [100788, 420, 1], PlutusV2: [100788, 420, 1, 1], PlutusV3: [100788, 420, 1, 1, 1000] },
};

/* -- ratio ------------------------------------------------------------------ */

const toRatio = toParamRatio;

test('toRatio recovers the exact rationals db-sync stores as doubles', () => {
  assert.deepEqual(toRatio(0.67), { numerator: 67, denominator: 100 });
  assert.deepEqual(toRatio(0.51), { numerator: 51, denominator: 100 });
  assert.deepEqual(toRatio(0.6), { numerator: 3, denominator: 5 });
  assert.deepEqual(toRatio(0.75), { numerator: 3, denominator: 4 });
  assert.deepEqual(toRatio(2 / 3), { numerator: 2, denominator: 3 });
  assert.deepEqual(toRatio(0), { numerator: 0, denominator: 1 });
  assert.deepEqual(toRatio(1), { numerator: 1, denominator: 1 });
  assert.deepEqual(toRatio('0.1'), { numerator: 1, denominator: 10 });
  // Execution prices need denominators far beyond a 1000 cap.
  assert.deepEqual(toRatio(0.0000721), { numerator: 721, denominator: 10_000_000 });
  assert.deepEqual(toRatio(0.0577), { numerator: 577, denominator: 10_000 });
  assert.deepEqual(toRatio(15), { numerator: 15, denominator: 1 });
  assert.equal(toRatio(null), undefined);
  assert.equal(toRatio(Number.NaN), undefined);
  assert.equal(toRatio(-0.5), undefined);
});

test('eraOf maps protocol majors and refuses unknown ones', () => {
  assert.equal(eraOf(9), 'conway');
  assert.equal(eraOf(11), 'conway');
  assert.equal(eraOf(8), 'babbage');
  assert.equal(eraOf(2), 'shelley');
  assert.throws(() => eraOf(99), (e) => e.code === 'INTERNAL');
});

/* -- network info ----------------------------------------------------------- */

test('getNetworkInfo reads the tip and names the era', async () => {
  const { chainData } = provider([[(s) => s.includes('FROM meta'), [{ ...tipBlock, network_name: 'preview' }]]]);
  const { data, meta } = await chainData.network.getNetworkInfo();
  assert.deepEqual(data, {
    network: 'preview',
    era: 'conway',
    tip: { epoch: 1430, slot: 123599244, block: 4692421, time: '2026-09-24T13:07:24.000Z' },
    currentEpoch: 1430,
  });
  assert.equal(meta.provider, 'dbsync');
});

test('getNetworkInfo refuses a database following another network', async () => {
  const { chainData } = provider([[(s) => s.includes('FROM meta'), [{ ...tipBlock, network_name: 'mainnet' }]]]);
  await rejectsWith(() => chainData.network.getNetworkInfo(), 'INTERNAL');
});

test('getNetworkInfo on an empty database is STALE_DATA, not a fabricated tip', async () => {
  const { chainData } = provider([[(s) => s.includes('FROM meta'), []]]);
  await rejectsWith(() => chainData.network.getNetworkInfo(), 'STALE_DATA');
});

/* -- protocol params -------------------------------------------------------- */

test('getProtocolParams maps a Conway epoch_param row to the contract shape', async () => {
  const { chainData, db } = provider([[PROTOCOL_PARAMS_SQL, [paramRow]]]);
  const { data } = await chainData.network.getProtocolParams();
  assert.deepEqual(db.calls[0].params, [null]);
  assert.equal(data.epoch, 1430);
  assert.deepEqual(data.protocolVersion, { major: 11, minor: 0 });
  assert.equal(data.govActionLifetime, 30);
  assert.equal(data.govActionDeposit, '1000000000');
  assert.equal(data.drepActivity, 31);
  assert.equal(data.committeeMaxTermLength, 365);
  assert.equal(data.coinsPerUtxoByte, '4310');
  assert.equal(data.maxValSize, 5000);
  assert.deepEqual(data.drepThresholds.committeeNoConfidence, { numerator: 3, denominator: 5 });
  assert.deepEqual(data.drepThresholds.ppGovGroup, { numerator: 3, denominator: 4 });
  assert.deepEqual(data.poolThresholds.ppSecurityGroup, { numerator: 2, denominator: 3 });
  assert.equal(Object.keys(data.drepThresholds).length, 10);
  assert.equal(Object.keys(data.poolThresholds).length, 5);
  assert.ok(!('tip_epoch' in data) && !('dvt_p_p_gov_group' in data), 'no source columns leak');
  assert.equal(data.maxBlockBodySize, 90112);
  assert.equal(data.maxBlockHeaderSize, 1100);
  assert.deepEqual(data.maxTxExecutionUnits, { memory: 17_500_000, steps: 10_000_000_000 });
  assert.deepEqual(data.maxBlockExecutionUnits, { memory: 77_500_000, steps: 20_000_000_000 });
  assert.equal(data.collateralPercentage, 150);
  assert.equal(data.maxCollateralInputs, 3);
  assert.deepEqual(data.executionUnitPrices, {
    memory: { numerator: 577, denominator: 10_000 },
    steps: { numerator: 721, denominator: 10_000_000 },
  });
  assert.deepEqual(data.minFeeRefScriptCostPerByte, { numerator: 15, denominator: 1 });
  assert.deepEqual(data.costModels, paramRow.cost_models);
  assert.equal(data.poolRetireMaxEpoch, 18);
  assert.equal(data.stakePoolTargetNum, 500);
  assert.deepEqual(data.poolPledgeInfluence, { numerator: 3, denominator: 10 });
  assert.deepEqual(data.monetaryExpansion, { numerator: 3, denominator: 1000 });
  assert.deepEqual(data.treasuryCut, { numerator: 1, denominator: 5 });
  assert.equal(data.minPoolCost, '75000000');
  for (const removed of ['decentralisation', 'extraEntropy', 'minUtxoValue', 'nonce']) {
    assert.ok(!(removed in data), `${removed} is not a current protocol parameter`);
  }
  for (const k of ['govActionDeposit', 'drepDeposit', 'keyDeposit', 'poolDeposit', 'coinsPerUtxoByte']) {
    assert.match(data[k], /^\d+$/);
  }
});

test('cost models: a language not in force is absent, a named-key object is refused', async () => {
  assert.deepEqual(toCostModels({ PlutusV1: [1, 2], PlutusV2: null }), { PlutusV1: [1, 2] });
  assert.equal(toCostModels({ PlutusV1: { 'addInteger-cpu-arguments-intercept': 1 } }), undefined);
  assert.equal(toCostModels({ PlutusV1: [] }), undefined);
  assert.equal(toCostModels(null), undefined);
  const { chainData } = provider([[PROTOCOL_PARAMS_SQL, [{ ...paramRow, cost_models: null }]]]);
  await rejectsWith(() => chainData.network.getProtocolParams(), 'INTERNAL');
});

test('getProtocolParams({ epoch }) passes the epoch as a bound parameter', async () => {
  const { chainData, db } = provider([[PROTOCOL_PARAMS_SQL, [{ ...paramRow, epoch_no: 1400 }]]]);
  const { data } = await chainData.network.getProtocolParams({ epoch: 1400 });
  assert.equal(data.epoch, 1400);
  assert.deepEqual(db.calls[0].params, [1400]);
  assert.ok(!db.calls[0].sql.includes('1400'), 'never interpolated');
});

test('getProtocolParams refuses a pre-Conway epoch rather than inventing thresholds', async () => {
  const preConway = { ...paramRow, epoch_no: 10, dvt_motion_no_confidence: null, gov_action_lifetime: null };
  const { chainData } = provider([[PROTOCOL_PARAMS_SQL, [preConway]]]);
  await rejectsWith(() => chainData.network.getProtocolParams({ epoch: 10 }), 'NOT_FOUND');
});

test('getProtocolParams: a missing current row is STALE_DATA; a missing past row NOT_FOUND', async () => {
  const empty = { tip_epoch: 1430, epoch_no: null };
  const { chainData } = provider([[PROTOCOL_PARAMS_SQL, [empty]]]);
  await rejectsWith(() => chainData.network.getProtocolParams(), 'STALE_DATA');
  await rejectsWith(() => chainData.network.getProtocolParams({ epoch: 9999 }), 'NOT_FOUND');
});

test('getProtocolParams: a current row missing a governance column is INTERNAL', async () => {
  const { chainData } = provider([[PROTOCOL_PARAMS_SQL, [{ ...paramRow, drep_deposit: null }]]]);
  await rejectsWith(() => chainData.network.getProtocolParams(), 'INTERNAL');
});

test('getProtocolParams validates the epoch argument before querying', async () => {
  const { chainData, db } = provider([]);
  for (const epoch of [-1, 1.5, '12', Number.NaN, null]) {
    await rejectsWith(() => chainData.network.getProtocolParams({ epoch }), 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});

test('capabilities declare protocolParams.epoch', async () => {
  const { chainData } = provider([]);
  const { data } = await chainData.system.getCapabilities();
  assert.ok(data.optionalArguments.includes('protocolParams.epoch'));
});

/* -- stake distribution ----------------------------------------------------- */

const stakeRow = {
  epoch_no: 1430,
  stake_complete: true,
  active_stake: '1552658969418227',
  drep_rows: '3025',
  active_dreps: '32177833939417',
  always_abstain: '127158395299138',
  always_no_confidence: '3729119157372',
  pool_rows: '682',
  pool_rows_with_vp: '682',
  pool_voting_power: '1553042315738513',
};

test('getStakeDistribution maps every computed field and never serves live stake', async () => {
  const { chainData } = provider([[STAKE_DISTRIBUTION_SQL, [stakeRow]]]);
  const { data } = await chainData.network.getStakeDistribution();
  assert.deepEqual(data, {
    epoch: 1430,
    totalActiveStake: '1552658969418227',
    totalStakeControlledByDReps: '32177833939417',
    totalStakeControlledBySPOs: '1553042315738513',
    alwaysAbstainVotingPower: '127158395299138',
    alwaysNoConfidenceVotingPower: '3729119157372',
  });
});

test('getStakeDistribution: an uncomputed DRep distribution omits the DRep fields', async () => {
  const row = { ...stakeRow, drep_rows: '0', active_dreps: null, always_abstain: null, always_no_confidence: null };
  const { chainData } = provider([[STAKE_DISTRIBUTION_SQL, [row]]]);
  const { data } = await chainData.network.getStakeDistribution();
  for (const k of ['totalStakeControlledByDReps', 'alwaysAbstainVotingPower', 'alwaysNoConfidenceVotingPower']) {
    assert.ok(!(k in data), `${k} omitted, not zero`);
  }
});

test('getStakeDistribution: a computed distribution with no predefined delegators is a known zero', async () => {
  const row = { ...stakeRow, always_abstain: null, always_no_confidence: null };
  const { chainData } = provider([[STAKE_DISTRIBUTION_SQL, [row]]]);
  const { data } = await chainData.network.getStakeDistribution();
  assert.equal(data.alwaysAbstainVotingPower, '0');
  assert.equal(data.alwaysNoConfidenceVotingPower, '0');
});

test('getStakeDistribution: SPO total omitted when any pool lacks a voting power', async () => {
  const row = { ...stakeRow, pool_rows_with_vp: '600' };
  const { chainData } = provider([[STAKE_DISTRIBUTION_SQL, [row]]]);
  const { data } = await chainData.network.getStakeDistribution();
  assert.ok(!('totalStakeControlledBySPOs' in data));
});

test('getStakeDistribution: an unfinished stake snapshot is STALE_DATA, not a partial sum', async () => {
  const { chainData } = provider([[STAKE_DISTRIBUTION_SQL, [{ ...stakeRow, stake_complete: false }]]]);
  await rejectsWith(() => chainData.network.getStakeDistribution(), 'STALE_DATA');
  const { chainData: c2 } = provider([[STAKE_DISTRIBUTION_SQL, [{ ...stakeRow, stake_complete: null, active_stake: null }]]]);
  await rejectsWith(() => c2.network.getStakeDistribution(), 'STALE_DATA');
});

/* -- treasury --------------------------------------------------------------- */

test('getTreasury maps ada_pots and honours an epoch', async () => {
  const { chainData, db } = provider([
    [TREASURY_SQL, (params) => [{ tip_epoch: 1430, epoch_no: params[0] ?? 1430, treasury: '7137216653470969', reserves: '7560917014429433' }]],
  ]);
  assert.deepEqual((await chainData.network.getTreasury()).data, {
    epoch: 1430,
    balance: '7137216653470969',
    reserves: '7560917014429433',
  });
  assert.equal((await chainData.network.getTreasury({ epoch: 1200 })).data.epoch, 1200);
  assert.deepEqual(db.calls.map((c) => c.params), [[null], [1200]]);
});

test('getTreasury: missing rows are STALE_DATA (current) or NOT_FOUND (past)', async () => {
  const { chainData } = provider([[TREASURY_SQL, [{ tip_epoch: 1430, epoch_no: null, treasury: null, reserves: null }]]]);
  await rejectsWith(() => chainData.network.getTreasury(), 'STALE_DATA');
  await rejectsWith(() => chainData.network.getTreasury({ epoch: 5000 }), 'NOT_FOUND');
  await rejectsWith(() => chainData.network.getTreasury({ epoch: -3 }), 'INVALID_INPUT');
});

/* -- transactions ----------------------------------------------------------- */

const HASH = 'ab'.repeat(32);

test('transactions.get: an included transaction is onChain with its block stamp', async () => {
  const { chainData, db } = provider([[TRANSACTION_SQL, [{ epoch_no: 1430, slot_no: '10', block_no: '20', time: new Date('2026-09-24T00:00:00Z') }]]]);
  const { data } = await chainData.transactions.get(HASH.toUpperCase());
  assert.deepEqual(data, {
    txHash: HASH,
    onChain: true,
    includedAt: { epoch: 1430, slot: 10, block: 20, time: '2026-09-24T00:00:00.000Z' },
  });
  assert.deepEqual(db.calls[0].params, [HASH]);
});

test('transactions.get: an unseen hash is onChain false, not NOT_FOUND', async () => {
  const { chainData } = provider([[TRANSACTION_SQL, []]]);
  assert.deepEqual((await chainData.transactions.get(HASH)).data, { txHash: HASH, onChain: false });
});

test('transactions.get rejects a malformed hash without querying', async () => {
  const { chainData, db } = provider([]);
  for (const bad of ['abcd', 'z'.repeat(64), `${HASH}00`, undefined, 42, "'; DROP TABLE tx; --"]) {
    await rejectsWith(() => chainData.transactions.get(bad), 'INVALID_INPUT');
  }
  assert.equal(db.calls.length, 0);
});

test('a driver failure surfaces as PROVIDER_UNAVAILABLE through the guard', async () => {
  const db = { query: async () => Promise.reject(Object.assign(new Error('connection refused'), { code: 'ECONNREFUSED' })) };
  const { chainData } = createDbSyncProvider({ network: 'preview', db });
  await rejectsWith(() => chainData.network.getProtocolParams(), 'PROVIDER_UNAVAILABLE');
  await rejectsWith(() => chainData.transactions.get(HASH), 'PROVIDER_UNAVAILABLE');
});
