/**
 * Network, transactions and system areas, and identifier handling, over a
 * fake Koios. Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import { decodeDRepId, encodeDRepId } from '../dist/ids.js';
import { mapProtocolParams } from '../dist/network.js';
import { toRatio } from '../dist/ratio.js';
import { EPOCH, at, chainRoutes, cip105, drepId, paramsRow, provider, rejectsWith, txHash } from './helpers.mjs';

test('CIP-129 DRep ids round-trip; CIP-105 is rejected, never guessed', () => {
  const id = drepId(7);
  const decoded = decodeDRepId(id);
  assert.equal(decoded.isScript, false);
  assert.equal(encodeDRepId(decoded.hash, decoded.isScript), id);
  assert.equal(decodeDRepId(drepId(7, true)).isScript, true);
  assert.throws(() => decodeDRepId(cip105(7)), (e) => e.code === 'INVALID_INPUT');
  assert.throws(() => decodeDRepId('drep1notbech32'), (e) => e.code === 'INVALID_INPUT');
});

test('thresholds are reconstructed as exact rationals', () => {
  assert.deepEqual(toRatio(0.67), { numerator: 67, denominator: 100 });
  assert.deepEqual(toRatio(0.6666666666666666), { numerator: 2, denominator: 3 });
  assert.deepEqual(toRatio(0.51), { numerator: 51, denominator: 100 });
  assert.equal(toRatio(null), undefined);
});

test('protocol params map to the typed camelCase object', async () => {
  const { chainData, calls } = provider({ ...chainRoutes, epoch_params: [paramsRow()] });
  const { data, meta } = await chainData.network.getProtocolParams();
  assert.deepEqual(meta, { provider: 'koios', network: 'mainnet' });
  assert.equal(data.epoch, EPOCH);
  assert.deepEqual(data.protocolVersion, { major: 11, minor: 0 });
  assert.equal(data.govActionDeposit, '100000000000');
  assert.equal(data.drepActivity, 20);
  assert.deepEqual(data.drepThresholds.updateToConstitution, { numerator: 3, denominator: 4 });
  assert.deepEqual(data.poolThresholds.ppSecurityGroup, { numerator: 51, denominator: 100 });
  assert.equal(data.coinsPerUtxoByte, '4310');
  assert.equal(calls.find((c) => c.endpoint === 'epoch_params').url.searchParams.get('_epoch_no'), String(EPOCH));
  assert.ok(!('nonce' in data) && !('cost_models' in data), 'no raw source columns');
});

test('short decimals from doubles are exact rationals, not approximations', () => {
  assert.deepEqual(toRatio(7.21e-5), { numerator: 721, denominator: 10000000 });
  assert.deepEqual(toRatio(0.0577), { numerator: 577, denominator: 10000 });
  assert.deepEqual(toRatio(0.003), { numerator: 3, denominator: 1000 });
  assert.deepEqual(toRatio(0.2), { numerator: 1, denominator: 5 });
  assert.deepEqual(toRatio(0.3), { numerator: 3, denominator: 10 });
  assert.deepEqual(toRatio(15), { numerator: 15, denominator: 1 });
  assert.deepEqual(toRatio('0.05'), { numerator: 1, denominator: 20 });
});

test('every current-era parameter is carried, typed', async () => {
  const { chainData } = provider({ ...chainRoutes, epoch_params: [paramsRow()] });
  const { data } = await chainData.network.getProtocolParams();
  const { epoch, protocolVersion, drepThresholds, poolThresholds, ...rest } = data;
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
  for (const k of ['decentralisation', 'extraEntropy', 'minUtxoValue', 'decentralisation', 'extra_entropy', 'min_utxo_value']) assert.ok(!(k in data), k);
});

test('cost models: a missing language is absent; a malformed or missing set is refused', () => {
  assert.deepEqual(mapProtocolParams({ ...paramsRow(), cost_models: { PlutusV1: [1, 2], PlutusV3: [3] } }).costModels, { PlutusV1: [1, 2], PlutusV3: [3] });
  assert.equal(mapProtocolParams({ ...paramsRow(), cost_models: null }), undefined);
  assert.equal(mapProtocolParams({ ...paramsRow(), cost_models: { PlutusV1: { 'addInteger-cpu-arguments-intercept': 1 } } }), undefined);
  assert.equal(mapProtocolParams({ ...paramsRow(), cost_models: { PlutusV1: [1.5] } }), undefined);
  assert.equal(mapProtocolParams({ ...paramsRow(), cost_models: { PlutusV1: [] } }), undefined);
  for (const column of ['max_block_size', 'max_tx_ex_steps', 'price_step', 'influence', 'min_pool_cost', 'min_fee_ref_script_cost_per_byte']) {
    assert.equal(mapProtocolParams({ ...paramsRow(), [column]: null }), undefined, column);
  }
});

test('execution units beyond a safe integer are an internal fault, never rounded', () => {
  assert.throws(() => mapProtocolParams({ ...paramsRow(), max_block_ex_steps: '90071992547409930' }), (e) => e.code === 'INTERNAL');
});

test('genesis parameters from /genesis, typed', async () => {
  const { chainData } = provider(chainRoutes);
  const { data, meta } = await chainData.network.getGenesisParams();
  assert.deepEqual(meta, { provider: 'koios', network: 'mainnet' });
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
  const partial = provider({ ...chainRoutes, genesis: [{ ...chainRoutes.genesis[0], securityparam: null }] });
  await rejectsWith(partial.chainData.network.getGenesisParams(), 'INTERNAL');
  const wrong = provider({ ...chainRoutes, genesis: [{ ...chainRoutes.genesis[0], networkmagic: '1' }] });
  await rejectsWith(wrong.chainData.network.getGenesisParams(), 'INTERNAL');
});

test('past epochs are served; pre-Conway and future epochs are NOT_FOUND', async () => {
  const rows = { 600: paramsRow(600, 10), 500: { ...paramsRow(500, 8), gov_action_lifetime: null, dvt_p_p_gov_group: null } };
  const { chainData } = provider({ ...chainRoutes, epoch_params: (url) => (rows[url.searchParams.get('_epoch_no')] ? [rows[url.searchParams.get('_epoch_no')]] : []) });
  assert.equal((await chainData.network.getProtocolParams({ epoch: 600 })).data.epoch, 600);
  await rejectsWith(chainData.network.getProtocolParams({ epoch: 500 }), 'NOT_FOUND');
  await rejectsWith(chainData.network.getProtocolParams({ epoch: EPOCH + 3 }), 'NOT_FOUND');
  await rejectsWith(chainData.network.getProtocolParams({ epoch: -1 }), 'INVALID_INPUT');
  assert.equal(mapProtocolParams(rows[500]), undefined);
});

test('network info reads the tip and checks the network magic', async () => {
  const { chainData } = provider(chainRoutes);
  const { data } = await chainData.network.getNetworkInfo();
  assert.deepEqual(data, {
    network: 'mainnet',
    era: 'conway',
    tip: { epoch: EPOCH, slot: 198699726, block: 13982865, time: new Date(at(EPOCH, 5000) * 1000).toISOString().replace('.000Z', 'Z') },
    currentEpoch: EPOCH,
  });
  const wrong = provider({ ...chainRoutes, genesis: [{ ...chainRoutes.genesis[0], networkmagic: '1' }] });
  await rejectsWith(wrong.chainData.network.getNetworkInfo(), 'INTERNAL');
});

test('treasury per epoch; a future epoch is NOT_FOUND', async () => {
  const { chainData } = provider({
    ...chainRoutes,
    totals: (url) => (Number(url.searchParams.get('_epoch_no')) <= EPOCH ? [{ epoch_no: Number(url.searchParams.get('_epoch_no')), treasury: '1363711219207891', reserves: '6107226792256530' }] : []),
  });
  assert.deepEqual((await chainData.network.getTreasury()).data, { epoch: EPOCH, balance: '1363711219207891', reserves: '6107226792256530' });
  assert.equal((await chainData.network.getTreasury({ epoch: 600 })).data.epoch, 600);
  await rejectsWith(chainData.network.getTreasury({ epoch: EPOCH + 1 }), 'NOT_FOUND');
});

test('stake distribution: active stake, predefined targets, DRep and SPO totals', async () => {
  const d1 = drepId(1);
  const d2 = drepId(2);
  const d3 = drepId(3);
  const info = {
    [d1]: { drep_id: d1, hex: '01'.repeat(28), has_script: false, drep_status: 'registered', expires_epoch_no: EPOCH + 5, active: true, amount: '100' },
    [d2]: { drep_id: d2, hex: '02'.repeat(28), has_script: false, drep_status: 'registered', expires_epoch_no: EPOCH - 1, active: false, amount: '50' },
    [d3]: { drep_id: d3, hex: '03'.repeat(28), has_script: false, drep_status: 'registered', expires_epoch_no: EPOCH, active: true, amount: '7' },
    drep_always_abstain: { drep_id: 'drep_always_abstain', hex: null, has_script: false, drep_status: 'registered', amount: '1000' },
    drep_always_no_confidence: { drep_id: 'drep_always_no_confidence', hex: null, has_script: false, drep_status: 'registered', amount: '0' },
  };
  const { chainData } = provider({
    ...chainRoutes,
    drep_list: [d1, d2, d3].map((id) => ({ drep_id: id, hex: info[id].hex, has_script: false, registered: true })),
    drep_info: (_url, init) => init.body._drep_ids.map((id) => info[id]),
    pool_voting_power_history: [{ pool_id_bech32: 'p1', amount: '30' }, { pool_id_bech32: 'p2', amount: '12' }],
  });
  const { data } = await chainData.network.getStakeDistribution();
  assert.deepEqual(data, {
    epoch: EPOCH,
    totalActiveStake: '21357778069987000',
    // d2 is past its expiry; d3 expires this epoch and is still active.
    totalStakeControlledByDReps: '107',
    alwaysAbstainVotingPower: '1000',
    alwaysNoConfidenceVotingPower: '0',
    totalStakeControlledBySPOs: '42',
  });
});

test('stake distribution refuses a missing active stake rather than reporting 0', async () => {
  const { chainData } = provider({
    ...chainRoutes,
    epoch_info: [{ ...chainRoutes.epoch_info[0], active_stake: null }],
    drep_list: [],
    drep_info: [],
    pool_voting_power_history: [],
  });
  await rejectsWith(chainData.network.getStakeDistribution(), 'STALE_DATA');
});

test('transactions: on chain, not on chain, malformed', async () => {
  const h = txHash(9);
  const { chainData, calls } = provider({
    tx_info: (_url, init) =>
      init.body._tx_hashes[0] === h ? [{ tx_hash: h, block_height: 100, epoch_no: EPOCH, absolute_slot: 5, tx_timestamp: at(EPOCH) }] : [],
  });
  const on = await chainData.transactions.get(h.toUpperCase());
  assert.equal(on.data.onChain, true);
  assert.deepEqual(on.data.includedAt, { epoch: EPOCH, slot: 5, block: 100, time: new Date(at(EPOCH) * 1000).toISOString().replace('.000Z', 'Z') });
  assert.equal(calls[0].body._inputs, false);
  assert.deepEqual((await chainData.transactions.get(txHash(1))).data, { txHash: txHash(1), onChain: false });
  await rejectsWith(chainData.transactions.get('abc'), 'INVALID_INPUT');
});

test('system: identity and health', async () => {
  const { chainData } = provider({ tip: [{ ...chainRoutes.tip[0], block_time: Math.floor(Date.now() / 1000) - 30 }] });
  assert.deepEqual((await chainData.system.getIdentity()).data, { id: 'koios', name: 'Koios' });
  const health = (await chainData.system.getHealth()).data;
  assert.equal(health.status, 'healthy');
  assert.ok(health.secondsSinceLastUpdate < 120);
  const stale = provider({ tip: [{ ...chainRoutes.tip[0], block_time: Math.floor(Date.now() / 1000) - 3600 }] });
  assert.equal((await stale.chainData.system.getHealth()).data.status, 'degraded');
  const down = provider({ tip: { status: 503 } });
  assert.equal((await down.chainData.system.getHealth()).data.status, 'unavailable');
});
