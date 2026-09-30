/**
 * The fixture provider, driven the way a backend drives it.
 *
 * Runs against ../dist, so `npm run build` comes first — `npm run verify` does
 * both. No network, no database, no credentials.
 */

import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createFixtureProvider } from '../dist/index.js';

const govtool = createFixtureProvider();
const chain = govtool.chainData;

/* -- the dataset itself ----------------------------------------------------- */

test('the fixture holds a real spread, not one of each', () => {
  const { dreps, proposals } = govtool.data;
  const statuses = new Set(dreps.map((d) => d.status));
  assert.ok(statuses.has('active') && statuses.has('inactive') && statuses.has('retired'));
  assert.ok(dreps.some((d) => d.kind === 'anonymous'), 'needs anonymous DReps');
  assert.ok(dreps.some((d) => d.kind === 'drep'), 'needs anchored DReps');

  const lifecycles = new Set(proposals.map((p) => p.lifecycle.status));
  assert.ok(lifecycles.has('live'), 'needs live proposals');
  assert.ok(lifecycles.has('expired'), 'needs expired proposals');
  assert.ok(lifecycles.has('enacted'), 'needs enacted proposals');
  assert.ok(new Set(proposals.map((p) => p.type)).size >= 4, 'needs several action types');
});

/* -- conventions ------------------------------------------------------------ */

test('thresholds are exact ratios, reconstructed from the source floats', async () => {
  const { data } = await chain.network.getProtocolParams();
  const t = data.drepThresholds.committeeNormal;
  assert.equal(typeof t.numerator, 'number');
  assert.notEqual(t.denominator, 0);
  // 0.67 on the wire is 67/100 in the ledger, not a float.
  assert.equal(t.numerator / t.denominator, 0.67);
});

test('protocol params carry every current-era parameter, with mainnet values', async () => {
  const { data } = await chain.network.getProtocolParams();
  assert.equal(data.epoch, 657);
  assert.deepEqual(data.executionUnitPrices, {
    memory: { numerator: 577, denominator: 10000 },
    steps: { numerator: 721, denominator: 10000000 },
  });
  assert.deepEqual(data.maxTxExecutionUnits, { memory: 16500000, steps: 10000000000 });
  assert.deepEqual(data.maxBlockExecutionUnits, { memory: 72000000, steps: 20000000000 });
  assert.deepEqual(data.minFeeRefScriptCostPerByte, { numerator: 15, denominator: 1 });
  assert.deepEqual(
    [data.maxBlockBodySize, data.maxBlockHeaderSize, data.collateralPercentage, data.maxCollateralInputs, data.poolRetireMaxEpoch, data.stakePoolTargetNum],
    [90112, 1100, 150, 3, 18, 500],
  );
  assert.deepEqual(data.poolPledgeInfluence, { numerator: 3, denominator: 10 });
  assert.deepEqual(data.monetaryExpansion, { numerator: 3, denominator: 1000 });
  assert.deepEqual(data.treasuryCut, { numerator: 1, denominator: 5 });
  assert.equal(data.minPoolCost, '170000000');
  assert.deepEqual(Object.keys(data.costModels), ['PlutusV1', 'PlutusV2', 'PlutusV3']);
  for (const costs of Object.values(data.costModels)) {
    assert.ok(costs.length > 0 && costs.every(Number.isSafeInteger));
  }
  assert.equal(data.costModels.PlutusV3.length, 350);
  assert.deepEqual(data.costModels.PlutusV1.slice(0, 5), [100788, 420, 1, 1, 1000]);
  for (const k of ['decentralisation', 'extraEntropy', 'minUtxoValue', 'nonce']) assert.ok(!(k in data), k);
});

test('genesis parameters are the mainnet Shelley genesis', async () => {
  const { data } = await chain.network.getGenesisParams();
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
});

test('parameter-change bodies use contract names and shapes', () => {
  const changes = govtool.data.proposals.filter((p) => p.type === 'ParameterChange').map((p) => p.body.changes);
  assert.ok(changes.length > 0 && changes.every((c) => Object.keys(c).length > 0));
  for (const c of changes) {
    if ('minPoolCost' in c) assert.match(c.minPoolCost, /^\d+$/);
    for (const k of ['maxTxExecutionUnits', 'maxBlockExecutionUnits']) if (c[k]) assert.ok(Number.isSafeInteger(c[k].memory) && Number.isSafeInteger(c[k].steps));
    for (const costs of Object.values(c.costModels ?? {})) assert.ok(costs.every(Number.isSafeInteger));
  }
});

test('lovelace is an integer string everywhere', async () => {
  const { data } = await chain.network.getStakeDistribution();
  assert.match(data.totalActiveStake, /^\d+$/);
});

test('paging is 1-based and reports the whole filtered set', async () => {
  const first = await chain.governance.proposals.list({ page: 1, size: 5 });
  const second = await chain.governance.proposals.list({ page: 2, size: 5 });
  assert.equal(first.data.elements.length, 5);
  assert.ok(first.data.total > 5);
  assert.equal(first.data.total, second.data.total);
  assert.notDeepEqual(
    first.data.elements.map((p) => p.id),
    second.data.elements.map((p) => p.id),
  );
});

/* -- DReps ------------------------------------------------------------------ */

test('the directory filters by status and by kind', async () => {
  const active = await chain.governance.dreps.list({ page: 1, size: 100, status: ['active'], sort: 'votingPower' });
  assert.ok(active.data.elements.every((d) => d.status === 'active'));

  const anon = await chain.governance.dreps.list({ page: 1, size: 100, kind: ['anonymous'], sort: 'votingPower' });
  // Anonymous is DEFINED as the absence of an anchor — never asserted separately.
  assert.ok(anon.data.elements.length > 0);
  assert.ok(anon.data.elements.every((d) => d.anchor === null));
});

test('a randomly ordered read is not paged', async () => {
  const first = await chain.governance.dreps.list({ page: 1, size: 5 });
  assert.equal(first.data.elements.length, 5);
  await assert.rejects(
    () => chain.governance.dreps.list({ page: 2, size: 5 }),
    (e) => e.code === 'INVALID_INPUT',
  );
});

test('the vote listing covers voted AND not-voted, and the counts agree', async () => {
  const voter = govtool.data.proposals
    .flatMap((p) => govtool.data.votes[p.id] ?? [])
    .find((v) => v.voter.role === 'drep');
  const id = voter.voter.id;

  const all = await chain.governance.dreps.listVotes(id, { page: 1, size: 500 });
  const voted = await chain.governance.dreps.listVotes(id, { page: 1, size: 500, voted: true });
  const notVoted = await chain.governance.dreps.listVotes(id, { page: 1, size: 500, voted: false });

  // The denominator IS the length of the unfiltered listing, so they cannot disagree.
  assert.equal(voted.data.total + notVoted.data.total, all.data.total);
  assert.ok(voted.data.elements.every((r) => r.voted === true && r.choice));
  assert.ok(notVoted.data.elements.every((r) => r.voted === false && r.choice === undefined));
});

/* -- proposals -------------------------------------------------------------- */

test('every proposal carries a typed body for its type', async () => {
  const { data } = await chain.governance.proposals.list({ page: 1, size: 100 });
  for (const action of data.elements) {
    assert.equal(action.body.type, action.type, `${action.id} body/type mismatch`);
  }
});

test('getEnacted is keyed by lineage, and committee covers two action types', async () => {
  const committee = await chain.governance.proposals.getEnacted('committee');
  // Either a real head or null — null is the genesis case, not a failure.
  assert.ok(committee.data === null || typeof committee.data.txHash === 'string');

  const enacted = govtool.data.proposals.filter((p) => p.lifecycle.status === 'enacted');
  const lineages = ['pparamUpdate', 'hardFork', 'committee', 'constitution'];
  for (const lineage of lineages) {
    const { data } = await chain.governance.proposals.getEnacted(lineage);
    if (data !== null) {
      assert.ok(enacted.some((p) => p.txHash === data.txHash), `${lineage} head is not an enacted action`);
    }
  }
});

test('vote aggregates declare their representation', async () => {
  const withVotes = Object.keys(govtool.data.votes)[0];
  const { data } = await chain.governance.proposals.get(withVotes);
  assert.ok(data.voteAggregates.length > 0);
  for (const aggregate of data.voteAggregates) {
    // The fixture counts heads and says so — a consumer must not render an ada
    // prefix on this.
    assert.equal(aggregate.representation, 'count');
    assert.match(aggregate.yes, /^\d+$/);
    assert.ok(aggregate.threshold.denominator > 0);
  }
});

test('the declared representation matches what is actually served', async () => {
  const { data: caps } = await chain.system.getCapabilities();
  const withVotes = Object.keys(govtool.data.votes)[0];
  const { data } = await chain.governance.proposals.get(withVotes);
  for (const aggregate of data.voteAggregates) {
    assert.ok(caps.voteAggregate.includes(aggregate.representation));
  }
});

/* -- committee and constitution --------------------------------------------- */

test('committee members are identified by the cold credential', async () => {
  const { data } = await chain.governance.committee.getCommittee();
  assert.ok(data.members.length > 0);
  for (const member of data.members) {
    assert.match(member.coldCredential, /^cc_cold1/);
    // Hot is nullable — it does not exist until the member authorises one.
    assert.ok(member.hotCredential === null || member.hotCredential.startsWith('cc_hot1'));
  }
  assert.ok(data.quorum.denominator > 0);
});

/* -- the index -------------------------------------------------------------- */

test('the index searches resolved metadata, which chain data does not hold', async () => {
  const named = govtool.data.dreps.find((d) => d._metadataBody?.body?.givenName);
  const name = named._metadataBody.body.givenName.split(/\s+/)[0];

  const { data } = await govtool.index.dreps.searchDReps({ page: 1, size: 20, term: name });
  assert.ok(data.elements.some((d) => d.id === named.id), `"${name}" should find ${named.id}`);

  // Chain data declares only exactId, so the same term finds nothing there.
  const chainSearch = await chain.governance.dreps.list({ page: 1, size: 20, search: name, sort: 'votingPower' });
  assert.equal(chainSearch.data.total, 0);
});

test('the index covers pools', async () => {
  const poolId = govtool.data.pools[0].poolId;
  const { data } = await govtool.index.pools.searchPools({ page: 1, size: 10, term: poolId.slice(5, 15) });
  assert.ok(data.elements.some((p) => p.poolId === poolId));
});

/* -- the satellite services -------------------------------------------------- */

test('metadata resolves a captured document by hash', async () => {
  const anchored = govtool.data.dreps.find((d) => d.anchor && d._metadataBody);
  const result = await govtool.metadata.getMetadata(anchored.anchor.dataHash);
  assert.equal(result.ok, true);
  assert.equal(result.hash, anchored.anchor.dataHash);
});

test('an unknown hash fails honestly rather than inventing a document', async () => {
  const result = await govtool.metadata.getMetadata('0'.repeat(64));
  assert.equal(result.ok, false);
  assert.equal(result.code, 'FETCH_ERROR');
});

test('pinning is content-addressed and blocks until pinned', async () => {
  const bytes = new TextEncoder().encode('{"body":{"givenName":"Test"}}');
  const precomputed = await govtool.pinning.getDataCid(bytes);
  const cid = await govtool.pinning.pinData(bytes, 'drep1test');
  // getDataCid computes without pinning, so an author can anchor before storing.
  assert.equal(cid, precomputed);
  assert.deepEqual(await govtool.pinning.fetch(cid), bytes);
  assert.equal(govtool.pinning.pins.get(cid).owner, 'drep1test');
});

test('the transaction monitor pushes mempool then confirmations', async () => {
  const seen = [];
  await new Promise((resolve) => {
    govtool.txMonitor.add('a'.repeat(64), (update) => {
      seen.push(update.state);
      if (update.confirmations === 5) resolve();
    });
  });
  assert.equal(seen[0], 'mempool');
  assert.equal(seen.filter((s) => s === 'confirmed').length, 5);
});
