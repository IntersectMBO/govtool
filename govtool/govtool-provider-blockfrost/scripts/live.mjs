#!/usr/bin/env node
/**
 * Live conformance check against hosted Blockfrost: every method, contract
 * invariants, and the request count of each read.
 *
 *   BLOCKFROST_PROJECT_ID=... [NETWORK=mainnet] [BLOCKFROST_URL=...] \
 *   [LIVE_FULL=1] npm run live
 *
 * The project id is read from the environment only. It is never printed, and
 * the request log below records paths without headers.
 *
 * LIVE_FULL=1 also reads the WHOLE DRep directory the way the backend's
 * snapshot does (about 4k requests on mainnet) to measure its cost. Without
 * it the directory cost is measured on one 100-row page and extrapolated.
 * Build first (npm run build); it drives ./dist. Exits non-zero on any failed
 * check.
 */
import { HOSTED_URLS, createBlockfrostProvider } from '../dist/index.js';

const env = process.env;
const projectId = env.BLOCKFROST_PROJECT_ID;
if (!projectId && !env.BLOCKFROST_URL) {
  console.error('set BLOCKFROST_PROJECT_ID (hosted) or BLOCKFROST_URL (self-hosted)');
  process.exit(2);
}
const network = env.NETWORK ?? 'mainnet';
const full = env.LIVE_FULL === '1';

/* -- request accounting ------------------------------------------------------ */

let requests = 0;
let throttled = 0;
const byRoute = new Map();
const route = (url) =>
  new URL(url).pathname
    .replace(/^\/api\/v0/, '')
    .replace(/[0-9a-f]{64}/g, '{tx}')
    .replace(/(drep|pool|stake|cc_cold|cc_hot)1[0-9a-z]+/g, '{$1}')
    .replace(/\/\d+(\/|$)/g, '/{n}$1');
const countingFetch = async (url, init) => {
  requests++;
  const key = route(String(url));
  byRoute.set(key, (byRoute.get(key) ?? 0) + 1);
  const response = await fetch(url, init);
  if (response.status === 429) throttled++;
  return response;
};

const { chainData } = createBlockfrostProvider({
  network,
  ...(projectId ? { projectId } : {}),
  ...(env.BLOCKFROST_URL ? { baseUrl: env.BLOCKFROST_URL } : {}),
  fetch: countingFetch,
});

/* -- tiny harness ------------------------------------------------------------ */

let failures = 0;
let passes = 0;
const check = (label, ok, detail) => {
  if (ok) passes++;
  else {
    failures++;
    console.log(`  FAIL ${label}${detail === undefined ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  }
};
const costs = {};
async function measured(label, fn) {
  const before = requests;
  const start = Date.now();
  try {
    return await fn();
  } finally {
    const n = requests - before;
    costs[label] = n;
    console.log(`  ${label}: ${n} requests, ${Date.now() - start} ms`);
  }
}
async function rejects(label, promise, code) {
  try {
    await promise;
    check(label, false, `expected ${code}, resolved`);
  } catch (e) {
    check(label, e?.code === code, `expected ${code}, got ${e?.code}: ${e?.message}`);
  }
}
const cmp = (a, b) => BigInt(a) === BigInt(b);
const sum = (...xs) => xs.reduce((a, x) => a + BigInt(x), 0n);
const CIP129_DREP = /^drep1[02-9ac-hj-np-z]+$/;

/** The backend's snapshot reader: pages of `size` until total or a short page. */
async function readAll(fetchPage, size = 500) {
  const out = [];
  for (let page = 1; page < 100; page++) {
    const { data } = await fetchPage({ page, size });
    out.push(...data.elements);
    if (data.elements.length === 0 || (data.total !== undefined ? out.length >= data.total : data.elements.length < size)) return { out, total: data.total };
    check(`page ${page} full while more remain`, data.elements.length === size);
  }
  throw new Error('did not finish paging');
}

/* -- system ------------------------------------------------------------------ */

console.log(`system (${network})`);
const { data: identity } = await chainData.system.getIdentity();
check('identity id', identity.id === 'blockfrost');
const { data: caps } = await chainData.system.getCapabilities();
const { data: health } = await chainData.system.getHealth();
check('health healthy', health.status === 'healthy', health);
// Structure versus declaration: vote sorts are declared only because dreps.listVotes exists.
check('votes sorts ⇒ dreps.listVotes', !caps.sorts.votes || typeof chainData.governance.dreps.listVotes === 'function');
check('exactId declared', caps.search.includes('exactId'));
check('voteAggregate non-empty', caps.voteAggregate.length > 0);
check('no voterContextOnList', !caps.optionalArguments.includes('proposals.voterContextOnList'));
for (const [area, method] of [
  ['dreps', 'listDelegators'],
  ['pools', 'listVotes'],
]) {
  check(`${area}.${method} omitted`, chainData.governance[area][method] === undefined);
}
check('accounts.getVotingPower omitted', chainData.accounts.getVotingPower === undefined);

/* -- network ----------------------------------------------------------------- */

console.log('network');
const { data: info } = await measured('network.getNetworkInfo', () => chainData.network.getNetworkInfo());
check('network name', info.network === network);
check('era conway', info.era === 'conway', info.era);
check('tip epoch = current', info.tip.epoch === info.currentEpoch);
const { data: params } = await measured('network.getProtocolParams', () => chainData.network.getProtocolParams());
check('params epoch current', params.epoch === info.currentEpoch, params.epoch);
for (const [k, r] of [...Object.entries(params.drepThresholds), ...Object.entries(params.poolThresholds)]) {
  check(`threshold ${k} is a ratio in [0,1]`, Number.isInteger(r.numerator) && r.denominator > 0 && r.numerator <= r.denominator, r);
}
check('lovelace fields are integer strings', ['govActionDeposit', 'drepDeposit', 'keyDeposit', 'poolDeposit', 'coinsPerUtxoByte', 'minPoolCost'].every((k) => /^\d+$/.test(params[k])));
const isPositiveInt = (n) => Number.isSafeInteger(n) && n > 0;
for (const k of ['maxBlockBodySize', 'maxBlockHeaderSize', 'collateralPercentage', 'maxCollateralInputs', 'poolRetireMaxEpoch', 'stakePoolTargetNum']) {
  check(`${k} positive integer`, isPositiveInt(params[k]), params[k]);
}
for (const k of ['maxTxExecutionUnits', 'maxBlockExecutionUnits']) check(`${k} positive safe integers`, isPositiveInt(params[k]?.memory) && isPositiveInt(params[k]?.steps), params[k]);
for (const [k, r] of Object.entries({
  minFeeRefScriptCostPerByte: params.minFeeRefScriptCostPerByte,
  priceMemory: params.executionUnitPrices.memory,
  priceSteps: params.executionUnitPrices.steps,
  poolPledgeInfluence: params.poolPledgeInfluence,
  monetaryExpansion: params.monetaryExpansion,
  treasuryCut: params.treasuryCut,
})) {
  check(`${k} ratio, denominator > 0`, Number.isInteger(r?.numerator) && Number.isInteger(r?.denominator) && r.denominator > 0, r);
}
check('cost models present', Object.keys(params.costModels).length > 0 && Array.isArray(params.costModels.PlutusV3), Object.keys(params.costModels));
for (const [l, c] of Object.entries(params.costModels)) check(`cost model ${l} non-empty integer array`, Array.isArray(c) && c.length > 0 && c.every(Number.isSafeInteger), c?.length);
const { data: genesis } = await measured('network.getGenesisParams', () => chainData.network.getGenesisParams());
if (network === 'mainnet') check('genesis networkMagic 764824073', genesis.networkMagic === 764824073 && genesis.networkId === 'Mainnet', genesis);
check('genesis integers', ['networkMagic', 'epochLength', 'securityParam', 'slotsPerKesPeriod', 'maxKesEvolutions', 'updateQuorum'].every((k) => isPositiveInt(genesis[k])), genesis);
check('genesis activeSlotsCoefficient ratio', genesis.activeSlotsCoefficient.denominator > 0, genesis.activeSlotsCoefficient);
const { data: past } = await chainData.network.getProtocolParams({ epoch: info.currentEpoch - 20 });
check('past params epoch', past.epoch === info.currentEpoch - 20);
if (network === 'mainnet') await rejects('pre-Conway params NOT_FOUND', chainData.network.getProtocolParams({ epoch: 300 }), 'NOT_FOUND');
await rejects('future params NOT_FOUND', chainData.network.getProtocolParams({ epoch: info.currentEpoch + 5 }), 'NOT_FOUND');
const { data: stake } = await measured('network.getStakeDistribution', () => chainData.network.getStakeDistribution());
check('active stake integer', /^\d+$/.test(stake.totalActiveStake));
check('DRep stake ≤ live stake', BigInt(stake.totalStakeControlledByDReps) <= BigInt(stake.totalLiveStake));
const { data: treasury } = await chainData.network.getTreasury();
check('treasury', /^\d+$/.test(treasury.balance) && /^\d+$/.test(treasury.reserves));
await rejects('past treasury refused', chainData.network.getTreasury({ epoch: info.currentEpoch - 1 }), 'CAPABILITY_UNSUPPORTED');

/* -- committee --------------------------------------------------------------- */

console.log('committee');
const { data: committee } = await measured('committee.getCommittee', () => chainData.governance.committee.getCommittee());
check('committee members', committee.members.length > 0);
check('cold ids CIP-129', committee.members.every((m) => m.coldCredential.startsWith('cc_cold1')));
check('hot ids CIP-129 or null', committee.members.every((m) => m.hotCredential === null || m.hotCredential.startsWith('cc_hot1')));
check('quorum', committee.quorum.denominator > 0);
const member = committee.members[0];
const { data: m1 } = await chainData.governance.committee.getMember(member.coldCredential);
check('getMember', m1.coldCredential === member.coldCredential);
const { data: constitution } = await measured('committee.getConstitution', () => chainData.governance.committee.getConstitution());
check('constitution anchor', typeof constitution.anchor.url === 'string' && /^[0-9a-f]{64}$/.test(constitution.anchor.dataHash));

/* -- proposals --------------------------------------------------------------- */

console.log('proposals');
const { out: proposals, total: proposalTotal } = await measured('proposals.list (full, size 500)', () =>
  readAll((p) => chainData.governance.proposals.list(p)),
);
check('proposal total = rows', proposalTotal === proposals.length, { proposalTotal, rows: proposals.length });
check('proposal ids unique', new Set(proposals.map((p) => p.id)).size === proposals.length);
let liveCount = 0;
for (const p of proposals) {
  const live = p.lifecycle.status === 'live';
  if (live) liveCount++;
  check(`${p.id} body typed`, p.body.type === p.type);
  check(`${p.id} anchor`, p.anchor && /^[0-9a-f]{64}$/.test(p.anchor.dataHash));
  check(`${p.id} submitted`, Number.isInteger(p.lifecycle.submitted.epoch) && typeof p.lifecycle.submitted.time === 'string');
  check(`${p.id} expires after submitted`, p.lifecycle.expires === null || p.lifecycle.expires.epoch > p.lifecycle.submitted.epoch);
  check(`${p.id} aggregates iff live`, live === (p.voteAggregates !== undefined));
  for (const a of p.voteAggregates ?? []) {
    check(`${p.id} ${a.role} sums`, cmp(sum(a.yes, a.no, a.abstain, a.notVoted), a.totalEligible), a);
    check(`${p.id} ${a.role} representation`, a.representation === (a.role === 'cc' ? 'count' : 'stake'));
    check(`${p.id} ${a.role} threshold`, a.threshold.denominator > 0 && a.threshold.numerator <= a.threshold.denominator);
  }
  if (p.type === 'TreasuryWithdrawals') {
    check(`${p.id} withdrawal total`, cmp(sum(...p.body.withdrawals.map((w) => w.amount)), p.body.totalAmount));
    check(`${p.id} withdrawal addresses`, p.body.withdrawals.every((w) => w.stakeAddress.startsWith(network === 'mainnet' ? 'stake1' : 'stake_test1')));
  }
  if (p.lifecycle.status === 'dropped') check(`${p.id} droppedAt`, p.lifecycle.droppedAt !== null);
  if (p.lifecycle.status === 'expired') check(`${p.id} expired not dropped`, p.lifecycle.droppedAt === null);
}
console.log(`  ${proposals.length} proposals, ${liveCount} live`);
const byStatus = {};
for (const p of proposals) byStatus[p.lifecycle.status] = (byStatus[p.lifecycle.status] ?? 0) + 1;
console.log('  by status', byStatus);

const { data: livePage } = await chainData.governance.proposals.list({ page: 1, size: 100, status: ['live'] });
check('status filter live total', livePage.total === liveCount, { total: livePage.total, liveCount });
const { data: expPage } = await chainData.governance.proposals.list({ page: 1, size: 3, sort: 'soonestToExpire' });
check('soonestToExpire order', expPage.elements.every((p, i, a) => i === 0 || (p.lifecycle.expires?.epoch ?? Infinity) >= (a[i - 1].lifecycle.expires?.epoch ?? Infinity)));
const { data: infoPage } = await chainData.governance.proposals.list({ page: 1, size: 2, type: ['InfoAction'], sort: 'oldest' });
check('type filter', infoPage.elements.every((p) => p.type === 'InfoAction') && infoPage.total === proposals.filter((p) => p.type === 'InfoAction').length);
await rejects('mostYesVotes refused', chainData.governance.proposals.list({ page: 1, size: 1, sort: 'mostYesVotes' }), 'CAPABILITY_UNSUPPORTED');
await rejects('voterId on list refused', chainData.governance.proposals.list({ page: 1, size: 1, voterId: committee.members[0].coldCredential }), 'CAPABILITY_UNSUPPORTED');

const liveOne = proposals.find((p) => p.lifecycle.status === 'live');
const concluded = proposals.find((p) => p.lifecycle.status === 'enacted');
if (liveOne) {
  const { data: detail } = await measured('proposals.get (live, with aggregates)', () => chainData.governance.proposals.get(liveOne.id));
  check('get equals list row', JSON.stringify(detail.body) === JSON.stringify(liveOne.body) && detail.voteAggregates?.length === liveOne.voteAggregates?.length);
  const { data: votes } = await measured('proposals.listVotes (page of 20)', () => chainData.governance.proposals.listVotes(liveOne.id, { page: 1, size: 20 }));
  check('listVotes total', votes.total >= votes.elements.length);
  check('listVotes anchors are anchors or null', votes.elements.every((v) => v.anchor === null || /^[0-9a-f]{64}$/.test(v.anchor.dataHash)));
  const dvote = votes.elements.find((v) => v.voter.role === 'drep');
  if (dvote) {
    check('drep voter CIP-129', CIP129_DREP.test(dvote.voter.id));
    const { data: withMine } = await chainData.governance.proposals.get(liveOne.id, { voterId: dvote.voter.id });
    check('myVote found', withMine.myVote?.choice === dvote.choice && withMine.myVote?.txRef.txHash === dvote.txRef.txHash);
    const drepAgg = liveOne.voteAggregates.find((a) => a.role === 'drep');
    check('drep aggregate saw votes', BigInt(drepAgg.yes) + BigInt(drepAgg.no) + BigInt(drepAgg.abstain) > 0n);
  }
  const ccVotes = (await chainData.governance.proposals.listVotes(liveOne.id, { page: 1, size: 1000 })).data.elements.filter((v) => v.voter.role === 'cc');
  check('cc voters hot CIP-129', ccVotes.every((v) => v.voter.hot.startsWith('cc_hot1')));
  console.log(`  live ${liveOne.id}: ${votes.total} votes, ${ccVotes.length} committee`);
  const { data: activity } = await chainData.governance.proposals.listActivity(liveOne.id, { page: 1, size: 10 });
  check('activity live first', activity.elements[0]?.status === 'live');
}
if (concluded) {
  const { data: detail } = await chainData.governance.proposals.get(concluded.id);
  check('concluded has no aggregates', detail.voteAggregates === undefined);
  const { data: act } = await chainData.governance.proposals.listActivity(concluded.id, { page: 1, size: 10 });
  check('enacted activity', act.elements.some((e) => e.status === 'enacted'));
}
for (const lineage of ['pparamUpdate', 'hardFork', 'committee', 'constitution']) {
  const { data: head } = await measured(`proposals.getEnacted(${lineage})`, () => chainData.governance.proposals.getEnacted(lineage));
  const types = { pparamUpdate: ['ParameterChange'], hardFork: ['HardForkInitiation'], committee: ['UpdateCommittee', 'NoConfidence'], constitution: ['NewConstitution'] }[lineage];
  const enacted = proposals.filter((p) => types.includes(p.type) && p.lifecycle.status === 'enacted');
  check(`${lineage} head enacted`, head === null ? enacted.length === 0 : enacted.some((p) => p.id === head.id));
  // Nothing enacted after the head names it as predecessor.
  if (head) check(`${lineage} head is not anyone's predecessor`, !enacted.some((p) => p.previousAction?.id === head.id));
  if (lineage === 'committee' && head && committee.enactedBy) check('committee head = committee.enactedBy', head.id === committee.enactedBy.id);
  if (lineage === 'constitution' && head) check('constitution enactedBy = head', constitution.enactedBy?.id === head.id);
}
await rejects('proposal CIP-129 required', chainData.governance.proposals.get(`${proposals[0].txHash}#0`), 'INVALID_INPUT');

/* -- dreps ------------------------------------------------------------------- */

console.log('dreps');
const { data: counts } = await measured('dreps.getCounts', () => chainData.governance.dreps.getCounts());
check('counts add up', counts.totalActive + counts.totalInactive === counts.totalRegistered, counts);
const { data: top } = await measured('dreps.list (votingPower, size 20)', () => chainData.governance.dreps.list({ page: 1, size: 20, sort: 'votingPower' }));
check('directory total ≥ registered', top.total >= counts.totalRegistered);
check('page full', top.elements.length === 20);
check('ordered by power', top.elements.every((d, i, a) => i === 0 || BigInt(d.votingPower.amount) <= BigInt(a[i - 1].votingPower.amount)));
for (const d of top.elements) {
  check(`${d.id} CIP-129`, CIP129_DREP.test(d.id));
  check(`${d.id} kind ↔ anchor`, (d.kind === 'anonymous') === (d.anchor === null));
  check(`${d.id} registration dated`, Number.isInteger(d.registration.latest.at.epoch) && typeof d.registration.latest.at.time === 'string');
  check(`${d.id} update after registration`, d.registration.latestUpdate === null || d.registration.latestUpdate.at.slot >= d.registration.latest.at.slot);
}
const { data: inactive } = await chainData.governance.dreps.list({ page: 1, size: 1, sort: 'votingPower', status: ['inactive'] });
check('status filter total = counts', inactive.total === counts.totalInactive, { total: inactive.total, counts: counts.totalInactive });
const { data: anon } = await chainData.governance.dreps.list({ page: 1, size: 1, sort: 'votingPower', kind: ['anonymous'], status: ['active', 'inactive'] });
check('kind filter total = counts.anonymous', anon.total === counts.anonymous);
const { data: random } = await chainData.governance.dreps.list({ page: 1, size: 5 });
check('random page', random.elements.length === 5);
await rejects('random page 2 refused', chainData.governance.dreps.list({ page: 2, size: 5 }), 'INVALID_INPUT');
await rejects('registrationDate refused', chainData.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate' }), 'CAPABILITY_UNSUPPORTED');
const sample = top.elements[0];
const { data: one } = await measured('dreps.get', () => chainData.governance.dreps.get(sample.id));
check('get equals list row', JSON.stringify(one) === JSON.stringify(sample));
const { data: found } = await chainData.governance.dreps.list({ page: 1, size: 5, sort: 'votingPower', search: sample.id });
check('exactId search', found.total === 1 && found.elements[0].id === sample.id);
// CIP-105: same credential, no header byte. Must be refused, not resolved.
const { bech32 } = await import('bech32');
const bytes = Buffer.from(bech32.fromWords(bech32.decode(sample.id, 1023).words));
const cip105 = bech32.encode('drep', bech32.toWords(bytes.subarray(1)), 1023);
await rejects('CIP-105 get refused', chainData.governance.dreps.get(cip105), 'INVALID_INPUT');
const { data: none } = await chainData.governance.dreps.list({ page: 1, size: 5, sort: 'votingPower', search: cip105 });
check('CIP-105 search matches nothing', none.total === 0);

const { data: votes } = await measured('dreps.listVotes (page of 20)', () => chainData.governance.dreps.listVotes(sample.id, { page: 1, size: 20 }));
const { data: votedOnly } = await chainData.governance.dreps.listVotes(sample.id, { page: 1, size: 1000, voted: true });
const { data: notVoted } = await chainData.governance.dreps.listVotes(sample.id, { page: 1, size: 1, voted: false });
check('voted + not voted = all', votedOnly.total + notVoted.total === votes.total, { voted: votedOnly.total, not: notVoted.total, all: votes.total });
check('voted rows carry choice and txRef', votedOnly.elements.every((r) => r.voted && r.choice && r.txRef.txHash));
check('not-voted rows carry no choice', notVoted.elements.every((r) => !r.voted && !('choice' in r) && !('anchor' in r)));
check('voted ⊆ votable', votedOnly.total <= votes.total);
console.log(`  ${sample.id}: voted ${votedOnly.total} of ${votes.total}`);
const { data: history } = await measured('dreps.listUpdateHistory', () => chainData.governance.dreps.listUpdateHistory(sample.id, { page: 1, size: 10, sort: 'asc' }));
check('history first is the registration', history.elements[0]?.deposit !== null && history.elements[0] !== undefined);
check('history last anchor = DRep anchor', sample.registration.latestUpdate === null || JSON.stringify(history.elements[history.elements.length - 1]?.anchor) === JSON.stringify(sample.anchor) || history.total > 10);

// A page-cost sample, or the full backend snapshot with LIVE_FULL=1.
if (full) {
  const { out: all, total } = await measured('dreps.list (full directory, size 500)', () =>
    readAll((p) => chainData.governance.dreps.list({ ...p, sort: 'votingPower' })),
  );
  check('full directory complete', all.length === total && new Set(all.map((d) => d.id)).size === all.length);
} else {
  await measured('dreps.list (votingPower, size 100)', () => chainData.governance.dreps.list({ page: 2, size: 100, sort: 'votingPower' }));
}

/* -- pools ------------------------------------------------------------------- */

console.log('pools');
const { data: pools } = await measured('pools.list (size 20)', () => chainData.governance.pools.list({ page: 1, size: 20 }));
check('pool page full', pools.elements.length === 20);
check('pool ids', pools.elements.every((p) => p.poolId.startsWith('pool1') && /^\d+$/.test(p.votingPower.amount)));
const { data: pool } = await measured('pools.get', () => chainData.governance.pools.get(pools.elements[0].poolId));
check('pool get', pool.poolId === pools.elements[0].poolId && pool.activeStake === pools.elements[0].activeStake);
check('pool anchor agrees', JSON.stringify(pool.anchor) === JSON.stringify(pools.elements[0].anchor));
const { data: poolSearch } = await chainData.governance.pools.list({ page: 1, size: 5, search: pool.poolId });
check('pool search', poolSearch.total === 1);
const { data: pools2 } = await chainData.governance.pools.list({ page: 2, size: 150 });
check('pool page spanning Blockfrost pages', pools2.elements.length === 150);

/* -- accounts, transactions ---------------------------------------------------- */

console.log('accounts, transactions');
// A delegator of the sample DRep, from Blockfrost itself.
const base = env.BLOCKFROST_URL ?? HOSTED_URLS[network];
const delegators = await (
  await countingFetch(`${base}/governance/dreps/${sample.id}/delegators?count=5`, { headers: projectId ? { project_id: projectId } : {} })
).json();
const delegator = delegators.find((d) => BigInt(d.amount) > 0n)?.address;
if (delegator) {
  const { data: account } = await measured('accounts.get', () => chainData.accounts.get(delegator));
  check('account registered', account.isRegistered === true && account.stakeAddress === delegator);
  const { data: delegation } = await measured('accounts.getDelegation', () => chainData.accounts.getDelegation(delegator));
  check('delegation to the DRep, CIP-129', delegation?.target.kind === 'drep' && delegation.target.drep.id === sample.id, delegation);
  const { data: poolDelegation } = await chainData.accounts.getPoolDelegation(delegator);
  check('pool delegation shape', poolDelegation === null || poolDelegation.poolId.startsWith('pool1'));
}
// A well-formed address nobody has used: a known "never registered".
const fresh = bech32.encode(network === 'mainnet' ? 'stake' : 'stake_test', bech32.toWords(Buffer.from([network === 'mainnet' ? 0xe1 : 0xe0, ...Buffer.alloc(28, 0x5a)])), 1023);
const { data: freshAccount } = await chainData.accounts.get(fresh);
check('unknown address not registered', freshAccount.isRegistered === false);
check('unknown address no delegation', (await chainData.accounts.getDelegation(fresh)).data === null);
const { data: tx } = await measured('transactions.get', () => chainData.transactions.get(proposals[0].txHash));
check('tx on chain', tx.onChain === true && tx.includedAt.epoch === proposals[0].lifecycle.submitted.epoch);
const { data: missing } = await chainData.transactions.get('00'.repeat(32));
check('unknown tx not on chain', missing.onChain === false);

/* -- report ------------------------------------------------------------------- */

console.log(`\n${passes} passed, ${failures} failed; ${requests} requests in total, ${throttled} answered 429`);
console.log('cost by method:', costs);
console.log('requests by route:', Object.fromEntries([...byRoute.entries()].sort((a, b) => b[1] - a[1])));
process.exit(failures ? 1 : 0);
