#!/usr/bin/env node
/**
 * Live conformance check against a real Koios deployment. Not part of
 * `npm run verify`: it needs the network, and chain data moves.
 *
 *   npm run build && npm run live
 *   KOIOS_NETWORK=preprod npm run live
 *   KOIOS_BASE_URL=https://koios.example/api/v1 KOIOS_NETWORK=mainnet npm run live
 *   KOIOS_TOKEN=... npm run live            # the token is never printed
 *
 * Exercises every method and asserts the contract's own invariants on real
 * answers — the things a fixture cannot catch because whoever writes the
 * fixture also writes the mapper. Requests are sequential, with a small pause
 * between checks, to stay inside the public tier's rate limit. Exits non-zero
 * on any failed check.
 */
import { bech32 } from 'bech32';

import { capabilities, createKoiosProvider } from '../dist/index.js';

const env = process.env;
const network = env.KOIOS_NETWORK ?? 'mainnet';
let requests = 0;
const countingFetch = (url, init) => {
  requests++;
  return fetch(url, init);
};
const { chainData: c } = createKoiosProvider({
  network,
  ...(env.KOIOS_BASE_URL ? { baseUrl: env.KOIOS_BASE_URL } : {}),
  ...(env.KOIOS_TOKEN ? { token: env.KOIOS_TOKEN } : {}),
  maxConcurrency: 2,
  fetch: countingFetch,
});

/* -- harness ----------------------------------------------------------------- */

let passes = 0;
let failures = 0;
const notes = [];
const check = (label, ok, detail) => {
  if (ok) passes++;
  else {
    failures++;
    console.log(`  FAIL ${label}${detail === undefined ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail).slice(0, 400)}`}`);
  }
};
const note = (text) => {
  notes.push(text);
  console.log(`  NOTE ${text}`);
};
const pause = (ms = 300) => new Promise((r) => setTimeout(r, ms));
async function step(label, fn) {
  const start = Date.now();
  const r0 = requests;
  try {
    const out = await fn();
    console.log(`  ${label}: ${Date.now() - start} ms, ${requests - r0} requests`);
    return out;
  } catch (error) {
    check(`${label} threw`, false, `${error.code ?? ''} ${error.message}`);
    return undefined;
  } finally {
    await pause();
  }
}
async function rejects(label, fn, code) {
  try {
    await fn();
    check(`${label} rejects with ${code}`, false, 'resolved');
  } catch (error) {
    check(`${label} rejects with ${code}`, error.code === code, `${error.code}: ${error.message}`);
  }
  await pause();
}

/* -- invariants -------------------------------------------------------------- */

const LOVELACE = /^\d+$/;
const isLovelace = (v) => typeof v === 'string' && LOVELACE.test(v);
const isRatio = (r) => r && Number.isInteger(r.numerator) && Number.isInteger(r.denominator) && r.denominator !== 0 && r.numerator >= 0;
const decode = (id) => {
  const d = bech32.decode(id, 1023);
  return { prefix: d.prefix, bytes: Buffer.from(bech32.fromWords(d.words)) };
};
function cip129(id, prefix, keyNibble) {
  try {
    const { prefix: p, bytes } = decode(id);
    return p === prefix && bytes.length === 29 && bytes[0] >> 4 === keyNibble && [2, 3].includes(bytes[0] & 0x0f);
  } catch {
    return false;
  }
}
const isDRepId = (id) => cip129(id, 'drep', 0x2);
const isColdId = (id) => cip129(id, 'cc_cold', 0x1);
const isHotId = (id) => cip129(id, 'cc_hot', 0x0);
function isPoolId(id) {
  try {
    const { prefix, bytes } = decode(id);
    return prefix === 'pool' && bytes.length === 28;
  } catch {
    return false;
  }
}
function isGovActionId(id, txHash, index) {
  try {
    const { prefix, bytes } = decode(id);
    return prefix === 'gov_action' && bytes.length === 33 && bytes.subarray(0, 32).toString('hex') === txHash && bytes[32] === index;
  } catch {
    return false;
  }
}
const stakePrefix = network === 'mainnet' ? 'stake' : 'stake_test';
function isStakeAddress(a) {
  try {
    const { prefix, bytes } = decode(a);
    return prefix === stakePrefix && bytes.length === 29 && [0xe0, 0xf0].includes(bytes[0] & 0xf0);
  } catch {
    return false;
  }
}
const isStamp = (s) => s && Number.isInteger(s.epoch) && s.epoch >= 0 && (s.time === undefined || !Number.isNaN(Date.parse(s.time)));
const isEnvelope = (e) => e && 'data' in e && e.meta?.provider === 'koios' && e.meta?.network === network;

function checkPage(label, env, size, page = 1) {
  check(`${label} envelope`, isEnvelope(env));
  const { elements, total } = env.data;
  check(`${label} total is an integer`, Number.isInteger(total), total);
  check(`${label} total >= elements`, total >= elements.length, { total, n: elements.length });
  if (size !== undefined) {
    const expected = Math.max(0, Math.min(size, total - (page - 1) * size));
    check(`${label} page not short unless last`, elements.length === expected, { n: elements.length, size, total, page });
  }
}

function checkAggregate(label, a) {
  const f = ['yes', 'no', 'abstain', 'notVoted', 'totalEligible'];
  check(`${label} figures are integer strings`, f.every((k) => isLovelace(a[k])), a);
  check(`${label} yes+no+abstain+notVoted = totalEligible`, BigInt(a.yes) + BigInt(a.no) + BigInt(a.abstain) + BigInt(a.notVoted) === BigInt(a.totalEligible), a);
  check(`${label} threshold is a ratio`, isRatio(a.threshold), a.threshold);
  check(`${label} representation`, a.representation === (a.role === 'cc' ? 'count' : 'stake'), a.representation);
}

const SPO_VOTES = (a) =>
  a.type === 'NoConfidence' || a.type === 'UpdateCommittee' || a.type === 'HardForkInitiation' || a.type === 'InfoAction' || a.type === 'ParameterChange';
const CC_VOTES = (a) => !['NoConfidence', 'UpdateCommittee'].includes(a.type);

function checkAction(label, a, currentEpoch) {
  check(`${label} id matches tx hash and index`, isGovActionId(a.id, a.txHash, a.index), a.id);
  check(`${label} body type is the action type`, a.body?.type === a.type, { type: a.type, body: a.body?.type });
  const l = a.lifecycle;
  check(`${label} submitted stamp`, isStamp(l.submitted) && l.submitted.time !== undefined, l.submitted);
  const expected = l.enactedAt ? 'enacted' : l.ratifiedAt ? 'ratified' : l.expiredAt ? 'expired' : l.droppedAt ? 'dropped' : 'live';
  check(`${label} status agrees with stamps`, l.status === expected, { status: l.status, expected });
  check(`${label} deposit lovelace`, a.deposit === null || isLovelace(a.deposit), a.deposit);
  check(`${label} return address`, a.depositReturnAddress === null || isStakeAddress(a.depositReturnAddress), a.depositReturnAddress);
  if (a.previousAction) check(`${label} previous action id`, isGovActionId(a.previousAction.id, a.previousAction.txHash, a.previousAction.index));
  if (a.type === 'ParameterChange') {
    const ch = a.body.changes ?? {};
    check(`${label} parameter change is not empty`, Object.keys(ch).length > 0, ch);
    check(`${label} parameter change carries no ledger-only key names`, !Object.keys(ch).some((k) => ['txFeePerByte', 'dRepDeposit', 'maxValueSize', 'utxoCostPerByte'].includes(k)), Object.keys(ch));
    for (const [l, costs] of Object.entries(ch.costModels ?? {})) check(`${label} changed cost model ${l} integers`, Array.isArray(costs) && costs.length > 0 && costs.every(Number.isSafeInteger));
    for (const k of ['maxTxExecutionUnits', 'maxBlockExecutionUnits']) if (ch[k]) check(`${label} changed ${k}`, Number.isSafeInteger(ch[k].memory) && Number.isSafeInteger(ch[k].steps), ch[k]);
    if (ch.executionUnitPrices) check(`${label} changed prices`, isRatio(ch.executionUnitPrices.memory) && isRatio(ch.executionUnitPrices.steps), ch.executionUnitPrices);
  }
  if (a.anchor) check(`${label} anchor hash hex`, /^[0-9a-f]{64}$/.test(a.anchor.dataHash), a.anchor);
  if (a.body.type === 'TreasuryWithdrawals') {
    check(`${label} withdrawals`, a.body.withdrawals.every((w) => isStakeAddress(w.stakeAddress) && isLovelace(w.amount)), a.body.withdrawals);
    check(`${label} total = sum`, a.body.withdrawals.reduce((s, w) => s + BigInt(w.amount), 0n) === BigInt(a.body.totalAmount));
  }
  if (a.body.type === 'UpdateCommittee') {
    check(`${label} committee quorum`, isRatio(a.body.quorum));
    check(`${label} committee credentials`, [...a.body.added, ...a.body.removed].every((m) => isColdId(m.coldCredential)));
  }
  const aggs = a.voteAggregates ?? [];
  const roles = aggs.map((x) => x.role);
  check(`${label} has a drep aggregate`, roles.includes('drep'), roles);
  if (SPO_VOTES(a) && a.type !== 'ParameterChange') check(`${label} has an spo aggregate`, roles.includes('spo'), roles);
  if (!SPO_VOTES(a)) check(`${label} has no spo aggregate`, !roles.includes('spo'), roles);
  const live = l.status === 'live';
  if (live && CC_VOTES(a)) check(`${label} live action has a cc aggregate`, roles.includes('cc'), roles);
  if (!CC_VOTES(a)) check(`${label} no cc aggregate`, !roles.includes('cc'), roles);
  for (const x of aggs) checkAggregate(`${label} ${x.role}`, x);
  void currentEpoch;
}

/* -- run --------------------------------------------------------------------- */

console.log(`Koios live conformance: ${network}${env.KOIOS_BASE_URL ? ` at ${env.KOIOS_BASE_URL}` : ''}`);

console.log('system');
const identity = await step('getIdentity', () => c.system.getIdentity());
check('identity', identity?.data.id === 'koios' && typeof identity.data.name === 'string');
const caps = await step('getCapabilities', () => c.system.getCapabilities());
check('capabilities match export', JSON.stringify(caps?.data) === JSON.stringify(capabilities()));
check('newest and oldest declared', ['newest', 'oldest'].every((s) => caps?.data.sorts.proposals.includes(s)));
check('exactId declared', caps?.data.search.includes('exactId'));
check('a vote aggregate representation declared', caps?.data.voteAggregate.length > 0);
const health = await step('getHealth', () => c.system.getHealth());
check('health', ['healthy', 'degraded'].includes(health?.data.status) && isStamp(health.data.tip), health?.data);

console.log('network');
const info = await step('getNetworkInfo', () => c.network.getNetworkInfo());
const currentEpoch = info?.data.currentEpoch;
check('network info', info?.data.network === network && info.data.era === 'conway' && info.data.tip.epoch === currentEpoch, info?.data);
const params = await step('getProtocolParams', () => c.network.getProtocolParams());
function checkParams(label, p, epoch) {
  check(`${label} epoch`, p.epoch === epoch, p.epoch);
  for (const k of ['govActionDeposit', 'drepDeposit', 'keyDeposit', 'poolDeposit', 'coinsPerUtxoByte']) check(`${label} ${k} lovelace`, isLovelace(p[k]), p[k]);
  for (const k of ['govActionLifetime', 'drepActivity', 'committeeMinSize', 'committeeMaxTermLength', 'minFeeA', 'minFeeB', 'maxTxSize', 'maxValSize']) {
    check(`${label} ${k} integer`, Number.isInteger(p[k]), p[k]);
  }
  check(`${label} drep thresholds`, Object.values(p.drepThresholds).length === 10 && Object.values(p.drepThresholds).every(isRatio), p.drepThresholds);
  check(`${label} pool thresholds`, Object.values(p.poolThresholds).length === 5 && Object.values(p.poolThresholds).every(isRatio), p.poolThresholds);
  check(`${label} thresholds <= 1`, [...Object.values(p.drepThresholds), ...Object.values(p.poolThresholds)].every((r) => r.numerator <= r.denominator));
  checkFullParams(label, p);
}
const isPositiveInt = (n) => Number.isSafeInteger(n) && n > 0;
/** The D136 parameters: every rational has a positive denominator, cost models are integer arrays. */
function checkFullParams(label, p) {
  for (const k of ['maxBlockBodySize', 'maxBlockHeaderSize', 'collateralPercentage', 'maxCollateralInputs', 'poolRetireMaxEpoch', 'stakePoolTargetNum']) {
    check(`${label} ${k} positive integer`, isPositiveInt(p[k]), p[k]);
  }
  for (const k of ['maxTxExecutionUnits', 'maxBlockExecutionUnits']) {
    check(`${label} ${k} positive safe integers`, isPositiveInt(p[k]?.memory) && isPositiveInt(p[k]?.steps), p[k]);
  }
  check(`${label} block budget >= tx budget`, p.maxBlockExecutionUnits.memory >= p.maxTxExecutionUnits.memory && p.maxBlockExecutionUnits.steps >= p.maxTxExecutionUnits.steps);
  const ratios = {
    minFeeRefScriptCostPerByte: p.minFeeRefScriptCostPerByte,
    priceMemory: p.executionUnitPrices?.memory,
    priceSteps: p.executionUnitPrices?.steps,
    poolPledgeInfluence: p.poolPledgeInfluence,
    monetaryExpansion: p.monetaryExpansion,
    treasuryCut: p.treasuryCut,
  };
  for (const [k, r] of Object.entries(ratios)) check(`${label} ${k} ratio, denominator > 0`, isRatio(r) && r.denominator > 0, r);
  check(`${label} minPoolCost lovelace`, isLovelace(p.minPoolCost), p.minPoolCost);
  const languages = Object.keys(p.costModels ?? {});
  check(`${label} cost models name only Plutus languages`, languages.length > 0 && languages.every((l) => ['PlutusV1', 'PlutusV2', 'PlutusV3'].includes(l)), languages);
  for (const [l, costs] of Object.entries(p.costModels ?? {})) {
    check(`${label} cost model ${l} non-empty integer array`, Array.isArray(costs) && costs.length > 0 && costs.every(Number.isSafeInteger), costs?.length);
  }
  check(`${label} Conway has a PlutusV3 cost model`, Array.isArray(p.costModels?.PlutusV3));
  for (const k of ['decentralisation', 'extraEntropy', 'minUtxoValue', 'nonce', 'cost_models']) check(`${label} does not carry ${k}`, !(k in p));
}
if (params) checkParams('params', params.data, currentEpoch);
const past = await step('getProtocolParams({ epoch })', () => c.network.getProtocolParams({ epoch: currentEpoch - 10 }));
if (past) checkParams('past params', past.data, currentEpoch - 10);
await rejects('params for a future epoch', () => c.network.getProtocolParams({ epoch: currentEpoch + 5 }), 'NOT_FOUND');
if (network === 'mainnet') await rejects('params before Conway', () => c.network.getProtocolParams({ epoch: 400 }), 'NOT_FOUND');
await rejects('params for a bad epoch', () => c.network.getProtocolParams({ epoch: -1 }), 'INVALID_INPUT');
const genesis = await step('getGenesisParams', () => c.network.getGenesisParams());
if (genesis) {
  const g = genesis.data;
  check('genesis envelope', isEnvelope(genesis));
  if (network === 'mainnet') {
    check('genesis networkMagic 764824073', g.networkMagic === 764824073, g.networkMagic);
    check('genesis networkId Mainnet', g.networkId === 'Mainnet', g.networkId);
    check('genesis systemStart', g.systemStart === '2017-09-23T21:44:51Z', g.systemStart);
  } else check('genesis networkId Testnet', g.networkId === 'Testnet', g.networkId);
  for (const k of ['networkMagic', 'epochLength', 'securityParam', 'slotsPerKesPeriod', 'maxKesEvolutions', 'updateQuorum']) check(`genesis ${k} positive integer`, isPositiveInt(g[k]), g[k]);
  check('genesis slotLength > 0', typeof g.slotLength === 'number' && g.slotLength > 0, g.slotLength);
  check('genesis activeSlotsCoefficient in (0, 1]', isRatio(g.activeSlotsCoefficient) && g.activeSlotsCoefficient.numerator > 0 && g.activeSlotsCoefficient.numerator <= g.activeSlotsCoefficient.denominator, g.activeSlotsCoefficient);
  check('genesis maxLovelaceSupply lovelace', isLovelace(g.maxLovelaceSupply), g.maxLovelaceSupply);
  check('genesis systemStart ISO', !Number.isNaN(Date.parse(g.systemStart)), g.systemStart);
}
const treasury = await step('getTreasury', () => c.network.getTreasury());
check('treasury', treasury && isLovelace(treasury.data.balance) && isLovelace(treasury.data.reserves) && treasury.data.epoch === currentEpoch, treasury?.data);
const pastTreasury = await step('getTreasury({ epoch })', () => c.network.getTreasury({ epoch: currentEpoch - 20 }));
check('past treasury', pastTreasury?.data.epoch === currentEpoch - 20 && isLovelace(pastTreasury.data.balance));
await rejects('treasury for a future epoch', () => c.network.getTreasury({ epoch: currentEpoch + 5 }), 'NOT_FOUND');
const stake = await step('getStakeDistribution', () => c.network.getStakeDistribution());
if (stake) {
  const s = stake.data;
  for (const [k, v] of Object.entries(s)) if (k !== 'epoch') check(`stake ${k} lovelace`, isLovelace(v), v);
  check('active stake > 0', BigInt(s.totalActiveStake) > 0n);
  check('DRep stake below all active DRep-able stake', BigInt(s.totalStakeControlledByDReps ?? 0) > 0n);
}

console.log('proposals');
const all = [];
let total;
for (let page = 1; ; page++) {
  const p = await step(`proposals.list page ${page}`, () => c.governance.proposals.list({ page, size: 50 }));
  if (!p) break;
  checkPage(`proposals page ${page}`, p, 50, page);
  total = p.data.total;
  all.push(...p.data.elements);
  if (p.data.elements.length < 50) break;
}
check('proposal pages cover the total', all.length === total, { n: all.length, total });
check('proposal pages are disjoint', new Set(all.map((a) => a.id)).size === all.length);
check(
  'newest first',
  all.every((a, i) => i === 0 || Date.parse(all[i - 1].lifecycle.submitted.time) >= Date.parse(a.lifecycle.submitted.time)),
);
for (const a of all) checkAction(a.id.slice(0, 22), a, currentEpoch);
const byStatus = Object.groupBy ? Object.groupBy(all, (a) => a.lifecycle.status) : {};
note(`proposals by status: ${Object.entries(byStatus).map(([k, v]) => `${k} ${v.length}`).join(', ')}`);
const concludedWithoutCc = all.filter((a) => a.lifecycle.status !== 'live' && CC_VOTES(a) && !(a.voteAggregates ?? []).some((x) => x.role === 'cc'));
note(`${concludedWithoutCc.length} concluded actions carry no cc aggregate (no historical committee in Koios)`);

// Expiry convention: the db-sync provider's, i.e. Koios' `expiration` (Decisions OPEN-76).
const sample = all.filter((a) => a.lifecycle.expires).slice(0, 3);
for (const a of sample) {
  const pp = await step(`params at submission of ${a.id.slice(0, 16)}`, () => c.network.getProtocolParams({ epoch: a.lifecycle.submitted.epoch }));
  if (pp) {
    const diff = a.lifecycle.expires.epoch - a.lifecycle.submitted.epoch - pp.data.govActionLifetime;
    check('expires = submitted + govActionLifetime + 1 (db-sync convention)', diff === 1, { diff });
  }
}

const live = await step('list status live', () => c.governance.proposals.list({ page: 1, size: 100, status: ['live'] }));
check('status filter', live?.data.elements.every((a) => a.lifecycle.status === 'live'));
check('status filter total', live?.data.total === all.filter((a) => a.lifecycle.status === 'live').length, { got: live?.data.total });
const multi = await step('list status enacted+expired', () => c.governance.proposals.list({ page: 1, size: 1, status: ['enacted', 'expired'] }));
check('multi-status total', multi?.data.total === all.filter((a) => ['enacted', 'expired'].includes(a.lifecycle.status)).length);
const tw = await step('list type TreasuryWithdrawals', () => c.governance.proposals.list({ page: 1, size: 10, type: ['TreasuryWithdrawals'] }));
check('type filter', tw?.data.elements.every((a) => a.type === 'TreasuryWithdrawals'));
check('type filter total', tw?.data.total === all.filter((a) => a.type === 'TreasuryWithdrawals').length);
const uc = await step('list type UpdateCommittee', () => c.governance.proposals.list({ page: 1, size: 50, type: ['UpdateCommittee'] }));
check('UpdateCommittee filter maps to Koios NewCommittee', uc?.data.total === all.filter((a) => a.type === 'UpdateCommittee').length);
const oldest = await step('list oldest', () => c.governance.proposals.list({ page: 1, size: 5, sort: 'oldest' }));
check('oldest is the reverse of newest', oldest?.data.elements[0]?.id === all[all.length - 1]?.id);
const soon = await step('list soonestToExpire', () => c.governance.proposals.list({ page: 1, size: 20, sort: 'soonestToExpire' }));
check('soonestToExpire ordered', soon?.data.elements.every((a, i, xs) => i === 0 || xs[i - 1].lifecycle.expires.epoch <= a.lifecycle.expires.epoch));
const p2a = await step('list page 1 size 7', () => c.governance.proposals.list({ page: 1, size: 7 }));
const p2b = await step('list page 2 size 7', () => c.governance.proposals.list({ page: 2, size: 7 }));
check('page 2 disjoint from page 1', p2a && p2b && p2b.data.elements.every((a) => !p2a.data.elements.some((b) => b.id === a.id)));
await rejects('unsupported sort', () => c.governance.proposals.list({ page: 1, size: 5, sort: 'mostYesVotes' }), 'CAPABILITY_UNSUPPORTED');
await rejects('page 0', () => c.governance.proposals.list({ page: 0, size: 5 }), 'INVALID_INPUT');
await rejects('size over max', () => c.governance.proposals.list({ page: 1, size: 1001 }), 'INVALID_INPUT');
const byId = await step('search by id', () => c.governance.proposals.list({ page: 1, size: 5, search: all[0].id }));
check('search by id', byId?.data.total === 1 && byId.data.elements[0].id === all[0].id);
const byTx = await step('search by tx hash', () => c.governance.proposals.list({ page: 1, size: 50, search: all[0].txHash }));
check('search by tx hash', byTx?.data.elements.every((a) => a.txHash === all[0].txHash) && byTx.data.total >= 1);
const junk = await step('search junk', () => c.governance.proposals.list({ page: 1, size: 5, search: 'hello world' }));
check('junk search matches nothing', junk?.data.total === 0 && junk.data.elements.length === 0);

const liveAction = live?.data.elements.find((a) => a.type !== 'InfoAction') ?? live?.data.elements[0] ?? all[0];
const one = await step('proposals.get', () => c.governance.proposals.get(liveAction.id));
check('get equals list row', one && JSON.stringify(one.data.voteAggregates) === JSON.stringify(liveAction.voteAggregates) && one.data.id === liveAction.id);
await rejects('get unknown action', () => c.governance.proposals.get(bech32.encode('gov_action', bech32.toWords(Buffer.alloc(33, 7)), 1023)), 'NOT_FOUND');
await rejects('get malformed id', () => c.governance.proposals.get('gov_action1notreal'), 'INVALID_INPUT');

const votes1 = await step('proposals.listVotes page 1', () => c.governance.proposals.listVotes(liveAction.id, { page: 1, size: 20 }));
const votes2 = await step('proposals.listVotes page 2', () => c.governance.proposals.listVotes(liveAction.id, { page: 2, size: 20 }));
if (votes1) {
  checkPage('votes', votes1, 20);
  const ok = votes1.data.elements.every(
    (v) =>
      ['yes', 'no', 'abstain'].includes(v.choice) &&
      /^[0-9a-f]{64}$/.test(v.txRef.txHash) &&
      isStamp(v.at) &&
      (v.voter.role === 'drep' ? isDRepId(v.voter.id) : v.voter.role === 'spo' ? isPoolId(v.voter.id) : isHotId(v.voter.hot) && (v.voter.cold === undefined || isColdId(v.voter.cold))),
  );
  check('vote records well-formed', ok);
  const key = (v) => v.voter.id ?? v.voter.hot;
  if (votes2) check('votes page 2 disjoint', votes2.data.elements.every((v) => !votes1.data.elements.some((w) => key(w) === key(v))));
  const drepVotes = votes1.data.elements.filter((v) => v.voter.role === 'drep');
  const agg = liveAction.voteAggregates.find((x) => x.role === 'drep');
  check('some DRep voted on the sample live action', drepVotes.length > 0 || agg.yes === '0');
  const ccVote = [...votes1.data.elements, ...(votes2?.data.elements ?? [])].find((v) => v.voter.role === 'cc');
  if (ccVote) check('cc vote resolves the cold credential', ccVote.voter.cold !== undefined, ccVote.voter);
  const dv = drepVotes[0];
  if (dv) {
    const mine = await step('get with voterId (DRep)', () => c.governance.proposals.get(liveAction.id, { voterId: dv.voter.id }));
    check('myVote matches the vote listing', mine?.data.myVote?.choice === dv.choice && mine.data.myVote.txRef.txHash === dv.txRef.txHash, mine?.data.myVote);
    const votedList = await step('list voterId voted=true', () => c.governance.proposals.list({ page: 1, size: 200, voterId: dv.voter.id, voted: true }));
    const notVotedList = await step('list voterId voted=false', () => c.governance.proposals.list({ page: 1, size: 200, voterId: dv.voter.id, voted: false }));
    check('voted=true rows all carry myVote', votedList?.data.elements.every((a) => a.myVote && a.myVote.voter.id === dv.voter.id));
    check('voted=false rows carry no myVote', notVotedList?.data.elements.every((a) => a.myVote === null));
    check('voted + not voted = all', votedList && notVotedList && votedList.data.total + notVotedList.data.total === total);
    const annotated = await step('list voterId annotate', () => c.governance.proposals.list({ page: 1, size: 20, voterId: dv.voter.id }));
    check('annotation present on every row', annotated?.data.elements.every((a) => a.myVote === null || a.myVote.voter.id === dv.voter.id));
  }
  const pv = [...votes1.data.elements, ...(votes2?.data.elements ?? [])].find((v) => v.voter.role === 'spo');
  if (pv) {
    const mine = await step('get with voterId (pool)', () => c.governance.proposals.get(liveAction.id, { voterId: pv.voter.id }));
    check('pool myVote', mine?.data.myVote?.choice === pv.choice);
  }
  if (ccVote) {
    const mine = await step('get with voterId (cc hot)', () => c.governance.proposals.get(liveAction.id, { voterId: ccVote.voter.hot }));
    check('cc myVote', mine?.data.myVote?.choice === ccVote.choice);
    if (ccVote.voter.cold) await rejects('cc cold voterId', () => c.governance.proposals.get(liveAction.id, { voterId: ccVote.voter.cold }), 'CAPABILITY_UNSUPPORTED');
  }
}
const activity = await step('proposals.listActivity', () => c.governance.proposals.listActivity(all[all.length - 1].id, { page: 1, size: 10 }));
check('activity starts live', activity?.data.elements[0]?.status === 'live' && activity.data.total === activity.data.elements.length);

const enacted = {};
for (const lineage of ['pparamUpdate', 'hardFork', 'committee', 'constitution']) {
  const e = await step(`getEnacted ${lineage}`, () => c.governance.proposals.getEnacted(lineage));
  enacted[lineage] = e?.data;
  if (e?.data) check(`enacted ${lineage} ref`, isGovActionId(e.data.id, e.data.txHash, e.data.index));
  const expectTypes = { pparamUpdate: ['ParameterChange'], hardFork: ['HardForkInitiation'], committee: ['UpdateCommittee', 'NoConfidence'], constitution: ['NewConstitution'] }[lineage];
  const enactedOfLineage = all.filter((a) => expectTypes.includes(a.type) && a.lifecycle.status === 'enacted');
  if (enactedOfLineage.length === 0) check(`${lineage} genesis case is null`, e?.data === null);
  else {
    check(`${lineage} head is enacted and of the lineage`, enactedOfLineage.some((a) => a.id === e?.data?.id));
    check(`${lineage} head is not anyone's predecessor among enacted`, !enactedOfLineage.some((a) => a.previousAction?.id === e?.data?.id));
  }
}
await rejects('getEnacted unknown lineage', () => c.governance.proposals.getEnacted('treasury'), 'INVALID_INPUT');

console.log('committee');
const committee = await step('getCommittee', () => c.governance.committee.getCommittee());
if (committee) {
  const m = committee.data.members;
  check('committee quorum', isRatio(committee.data.quorum));
  check('committee enactedBy equals committee lineage head', committee.data.enactedBy?.id === enacted.committee?.id);
  check('members', m.length > 0 && m.every((x) => isColdId(x.coldCredential) && (x.hotCredential === null || isHotId(x.hotCredential))));
  check('members have expiry', m.every((x) => Number.isInteger(x.termExpiryEpoch)));
  check('resigned members have no hot key', m.every((x) => !x.hasResigned || x.hotCredential === null));
  const unknownStart = m.filter((x) => x.termStartEpoch === null).length;
  if (unknownStart) note(`${unknownStart}/${m.length} committee members have termStartEpoch null (genesis membership not in Koios)`);
  check('term start before expiry', m.every((x) => x.termStartEpoch === null || x.termStartEpoch <= x.termExpiryEpoch));
  const member = await step('getMember', () => c.governance.committee.getMember(m[0].coldCredential));
  check('getMember', member?.data.coldCredential === m[0].coldCredential);
  await rejects('getMember unknown', () => c.governance.committee.getMember(bech32.encode('cc_cold', bech32.toWords(Buffer.from([0x12, ...Buffer.alloc(28, 9)])), 1023)), 'NOT_FOUND');
  await rejects('getMember hot id', () => c.governance.committee.getMember(m.find((x) => x.hotCredential)?.hotCredential ?? 'x'), 'INVALID_INPUT');
}
const constitution = await step('getConstitution', () => c.governance.committee.getConstitution());
check('constitution', constitution && /^[0-9a-f]{64}$/.test(constitution.data.anchor.dataHash) && constitution.data.enactedBy?.id === enacted.constitution?.id, constitution?.data);

console.log('dreps');
const random = await step('dreps.list random', () => c.governance.dreps.list({ page: 1, size: 10 }));
if (random) checkPage('dreps random', random, 10);
await rejects('random page 2', () => c.governance.dreps.list({ page: 2, size: 10 }), 'INVALID_INPUT');
function checkDRep(label, d, epoch) {
  check(`${label} CIP-129 id`, isDRepId(d.id), d.id);
  check(`${label} kind from anchor`, (d.anchor === null) === (d.kind === 'anonymous'));
  check(`${label} registration dated`, isStamp(d.registration.latest.at) && d.registration.latest.at.time !== undefined);
  check(`${label} deposit`, d.registration.latest.deposit === null || isLovelace(d.registration.latest.deposit));
  check(`${label} status`, ['active', 'inactive', 'retired'].includes(d.status));
  check(`${label} retiredAt iff retired`, (d.status === 'retired') === (d.registration.retiredAt !== null && d.registration.retiredAt !== undefined));
  if (d.expiryEpoch !== undefined) check(`${label} status read off expiry`, d.status === (epoch > d.expiryEpoch ? 'inactive' : 'active'), d);
  check(`${label} voting power`, d.votingPower === null || (isLovelace(d.votingPower.amount) && d.votingPower.basis === 'active'));
  check(`${label} no CIP-105 leak`, !('cip105Id' in d));
}
for (const d of random?.data.elements ?? []) checkDRep('random drep', d, currentEpoch);
const reg1 = await step('dreps.list registrationDate p1', () => c.governance.dreps.list({ page: 1, size: 25, sort: 'registrationDate' }));
const reg2 = await step('dreps.list registrationDate p2', () => c.governance.dreps.list({ page: 2, size: 25, sort: 'registrationDate' }));
if (reg1 && reg2) {
  checkPage('dreps by date', reg1, 25);
  check('dreps page 2 disjoint', reg2.data.elements.every((d) => !reg1.data.elements.some((e) => e.id === d.id)));
  const rows = [...reg1.data.elements, ...reg2.data.elements];
  check('newest registration first', rows.every((d, i) => i === 0 || Date.parse(rows[i - 1].registration.latest.at.time) >= Date.parse(d.registration.latest.at.time)));
  for (const d of rows) checkDRep('dated drep', d, currentEpoch);
}
const byPower = await step('dreps.list votingPower active', () => c.governance.dreps.list({ page: 1, size: 20, sort: 'votingPower', status: ['active'] }));
if (byPower) {
  check('active filter', byPower.data.elements.every((d) => d.status === 'active'));
  const amounts = byPower.data.elements.map((d) => BigInt(d.votingPower?.amount ?? -1));
  check('voting power descending', amounts.every((a, i) => i === 0 || amounts[i - 1] >= a));
}
const counts = await step('dreps.getCounts', () => c.governance.dreps.getCounts());
if (counts) {
  const k = counts.data;
  check('counts add up', k.totalActive + k.totalInactive === k.totalRegistered && k.anonymous <= k.totalRegistered, k);
  check('active filter total = count', byPower?.data.total === k.totalActive, { list: byPower?.data.total, counts: k.totalActive });
}
const inactive = await step('dreps.list inactive', () => c.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate', status: ['inactive'] }));
check('inactive filter', inactive?.data.elements.every((d) => d.status === 'inactive') && inactive.data.total === counts?.data.totalInactive);
const retired = await step('dreps.list retired', () => c.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate', status: ['retired'] }));
check('retired filter', retired?.data.elements.every((d) => d.status === 'retired'));
const anon = await step('dreps.list anonymous', () => c.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', kind: ['anonymous'] }));
check('anonymous filter', anon?.data.elements.every((d) => d.anchor === null && d.kind === 'anonymous'));
const named = await step('dreps.list drep kind', () => c.governance.dreps.list({ page: 1, size: 10, sort: 'registrationDate', kind: ['drep'] }));
check('drep kind filter', named?.data.elements.every((d) => d.anchor !== null));
check('kinds partition the directory', anon && named && reg1 && anon.data.total + named.data.total === reg1.data.total);
await rejects('unsupported DRep sort', () => c.governance.dreps.list({ page: 1, size: 5, sort: 'activity' }), 'CAPABILITY_UNSUPPORTED');

const sampleDRep = byPower?.data.elements[0];
if (sampleDRep) {
  const found = await step('search exact id', () => c.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate', search: sampleDRep.id }));
  check('search exact id', found?.data.total === 1 && found.data.elements[0].id === sampleDRep.id);
  const { bytes } = decode(sampleDRep.id);
  const cip105 = bech32.encode('drep', bech32.toWords(bytes.subarray(1)), 1023);
  const miss = await step('search CIP-105', () => c.governance.dreps.list({ page: 1, size: 5, sort: 'registrationDate', search: cip105 }));
  check('CIP-105 search matches nothing', miss?.data.total === 0);
  await rejects('get with CIP-105 id', () => c.governance.dreps.get(cip105), 'INVALID_INPUT');
  const got = await step('dreps.get', () => c.governance.dreps.get(sampleDRep.id));
  if (got) {
    checkDRep('got drep', got.data, currentEpoch);
    check('get matches list', got.data.votingPower?.amount === sampleDRep.votingPower?.amount && got.data.status === sampleDRep.status);
    check('activity voted <= votable', got.data.activity && got.data.activity.voted <= got.data.activity.votable, got.data.activity);
    const v = await step('dreps.listVotes', () => c.governance.dreps.listVotes(sampleDRep.id, { page: 1, size: 1000 }));
    if (v) {
      check('votable = listing length', v.data.total === got.data.activity.votable, { total: v.data.total, act: got.data.activity });
      check('voted rows', v.data.elements.filter((r) => r.voted).length === got.data.activity.voted);
      check('vote rows well-formed', v.data.elements.every((r) => isGovActionId(r.action.id, decode(r.action.id).bytes.subarray(0, 32).toString('hex'), decode(r.action.id).bytes[32]) && (r.voted ? ['yes', 'no', 'abstain'].includes(r.choice) && !!r.txRef : !('choice' in r) && !('anchor' in r))));
    }
    const vv = await step('dreps.listVotes voted=true oldest', () => c.governance.dreps.listVotes(sampleDRep.id, { page: 1, size: 5, voted: true, sort: 'oldest' }));
    check('voted filter', vv?.data.elements.every((r) => r.voted) && vv.data.total === got.data.activity.voted);
    const h = await step('dreps.listUpdateHistory', () => c.governance.dreps.listUpdateHistory(sampleDRep.id, { page: 1, size: 10, sort: 'asc' }));
    check('history', h && h.data.total >= 1 && h.data.elements.every((e) => isStamp(e.at) && /^[0-9a-f]{64}$/.test(e.txRef.txHash)));
    check('history asc', h?.data.elements.every((e, i, xs) => i === 0 || Date.parse(xs[i - 1].at.time) <= Date.parse(e.at.time)));
  }
}
await rejects('unknown DRep', () => c.governance.dreps.get(bech32.encode('drep', bech32.toWords(Buffer.from([0x22, ...Buffer.alloc(28, 3)])), 1023)), 'NOT_FOUND');

console.log('pools');
const pools1 = await step('pools.list p1', () => c.governance.pools.list({ page: 1, size: 20 }));
const pools2 = await step('pools.list p2', () => c.governance.pools.list({ page: 2, size: 20 }));
if (pools1 && pools2) {
  checkPage('pools', pools1, 20);
  check('pools page 2 disjoint', pools2.data.elements.every((p) => !pools1.data.elements.some((q) => q.id === p.id)));
  const rows = [...pools1.data.elements, ...pools2.data.elements];
  check('pool ids', rows.every((p) => isPoolId(p.id) && p.poolId === p.id));
  const amounts = rows.map((p) => BigInt(p.votingPower?.amount ?? -1));
  check('pools by voting power', amounts.every((a, i) => i === 0 || amounts[i - 1] >= a));
  check('pool lovelace', rows.every((p) => ['activeStake', 'pledge'].every((k) => p[k] === undefined || isLovelace(p[k]))));
  if (stake?.data.totalStakeControlledBySPOs) check('top pool power below SPO total', amounts[0] < BigInt(stake.data.totalStakeControlledBySPOs));
}
const pool = pools1?.data.elements[0];
if (pool) {
  const got = await step('pools.get', () => c.governance.pools.get(pool.id));
  check('pools.get matches list', got?.data.votingPower?.amount === pool.votingPower?.amount, { get: got?.data.votingPower, list: pool.votingPower });
  const found = await step('pools.list search', () => c.governance.pools.list({ page: 1, size: 5, search: pool.id }));
  check('pool search', found?.data.total === 1 && found.data.elements[0].id === pool.id);
  const pv = await step('pools.listVotes', () => c.governance.pools.listVotes(pool.id, { page: 1, size: 10 }));
  check('pool votes', pv && pv.data.elements.every((v) => v.voter.id === pool.id && isGovActionId(v.action.id, decode(v.action.id).bytes.subarray(0, 32).toString('hex'), decode(v.action.id).bytes[32])));
}
await rejects('unknown pool', () => c.governance.pools.get(bech32.encode('pool', bech32.toWords(Buffer.alloc(28, 5)), 1023)), 'NOT_FOUND');

console.log('accounts');
const holder = all.find((a) => a.depositReturnAddress)?.depositReturnAddress;
if (holder) {
  const acct = await step('accounts.get', () => c.accounts.get(holder));
  check('account', acct?.data.stakeAddress === holder && /^[0-9a-f]{56}$/.test(acct.data.stakeKeyHash) && typeof acct.data.isRegistered === 'boolean');
  const del = await step('accounts.getDelegation', () => c.accounts.getDelegation(holder));
  if (del?.data) {
    const t = del.data.target;
    check('delegation target', t.kind === 'predefined' ? ['alwaysAbstain', 'alwaysNoConfidence'].includes(t.target) : isDRepId(t.drep.id), t);
    check('delegation txRef', /^[0-9a-f]{64}$/.test(del.data.txRef?.txHash) && isStamp(del.data.since));
  }
  const pdel = await step('accounts.getPoolDelegation', () => c.accounts.getPoolDelegation(holder));
  if (pdel?.data) check('pool delegation', isPoolId(pdel.data.poolId) && /^[0-9a-f]{64}$/.test(pdel.data.txRef.txHash));
  const vp = await step('accounts.getVotingPower', () => c.accounts.getVotingPower(holder));
  check('account voting power', vp && (vp.data === null || (isLovelace(vp.data.amount) && vp.data.basis === 'live')), vp?.data);
}
const fresh = bech32.encode(stakePrefix, bech32.toWords(Buffer.from([network === 'mainnet' ? 0xe1 : 0xe0, ...Buffer.alloc(28, 0xab)])), 1023);
const nobody = await step('accounts.get unknown', () => c.accounts.get(fresh));
check('unknown account is unregistered, not missing', nobody?.data.isRegistered === false);
const nobodyDel = await step('accounts.getDelegation unknown', () => c.accounts.getDelegation(fresh));
check('unknown account delegates to nobody', nobodyDel?.data === null);
const nobodyVp = await step('accounts.getVotingPower unknown', () => c.accounts.getVotingPower(fresh));
check('unknown account has no voting power (null, not 0)', nobodyVp?.data === null);
await rejects('wrong-network address', () => c.accounts.get(bech32.encode(network === 'mainnet' ? 'stake_test' : 'stake', bech32.toWords(Buffer.from([0xe0, ...Buffer.alloc(28, 1)])), 1023)), 'INVALID_INPUT');

console.log('transactions');
const txHash = votes1?.data.elements[0]?.txRef.txHash ?? all[0].txHash;
const tx = await step('transactions.get', () => c.transactions.get(txHash));
check('on chain', tx?.data.onChain === true && isStamp(tx.data.includedAt), tx?.data);
const missing = await step('transactions.get unknown', () => c.transactions.get('ab'.repeat(32)));
check('unknown tx is not on chain', missing?.data.onChain === false && missing.data.includedAt === undefined);
await rejects('bad tx hash', () => c.transactions.get('xyz'), 'INVALID_INPUT');


/* -- independent cross-checks against raw Koios ------------------------------ */

console.log('cross-checks (raw Koios)');
const base = env.KOIOS_BASE_URL ?? { mainnet: 'https://api.koios.rest/api/v1', preprod: 'https://preprod.koios.rest/api/v1', preview: 'https://preview.koios.rest/api/v1' }[network];
async function raw(path, body) {
  requests++;
  const res = await fetch(`${base}/${path}`, {
    method: body ? 'POST' : 'GET',
    headers: { accept: 'application/json', ...(body ? { 'content-type': 'application/json' } : {}), ...(env.KOIOS_TOKEN ? { authorization: `Bearer ${env.KOIOS_TOKEN}` } : {}) },
    ...(body ? { body: JSON.stringify(body) } : {}),
  });
  await pause();
  if (!res.ok) throw new Error(`raw ${path}: ${res.status}`);
  return res.json();
}
try {
  const liveRows = all.filter((a) => a.lifecycle.status === 'live');
  const drepTotals = new Set(liveRows.map((a) => a.voteAggregates.find((x) => x.role === 'drep')?.totalEligible));
  check('live actions share one DRep denominator (same epoch)', drepTotals.size <= 1, [...drepTotals]);
  const spoInfo = liveRows.find((a) => a.type === 'InfoAction')?.voteAggregates.find((x) => x.role === 'spo');
  if (spoInfo && stake?.data.totalStakeControlledBySPOs) {
    check('live InfoAction SPO denominator = SPO distribution', spoInfo.totalEligible === stake.data.totalStakeControlledBySPOs, { agg: spoInfo.totalEligible, dist: stake.data.totalStakeControlledBySPOs });
  }
  const drepLive = liveRows[0]?.voteAggregates.find((x) => x.role === 'drep');
  if (drepLive && stake?.data.totalStakeControlledByDReps) {
    const ours = BigInt(stake.data.totalStakeControlledByDReps) + BigInt(stake.data.alwaysNoConfidenceVotingPower);
    const diff = BigInt(drepLive.totalEligible) - ours;
    note(`DRep denominator (Koios inactive rule) minus active-DRep stake + no-confidence (expiry rule): ${diff} lovelace (${Number((diff * 10000n) / ours) / 100}%)`);
  }
  const ccLive = liveRows.find((a) => a.voteAggregates.some((x) => x.role === 'cc'))?.voteAggregates.find((x) => x.role === 'cc');
  if (ccLive && committee) {
    const eligible = committee.data.members.filter((m) => m.hotCredential && !m.hasResigned && m.termExpiryEpoch >= currentEpoch).length;
    check('cc denominator = authorised unexpired members', Number(ccLive.totalEligible) === eligible, { agg: ccLive.totalEligible, eligible });
  }
  const twLive = liveRows.find((a) => a.type === 'TreasuryWithdrawals');
  if (twLive && params) {
    const t = twLive.voteAggregates.find((x) => x.role === 'drep').threshold;
    const e = params.data.drepThresholds.treasuryWithdrawal;
    check('TW DRep threshold is the treasuryWithdrawal parameter', t.numerator * e.denominator === e.numerator * t.denominator, { t, e });
  }
  if (liveAction && votes1) {
    const pv = await raw(`proposal_votes?_proposal_id=${encodeURIComponent(liveAction.id)}`);
    note(`votes on ${liveAction.id.slice(0, 18)}: listVotes total ${votes1.data.total}, Koios proposal_votes ${pv.length} (it drops votes of DReps retired since)`);
    check('listVotes covers proposal_votes', votes1.data.total >= pv.length);
  }
  if (sampleDRep) {
    const h = await raw(`drep_voting_power_history?_drep_id=${encodeURIComponent(sampleDRep.id)}&_epoch_no=${currentEpoch}`);
    check('DRep power = drep_voting_power_history for the epoch', h[0]?.amount === sampleDRep.votingPower?.amount, { history: h[0]?.amount, ours: sampleDRep.votingPower });
    const v = await raw(`vote_list?voter_id=eq.${encodeURIComponent(sampleDRep.id)}&select=proposal_id`);
    const got = await c.governance.dreps.get(sampleDRep.id);
    check('DRep activity.voted = distinct actions in vote_list', got.data.activity.voted === new Set(v.map((r) => r.proposal_id)).size, { ours: got.data.activity.voted, raw: new Set(v.map((r) => r.proposal_id)).size });
    const d = await raw(`drep_delegators?_drep_id=${encodeURIComponent(sampleDRep.id)}&limit=3`);
    for (const row of d.filter((r) => r.stake_address.startsWith(stakePrefix + '1'))) {
      const del = await c.accounts.getDelegation(row.stake_address);
      check('delegator delegates to the DRep', del.data?.target.kind === 'drep' && del.data.target.drep.id === sampleDRep.id, del.data);
      const vp = await c.accounts.getVotingPower(row.stake_address);
      note(`account voting power ${vp.data?.amount} vs Koios drep_delegators amount ${row.amount}`);
      await pause();
    }
  }
  if (pool) {
    const h = await raw(`pool_voting_power_history?_pool_bech32=${encodeURIComponent(pool.id)}&_epoch_no=${currentEpoch}`);
    check('pool power = pool_voting_power_history for the epoch', h[0]?.amount === pool.votingPower?.amount, { history: h[0]?.amount, ours: pool.votingPower });
  }
  // DRep status: the expiry rule versus Koios' own `active` flag.
  const page = (reg1?.data.elements ?? []).filter((d) => d.status !== 'retired').slice(0, 25);
  if (page.length) {
    const infos = await raw('drep_info', { _drep_ids: page.map((d) => d.id) });
    const flag = new Map(infos.map((r) => [r.drep_id, r.active]));
    const disagree = page.filter((d) => (d.status === 'active') !== flag.get(d.id));
    if (disagree.length) note(`${disagree.length}/${page.length} DReps: expiry-rule status differs from Koios' active flag: ${disagree.map((d) => `${d.id.slice(0, 16)} ${d.status} exp ${d.expiryEpoch}`).join('; ')}`);
    else check('status agrees with Koios active flag on a sample', true);
  }
} catch (error) {
  check('cross-checks ran', false, error.message);
}

console.log(`\n${passes} passed, ${failures} failed, ${requests} requests`);
for (const n of notes) console.log(`note: ${n}`);
process.exit(failures === 0 ? 0 : 1);
