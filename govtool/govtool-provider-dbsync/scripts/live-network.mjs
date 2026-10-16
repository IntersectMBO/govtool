#!/usr/bin/env node
/**
 * Live check of the network, accounts and transactions areas against a real
 * db-sync, with a cross-check against Koios for the same network.
 *
 *   DBSYNC_POSTGRES_HOST=... DBSYNC_POSTGRES_PORT=... DBSYNC_POSTGRES_USER=...
 *   DBSYNC_POSTGRES_PASSWORD=... DBSYNC_DATABASE=... [NETWORK=preview] \
 *   [KOIOS_URL=https://preview.koios.rest/api/v1] node scripts/live-network.mjs
 *
 * Read-only: sample ids are found with SELECTs, and nothing is written. Build
 * first (npm run build); it drives ./dist. Exits non-zero on any failed check.
 * Set KOIOS_URL= (empty) to skip the Koios cross-check.
 */
import pg from 'pg';

import { createDbSyncProvider } from '../dist/index.js';

const env = process.env;
for (const key of ['DBSYNC_POSTGRES_HOST', 'DBSYNC_POSTGRES_USER', 'DBSYNC_DATABASE']) {
  if (!env[key]) {
    console.error(`missing ${key}`);
    process.exit(2);
  }
}
const network = env.NETWORK ?? 'preview';
const koiosUrl = env.KOIOS_URL ?? (network === 'mainnet' ? 'https://api.koios.rest/api/v1' : `https://${network}.koios.rest/api/v1`);
const connection = {
  host: env.DBSYNC_POSTGRES_HOST,
  port: Number(env.DBSYNC_POSTGRES_PORT ?? 5432),
  user: env.DBSYNC_POSTGRES_USER,
  password: env.DBSYNC_POSTGRES_PASSWORD,
  database: env.DBSYNC_DATABASE,
};

const { chainData, close } = createDbSyncProvider({ network, connection });
const sql = new pg.Pool({ ...connection, max: 2 });

/* -- tiny harness ------------------------------------------------------------ */

let failures = 0;
let passes = 0;
const mismatches = [];
const check = (label, ok, detail) => {
  if (ok) passes++;
  else {
    failures++;
    console.log(`  FAIL ${label}${detail === undefined ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  }
};
const SLOW_MS = 5000;
async function timed(label, fn) {
  const start = Date.now();
  try {
    return await fn();
  } finally {
    const ms = Date.now() - start;
    console.log(`  ${label}: ${ms} ms${ms > SLOW_MS ? '  (SLOW)' : ''}`);
    check(`${label} under 30 s`, ms < 30_000, `${ms} ms`);
  }
}
async function expectCode(label, fn, code) {
  let returned;
  try {
    const p = fn();
    check(`${label} returns a promise (no synchronous throw)`, p instanceof Promise);
    returned = await p;
  } catch (error) {
    check(`${label} -> ${code}`, error?.code === code, `${error?.code}: ${error?.message}`);
    return;
  }
  check(`${label} -> ${code}`, false, `resolved with ${JSON.stringify(returned?.data)}`);
}
const isLovelace = (v) => typeof v === 'string' && /^\d+$/.test(v);
const isRatio = (r) =>
  r && Number.isInteger(r.numerator) && Number.isInteger(r.denominator) && r.denominator !== 0 && r.numerator >= 0;
const isInt = (v) => Number.isInteger(v) && v >= 0;
const isIso = (v) => typeof v === 'string' && !Number.isNaN(Date.parse(v)) && v.endsWith('Z');
const checkStamp = (label, s) => {
  check(`${label}.epoch`, isInt(s?.epoch), s);
  if (s?.slot !== undefined) check(`${label}.slot`, isInt(s.slot), s);
  if (s?.block !== undefined) check(`${label}.block`, isInt(s.block), s);
  if (s?.time !== undefined) check(`${label}.time`, isIso(s.time), s);
};
const checkMeta = (label, env) => {
  check(`${label} meta.provider`, env?.meta?.provider === 'dbsync', env?.meta);
  check(`${label} meta.network`, env?.meta?.network === network, env?.meta);
};
const ratioEq = (r, x) => Math.abs(r.numerator / r.denominator - Number(x)) < 1e-9;
const compare = (what, ours, theirs) => {
  const same = String(ours) === String(theirs);
  check(`koios ${what}`, same, `dbsync=${ours} koios=${theirs}`);
  if (same) console.log(`  ok ${what}: ${ours}`);
  if (!same) mismatches.push({ what, dbsync: ours, koios: theirs });
};

async function koios(path, body) {
  if (!koiosUrl) return undefined;
  const response = await fetch(`${koiosUrl}${path}`, {
    method: body ? 'POST' : 'GET',
    headers: { accept: 'application/json', ...(body ? { 'content-type': 'application/json' } : {}) },
    body: body ? JSON.stringify(body) : undefined,
    signal: AbortSignal.timeout(30_000),
  });
  if (!response.ok) throw new Error(`koios ${path}: HTTP ${response.status}`);
  return response.json();
}

/** Last 28 bytes of a bech32 id's payload, so CIP-105 and CIP-129 DRep ids compare by credential. */
function credentialOf(id) {
  const CHARSET = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';
  const data = id.slice(id.lastIndexOf('1') + 1, -6);
  let acc = 0;
  let bits = 0;
  const out = [];
  for (const c of data) {
    acc = (acc << 5) | CHARSET.indexOf(c);
    bits += 5;
    while (bits >= 8) {
      bits -= 8;
      out.push((acc >> bits) & 0xff);
    }
  }
  return Buffer.from(out.slice(-28)).toString('hex');
}

/* -- network ----------------------------------------------------------------- */

console.log(`db-sync ${connection.host}/${connection.database} as ${network}; koios ${koiosUrl || '(skipped)'}`);
console.log('\nnetwork');

const info = await timed('getNetworkInfo', () => chainData.network.getNetworkInfo());
checkMeta('getNetworkInfo', info);
check('network', info.data.network === network, info.data.network);
check('era', typeof info.data.era === 'string' && info.data.era.length > 0, info.data.era);
checkStamp('tip', info.data.tip);
check('currentEpoch = tip.epoch', info.data.currentEpoch === info.data.tip.epoch);
const current = info.data.currentEpoch;
console.log(`  tip epoch ${current} block ${info.data.tip.block} era ${info.data.era}`);

const pp = await timed('getProtocolParams()', () => chainData.network.getProtocolParams());
checkMeta('getProtocolParams', pp);
const p = pp.data;
check('params.epoch is current', p.epoch === current, p.epoch);
for (const k of ['govActionLifetime', 'drepActivity', 'committeeMinSize', 'committeeMaxTermLength', 'minFeeA', 'minFeeB', 'maxTxSize', 'maxValSize']) {
  check(`params.${k} integer`, isInt(p[k]), p[k]);
}
for (const k of ['govActionDeposit', 'drepDeposit', 'keyDeposit', 'poolDeposit', 'coinsPerUtxoByte']) {
  check(`params.${k} lovelace`, isLovelace(p[k]), p[k]);
}
for (const [group, ratios] of [['drepThresholds', p.drepThresholds], ['poolThresholds', p.poolThresholds]]) {
  for (const [k, r] of Object.entries(ratios)) check(`params.${group}.${k} ratio`, isRatio(r), r);
}
check('drepThresholds has 10', Object.keys(p.drepThresholds).length === 10);
check('poolThresholds has 5', Object.keys(p.poolThresholds).length === 5);
check('no source columns leak', !('cost_model_id' in p) && !('nonce' in p) && !('id' in p));

const past = await timed('getProtocolParams({ epoch: current - 5 })', () =>
  chainData.network.getProtocolParams({ epoch: current - 5 }),
);
check('past params epoch', past.data.epoch === current - 5, past.data.epoch);
await expectCode('getProtocolParams({ epoch: 0 }) pre-Conway', () => chainData.network.getProtocolParams({ epoch: 0 }), 'NOT_FOUND');
await expectCode('getProtocolParams({ epoch: current + 100 })', () => chainData.network.getProtocolParams({ epoch: current + 100 }), 'NOT_FOUND');
await expectCode('getProtocolParams({ epoch: -1 })', () => chainData.network.getProtocolParams({ epoch: -1 }), 'INVALID_INPUT');
await expectCode('getProtocolParams({ epoch: 1.5 })', () => chainData.network.getProtocolParams({ epoch: 1.5 }), 'INVALID_INPUT');
await expectCode('getProtocolParams({ epoch: "12" })', () => chainData.network.getProtocolParams({ epoch: '12' }), 'INVALID_INPUT');

const caps = await chainData.system.getCapabilities();
check('capabilities declare protocolParams.epoch', caps.data.optionalArguments.includes('protocolParams.epoch'));

const sd = await timed('getStakeDistribution', () => chainData.network.getStakeDistribution());
checkMeta('getStakeDistribution', sd);
const s = sd.data;
check('stake.epoch current', s.epoch === current, s.epoch);
check('totalActiveStake lovelace', isLovelace(s.totalActiveStake), s.totalActiveStake);
check('totalActiveStake > 0', BigInt(s.totalActiveStake) > 0n);
check('totalLiveStake omitted, not faked', !('totalLiveStake' in s));
for (const k of ['totalStakeControlledByDReps', 'totalStakeControlledBySPOs', 'alwaysAbstainVotingPower', 'alwaysNoConfidenceVotingPower']) {
  check(`${k} lovelace when present`, s[k] === undefined || isLovelace(s[k]), s[k]);
}
console.log(`  active ${s.totalActiveStake} dreps ${s.totalStakeControlledByDReps} spos ${s.totalStakeControlledBySPOs} abstain ${s.alwaysAbstainVotingPower} noConf ${s.alwaysNoConfidenceVotingPower}`);

check('getTreasury present', typeof chainData.network.getTreasury === 'function');
const tr = await timed('getTreasury()', () => chainData.network.getTreasury());
checkMeta('getTreasury', tr);
check('treasury epoch current', tr.data.epoch === current, tr.data.epoch);
check('treasury balance lovelace', isLovelace(tr.data.balance) && BigInt(tr.data.balance) > 0n, tr.data.balance);
check('treasury reserves lovelace', isLovelace(tr.data.reserves), tr.data.reserves);
const trPast = await timed('getTreasury({ epoch: current - 1 })', () => chainData.network.getTreasury({ epoch: current - 1 }));
check('past treasury epoch', trPast.data.epoch === current - 1);
await expectCode('getTreasury({ epoch: current + 100 })', () => chainData.network.getTreasury({ epoch: current + 100 }), 'NOT_FOUND');

/* -- sample accounts, found read-only --------------------------------------- */

const q1 = async (text, params = []) => (await sql.query(text, params)).rows;
const [{ view: abstainer } = {}] = await q1(`SELECT sa.view FROM delegation_vote dv
  JOIN stake_address sa ON sa.id = dv.addr_id JOIN drep_hash h ON h.id = dv.drep_hash_id
  WHERE h.view = 'drep_always_abstain' ORDER BY dv.id DESC LIMIT 1`);
const [{ view: noConfidence } = {}] = await q1(`SELECT sa.view FROM delegation_vote dv
  JOIN stake_address sa ON sa.id = dv.addr_id JOIN drep_hash h ON h.id = dv.drep_hash_id
  WHERE h.view = 'drep_always_no_confidence' ORDER BY dv.id DESC LIMIT 1`);
const drepDelegators = (
  await q1(`SELECT DISTINCT ON (dv.addr_id) sa.view, dv.id FROM delegation_vote dv
  JOIN stake_address sa ON sa.id = dv.addr_id JOIN drep_hash h ON h.id = dv.drep_hash_id
  WHERE h.raw IS NOT NULL AND EXISTS (SELECT 1 FROM reward r WHERE r.addr_id = sa.id)
  ORDER BY dv.addr_id DESC, dv.id DESC LIMIT 40`)
).map((r) => r.view);
const [{ view: busiest } = {}] = await q1(`SELECT sa.view FROM delegation_vote dv
  JOIN stake_address sa ON sa.id = dv.addr_id GROUP BY sa.view ORDER BY count(*) DESC LIMIT 1`);
const retiredPoolDelegators = (
  await q1(`SELECT sa.view FROM delegation d JOIN stake_address sa ON sa.id = d.addr_id
  WHERE d.pool_hash_id IN (SELECT hash_id FROM pool_retire WHERE retiring_epoch < $1)
  ORDER BY d.id DESC LIMIT 5`, [current])
).map((r) => r.view);
const retiredDRepDelegators = (
  await q1(`SELECT sa.view FROM (
    SELECT DISTINCT ON (addr_id) addr_id, tx_id, drep_hash_id FROM delegation_vote ORDER BY addr_id, tx_id DESC, cert_index DESC
  ) l JOIN stake_address sa ON sa.id = l.addr_id
  WHERE EXISTS (SELECT 1 FROM drep_registration r WHERE r.drep_hash_id = l.drep_hash_id AND r.deposit < 0 AND r.tx_id > l.tx_id)
  ORDER BY l.tx_id DESC LIMIT 3`)
).map((r) => r.view);
const [{ hash: txHash } = {}] = await q1(`SELECT encode(hash, 'hex') AS hash FROM tx ORDER BY id DESC LIMIT 1 OFFSET 10`);

// A well-formed address db-sync has never seen: a fresh random key hash.
const { encodeStakeAddress } = await import('../dist/ids.js');
const { randomBytes } = await import('node:crypto');
const unseen = encodeStakeAddress(randomBytes(28).toString('hex'), false, network);

/* -- accounts ---------------------------------------------------------------- */

console.log('\naccounts');
const A = chainData.accounts;
for (const m of ['getPoolDelegation', 'getVotingPower', 'listDelegationHistory']) {
  check(`accounts.${m} present`, typeof A[m] === 'function');
}

const account = await timed('get(abstainer)', () => A.get(abstainer));
checkMeta('accounts.get', account);
check('account.stakeAddress canonical', account.data.stakeAddress === abstainer, account.data);
check('account.stakeKeyHash 56 hex', /^[0-9a-f]{56}$/.test(account.data.stakeKeyHash), account.data);
check('account.isRegistered boolean', typeof account.data.isRegistered === 'boolean');
check('account.balance omitted', !('balance' in account.data));
const upper = await A.get(abstainer.toUpperCase());
check('get accepts upper-case bech32 and canonicalises', upper.data.stakeAddress === abstainer);

const abstainDel = await timed('getDelegation(abstainer)', () => A.getDelegation(abstainer));
check('abstainer -> predefined alwaysAbstain', abstainDel.data?.target?.kind === 'predefined' && abstainDel.data.target.target === 'alwaysAbstain', abstainDel.data);
if (abstainDel.data) {
  check('delegation.txRef.txHash hex', /^[0-9a-f]{64}$/.test(abstainDel.data.txRef?.txHash ?? ''), abstainDel.data.txRef);
  checkStamp('delegation.since', abstainDel.data.since);
}
if (noConfidence) {
  const d = await timed('getDelegation(noConfidence)', () => A.getDelegation(noConfidence));
  check('noConfidence -> predefined alwaysNoConfidence or cleared', d.data === null || d.data.target.target === 'alwaysNoConfidence', d.data);
}

let drepSample;
for (const addr of drepDelegators) {
  const d = await A.getDelegation(addr);
  if (d.data?.target.kind === 'drep') {
    drepSample = { addr, delegation: d.data };
    break;
  }
}
check('found a live DRep delegation', drepSample !== undefined);
if (drepSample) {
  const { drep } = drepSample.delegation.target;
  check('drep VoterRef role', drep.role === 'drep');
  check('drep id is CIP-129 (drep1, 29-byte payload)', drep.id.startsWith('drep1') && drep.id.length === 58, drep.id);
  check('drep isScriptBased boolean', typeof drep.isScriptBased === 'boolean');
  console.log(`  ${drepSample.addr} -> ${drep.id}`);
}

for (const addr of retiredDRepDelegators) {
  const d = await timed(`getDelegation(retired-DRep delegator)`, () => A.getDelegation(addr));
  check('delegation to a since-retired DRep is null', d.data === null, d.data);
}

const unseenAccount = await timed('get(unseen)', () => A.get(unseen));
check('unseen address: isRegistered false', unseenAccount.data.isRegistered === false, unseenAccount.data);
check('unseen address: stakeKeyHash from the address', unseenAccount.data.stakeAddress === unseen);
const unseenDel = await A.getDelegation(unseen);
check('unseen address: delegation null (known undelegated)', unseenDel.data === null);
const unseenPool = await A.getPoolDelegation(unseen);
check('unseen address: pool delegation null', unseenPool.data === null);
const unseenVp = await A.getVotingPower(unseen);
check('unseen address: voting power null, never 0', unseenVp.data === null, unseenVp.data);
const unseenHist = await A.listDelegationHistory(unseen, { page: 1, size: 10 });
check('unseen address: empty history, total 0', unseenHist.data.elements.length === 0 && unseenHist.data.total === 0, unseenHist.data);

const vpTarget = drepSample?.addr ?? abstainer;
const vp = await timed('getVotingPower(sample)', () => A.getVotingPower(vpTarget));
checkMeta('getVotingPower', vp);
if (vp.data) {
  check('voting power lovelace', isLovelace(vp.data.amount), vp.data);
  check('voting power basis live', vp.data.basis === 'live');
  check('voting power epoch current', vp.data.epoch === current);
}

const poolDel = await timed('getPoolDelegation(abstainer)', () => A.getPoolDelegation(abstainer));
if (poolDel.data) {
  check('poolId is pool1', poolDel.data.poolId?.startsWith('pool1'), poolDel.data);
  checkStamp('poolDelegation.since', poolDel.data.since);
}
for (const addr of retiredPoolDelegators.slice(0, 3)) {
  const d = await timed('getPoolDelegation(retired-pool delegator)', () => A.getPoolDelegation(addr));
  check('pool delegation shape', d.data === null || d.data.poolId.startsWith('pool1'), d.data);
}

// Paging over the busiest account's history: totals consistent, pages disjoint and ordered.
const size = 7;
const first = await timed('listDelegationHistory(busiest, p1)', () => A.listDelegationHistory(busiest, { page: 1, size }));
const total = first.data.total;
check('history total integer', isInt(total), total);
check('history page 1 full', first.data.elements.length === Math.min(size, total));
const seen = new Set();
let collected = 0;
const pages = Math.ceil(total / size);
for (let page = 1; page <= Math.min(pages, 40); page++) {
  const r = page === 1 ? first : await A.listDelegationHistory(busiest, { page, size });
  check(`history page ${page} total stable`, r.data.total === total, r.data.total);
  for (const e of r.data.elements) {
    const key = `${e.txRef.txHash}:${e.txRef.index}:${e.kind}`;
    check('history rows unique across pages', !seen.has(key), key);
    seen.add(key);
    checkStamp('history.at', e.at);
    check('history kind shape', e.kind === 'pool' ? e.poolId?.startsWith('pool1') && !e.target : !!e.target && !e.poolId, e);
  }
  collected += r.data.elements.length;
  if (page < pages) check(`history page ${page} is full`, r.data.elements.length === size);
}
if (pages <= 40) check('history pages sum to total', collected === total, `${collected} vs ${total}`);
const beyond = await A.listDelegationHistory(busiest, { page: pages + 5, size });
check('page past the end: empty with total', beyond.data.elements.length === 0 && beyond.data.total === total, beyond.data);
const govOnly = await A.listDelegationHistory(busiest, { page: 1, size: 50, kind: 'governance' });
check('kind=governance filters', govOnly.data.elements.every((e) => e.kind === 'governance'));
const poolOnly = await A.listDelegationHistory(busiest, { page: 1, size: 50, kind: 'pool' });
check('kind=pool filters', poolOnly.data.elements.every((e) => e.kind === 'pool'));
check('kind totals add up', govOnly.data.total + poolOnly.data.total === total, `${govOnly.data.total}+${poolOnly.data.total} vs ${total}`);
console.log(`  busiest ${busiest}: ${total} events (${govOnly.data.total} governance, ${poolOnly.data.total} pool)`);

await expectCode('listDelegationHistory page 0', () => A.listDelegationHistory(busiest, { page: 0, size: 5 }), 'INVALID_INPUT');
await expectCode('listDelegationHistory size 0', () => A.listDelegationHistory(busiest, { page: 1, size: 0 }), 'INVALID_INPUT');
await expectCode('listDelegationHistory bad kind', () => A.listDelegationHistory(busiest, { page: 1, size: 5, kind: 'drep' }), 'INVALID_INPUT');

const mainnetAddr = 'stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw';
for (const [label, bad] of [
  ['mainnet stake1 address', mainnetAddr],
  ['garbage', 'not-an-address'],
  ['empty', ''],
  ['DRep id as address', drepSample?.delegation.target.drep.id ?? 'drep1xyz'],
  ['number', 42],
]) {
  for (const m of ['get', 'getDelegation', 'getPoolDelegation', 'getVotingPower']) {
    await expectCode(`${m}(${label})`, () => A[m](bad), 'INVALID_INPUT');
  }
  await expectCode(`listDelegationHistory(${label})`, () => A.listDelegationHistory(bad, { page: 1, size: 5 }), 'INVALID_INPUT');
}

/* -- transactions ------------------------------------------------------------ */

console.log('\ntransactions');
const tx = await timed('transactions.get(onchain)', () => chainData.transactions.get(txHash.toUpperCase()));
checkMeta('transactions.get', tx);
check('tx onChain', tx.data.onChain === true && tx.data.txHash === txHash, tx.data);
checkStamp('tx.includedAt', tx.data.includedAt);
const missingTx = await timed('transactions.get(unknown)', () => chainData.transactions.get(randomBytes(32).toString('hex')));
check('unknown tx onChain false, no includedAt', missingTx.data.onChain === false && !('includedAt' in missingTx.data), missingTx.data);
await expectCode('transactions.get(short)', () => chainData.transactions.get('abcd'), 'INVALID_INPUT');
await expectCode('transactions.get(non-hex)', () => chainData.transactions.get('z'.repeat(64)), 'INVALID_INPUT');
await expectCode('transactions.get(undefined)', () => chainData.transactions.get(undefined), 'INVALID_INPUT');

/* -- Koios cross-check ------------------------------------------------------- */

if (koiosUrl) {
  console.log(`\nkoios cross-check (${koiosUrl})`);
  try {
    const [kp] = await koios(`/epoch_params?_epoch_no=${p.epoch}`);
    compare('protocol_major', p.protocolVersion.major, kp.protocol_major);
    compare('protocol_minor', p.protocolVersion.minor, kp.protocol_minor);
    const pairs = {
      govActionLifetime: 'gov_action_lifetime', govActionDeposit: 'gov_action_deposit', drepDeposit: 'drep_deposit',
      drepActivity: 'drep_activity', committeeMinSize: 'committee_min_size', committeeMaxTermLength: 'committee_max_term_length',
      keyDeposit: 'key_deposit', poolDeposit: 'pool_deposit', coinsPerUtxoByte: 'coins_per_utxo_size',
      minFeeA: 'min_fee_a', minFeeB: 'min_fee_b', maxTxSize: 'max_tx_size', maxValSize: 'max_val_size',
    };
    for (const [ours, theirs] of Object.entries(pairs)) compare(`params.${ours}`, p[ours], kp[theirs]);
    const dvt = {
      motionNoConfidence: 'dvt_motion_no_confidence', committeeNormal: 'dvt_committee_normal',
      committeeNoConfidence: 'dvt_committee_no_confidence', updateToConstitution: 'dvt_update_to_constitution',
      hardForkInitiation: 'dvt_hard_fork_initiation', ppNetworkGroup: 'dvt_p_p_network_group',
      ppEconomicGroup: 'dvt_p_p_economic_group', ppTechnicalGroup: 'dvt_p_p_technical_group',
      ppGovGroup: 'dvt_p_p_gov_group', treasuryWithdrawal: 'dvt_treasury_withdrawal',
    };
    for (const [ours, theirs] of Object.entries(dvt)) {
      const r = p.drepThresholds[ours];
      check(`koios drepThresholds.${ours} ${r.numerator}/${r.denominator} = ${kp[theirs]}`, ratioEq(r, kp[theirs]));
    }
    const pvt = {
      motionNoConfidence: 'pvt_motion_no_confidence', committeeNormal: 'pvt_committee_normal',
      committeeNoConfidence: 'pvt_committee_no_confidence', hardForkInitiation: 'pvt_hard_fork_initiation',
      ppSecurityGroup: 'pvtpp_security_group',
    };
    for (const [ours, theirs] of Object.entries(pvt)) {
      const r = p.poolThresholds[ours];
      check(`koios poolThresholds.${ours} ${r.numerator}/${r.denominator} = ${kp[theirs]}`, ratioEq(r, kp[theirs]));
    }

    const [kt] = await koios(`/totals?_epoch_no=${tr.data.epoch}`);
    compare('treasury', tr.data.balance, kt.treasury);
    compare('reserves', tr.data.reserves, kt.reserves);

    const [ke] = await koios(`/epoch_info?_epoch_no=${current}`);
    compare('totalActiveStake (epoch_info.active_stake)', s.totalActiveStake, ke.active_stake);

    for (const [target, field] of [['drep_always_abstain', 'alwaysAbstainVotingPower'], ['drep_always_no_confidence', 'alwaysNoConfidenceVotingPower']]) {
      if (s[field] === undefined) continue;
      const hist = await koios(`/drep_voting_power_history?_drep_id=${target}&_epoch_no=${current}`);
      if (hist?.[0]) compare(field, s[field], hist[0].amount);
      else console.log(`  koios has no ${target} voting power for epoch ${current}`);
    }

    const sampleAddrs = [abstainer, drepSample?.addr, noConfidence, ...retiredDRepDelegators.slice(0, 2), ...retiredPoolDelegators.slice(0, 2)].filter(Boolean);
    const kaccounts = await koios('/account_info', { _stake_addresses: sampleAddrs });
    for (const ka of kaccounts) {
      const addr = ka.stake_address;
      const [acc, del, pdel, power] = await Promise.all([A.get(addr), A.getDelegation(addr), A.getPoolDelegation(addr), A.getVotingPower(addr)]);
      const short = `${addr.slice(0, 20)}…`;
      compare(`${short} registered`, acc.data.isRegistered, ka.status === 'registered');
      const ourDrep = del.data === null ? null
        : del.data.target.kind === 'predefined'
          ? { alwaysAbstain: 'drep_always_abstain', alwaysNoConfidence: 'drep_always_no_confidence' }[del.data.target.target]
          : credentialOf(del.data.target.drep.id);
      const theirDrep = ka.delegated_drep === null ? null
        : ka.delegated_drep.startsWith('drep_always') ? ka.delegated_drep : credentialOf(ka.delegated_drep);
      compare(`${short} delegated drep`, ourDrep, theirDrep);
      compare(`${short} delegated pool`, pdel.data?.poolId ?? null, ka.delegated_pool);
      if (power.data) {
        // Koios total_balance = utxo + rewards_available; ours adds held proposal deposits.
        const theirs = BigInt(ka.utxo) + BigInt(ka.rewards_available);
        compare(`${short} voting power vs utxo+rewards_available`, power.data.amount, theirs.toString());
      } else {
        check(`${short} null voting power only when unregistered`, ka.status !== 'registered', ka.status);
      }
    }
  } catch (error) {
    failures++;
    console.log(`  FAIL koios cross-check errored: ${error.message}`);
  }
}

await sql.end();
await close();
console.log(`\n${passes} passed, ${failures} failed`);
if (mismatches.length) console.log('koios mismatches:', JSON.stringify(mismatches, null, 2));
process.exit(failures ? 1 : 0);
