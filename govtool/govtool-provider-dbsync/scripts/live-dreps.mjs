#!/usr/bin/env node
/**
 * Live check of governance.dreps against a real db-sync, cross-checked with
 * Koios. Read-only on both.
 *
 *   npm run build
 *   DBSYNC_POSTGRES_HOST=... DBSYNC_POSTGRES_PORT=... DBSYNC_POSTGRES_USER=... \
 *   DBSYNC_POSTGRES_PASSWORD=... DBSYNC_DATABASE=... \
 *   [NETWORK=preview] [KOIOS_URL=https://preview.koios.rest/api/v1] [SKIP_KOIOS=1] \
 *   node scripts/live-dreps.mjs
 *
 * Exits non-zero when a contract check fails. Koios mismatches are reported
 * but do not fail the run: Koios is a second opinion, not the ledger.
 */
import { createRequire } from 'node:module';
import { randomBytes } from 'node:crypto';

const require = createRequire(import.meta.url);
const { createDbSyncProvider } = require('../dist/index.js');
const { capabilities } = require('../dist/system.js');
const { Client } = require('pg');
const { bech32 } = require('bech32');

const env = process.env;
const NETWORK = env.NETWORK ?? 'preview';
const KOIOS = env.KOIOS_URL ?? `https://${NETWORK === 'mainnet' ? 'api' : NETWORK}.koios.rest/api/v1`;
const connection = {
  host: env.DBSYNC_POSTGRES_HOST,
  port: Number(env.DBSYNC_POSTGRES_PORT ?? 5432),
  user: env.DBSYNC_POSTGRES_USER,
  password: env.DBSYNC_POSTGRES_PASSWORD,
  database: env.DBSYNC_DATABASE,
};
if (!connection.host || !connection.database) {
  console.error('Set DBSYNC_POSTGRES_HOST, _PORT, _USER, _PASSWORD and DBSYNC_DATABASE');
  process.exit(2);
}

const { chainData, close } = createDbSyncProvider({ network: NETWORK, connection });
const dreps = chainData.governance.dreps;

let failures = 0;
const koiosMismatches = [];
const timings = {};
const ok = (cond, message, extra) => {
  if (cond) return true;
  failures += 1;
  console.log(`  FAIL ${message}${extra === undefined ? '' : ` ${JSON.stringify(extra)}`}`);
  return false;
};
const section = (name) => console.log(`\n== ${name}`);

async function timed(name, fn) {
  const start = Date.now();
  try {
    return await fn();
  } finally {
    const ms = Date.now() - start;
    timings[name] = Math.max(timings[name] ?? 0, ms);
  }
}

async function rejectsWith(code, fn, label) {
  let promise;
  try {
    promise = fn();
  } catch (error) {
    ok(false, `${label}: threw synchronously instead of rejecting`, { code: error?.code });
    return;
  }
  ok(promise instanceof Promise, `${label}: did not return a promise`);
  try {
    await promise;
    ok(false, `${label}: resolved, expected ${code}`);
  } catch (error) {
    ok(error?.code === code, `${label}: expected ${code}, got ${error?.code}`, error?.message);
  }
}

/** Read every page of a sorted listing; check totals, uniqueness and short pages. */
async function readAll(query, size) {
  const all = [];
  let total;
  for (let page = 1; ; page += 1) {
    const { data } = await timed(`list(${query.sort})`, () => dreps.list({ ...query, page, size }));
    if (total === undefined) total = data.total;
    ok(data.total === total, `total changed between pages`, { page, was: total, now: data.total });
    all.push(...data.elements);
    const last = all.length >= total;
    if (!last) ok(data.elements.length === size, `short page before the end`, { page, got: data.elements.length });
    if (last || data.elements.length === 0) break;
  }
  ok(all.length === total, `concatenated pages != total`, { rows: all.length, total, query });
  ok(new Set(all.map((d) => d.id)).size === all.length, `duplicate ids across pages`, query);
  return { all, total };
}

const powerOf = (d) => (d.votingPower === null ? -1n : BigInt(d.votingPower.amount));

function checkEntity(d, label) {
  ok(d.role === 'drep', `${label}: role`);
  ok(/^drep1/.test(d.id), `${label}: id prefix`);
  ok((d.anchor === null) === (d.kind === 'anonymous'), `${label}: kind does not follow the anchor`, d.id);
  ok(['active', 'inactive', 'retired'].includes(d.status), `${label}: status`, d.status);
  ok(d.registration?.latest?.at?.time !== undefined, `${label}: latest registration has no date`, d.id);
  ok(d.registration.latestUpdate === null || d.registration.latestUpdate.at.time !== undefined, `${label}: update has no date`);
  ok((d.status === 'retired') === (d.registration.retiredAt !== null), `${label}: retiredAt vs status`, d.id);
  if (d.votingPower !== null) {
    ok(/^\d+$/.test(d.votingPower.amount) && d.votingPower.basis === 'active', `${label}: voting power shape`, d.votingPower);
  }
  if (d.activity) ok(d.activity.voted <= d.activity.votable, `${label}: voted > votable`, d.activity);
  if (d.status === 'retired') ok(d.expiryEpoch === undefined, `${label}: retired DRep carries an expiry`, d.id);
}

async function main() {
  const caps = capabilities();
  console.log(`db-sync ${connection.database} (${NETWORK}); declared sorts=${JSON.stringify(caps.sorts.dreps)} ` +
    `filters=${JSON.stringify(caps.filters.dreps)} search=${JSON.stringify(caps.search)}`);

  /* -- sorts ------------------------------------------------------------- */
  section('sorts: whole directory, page by page');
  const byPower = await readAll({ sort: 'votingPower' }, 1000);
  console.log(`  votingPower: ${byPower.total} DReps`);
  for (let i = 1; i < byPower.all.length; i += 1) {
    if (!ok(powerOf(byPower.all[i - 1]) >= powerOf(byPower.all[i]), 'votingPower order broken', { at: i })) break;
  }
  byPower.all.forEach((d, i) => i % 50 === 0 && checkEntity(d, 'votingPower row'));

  const byDate = await readAll({ sort: 'registrationDate' }, 1000);
  console.log(`  registrationDate: ${byDate.total} DReps`);
  // Ordered by the latest registration's transaction, so block numbers never increase.
  const block = (d) => d.registration.latest.txRef.block;
  for (let i = 1; i < byDate.all.length; i += 1) {
    const [a, b] = [block(byDate.all[i - 1]), block(byDate.all[i])];
    if (!ok(a >= b, 'registrationDate order broken', { at: i, a, b })) break;
  }
  ok(byPower.total === byDate.total, 'sorts disagree on the directory size');

  // Small pages exercise the page boundaries many times over.
  const smallPages = await readAll({ sort: 'votingPower', status: ['active'] }, 7);
  console.log(`  votingPower+active in pages of 7: ${smallPages.total}`);
  for (let i = 1; i < smallPages.all.length; i += 1) {
    ok(powerOf(smallPages.all[i - 1]) >= powerOf(smallPages.all[i]), 'order broken across small pages', { at: i });
  }

  await rejectsWith('CAPABILITY_UNSUPPORTED', () => dreps.list({ page: 1, size: 5, sort: 'activity' }), 'sort=activity');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 1, size: 5, sort: 'name' }), 'unknown sort');

  /* -- filters ----------------------------------------------------------- */
  section('filters');
  const totals = {};
  for (const status of ['active', 'inactive', 'retired']) {
    const { all, total } = await readAll({ sort: 'registrationDate', status: [status] }, 1000);
    totals[status] = total;
    ok(all.every((d) => d.status === status), `status=${status} returned another status`);
  }
  for (const kind of ['drep', 'anonymous']) {
    const { all, total } = await readAll({ sort: 'votingPower', kind: [kind] }, 1000);
    totals[kind] = total;
    ok(all.every((d) => d.kind === kind), `kind=${kind} returned another kind`);
  }
  console.log(`  ${JSON.stringify(totals)}`);
  ok(totals.active + totals.inactive + totals.retired === byPower.total, 'status totals do not sum to the directory');
  ok(totals.drep + totals.anonymous === byPower.total, 'kind totals do not sum to the directory');
  const both = (await dreps.list({ page: 1, size: 1, sort: 'votingPower', status: ['active', 'inactive'] })).data.total;
  ok(both === totals.active + totals.inactive, 'multi-value status filter', { both });
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 1, size: 5, status: ['dormant'] }), 'unknown status');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 1, size: 5, kind: ['sole'] }), 'unknown kind');

  /* -- random ------------------------------------------------------------ */
  section('random');
  const r1 = await timed('list(random)', () => dreps.list({ page: 1, size: 20 }));
  const r2 = await timed('list(random)', () => dreps.list({ page: 1, size: 20, sort: 'random' }));
  ok(r1.data.elements.length === Math.min(20, byPower.total), 'random did not return size rows');
  ok(r1.data.total === byPower.total, 'random total');
  const overlap = r1.data.elements.filter((d) => r2.data.elements.some((e) => e.id === d.id)).length;
  console.log(`  two random reads share ${overlap}/20 rows`);
  ok(overlap < 20 || byPower.total <= 20, 'two random reads were identical');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 2, size: 20 }), 'random default, page 2');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 2, size: 20, sort: 'random' }), 'random, page 2');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 0, size: 20 }), 'page 0');
  await rejectsWith('INVALID_INPUT', () => dreps.list({ page: 1, size: 0 }), 'size 0');

  /* -- search and get ---------------------------------------------------- */
  section('search, get, ids');
  const sample = byPower.all[0];
  const anonymous = byPower.all.find((d) => d.kind === 'anonymous');
  const script = byPower.all.find((d) => d.isScriptBased);
  for (const d of [sample, anonymous, script].filter(Boolean)) {
    const found = await dreps.list({ page: 1, size: 5, search: d.id });
    ok(found.data.total === 1 && found.data.elements[0]?.id === d.id, `exactId search for ${d.kind}${d.isScriptBased ? ' script' : ''} DRep`, d.id);
    const got = await timed('get', () => dreps.get(d.id));
    ok(JSON.stringify(got.data) === JSON.stringify(found.data.elements[0]), 'get differs from the listing row', d.id);
    checkEntity(got.data, 'get');
  }
  const upper = await dreps.list({ page: 1, size: 5, search: `  ${sample.id.toUpperCase()} ` });
  ok(upper.data.total === 1, 'search ignores case and surrounding blanks');

  const pg = new Client(connection);
  await pg.connect();
  const [{ view: cip105 }] = (await pg.query(
    `SELECT view FROM drep_hash WHERE raw IS NOT NULL AND view LIKE 'drep1%' AND NOT has_script LIMIT 1`,
  )).rows;
  await pg.end();
  const cip105Search = await dreps.list({ page: 1, size: 5, search: cip105 });
  ok(cip105Search.data.total === 0, 'a CIP-105 id matched a search', cip105);
  ok((await dreps.list({ page: 1, size: 5, search: 'spannercode' })).data.total === 0, 'name search matched');
  await rejectsWith('INVALID_INPUT', () => dreps.get(cip105), `get(CIP-105 ${cip105})`);
  await rejectsWith('INVALID_INPUT', () => dreps.get('not-a-drep'), 'get(junk)');
  await rejectsWith('INVALID_INPUT', () => dreps.get(123), 'get(number)');
  const unknown = bech32.encode('drep', bech32.toWords(Buffer.concat([Buffer.from([0x22]), randomBytes(28)])), 1023);
  await rejectsWith('NOT_FOUND', () => dreps.get(unknown), 'get(unknown CIP-129)');
  await rejectsWith('NOT_FOUND', () => dreps.listVotes(unknown, { page: 1, size: 5 }), 'listVotes(unknown)');
  await rejectsWith('NOT_FOUND', () => dreps.listUpdateHistory(unknown, { page: 1, size: 5 }), 'listUpdateHistory(unknown)');
  await rejectsWith('INVALID_INPUT', () => dreps.listVotes(cip105, { page: 1, size: 5 }), 'listVotes(CIP-105)');
  ok(dreps.listDelegators === undefined, 'listDelegators is declared absent');

  /* -- votes ------------------------------------------------------------- */
  section('listVotes');
  const voters = byDate.all.filter((d) => d.activity && d.activity.voted > 0).slice(0, 3);
  for (const d of [...voters, sample]) {
    const all = await readVotes(d.id, {});
    const yes = await readVotes(d.id, { voted: true });
    const no = await readVotes(d.id, { voted: false });
    ok(all.length === d.activity.votable, 'unfiltered vote listing != activity.votable', { id: d.id, rows: all.length, activity: d.activity });
    ok(yes.length === d.activity.voted, 'voted listing != activity.voted', { id: d.id, rows: yes.length });
    ok(yes.length + no.length === all.length, 'voted + not voted != all', d.id);
    ok(yes.every((r) => r.voted && r.choice && r.txRef?.txHash && 'anchor' in r), 'voted row shape', d.id);
    ok(no.every((r) => !r.voted && !('choice' in r) && !('anchor' in r)), 'not-voted row carries vote fields', d.id);
    ok(new Set(all.map((r) => r.action.id)).size === all.length, 'an action appears twice', d.id);
    const oldest = await readVotes(d.id, { sort: 'oldest' });
    ok(JSON.stringify(oldest.map((r) => r.action.id)) === JSON.stringify(all.map((r) => r.action.id).reverse()), 'oldest is not newest reversed', d.id);
    const titled = all.filter((r) => r.action.title !== undefined).length;
    console.log(`  ${d.id}: ${yes.length}/${all.length} voted, ${titled} rows titled`);
  }
  const past = await dreps.listVotes(sample.id, { page: 10_000, size: 50 });
  ok(past.data.elements.length === 0 && past.data.total === sample.activity.votable, 'page past the end keeps total', past.data);
  await rejectsWith('INVALID_INPUT', () => dreps.listVotes(sample.id, { page: 1, size: 5, sort: 'loudest' }), 'unknown vote sort');

  /* -- update history ---------------------------------------------------- */
  section('listUpdateHistory');
  const updated = byDate.all.find((d) => d.registration.latestUpdate !== null) ?? sample;
  const desc = await timed('listUpdateHistory', () => dreps.listUpdateHistory(updated.id, { page: 1, size: 1000 }));
  const asc = await dreps.listUpdateHistory(updated.id, { page: 1, size: 1000, sort: 'asc' });
  ok(desc.data.total === desc.data.elements.length && desc.data.total >= 1, 'history total');
  ok(JSON.stringify(asc.data.elements) === JSON.stringify([...desc.data.elements].reverse()), 'asc is not desc reversed');
  ok(desc.data.elements.every((e) => e.at.time && 'anchor' in e), 'history rows need an anchor field and a date');
  ok(desc.data.elements.some((e) => e.txRef.txHash === updated.registration.latest.txRef.txHash), 'latest registration missing from history');
  console.log(`  ${updated.id}: ${desc.data.total} metadata changes`);
  await rejectsWith('INVALID_INPUT', () => dreps.listUpdateHistory(updated.id, { page: 1, size: 5, sort: 'up' }), 'history sort');

  /* -- counts ------------------------------------------------------------ */
  section('getCounts');
  const counts = (await timed('getCounts', () => dreps.getCounts())).data;
  console.log(`  ${JSON.stringify(counts)}`);
  ok(counts.totalActive === totals.active, 'counts.totalActive != list(status=active).total');
  ok(counts.totalInactive === totals.inactive, 'counts.totalInactive != list(status=inactive).total');
  ok(counts.totalRegistered === totals.active + totals.inactive, 'counts.totalRegistered != active + inactive');
  const anon = (await dreps.list({ page: 1, size: 1, sort: 'votingPower', kind: ['anonymous'], status: ['active', 'inactive'] })).data.total;
  ok(counts.anonymous === anon, 'counts.anonymous != registered anonymous DReps', { anon });

  /* -- Koios ------------------------------------------------------------- */
  if (!env.SKIP_KOIOS) await koios(byPower.all);

  section('timings (slowest call, ms)');
  for (const [name, ms] of Object.entries(timings)) console.log(`  ${name.padEnd(24)} ${ms}`);
  ok(Math.max(...Object.values(timings)) < 30_000, 'a call took 30 s or more');

  section('result');
  console.log(`  ${failures} contract check(s) failed; ${koiosMismatches.length} Koios mismatch(es)`);
}

async function readVotes(id, q) {
  const rows = [];
  for (let page = 1; ; page += 1) {
    const { data } = await timed('listVotes', () => dreps.listVotes(id, { ...q, page, size: 500 }));
    rows.push(...data.elements);
    if (rows.length >= data.total || data.elements.length === 0) {
      ok(rows.length === data.total, 'vote pages != total', { id, q });
      return rows;
    }
  }
}

async function koiosPost(path, body) {
  const res = await fetch(`${KOIOS}${path}`, {
    method: 'POST',
    headers: { 'content-type': 'application/json', accept: 'application/json' },
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`Koios ${path}: HTTP ${res.status}`);
  return res.json();
}

async function koiosGet(path) {
  const res = await fetch(`${KOIOS}${path}`, { headers: { accept: 'application/json' } });
  if (!res.ok) throw new Error(`Koios ${path}: HTTP ${res.status}`);
  return res.json();
}

async function koios(directory) {
  section(`Koios cross-check (${KOIOS})`);
  const pick = (pred, n) => directory.filter(pred).slice(0, n);
  const noExpiry = (d) => d.status !== 'retired' && d.expiryEpoch === undefined;
  const shuffled = [...directory].sort(() => Math.random() - 0.5);
  const sample = [
    ...directory.slice(0, 10),
    ...shuffled.slice(0, 15),
    ...pick((d) => d.status === 'retired', 5),
    ...pick((d) => d.kind === 'anonymous' && d.status !== 'retired', 5),
    ...pick((d) => noExpiry(d) && d.status === 'active', 5),
    ...pick((d) => noExpiry(d) && d.status === 'inactive', 5),
    ...pick((d) => d.status === 'active' && d.expiryEpoch !== undefined, 5),
  ];
  const unique = [...new Map(sample.map((d) => [d.id, d])).values()];
  const info = new Map((await koiosPost('/drep_info', { _drep_ids: unique.map((d) => d.id) })).map((k) => [k.drep_id, k]));
  const tip = (await koiosGet('/tip'))[0];
  console.log(`  ${unique.length} DReps sampled; Koios tip epoch ${tip.epoch_no}`);

  const byField = {};
  const mismatch = (field, d, ours, theirs) => {
    byField[field] = (byField[field] ?? 0) + 1;
    koiosMismatches.push({ field, id: d.id, ours, koios: theirs });
  };
  for (const d of unique) {
    const k = info.get(d.id);
    if (!k) {
      mismatch('missing in Koios', d, d.status, null);
      continue;
    }
    const kStatus = k.drep_status === 'deregistered' ? 'retired' : k.active ? 'active' : 'inactive';
    if (kStatus !== d.status) mismatch(`status${noExpiry(d) ? ' (no db-sync expiry)' : ''}`, d, d.status, kStatus);
    if (d.expiryEpoch !== undefined && k.expires_epoch_no !== d.expiryEpoch) mismatch('expiryEpoch', d, d.expiryEpoch, k.expires_epoch_no);
    if (d.status !== 'retired' && k.deposit !== d.registration.latest.deposit) mismatch('deposit', d, d.registration.latest.deposit, k.deposit);
    const ourAmount = d.votingPower?.amount ?? null;
    if (ourAmount !== null && ourAmount !== k.amount) mismatch('votingPower (drep_info.amount)', d, ourAmount, k.amount);
    if (d.delegatorCount !== k.live_delegator_count) mismatch('delegatorCount', d, d.delegatorCount, k.live_delegator_count);
    if (d.status !== 'retired' && (d.anchor?.url ?? null) !== k.meta_url) mismatch('anchor.url', d, d.anchor?.url ?? null, k.meta_url);
    if (d.status !== 'retired' && (d.anchor?.dataHash ?? null) !== k.meta_hash) mismatch('anchor.dataHash', d, d.anchor?.dataHash ?? null, k.meta_hash);
  }

  // Voting power at the snapshot epoch, and the delegator list, for a few.
  for (const d of unique.filter((x) => x.votingPower !== null).slice(0, 8)) {
    const epoch = d.votingPower.epoch;
    const hist = await koiosGet(`/drep_voting_power_history?_drep_id=${d.id}&_epoch_no=${epoch}`);
    const theirs = hist[0]?.amount ?? null;
    if (theirs !== d.votingPower.amount && !(theirs === null && d.votingPower.amount === '0')) {
      mismatch(`votingPower (history @${epoch})`, d, d.votingPower.amount, theirs);
    }
  }
  for (const d of unique.filter((x) => (x.delegatorCount ?? 0) > 0).slice(0, 5)) {
    const delegators = await koiosGet(`/drep_delegators?_drep_id=${d.id}`);
    if (delegators.length !== d.delegatorCount) mismatch('delegatorCount (drep_delegators)', d, d.delegatorCount, delegators.length);
  }

  console.log(`  mismatches by field: ${JSON.stringify(byField)}`);
  for (const m of koiosMismatches.slice(0, 40)) console.log(`   - ${JSON.stringify(m)}`);
}

try {
  await main();
} catch (error) {
  failures += 1;
  console.error('Unexpected error:', error?.code ?? '', error?.message, error?.cause?.message ?? '');
} finally {
  await close();
}
process.exit(failures === 0 ? 0 : 1);
