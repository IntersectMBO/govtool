#!/usr/bin/env node
/**
 * Live conformance for ProposalsApi against a real db-sync, plus a Koios
 * cross-check of statuses and vote aggregates. READ-ONLY.
 *
 *   set -a; . ../../tests/test-infrastructure/.env; set +a
 *   npm run build && node scripts/live-proposals.mjs [--network preview] [--koios-sample 40] [--no-koios]
 *
 * Needs DBSYNC_POSTGRES_HOST/PORT/USER/PASSWORD and DBSYNC_DATABASE. Fixture
 * tests cannot see mapping bugs; this is where they surface.
 */
import { createRequire } from 'node:module';
import { randomBytes } from 'node:crypto';

const require = createRequire(import.meta.url);
const { createDbSyncProvider } = require('../dist/index.js');
const { encodeGovActionId } = require('../dist/ids.js');
const pg = require('pg');
const { bech32 } = require('bech32');

const args = process.argv.slice(2);
const arg = (name, fallback) => {
  const i = args.indexOf(`--${name}`);
  return i === -1 ? fallback : args[i + 1];
};
const network = arg('network', 'preview');
const koiosSample = Number(arg('koios-sample', '40'));
const koiosBase = arg('koios', `https://${network === 'mainnet' ? 'api' : network}.koios.rest/api/v1`);
const skipKoios = args.includes('--no-koios');

const env = process.env;
for (const k of ['DBSYNC_POSTGRES_HOST', 'DBSYNC_POSTGRES_PORT', 'DBSYNC_POSTGRES_USER', 'DBSYNC_DATABASE']) {
  if (!env[k]) {
    console.error(`missing ${k}`);
    process.exit(2);
  }
}
const connection = {
  host: env.DBSYNC_POSTGRES_HOST,
  port: Number(env.DBSYNC_POSTGRES_PORT),
  user: env.DBSYNC_POSTGRES_USER,
  password: env.DBSYNC_POSTGRES_PASSWORD,
  database: env.DBSYNC_DATABASE,
};
const { chainData, close } = createDbSyncProvider({ network, connection });
const api = chainData.governance.proposals;
const sql = new pg.Pool({ ...connection, max: 4 });
const q = async (text, params = []) => (await sql.query(text, params)).rows;

let failures = 0;
const timings = [];
const check = (ok, what, detail) => {
  if (ok) console.log(`  ok   ${what}`);
  else {
    failures++;
    console.log(`  FAIL ${what}${detail === undefined ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  }
};
async function timed(label, fn) {
  const t = Date.now();
  try {
    return await fn();
  } finally {
    timings.push([label, Date.now() - t]);
  }
}
async function expectCode(code, what, fn) {
  try {
    await fn();
    check(false, what, 'resolved');
  } catch (e) {
    check(e?.code === code, what, e?.code ?? String(e));
  }
}

/** Read every page of a listing; checks the total and duplicates. */
async function readAll(label, query, size) {
  const out = [];
  let total;
  for (let page = 1; ; page++) {
    const r = await timed(`${label} p${page}`, () => api.list({ ...query, page, size }));
    total = r.data.total;
    out.push(...r.data.elements);
    if (r.data.elements.length < size) break;
  }
  return { rows: out, total };
}

const submittedKey = (a) => [a.lifecycle.submitted.slot, a.index];
const cmp = (x, y) => (x[0] - y[0]) || (x[1] - y[1]);

console.log(`db-sync ${connection.database} (${network})`);

/* -- list: every sort, full traversal ------------------------------------ */
console.log('\nlist — sorts');
const [{ n: dbTotal }] = await q('SELECT count(*)::int AS n FROM gov_action_proposal');
const sorts = (await chainData.system.getCapabilities()).data.sorts.proposals;
console.log(`  declared sorts: ${sorts.join(', ')}`);
const bySort = {};
for (const sort of sorts) {
  const a = await readAll(`list ${sort} size 200`, { sort }, 200);
  const b = await readAll(`list ${sort} size 450`, { sort }, 450);
  bySort[sort] = a.rows;
  const ids = a.rows.map((r) => r.id);
  check(a.total === dbTotal && a.rows.length === dbTotal, `${sort}: pages concatenate to total ${dbTotal}`, { total: a.total, rows: a.rows.length });
  check(new Set(ids).size === ids.length, `${sort}: no duplicates across pages`);
  check(ids.join() === b.rows.map((r) => r.id).join(), `${sort}: same order at page size 200 and 450`);
  const pairs = a.rows.slice(1).map((r, i) => [a.rows[i], r]);
  const drepYes = (x) => BigInt(x.voteAggregates.find((v) => v.role === 'drep')?.yes ?? '0');
  const monotone = {
    newest: ([x, y]) => cmp(submittedKey(x), submittedKey(y)) >= 0,
    oldest: ([x, y]) => cmp(submittedKey(x), submittedKey(y)) <= 0,
    soonestToExpire: ([x, y]) => (x.lifecycle.expires?.epoch ?? Infinity) <= (y.lifecycle.expires?.epoch ?? Infinity),
    mostYesVotes: ([x, y]) => drepYes(x) >= drepYes(y),
  }[sort];
  if (monotone) {
    const bad = pairs.findIndex((p) => !monotone(p));
    check(bad === -1, `${sort}: ordered across page boundaries`, bad === -1 ? undefined : { at: bad, a: pairs[bad][0].id, b: pairs[bad][1].id });
  }
}
if (bySort.highestParticipation) {
  // Independent recomputation of the participation key for the whole set.
  const rows = await q(`
    WITH cur AS (SELECT max(no) no FROM epoch),
    f AS (SELECT g.id, COALESCE(g.ratified_epoch, g.expired_epoch, g.dropped_epoch, cur.no) e FROM gov_action_proposal g, cur),
    v AS (SELECT DISTINCT ON (gov_action_proposal_id, drep_voter) gov_action_proposal_id pid, drep_voter FROM voting_procedure
           WHERE drep_voter IS NOT NULL AND invalid IS NULL ORDER BY gov_action_proposal_id, drep_voter, tx_id DESC, id DESC),
    voted AS (SELECT f.id, sum(dd.amount) s FROM f JOIN v ON v.pid = f.id JOIN drep_distr dd ON dd.hash_id = v.drep_voter AND dd.epoch_no = f.e AND dd.active_until >= f.e GROUP BY f.id),
    tot AS (SELECT dd.epoch_no e, sum(dd.amount) FILTER (WHERE dd.active_until >= dd.epoch_no OR dh.view = 'drep_always_no_confidence') s
              FROM drep_distr dd JOIN drep_hash dh ON dh.id = dd.hash_id WHERE dd.epoch_no IN (SELECT e FROM f) GROUP BY 1)
    SELECT encode(t.hash,'hex') h, g.index i, COALESCE(voted.s, 0)::numeric / NULLIF(tot.s, 0) k
      FROM f JOIN gov_action_proposal g ON g.id = f.id JOIN tx t ON t.id = g.tx_id
      LEFT JOIN voted ON voted.id = f.id LEFT JOIN tot ON tot.e = f.e`);
  const key = new Map(rows.map((r) => [`${r.h}#${r.i}`, r.k === null ? -1 : Number(r.k)]));
  const seq = bySort.highestParticipation.map((a) => key.get(`${a.txHash}#${a.index}`));
  const bad = seq.findIndex((k, i) => i > 0 && k > seq[i - 1] + 1e-12);
  check(bad === -1, 'highestParticipation: ordered by independently computed key', bad === -1 ? undefined : { at: bad });
}

/* -- list: filters -------------------------------------------------------- */
console.log('\nlist — filters');
const statusCounts = {};
for (const status of ['live', 'ratified', 'enacted', 'expired', 'dropped']) {
  const r = await timed(`list status ${status}`, () => api.list({ page: 1, size: 1000, status: [status] }));
  statusCounts[status] = r.data.total;
  check(r.data.elements.every((a) => a.lifecycle.status === status), `status ${status}: every row has that status (${r.data.total})`);
}
check(Object.values(statusCounts).reduce((a, b) => a + b, 0) === dbTotal, 'statuses partition the set', statusCounts);
const typeCounts = {};
const dbTypes = Object.fromEntries((await q('SELECT type::text t, count(*)::int n FROM gov_action_proposal GROUP BY 1')).map((r) => [r.t === 'NewCommittee' ? 'UpdateCommittee' : r.t, r.n]));
for (const type of ['ParameterChange', 'HardForkInitiation', 'TreasuryWithdrawals', 'NoConfidence', 'UpdateCommittee', 'NewConstitution', 'InfoAction']) {
  const r = await timed(`list type ${type}`, () => api.list({ page: 1, size: 5, type: [type] }));
  typeCounts[type] = r.data.total;
  check(r.data.total === (dbTypes[type] ?? 0) && r.data.elements.every((a) => a.type === type && a.body.type === type), `type ${type}: ${r.data.total} rows, all typed`);
}
const combo = await api.list({ page: 1, size: 50, type: ['ParameterChange', 'UpdateCommittee'], status: ['enacted'] });
const [{ n: comboDb }] = await q(`SELECT count(*)::int n FROM gov_action_proposal WHERE type IN ('ParameterChange','NewCommittee') AND enacted_epoch IS NOT NULL`);
check(combo.data.total === comboDb, `type+status combined (${combo.data.total})`);
const beyond = await api.list({ page: 1000, size: 10 });
check(beyond.data.elements.length === 0 && beyond.data.total === dbTotal, 'page past the end is empty and still carries total');

/* -- search --------------------------------------------------------------- */
console.log('\nlist — search');
const sample = bySort.newest[0];
const byId = await api.list({ page: 1, size: 10, search: sample.id });
check(byId.data.total === 1 && byId.data.elements[0].id === sample.id, 'search by CIP-129 id');
const byTx = await api.list({ page: 1, size: 10, search: sample.txHash.toUpperCase() });
check(byTx.data.elements.some((a) => a.id === sample.id), 'search by tx hash');
const junk = await api.list({ page: 1, size: 10, search: 'not an id' });
check(junk.data.total === 0 && junk.data.elements.length === 0, 'unmatched search is empty');

/* -- get, errors ---------------------------------------------------------- */
console.log('\nget');
for (const a of [bySort.newest[0], bySort.oldest[0], ...bySort.newest.filter((x) => x.lifecycle.status === 'enacted').slice(0, 2)]) {
  const g = await timed(`get ${a.type}`, () => api.get(a.id));
  check(g.data.id === a.id && JSON.stringify(g.data.voteAggregates) === JSON.stringify(a.voteAggregates), `get ${a.type} ${a.lifecycle.status} matches its list row`);
}
const unknownId = encodeGovActionId(randomBytes(32).toString('hex'), 0);
await expectCode('NOT_FOUND', 'unknown-but-valid id is NOT_FOUND', () => api.get(unknownId));
await expectCode('INVALID_INPUT', 'txHash#index is INVALID_INPUT', () => api.get(`${sample.txHash}#${sample.index}`));
await expectCode('INVALID_INPUT', 'CIP-105-style drep id as voterId is INVALID_INPUT', () =>
  api.get(sample.id, { voterId: bech32.encode('drep', bech32.toWords(Buffer.alloc(28, 7)), 1023) }));
const sync = (() => {
  try {
    const p = api.get(42);
    return p instanceof Promise ? (p.catch(() => {}), 'promise') : 'value';
  } catch {
    return 'threw';
  }
})();
check(sync === 'promise', 'bad input rejects, never throws synchronously', sync);

/* -- aggregates sanity ---------------------------------------------------- */
console.log('\naggregates');
let aggBad = 0;
for (const a of bySort.newest) {
  for (const v of a.voteAggregates) {
    const sum = BigInt(v.yes) + BigInt(v.no) + BigInt(v.abstain) + BigInt(v.notVoted);
    if (sum !== BigInt(v.totalEligible) || v.threshold.denominator === 0 || BigInt(v.notVoted) < 0n) aggBad++;
    if ((v.role === 'cc') !== (v.representation === 'count')) aggBad++;
  }
  const roles = a.voteAggregates.map((v) => v.role).join();
  const expect = {
    NoConfidence: ['drep,spo'],
    UpdateCommittee: ['drep,spo'],
    NewConstitution: ['drep,cc', 'drep'],
    HardForkInitiation: ['drep,spo,cc', 'drep,spo'],
    TreasuryWithdrawals: ['drep,cc', 'drep'],
    InfoAction: ['drep,spo,cc', 'drep,spo'],
    ParameterChange: ['drep,cc', 'drep,spo,cc', 'drep', 'drep,spo'],
  }[a.type];
  if (!expect.includes(roles)) aggBad++;
}
check(aggBad === 0, `every aggregate balances, roles fit the type (${bySort.newest.length} actions)`, aggBad);

/* -- getEnacted ----------------------------------------------------------- */
console.log('\ngetEnacted');
const lineageTypes = { pparamUpdate: ['ParameterChange'], hardFork: ['HardForkInitiation'], committee: ['NewCommittee', 'NoConfidence'], constitution: ['NewConstitution'] };
for (const [lineage, types] of Object.entries(lineageTypes)) {
  const r = await timed(`getEnacted ${lineage}`, () => api.getEnacted(lineage));
  const [row] = await q(`SELECT encode(t.hash,'hex') h, g.index i FROM gov_action_proposal g JOIN tx t ON t.id = g.tx_id
    WHERE g.type::text = ANY($1) AND g.enacted_epoch IS NOT NULL ORDER BY g.enacted_epoch DESC, g.id DESC LIMIT 1`, [types]);
  const expected = row ? `${row.h}#${row.i}` : null;
  const got = r.data ? `${r.data.txHash}#${r.data.index}` : null;
  check(got === expected, `${lineage}: ${r.data?.id ?? 'null (genesis)'}`);
  if (r.data) {
    const back = await api.get(r.data.id);
    check(back.data.lifecycle.status === 'enacted', `${lineage}: head is enacted`);
    // No enacted action of the lineage names the head as its predecessor.
    const child = bySort.newest.find((a) => a.previousAction?.id === r.data.id && a.lifecycle.status === 'enacted');
    check(!child, `${lineage}: head has no enacted successor`);
  }
}
const committeeHead = (await api.getEnacted('committee')).data;
// Shared lineage: some enacted committee-lineage action is named as the
// predecessor by BOTH NoConfidence and UpdateCommittee proposals, and the
// head returned for the lineage is reachable from it through enacted links.
const enactedCommittee = bySort.oldest.filter((a) => ['NoConfidence', 'UpdateCommittee'].includes(a.type) && a.lifecycle.status === 'enacted');
const namedBy = (head, type) => bySort.newest.filter((a) => a.type === type && a.previousAction?.id === head.id).length;
const shared = enactedCommittee.map((h) => [h.id, namedBy(h, 'NoConfidence'), namedBy(h, 'UpdateCommittee')]).filter(([, n, u]) => n > 0 && u > 0);
console.log(`  info enacted committee-lineage actions named by both types: ${shared.map(([id, n, u]) => `${id.slice(0, 22)}… (NoConfidence ${n}, UpdateCommittee ${u})`).join('; ') || 'none'}`);
check(shared.length > 0, 'NoConfidence and UpdateCommittee proposals chain onto the same enacted committee actions');
const chain = [];
for (let cur = committeeHead && bySort.newest.find((a) => a.id === committeeHead.id); cur; cur = cur.previousAction && bySort.newest.find((a) => a.id === cur.previousAction.id)) chain.push(cur.id);
check(shared.every(([id]) => chain.includes(id)), `committee head's enacted ancestry (${chain.length} deep) contains every shared predecessor`);
const everNoConfidence = bySort.newest.some((a) => a.type === 'NoConfidence' && a.lifecycle.status === 'enacted');
console.log(`  info a NoConfidence has ${everNoConfidence ? '' : 'never '}been enacted here; a per-type lookup would return ${everNoConfidence ? 'a different head' : 'null'} for it`);

/* -- votes, myVote, voter context ---------------------------------------- */
console.log('\nvotes and voter context');
const [busy] = await q(`SELECT g.id, encode(t.hash,'hex') h, g.index i, count(DISTINCT coalesce(vp.drep_voter, vp.pool_voter, vp.committee_voter)::text || vp.voter_role::text)::int n
  FROM voting_procedure vp JOIN gov_action_proposal g ON g.id = vp.gov_action_proposal_id JOIN tx t ON t.id = g.tx_id
  WHERE vp.invalid IS NULL GROUP BY 1,2,3 ORDER BY n DESC LIMIT 1`);
const busyId = encodeGovActionId(busy.h, Number(busy.i));
const votes = [];
for (let page = 1; ; page++) {
  const r = await timed(`listVotes p${page}`, () => api.listVotes(busyId, { page, size: 7 }));
  votes.push(...r.data.elements);
  if (page === 1) check(r.data.total === busy.n, `listVotes total = distinct voters (${busy.n})`, r.data.total);
  if (r.data.elements.length < 7) break;
}
check(votes.length === busy.n, 'listVotes pages concatenate to total');
const voterKey = (v) => (v.voter.role === 'cc' ? v.voter.hot : v.voter.id);
check(new Set(votes.map(voterKey)).size === votes.length, 'one row per voter');
const ccVotes = votes.filter((v) => v.voter.role === 'cc');
check(ccVotes.every((v) => v.voter.cold), `committee votes carry the cold credential (${ccVotes.length})`);
for (const role of ['drep', 'spo', 'cc']) {
  const v = votes.find((x) => x.voter.role === role);
  if (!v) continue;
  const voterId = role === 'cc' ? v.voter.cold : v.voter.id;
  const g = await timed(`get myVote ${role}`, () => api.get(busyId, { voterId }));
  check(g.data.myVote?.choice === v.choice && g.data.myVote?.txRef.txHash === v.txRef.txHash, `myVote for a real ${role} voter (${voterId.slice(0, 20)}…)`);
  if (role === 'cc') {
    const h = await api.get(busyId, { voterId: v.voter.hot });
    check(h.data.myVote?.choice === v.choice, 'myVote by the hot credential too');
  }
}
const drepVoter = votes.find((x) => x.voter.role === 'drep');
if (drepVoter) {
  const other = bySort.newest.find((a) => a.id !== busyId);
  const [{ n: votedOn }] = await q(`SELECT count(DISTINCT vp.gov_action_proposal_id)::int n FROM voting_procedure vp JOIN drep_hash dh ON dh.id = vp.drep_voter
    WHERE vp.invalid IS NULL AND dh.raw = decode($1,'hex')`, [(await import('../dist/ids.js')).decodeDRepId(drepVoter.voter.id).hash]);
  const yes = await timed('list voted=true', () => api.list({ page: 1, size: 1000, voterId: drepVoter.voter.id, voted: true }));
  const no = await timed('list voted=false', () => api.list({ page: 1, size: 5, voterId: drepVoter.voter.id, voted: false }));
  check(yes.data.total === votedOn && no.data.total === dbTotal - votedOn, `voted filter partitions (${votedOn} voted / ${no.data.total} not)`);
  check(yes.data.elements.every((a) => a.myVote && a.myVote.voter.id === drepVoter.voter.id), 'voted=true rows are annotated with myVote');
  check(no.data.elements.every((a) => a.myVote === null), 'voted=false rows carry myVote: null');
  const none = await api.get(other.id, { voterId: drepVoter.voter.id });
  check('myVote' in none.data, 'myVote present on get with voterId (null when not voted)');
}
await expectCode('INVALID_INPUT', 'voted without voterId is INVALID_INPUT', () => api.list({ page: 1, size: 5, voted: true }));

/* -- listActivity --------------------------------------------------------- */
console.log('\nlistActivity');
for (const status of ['live', 'enacted', 'expired', 'dropped']) {
  const a = bySort.newest.find((x) => x.lifecycle.status === status);
  if (!a) continue;
  const r = await api.listActivity(a.id, { page: 1, size: 10 });
  const seq = r.data.elements.map((e) => e.status).join('>');
  const ok = { live: 'live', enacted: 'live>ratified>enacted', expired: 'live>expired', dropped: 'live>dropped' }[status] === seq;
  check(ok, `${status}: ${seq}`);
}

/* -- Koios cross-check ---------------------------------------------------- */
const mismatches = [];
if (!skipKoios) {
  console.log(`\nKoios cross-check (${koiosBase})`);
  const koiosFetch = async (path) => {
    for (let attempt = 0; attempt < 4; attempt++) {
      const r = await fetch(`${koiosBase}${path}`, { headers: { accept: 'application/json' } });
      if (r.ok) return r.json();
      await new Promise((ok) => setTimeout(ok, 1500 * (attempt + 1)));
    }
    throw new Error(`Koios ${path} failed`);
  };
  const koiosList = [];
  for (let offset = 0; ; offset += 1000) {
    const page = await koiosFetch(`/proposal_list?select=proposal_id,proposal_type,proposed_epoch,ratified_epoch,enacted_epoch,dropped_epoch,expired_epoch,expiration&offset=${offset}&limit=1000`);
    koiosList.push(...page);
    if (page.length < 1000) break;
  }
  const koios = new Map(koiosList.map((k) => [k.proposal_id, k]));
  check(koios.size === dbTotal, `Koios lists the same number of actions (${koios.size} vs ${dbTotal})`);
  let epochBad = 0;
  for (const a of bySort.newest) {
    const k = koios.get(a.id);
    if (!k) { mismatches.push({ id: a.id, field: 'missing in Koios' }); epochBad++; continue; }
    const L = a.lifecycle;
    const pairs = [
      ['type', a.type === 'UpdateCommittee' ? 'NewCommittee' : a.type, k.proposal_type],
      ['submitted', L.submitted.epoch, k.proposed_epoch],
      ['ratified', L.ratifiedAt?.epoch ?? null, k.ratified_epoch],
      ['enacted', L.enactedAt?.epoch ?? null, k.enacted_epoch],
      ['expired', L.expiredAt?.epoch ?? null, k.expired_epoch],
      // Koios passes db-sync's dropped_epoch through, which is also set on expired actions.
      ['dropped', L.droppedAt?.epoch ?? null, L.status === 'expired' ? null : k.dropped_epoch],
      ['expires', L.expires?.epoch ?? null, k.expiration],
    ];
    for (const [field, ours, theirs] of pairs) if (ours !== theirs) { mismatches.push({ id: a.id, field, ours, theirs }); epochBad++; }
  }
  check(epochBad === 0, `status epochs and types agree for all ${bySort.newest.length} actions`, epochBad);

  // Aggregates: every live and enacted action plus a spread of the rest.
  const pick = [
    ...bySort.newest.filter((a) => ['live', 'ratified', 'enacted'].includes(a.lifecycle.status)),
    ...bySort.newest.filter((a) => a.lifecycle.status === 'dropped').slice(0, 5),
  ];
  const rest = bySort.newest.filter((a) => a.lifecycle.status === 'expired' && a.voteAggregates.some((v) => v.yes !== '0'));
  for (let i = 0; pick.length < koiosSample && i < rest.length; i += Math.max(1, Math.floor(rest.length / koiosSample))) pick.push(rest[i]);
  const tally = { compared: 0, agree: 0 };
  for (const a of pick) {
    const [s] = await koiosFetch(`/proposal_voting_summary?_proposal_id=${a.id}`);
    if (!s) { mismatches.push({ id: a.id, field: 'no Koios summary' }); continue; }
    const agg = Object.fromEntries(a.voteAggregates.map((v) => [v.role, v]));
    const cmps = [];
    if (agg.drep) {
      const d = agg.drep;
      cmps.push(['drep.yes', d.yes, s.drep_yes_vote_power]);
      cmps.push(['drep.no+notVoted', (BigInt(d.no) + BigInt(d.notVoted)).toString(), s.drep_no_vote_power]);
      cmps.push(['drep.abstain', d.abstain, s.drep_active_abstain_vote_power]);
    }
    if (agg.spo) {
      const p = agg.spo;
      cmps.push(['spo.yes', p.yes, s.pool_yes_vote_power]);
      cmps.push(['spo.no+notVoted', (BigInt(p.no) + BigInt(p.notVoted)).toString(), s.pool_no_vote_power]);
      cmps.push(['spo.abstain', p.abstain, (BigInt(s.pool_active_abstain_vote_power) + BigInt(s.pool_passive_always_abstain_vote_power)).toString()]);
    }
    if (agg.cc) {
      cmps.push(['cc.yes', agg.cc.yes, String(s.committee_yes_votes_cast)]);
      cmps.push(['cc.abstain', agg.cc.abstain, String(s.committee_abstain_votes_cast)]);
    }
    cmps.push(['tally epoch', null, null]);
    for (const [field, ours, theirs] of cmps) {
      if (field === 'tally epoch') continue;
      tally.compared++;
      if (ours === theirs) tally.agree++;
      else mismatches.push({ id: a.id, type: a.type, status: a.lifecycle.status, epoch: s.epoch_no, field, ours, theirs, diff: (BigInt(ours) - BigInt(theirs)).toString() });
    }
  }
  console.log(`  info aggregates: ${tally.agree}/${tally.compared} figures agree across ${pick.length} actions`);
  const byField = {};
  for (const m of mismatches) byField[m.field] = (byField[m.field] ?? 0) + 1;
  console.log('  info mismatches by field:', JSON.stringify(byField));
  for (const m of mismatches.slice(0, 40)) console.log('   ', JSON.stringify(m));
}

/* -- summary -------------------------------------------------------------- */
timings.sort((a, b) => b[1] - a[1]);
console.log('\nslowest calls:');
for (const [label, ms] of timings.slice(0, 8)) console.log(`  ${String(ms).padStart(6)} ms  ${label}`);
check(timings[0][1] < 30_000, 'every call well within 30 s', timings[0]);
console.log(`\n${failures === 0 ? 'PASS' : `FAIL (${failures})`}; ${mismatches.length} Koios mismatches (reported, not failed)`);
await close();
await sql.end();
process.exit(failures === 0 ? 0 : 1);
