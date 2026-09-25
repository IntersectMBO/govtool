#!/usr/bin/env node
/**
 * Live conformance for CommitteeApi and PoolsApi against a real db-sync.
 *
 *   set -a; . ../../tests/test-infrastructure/.env; set +a
 *   npm run build && node scripts/live-committee-pools.mjs
 *
 * Reads DBSYNC_POSTGRES_HOST/PORT/USER/PASSWORD and DBSYNC_DATABASE, plus
 * NETWORK (default preview) and KOIOS_URL (default the public Koios for that
 * network; set KOIOS_URL=off to skip the cross-check). Read-only.
 *
 * What it checks:
 *   - contract invariants on every method, with real ids
 *   - committee membership three ways: the provider, an independent replay of
 *     genesis plus every enacted UpdateCommittee delta from the action bodies,
 *     and db-sync's own epoch_state.committee_id
 *   - the constitution against epoch_state.constitution_id
 *   - paging consistency over the whole pool listing
 *   - committee, pools, voting power and pool votes against Koios
 *   - every call finishes within 30 s
 */
import assert from 'node:assert/strict';
import pg from 'pg';

import { createDbSyncProvider } from '../dist/index.js';
import { decodeCommitteeColdId, decodeCommitteeHotId, decodeGovActionId, decodePoolId, encodeCommitteeColdId, encodePoolId } from '../dist/ids.js';

const env = process.env;
const network = env.NETWORK ?? 'preview';
const koiosBase = env.KOIOS_URL ?? (network === 'mainnet' ? 'https://api.koios.rest/api/v1' : `https://${network}.koios.rest/api/v1`);
const connection = {
  host: env.DBSYNC_POSTGRES_HOST,
  port: Number(env.DBSYNC_POSTGRES_PORT ?? 5432),
  user: env.DBSYNC_POSTGRES_USER,
  password: env.DBSYNC_POSTGRES_PASSWORD,
  database: env.DBSYNC_DATABASE,
};
if (!connection.host || !connection.database) {
  console.error('Set DBSYNC_POSTGRES_HOST, DBSYNC_DATABASE and the rest (see the header).');
  process.exit(2);
}

const BUDGET_MS = 30_000;
const { chainData, close } = createDbSyncProvider({ network, connection });
const { committee, pools } = chainData.governance;
const raw = new pg.Pool({ ...connection, max: 2 });
const sql = async (text, params = []) => (await raw.query(text, params)).rows;

let failures = 0;
const mismatches = [];
const timings = [];

async function timed(label, fn) {
  const started = Date.now();
  const result = await fn();
  const ms = Date.now() - started;
  timings.push([label, ms]);
  if (ms > BUDGET_MS) throw new Error(`${label} took ${ms} ms, over the ${BUDGET_MS} ms budget`);
  return result;
}

async function check(name, fn) {
  try {
    await fn();
    console.log(`ok    ${name}`);
  } catch (error) {
    failures++;
    console.log(`FAIL  ${name}\n      ${error?.stack ?? error}`);
  }
}

/** A method must return a promise that rejects with `code`, never throw synchronously. */
async function rejectsWith(code, call) {
  let promise;
  try {
    promise = call();
  } catch (error) {
    throw new Error(`threw synchronously instead of rejecting: ${error?.message}`);
  }
  assert.ok(promise && typeof promise.then === 'function', 'did not return a promise');
  await assert.rejects(promise, (error) => {
    assert.equal(error?.code, code, `expected ${code}, got ${error?.code}: ${error?.message}`);
    return true;
  });
}

async function koios(path, body) {
  if (koiosBase === 'off') return undefined;
  const response = await fetch(`${koiosBase}${path}`, {
    method: body ? 'POST' : 'GET',
    headers: { accept: 'application/json', ...(body ? { 'content-type': 'application/json' } : {}) },
    ...(body ? { body: JSON.stringify(body) } : {}),
  });
  if (!response.ok) throw new Error(`Koios ${path} answered ${response.status}`);
  return response.json();
}

function mismatch(what, ours, theirs) {
  mismatches.push({ what, ours, theirs });
}

const ratioEq = (a, b) => a.numerator * b.denominator === b.numerator * a.denominator;

/* ------------------------------------------------------------------------- */
/* Committee                                                                  */
/* ------------------------------------------------------------------------- */

let current;
await check('getCommittee: contract invariants', async () => {
  current = (await timed('committee.getCommittee', () => committee.getCommittee())).data;
  assert.ok(Array.isArray(current.members));
  assert.ok(Number.isInteger(current.quorum.numerator) && Number.isInteger(current.quorum.denominator));
  assert.ok(current.quorum.denominator > 0, 'quorum denominator is 0');
  assert.equal(typeof current.isDissolved, 'boolean');
  if (current.isDissolved) assert.equal(current.members.length, 0);
  if (current.enactedBy !== null) {
    const parts = decodeGovActionId(current.enactedBy.id);
    assert.equal(parts.txHash, current.enactedBy.txHash);
    assert.equal(parts.index, current.enactedBy.index);
  }
  const seen = new Set();
  for (const m of current.members) {
    assert.equal(m.role, 'cc');
    const cold = decodeCommitteeColdId(m.coldCredential);
    assert.equal(m.isScriptBased, cold.isScript, 'isScriptBased disagrees with the CIP-129 header');
    assert.ok(!seen.has(m.coldCredential), `duplicate member ${m.coldCredential}`);
    seen.add(m.coldCredential);
    if (m.hotCredential !== null) decodeCommitteeHotId(m.hotCredential);
    if (m.hasResigned) assert.equal(m.hotCredential, null, 'a resigned member carries a hot credential');
    assert.ok(m.termExpiryEpoch === null || Number.isInteger(m.termExpiryEpoch));
    assert.ok(m.termStartEpoch === null || Number.isInteger(m.termStartEpoch));
    if (m.termStartEpoch !== null && m.termExpiryEpoch !== null) assert.ok(m.termStartEpoch <= m.termExpiryEpoch);
  }
});

/** Replay genesis plus every enacted committee-lineage action from the action bodies alone. */
async function replayCommittee() {
  const [genesis] = await sql(`SELECT id, quorum_numerator::int AS n, quorum_denominator::int AS d FROM committee WHERE gov_action_proposal_id IS NULL`);
  const genesisMembers = await sql(
    `SELECT encode(ch.raw, 'hex') AS hash, ch.has_script, cm.expiration_epoch::int AS expiry
       FROM committee_member cm JOIN committee_hash ch ON ch.id = cm.committee_hash_id WHERE cm.committee_id = $1`,
    [genesis.id],
  );
  const enacted = await sql(
    `SELECT g.id::text, g.prev_gov_action_proposal::text AS prev, g.type::text, g.description, g.enacted_epoch
       FROM gov_action_proposal g
      WHERE g.type IN ('NewCommittee', 'NoConfidence') AND g.enacted_epoch IS NOT NULL`,
  );
  const ordered = [];
  let prev = null;
  for (;;) {
    const next = enacted.filter((row) => row.prev === prev);
    if (next.length === 0) break;
    assert.equal(next.length, 1, 'forked committee lineage');
    ordered.push(next[0]);
    prev = next[0].id;
  }
  assert.equal(ordered.length, enacted.length, 'enacted committee actions off the chain');

  const key = (isScript, hash) => `${isScript ? 's' : 'k'}:${hash}`;
  let members = new Map(genesisMembers.map((m) => [key(m.has_script, m.hash), m.expiry]));
  let quorum = { numerator: genesis.n, denominator: genesis.d };
  let dissolved = false;
  for (const action of ordered) {
    if (action.type === 'NoConfidence') {
      members = new Map();
      dissolved = true;
      continue;
    }
    const [, removed, added, q] = action.description.contents;
    for (const cred of removed) {
      const isScript = 'scriptHash' in cred;
      members.delete(key(isScript, isScript ? cred.scriptHash : cred.keyHash));
    }
    for (const [cred, expiry] of Object.entries(added)) {
      const [, kind, hash] = /^(scriptHash|keyHash)-([0-9a-f]{56})$/.exec(cred);
      members.set(key(kind === 'scriptHash', hash), expiry);
    }
    quorum = typeof q === 'number' ? null : { numerator: q.numerator, denominator: q.denominator };
    dissolved = false;
  }
  return { members, quorum, dissolved, head: ordered.at(-1) ?? null };
}

await check('getCommittee: membership equals a replay of genesis plus every enacted delta', async () => {
  const replay = await replayCommittee();
  const ours = new Map(
    current.members.map((m) => {
      const c = decodeCommitteeColdId(m.coldCredential);
      return [`${c.isScript ? 's' : 'k'}:${c.hash}`, m.termExpiryEpoch];
    }),
  );
  assert.deepEqual([...ours.entries()].sort(), [...replay.members.entries()].sort());
  if (replay.quorum) assert.ok(ratioEq(current.quorum, replay.quorum), 'quorum differs from the replay');
  assert.equal(current.isDissolved, replay.dissolved);
  console.log(`      replay: ${replay.members.size} members, quorum ${current.quorum.numerator}/${current.quorum.denominator}`);
});

await check('getCommittee: membership equals epoch_state.committee_id at the latest epoch', async () => {
  const [state] = await sql(`SELECT committee_id, no_confidence_id FROM epoch_state ORDER BY epoch_no DESC LIMIT 1`);
  if (!state) return console.log('      epoch_state is empty (db-sync without ledger state); skipped');
  if (state.committee_id === null) return assert.equal(current.isDissolved, true);
  const rows = await sql(
    `SELECT encode(ch.raw, 'hex') AS hash, ch.has_script, cm.expiration_epoch::int AS expiry, c.quorum_numerator::int AS n, c.quorum_denominator::int AS d
       FROM committee c JOIN committee_member cm ON cm.committee_id = c.id JOIN committee_hash ch ON ch.id = cm.committee_hash_id
      WHERE c.id = $1`,
    [state.committee_id],
  );
  const theirs = rows.map((r) => [encodeCommitteeColdId(r.hash, r.has_script), r.expiry]).sort();
  const ours = current.members.map((m) => [m.coldCredential, m.termExpiryEpoch]).sort();
  assert.deepEqual(ours, theirs);
  if (rows[0]) assert.ok(ratioEq(current.quorum, { numerator: rows[0].n, denominator: rows[0].d }));
});

await check('getCommittee: hot credentials equal the latest certificate per cold credential', async () => {
  const certs = await sql(
    `SELECT encode(c.raw, 'hex') AS cold, c.has_script, encode(h.raw, 'hex') AS hot, h.has_script AS hot_script, x.resign
       FROM (SELECT cold_key_id, hot_key_id, tx_id, cert_index, false AS resign FROM committee_registration
             UNION ALL SELECT cold_key_id, NULL, tx_id, cert_index, true FROM committee_de_registration) x
       JOIN committee_hash c ON c.id = x.cold_key_id LEFT JOIN committee_hash h ON h.id = x.hot_key_id
      ORDER BY x.tx_id, x.cert_index`,
  );
  const latest = new Map();
  for (const cert of certs) latest.set(encodeCommitteeColdId(cert.cold, cert.has_script), cert);
  for (const m of current.members) {
    const cert = latest.get(m.coldCredential);
    if (!cert) assert.equal(m.hotCredential, null);
    else assert.equal(m.hasResigned, cert.resign);
  }
});

await check('getCommittee: Koios committee_info agrees', async () => {
  const info = await koios('/committee_info');
  if (!info) return console.log('      Koios skipped');
  const k = info[0];
  const kq = { numerator: k.quorum_numerator, denominator: k.quorum_denominator };
  if (!ratioEq(kq, current.quorum)) mismatch('committee quorum', current.quorum, kq);
  if ((current.enactedBy?.id ?? null) !== (k.proposal_id ?? null)) mismatch('committee enactedBy', current.enactedBy?.id, k.proposal_id);
  const kMembers = new Map(k.members.map((m) => [m.cc_cold_id, m]));
  for (const m of current.members) {
    const km = kMembers.get(m.coldCredential);
    if (!km) {
      mismatch('committee member missing from Koios', m.coldCredential, null);
      continue;
    }
    if ((km.cc_hot_id ?? null) !== m.hotCredential) mismatch(`hot of ${m.coldCredential}`, m.hotCredential, km.cc_hot_id);
    if (km.expiration_epoch !== m.termExpiryEpoch) mismatch(`expiry of ${m.coldCredential}`, m.termExpiryEpoch, km.expiration_epoch);
    if ((km.status === 'resigned') !== m.hasResigned) mismatch(`resigned of ${m.coldCredential}`, m.hasResigned, km.status);
    kMembers.delete(m.coldCredential);
  }
  for (const id of kMembers.keys()) mismatch('Koios committee member missing from provider', null, id);
  console.log(`      Koios: ${k.members.length} members, quorum ${kq.numerator}/${kq.denominator}, proposal ${k.proposal_id}`);
});

await check('getMember: every current member, byte-identical to getCommittee', async () => {
  for (const m of current.members) {
    const got = (await timed('committee.getMember', () => committee.getMember(m.coldCredential))).data;
    assert.deepEqual(got, m);
  }
});

await check('getMember: a credential outside the current committee is NOT_FOUND', async () => {
  const outside = (await sql(`SELECT encode(raw, 'hex') AS hash, has_script FROM committee_hash`))
    .map((r) => encodeCommitteeColdId(r.hash, r.has_script))
    .filter((id) => !current.members.some((m) => m.coldCredential === id));
  if (outside.length === 0) return console.log('      no non-member credential on this network; skipped');
  await rejectsWith('NOT_FOUND', () => committee.getMember(outside[0]));
});

await check('getMember: non-CIP-129 input is INVALID_INPUT, as a rejection', async () => {
  await rejectsWith('INVALID_INPUT', () => committee.getMember('not-an-id'));
  await rejectsWith('INVALID_INPUT', () => committee.getMember(''));
  const hot = current.members.find((m) => m.hotCredential)?.hotCredential;
  if (hot) await rejectsWith('INVALID_INPUT', () => committee.getMember(hot));
  // A CIP-105 style cold id: the bare 28-byte hash with no CIP-129 header.
  const { bech32 } = await import('bech32');
  const bare = bech32.encode('cc_cold', bech32.toWords(Buffer.alloc(28, 7)), 1023);
  await rejectsWith('INVALID_INPUT', () => committee.getMember(bare));
});

/* ------------------------------------------------------------------------- */
/* Constitution                                                               */
/* ------------------------------------------------------------------------- */

await check('getConstitution: invariants and epoch_state agreement', async () => {
  const c = (await timed('committee.getConstitution', () => committee.getConstitution())).data;
  assert.equal(typeof c.anchor.url, 'string');
  assert.match(c.anchor.dataHash, /^[0-9a-f]{64}$/);
  assert.ok(c.guardrailsScriptHash === null || /^[0-9a-f]{56}$/.test(c.guardrailsScriptHash));
  assert.equal(c.enactedBy === null, c.enactedAt === null);
  if (c.enactedBy) decodeGovActionId(c.enactedBy.id);
  const [state] = await sql(
    `SELECT va.url, encode(va.data_hash, 'hex') AS hash, encode(k.script_hash, 'hex') AS script
       FROM epoch_state es JOIN constitution k ON k.id = es.constitution_id JOIN voting_anchor va ON va.id = k.voting_anchor_id
      ORDER BY es.epoch_no DESC LIMIT 1`,
  );
  if (state) {
    assert.equal(c.anchor.url, state.url);
    assert.equal(c.anchor.dataHash, state.hash);
    assert.equal(c.guardrailsScriptHash, state.script);
  }
  const enacted = await koios(`/proposal_list?proposal_type=eq.NewConstitution&enacted_epoch=not.is.null&select=proposal_id,enacted_epoch&order=enacted_epoch.desc`);
  if (enacted) {
    const koiosHead = enacted[0]?.proposal_id ?? null;
    if (koiosHead !== (c.enactedBy?.id ?? null)) mismatch('constitution enactedBy', c.enactedBy?.id ?? null, koiosHead);
  }
  console.log(`      ${c.anchor.url} ${c.anchor.dataHash} guardrails ${c.guardrailsScriptHash} enactedBy ${c.enactedBy?.id ?? 'genesis'}`);
});

/* ------------------------------------------------------------------------- */
/* Pools                                                                      */
/* ------------------------------------------------------------------------- */

function assertSpoVoter(p) {
  assert.equal(p.role, 'spo');
  assert.equal(p.id, p.poolId);
  assert.equal(encodePoolId(decodePoolId(p.poolId)), p.poolId);
  assert.ok(!('ticker' in p) && !('name' in p), 'pool metadata leaked into chain data');
  assert.ok(p.anchor === null || (typeof p.anchor.url === 'string' && /^[0-9a-f]{64}$/.test(p.anchor.dataHash)));
  if (p.votingPower !== null) {
    assert.match(p.votingPower.amount, /^\d+$/);
    assert.equal(p.votingPower.basis, 'active');
    assert.ok(Number.isInteger(p.votingPower.epoch));
  }
  for (const field of ['activeStake', 'liveStake', 'pledge']) if (field in p) assert.match(p[field], /^\d+$/);
}

const all = [];
let listTotal;
await check('pools.list: page through everything; totals, uniqueness, order', async () => {
  const size = 250;
  for (let page = 1; ; page++) {
    const { data } = await timed('pools.list', () => pools.list({ page, size }));
    assert.equal(typeof data.total, 'number');
    listTotal ??= data.total;
    assert.equal(data.total, listTotal, 'total moved between pages');
    data.elements.forEach(assertSpoVoter);
    all.push(...data.elements);
    if (data.elements.length < size) break;
  }
  assert.equal(all.length, listTotal);
  assert.equal(new Set(all.map((p) => p.id)).size, all.length, 'a pool appears twice');
  for (let i = 1; i < all.length; i++) {
    const a = all[i - 1].votingPower ? BigInt(all[i - 1].votingPower.amount) : -1n;
    const b = all[i].votingPower ? BigInt(all[i].votingPower.amount) : -1n;
    assert.ok(a >= b, `not ordered by voting power at ${i}`);
  }
  const past = await pools.list({ page: Math.ceil(listTotal / size) + 1, size });
  assert.equal(past.data.elements.length, 0);
  assert.equal(past.data.total, listTotal, 'an empty page past the end lost the total');
  console.log(`      ${listTotal} registered pools; ${all.filter((p) => p.votingPower).length} with voting power`);
});

await check('pools.list: paging and validation', async () => {
  const p1 = (await pools.list({ page: 1, size: 7 })).data.elements.map((p) => p.id);
  const p2 = (await pools.list({ page: 2, size: 7 })).data.elements.map((p) => p.id);
  assert.deepEqual([...p1, ...p2], all.slice(0, 14).map((p) => p.id));
  await rejectsWith('INVALID_INPUT', () => pools.list({ page: 0, size: 10 }));
  await rejectsWith('INVALID_INPUT', () => pools.list({ page: 1, size: 0 }));
  await rejectsWith('INVALID_INPUT', () => pools.list({ page: 1.5, size: 10 }));
});

await check('pools.list: exactId search', async () => {
  const target = all[Math.floor(all.length / 2)];
  const hit = (await pools.list({ page: 1, size: 10, search: target.id })).data;
  assert.equal(hit.total, 1);
  assert.deepEqual(hit.elements[0], target);
  const miss = (await pools.list({ page: 1, size: 10, search: 'SOMETICKER' })).data;
  assert.deepEqual(miss, { elements: [], total: 0 });
  const blank = (await pools.list({ page: 1, size: 5, search: '  ' })).data;
  assert.equal(blank.total, listTotal);
});

await check('pools.get: equals the listing row; errors reject', async () => {
  for (const p of [all[0], all[1], all[Math.floor(all.length / 2)], all.at(-1)]) {
    const got = (await timed('pools.get', () => pools.get(p.id))).data;
    assert.deepEqual(got, p);
  }
  await rejectsWith('INVALID_INPUT', () => pools.get('pool1notvalid'));
  await rejectsWith('INVALID_INPUT', () => pools.get('drep1ygqq'));
  await rejectsWith('NOT_FOUND', () => pools.get(encodePoolId('00'.repeat(28))));
});

await check('pools.get: a retired pool answers but is not listed', async () => {
  const listed = new Set(all.map((p) => p.id));
  const [retired] = await sql(
    `SELECT encode(ph.hash_raw, 'hex') AS hash FROM pool_hash ph JOIN pool_retire pr ON pr.hash_id = ph.id
      WHERE pr.retiring_epoch < (SELECT max(epoch_no) FROM block) - 5
        AND NOT EXISTS (SELECT 1 FROM pool_update pu WHERE pu.hash_id = ph.id AND pu.registered_tx_id > pr.announced_tx_id)
      LIMIT 1`,
  );
  if (!retired) return console.log('      no retired pool; skipped');
  const id = encodePoolId(retired.hash);
  assert.ok(!listed.has(id));
  const got = (await pools.get(id)).data;
  assertSpoVoter(got);
  assert.equal(got.votingPower, null, 'a retired pool carries voting power');
  const search = (await pools.list({ page: 1, size: 5, search: id })).data;
  assert.equal(search.total, 0);
});

await check('pools: Koios registered set, voting power, active stake, pledge, anchor', async () => {
  const kList = [];
  if (koiosBase === 'off') return console.log('      Koios skipped');
  for (let offset = 0; ; offset += 1000) {
    const page = await koios(`/pool_list?pool_status=in.(registered,retiring)&select=pool_id_bech32,pool_status&offset=${offset}&limit=1000`);
    kList.push(...page);
    if (page.length < 1000) break;
  }
  const ours = new Set(all.map((p) => p.id));
  const theirs = new Set(kList.map((p) => p.pool_id_bech32));
  const onlyKoios = [...theirs].filter((id) => !ours.has(id));
  const onlyOurs = [...ours].filter((id) => !theirs.has(id));
  if (onlyOurs.length) mismatch('registered pools only the provider lists', onlyOurs.length, onlyOurs.slice(0, 5));
  if (onlyKoios.length) {
    // Explain each: a pool registered and retired in one transaction, whose
    // retirement has passed, is retired in the ledger (it is absent from the
    // current stake snapshot) but Koios compares transactions, not certificates.
    const explained = await sql(
      `SELECT ph.view FROM pool_hash ph
         JOIN LATERAL (SELECT registered_tx_id, cert_index FROM pool_update WHERE hash_id = ph.id ORDER BY registered_tx_id DESC, cert_index DESC LIMIT 1) lu ON true
         JOIN LATERAL (SELECT announced_tx_id, cert_index, retiring_epoch FROM pool_retire WHERE hash_id = ph.id ORDER BY announced_tx_id DESC, cert_index DESC LIMIT 1) lr ON true
        WHERE ph.view = ANY($1) AND lr.announced_tx_id = lu.registered_tx_id AND lr.cert_index > lu.cert_index
          AND lr.retiring_epoch <= (SELECT max(epoch_no) FROM block)`,
      [onlyKoios],
    );
    const explainedSet = new Set(explained.map((r) => r.view));
    const unexplained = onlyKoios.filter((id) => !explainedSet.has(id));
    mismatch(
      'pools Koios lists as registered that the provider treats as retired',
      `${onlyKoios.length} (${explainedSet.size} registered and retired in one transaction, retirement passed)`,
      onlyKoios.slice(0, 3),
    );
    if (unexplained.length) mismatch('UNEXPLAINED Koios-only pools', unexplained.length, unexplained.slice(0, 5));
    for (const id of onlyKoios.slice(0, 5)) {
      const got = (await pools.get(id)).data;
      assert.equal(got.votingPower, null, `${id} is in the stake snapshot, so it is not retired`);
    }
  }

  const sample = [...all.slice(0, 5), ...all.filter((p) => p.anchor).slice(0, 5), all[Math.floor(all.length / 2)], all.at(-1)];
  const info = await koios('/pool_info', { _pool_bech32_ids: sample.map((p) => p.id) });
  const byId = new Map(info.map((p) => [p.pool_id_bech32, p]));
  for (const p of sample) {
    const k = byId.get(p.id);
    if (!k) {
      mismatch('pool missing from Koios pool_info', p.id, null);
      continue;
    }
    if ((k.active_stake ?? null) !== (p.activeStake ?? null) && !(k.active_stake === null && p.activeStake === '0'))
      mismatch(`activeStake of ${p.id}`, p.activeStake, k.active_stake);
    if (k.pledge !== p.pledge) mismatch(`pledge of ${p.id}`, p.pledge, k.pledge);
    if ((k.meta_url ?? null) !== (p.anchor?.url ?? null)) mismatch(`anchor url of ${p.id}`, p.anchor?.url, k.meta_url);
    if ((k.meta_hash ?? null) !== (p.anchor?.dataHash ?? null)) mismatch(`anchor hash of ${p.id}`, p.anchor?.dataHash, k.meta_hash);
    if (!['registered', 'retiring'].includes(k.pool_status)) mismatch(`status of ${p.id}`, 'registered', k.pool_status);
    if (p.votingPower) {
      const history = await koios(`/pool_voting_power_history?_pool_bech32=${p.id}&_epoch_no=${p.votingPower.epoch}`);
      const amount = history?.[0]?.amount ?? null;
      if (amount !== p.votingPower.amount) mismatch(`voting power of ${p.id} at ${p.votingPower.epoch}`, p.votingPower.amount, amount);
    }
  }
  console.log(`      Koios registered+retiring ${theirs.size}, provider ${ours.size}; sampled ${sample.length} pools`);
});

/* ------------------------------------------------------------------------- */
/* Pool votes                                                                 */
/* ------------------------------------------------------------------------- */

await check('pools.listVotes: invariants, paging, one effective vote per action, Koios', async () => {
  assert.equal(typeof pools.listVotes, 'function');
  const [voter] = await sql(
    `SELECT encode(ph.hash_raw, 'hex') AS hash, count(DISTINCT vp.gov_action_proposal_id)::int AS actions, count(*)::int AS votes
       FROM voting_procedure vp JOIN pool_hash ph ON ph.id = vp.pool_voter
      WHERE vp.voter_role = 'SPO' AND vp.invalid IS NULL
      GROUP BY ph.hash_raw ORDER BY count(*) - count(DISTINCT vp.gov_action_proposal_id) DESC, count(*) DESC LIMIT 1`,
  );
  if (!voter) return console.log('      no SPO votes; skipped');
  const id = encodePoolId(voter.hash);
  const rows = [];
  const size = 3;
  let total;
  for (let page = 1; ; page++) {
    const { data } = await timed('pools.listVotes', () => pools.listVotes(id, { page, size }));
    total ??= data.total;
    assert.equal(data.total, total);
    rows.push(...data.elements);
    if (data.elements.length < size) break;
  }
  assert.equal(total, voter.actions, 'total is not the number of actions voted on');
  assert.equal(rows.length, total);
  const actions = rows.map((r) => r.action.id);
  assert.equal(new Set(actions).size, actions.length, 'two rows for one action');
  for (const r of rows) {
    assert.deepEqual(r.voter, { role: 'spo', id, isScriptBased: false });
    assert.ok(['yes', 'no', 'abstain'].includes(r.choice));
    assert.match(r.txRef.txHash, /^[0-9a-f]{64}$/);
    assert.ok(r.at && Number.isInteger(r.at.epoch) && typeof r.at.time === 'string');
    decodeGovActionId(r.action.id);
  }
  for (let i = 1; i < rows.length; i++) assert.ok(rows[i - 1].at.slot >= rows[i].at.slot, 'not newest first');
  await rejectsWith('INVALID_INPUT', () => pools.listVotes('nope', { page: 1, size: 10 }));
  await rejectsWith('NOT_FOUND', () => pools.listVotes(encodePoolId('00'.repeat(28)), { page: 1, size: 10 }));
  await rejectsWith('INVALID_INPUT', () => pools.listVotes(id, { page: 0, size: 10 }));
  console.log(`      ${id}: ${voter.votes} votes cast on ${voter.actions} actions -> ${rows.length} rows`);

  const kVotes = await koios(`/pool_votes?_pool_bech32=${id}`);
  if (kVotes) {
    // Koios lists every vote cast; reduce to the latest per action to compare.
    const latest = new Map();
    for (const v of [...kVotes].sort((a, b) => a.block_time - b.block_time)) latest.set(`${v.proposal_tx_hash}#${v.proposal_index}`, v);
    if (latest.size !== rows.length) mismatch(`effective vote count of ${id}`, rows.length, latest.size);
    for (const r of rows) {
      const parts = decodeGovActionId(r.action.id);
      const k = latest.get(`${parts.txHash}#${parts.index}`);
      if (!k) mismatch(`vote on ${r.action.id} missing from Koios`, r.choice, null);
      else {
        if (k.vote.toLowerCase() !== r.choice) mismatch(`choice on ${r.action.id}`, r.choice, k.vote);
        if (k.vote_tx_hash !== r.txRef.txHash) mismatch(`vote tx on ${r.action.id}`, r.txRef.txHash, k.vote_tx_hash);
      }
    }
  }
});

/* ------------------------------------------------------------------------- */

await close();
await raw.end();

console.log('\nTimings (ms, slowest per method):');
const slowest = new Map();
for (const [label, ms] of timings) slowest.set(label, Math.max(ms, slowest.get(label) ?? 0));
for (const [label, ms] of slowest) console.log(`  ${label.padEnd(28)} ${ms}`);

console.log(`\nKoios mismatches: ${mismatches.length}`);
for (const m of mismatches) console.log(`  ${m.what}\n    provider: ${JSON.stringify(m.ours)}\n    koios:    ${JSON.stringify(m.theirs)}`);

console.log(failures ? `\n${failures} check(s) failed` : '\nall checks passed');
process.exit(failures ? 1 : 0);
