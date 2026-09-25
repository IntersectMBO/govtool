/**
 * Test doubles: a fake `fetch` that answers Blockfrost-shaped JSON from a
 * route table, a tiny CBOR encoder to build transaction bodies, and fixture
 * builders. Every value is invented. Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { bech32 } from 'bech32';

import { createBlockfrostProvider } from '../dist/index.js';

export const PROJECT_ID = 'test-project-id-not-a-real-key';

/**
 * `routes` maps a path (without /api/v0, query ignored unless the key has one)
 * to a value, a function of (url) returning a value, or `{ status, body, headers }`.
 * Unrouted paths answer 404, as Blockfrost does.
 */
export function fakeFetch(routes) {
  const calls = [];
  const fn = async (input, init) => {
    const url = new URL(String(input));
    calls.push({ url: url.toString(), path: url.pathname.replace(/^\/api\/v0/, ''), headers: init?.headers ?? {} });
    const path = url.pathname.replace(/^\/api\/v0/, '');
    const withQuery = path + url.search;
    let answer = withQuery in routes ? routes[withQuery] : path in routes ? routes[path] : undefined;
    if (typeof answer === 'function') answer = await answer(url);
    if (answer === undefined) {
      return new Response(JSON.stringify({ status_code: 404, error: 'Not Found', message: 'The requested component has not been found.' }), { status: 404 });
    }
    if (answer && typeof answer === 'object' && 'status' in answer && 'body' in answer) {
      return new Response(typeof answer.body === 'string' ? answer.body : JSON.stringify(answer.body), {
        status: answer.status,
        headers: answer.headers ?? {},
      });
    }
    return new Response(JSON.stringify(answer), { status: 200 });
  };
  fn.calls = calls;
  return fn;
}

/** A paged Blockfrost collection: `?page=n` answers the n-th 100 rows. */
export function paged(rows, size = 100) {
  return (url) => {
    const page = Number(url.searchParams.get('page') ?? 1);
    const count = Number(url.searchParams.get('count') ?? size);
    const ordered = url.searchParams.get('order') === 'desc' ? [...rows].reverse() : rows;
    return ordered.slice((page - 1) * count, page * count);
  };
}

export function provider(routes, options = {}) {
  const fetch = fakeFetch(routes);
  const { chainData } = createBlockfrostProvider({
    network: 'mainnet',
    projectId: PROJECT_ID,
    fetch,
    sleep: async () => {},
    rateLimit: null,
    ...options,
  });
  return { chainData, fetch };
}

export async function rejectsWith(promise, code) {
  assert.ok(promise instanceof Promise, 'must return a promise, never throw synchronously');
  let error;
  await assert.rejects(promise, (e) => {
    error = e;
    return e.code === code || assert.fail(`expected ${code}, got ${e.code}: ${e.message}`);
  });
  return error;
}

/* -- identifiers ------------------------------------------------------------ */

const enc = (prefix, bytes) => bech32.encode(prefix, bech32.toWords(Buffer.from(bytes)), 1023);
export const hash28 = (n) => Buffer.alloc(28, n).toString('hex');
export const hash32 = (n) => Buffer.alloc(32, n).toString('hex');
export const drepId = (n, script = false) => enc('drep', [script ? 0x23 : 0x22, ...Buffer.alloc(28, n)]);
export const drepCip105 = (n, script = false) => enc(script ? 'drep_script' : 'drep', Buffer.alloc(28, n));
export const poolId = (n) => enc('pool', Buffer.alloc(28, n));
export const coldId = (n, script = false) => enc('cc_cold', [script ? 0x13 : 0x12, ...Buffer.alloc(28, n)]);
export const hotId = (n, script = false) => enc('cc_hot', [script ? 0x03 : 0x02, ...Buffer.alloc(28, n)]);
export const stakeAddr = (n, script = false) => enc('stake', [script ? 0xf1 : 0xe1, ...Buffer.alloc(28, n)]);
export const govActionId = (tx, i) => enc('gov_action', [...Buffer.from(tx, 'hex'), i]);

/* -- CBOR ------------------------------------------------------------------- */

export class Tag {
  constructor(tag, value) {
    this.tag = tag;
    this.value = value;
  }
}
export class CMap {
  constructor(entries) {
    this.entries = entries;
  }
}
export const bytes = (hex) => Buffer.from(hex, 'hex');

function head(major, n) {
  if (n < 24) return Buffer.from([(major << 5) | n]);
  if (n < 256) return Buffer.from([(major << 5) | 24, n]);
  if (n < 65536) return Buffer.from([(major << 5) | 25, n >> 8, n & 0xff]);
  if (n < 2 ** 32) {
    const b = Buffer.alloc(5);
    b[0] = (major << 5) | 26;
    b.writeUInt32BE(n, 1);
    return b;
  }
  const b = Buffer.alloc(9);
  b[0] = (major << 5) | 27;
  b.writeBigUInt64BE(BigInt(n), 1);
  return b;
}

export function cbor(v) {
  if (v === null) return Buffer.from([0xf6]);
  if (v === true || v === false) return Buffer.from([v ? 0xf5 : 0xf4]);
  if (typeof v === 'number') return v >= 0 ? head(0, v) : head(1, -1 - v);
  if (typeof v === 'string') return Buffer.concat([head(3, Buffer.byteLength(v)), Buffer.from(v)]);
  if (Buffer.isBuffer(v)) return Buffer.concat([head(2, v.length), v]);
  if (Array.isArray(v)) return Buffer.concat([head(4, v.length), ...v.map(cbor)]);
  if (v instanceof CMap) return Buffer.concat([head(5, v.entries.length), ...v.entries.flatMap(([k, x]) => [cbor(k), cbor(x)])]);
  if (v instanceof Tag) return Buffer.concat([head(6, v.tag), cbor(v.value)]);
  throw new Error(`cannot encode ${v}`);
}

/**
 * A transaction `[body, witnesses, valid, aux]` with the given voting
 * procedures, proposal procedures and certificates.
 */
export function txCbor({ votes = [], proposals = [], certs = [] } = {}) {
  const body = [[0, new Tag(258, [])], [1, []], [2, 170000]];
  if (certs.length) body.push([4, certs]);
  if (votes.length) {
    const byVoter = new Map();
    for (const v of votes) {
      const key = `${v.kind}:${v.hash}`;
      if (!byVoter.has(key)) byVoter.set(key, { voter: [v.kind, bytes(v.hash)], actions: [] });
      byVoter.get(key).actions.push([[bytes(v.actionTx), v.actionIndex], [v.vote, v.anchor ? [v.anchor.url, bytes(v.anchor.hash)] : null]]);
    }
    body.push([19, new CMap([...byVoter.values()].map((x) => [x.voter, new CMap(x.actions)]))]);
  }
  if (proposals.length) {
    body.push([
      20,
      proposals.map((p) => [100000000000, bytes(`e1${hash28(9)}`), [6], [p.url, bytes(p.hash)]]),
    ]);
  }
  return { cbor: cbor([new CMap(body), new CMap([]), true, null]).toString('hex') };
}

/** CDDL vote codes. */
export const VOTE = { no: 0, yes: 1, abstain: 2 };
/** CDDL voter kinds. */
export const VOTER = { ccHotKey: 0, ccHotScript: 1, drepKey: 2, drepScript: 3, spo: 4 };

/* -- chain fixtures ------------------------------------------------------------ */

/** Epoch 600 starts at unix 1 000 000 000; epochs are 432 000 s. */
export const EPOCH = { epoch: 600, start_time: 1_000_000_000, end_time: 1_000_432_000, active_stake: '21000000000000000' };
export const timeIn = (epoch, offset = 1000) => EPOCH.start_time + (epoch - EPOCH.epoch) * 432_000 + offset;

export const txRow = (hash, epoch, height = 10_000_000 + epoch) => ({
  hash,
  block_height: height,
  block_time: timeIn(epoch),
  slot: 100_000_000 + (epoch - 500) * 432_000,
  index: 0,
});

export const PARAMS = {
  epoch: 600,
  min_fee_a: 44,
  min_fee_b: 155381,
  max_tx_size: 16384,
  key_deposit: '2000000',
  pool_deposit: '500000000',
  protocol_major_ver: 10,
  protocol_minor_ver: 0,
  max_val_size: '5000',
  coins_per_utxo_size: '4310',
  pvt_motion_no_confidence: 0.51,
  pvt_committee_normal: 0.51,
  pvt_committee_no_confidence: 0.51,
  pvt_hard_fork_initiation: 0.51,
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
  committee_min_size: '5',
  committee_max_term_length: '146',
  gov_action_lifetime: '6',
  gov_action_deposit: '100000000000',
  drep_deposit: '500000000',
  drep_activity: '20',
  pvtpp_security_group: 0.51,
  // As mainnet serves them (epoch 657).
  min_fee_ref_script_cost_per_byte: 15,
  max_block_size: 90112,
  max_block_header_size: 1100,
  max_tx_ex_mem: '16500000',
  max_tx_ex_steps: '10000000000',
  max_block_ex_mem: '72000000',
  max_block_ex_steps: '20000000000',
  collateral_percent: 150,
  max_collateral_inputs: 3,
  price_mem: 0.0577,
  price_step: 7.21e-5,
  // Keyed by name, in an order that is not the ledger's: never read.
  cost_models: { PlutusV1: { 'addInteger-cpu-arguments-slope': 420, 'addInteger-cpu-arguments-intercept': 100788 } },
  cost_models_raw: { PlutusV1: [100788, 420, 1, 1, 1000], PlutusV2: [100788, 420, 1, 1, -900], PlutusV3: [100788, 420, 1, 1, 1000, 173] },
  e_max: 18,
  n_opt: 500,
  a0: 0.3,
  rho: 0.003,
  tau: 0.2,
  min_pool_cost: '170000000',
  decentralisation_param: 0,
  extra_entropy: null,
  min_utxo: '4310',
  nonce: '45814ea700147322e9747d558f97ee97ff55846b8eb886dcdedc20461e5d9e63',
};

/** `/genesis` on mainnet. */
export const GENESIS = {
  active_slots_coefficient: 0.05,
  update_quorum: 5,
  max_lovelace_supply: '45000000000000000',
  network_magic: 764824073,
  epoch_length: 432000,
  system_start: 1506203091,
  slots_per_kes_period: 129600,
  slot_length: 1,
  max_kes_evolutions: 62,
  security_param: 2160,
};

export const drepRow = (n, over = {}) => ({
  drep_id: drepId(n, over.has_script ?? false),
  hex: `${over.has_script ? '23' : '22'}${hash28(n)}`,
  amount: '1000000',
  has_script: false,
  retired: false,
  expired: false,
  last_active_epoch: 590,
  metadata: { url: `https://example.org/drep${n}.json`, hash: hash32(n), json_metadata: null, bytes: null },
  ...over,
});

export const PREDEFINED = [
  { drep_id: 'drep_always_abstain', hex: '', amount: '7000', has_script: false, retired: false, expired: false, last_active_epoch: null, metadata: null },
  { drep_id: 'drep_always_no_confidence', hex: '', amount: '500', has_script: false, retired: false, expired: false, last_active_epoch: null, metadata: null },
];

export const committeeRow = (members) => ({
  gov_action_id: govActionId(hash32(0x70), 0),
  proposal_tx_hash: hash32(0x70),
  proposal_index: 0,
  is_dissolved: false,
  quorum: { numerator: 2, denominator: 3 },
  members,
});

export const memberRow = (n, over = {}) => ({
  cc_cold_id: coldId(n, over.cc_cold_has_script ?? false),
  cc_cold_hex: hash28(n),
  cc_cold_has_script: false,
  cc_hot_id: hotId(n + 100),
  cc_hot_hex: hash28(n + 100),
  cc_hot_has_script: false,
  status: 'authorized',
  expiration_epoch: 700,
  ...over,
});
