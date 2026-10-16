/**
 * Test doubles: a fake `fetch` that answers Koios-shaped JSON by endpoint,
 * and recorded-shape fixtures. Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { bech32 } from 'bech32';

import { createKoiosProvider } from '../dist/index.js';

/**
 * A fake fetch. `routes` maps an endpoint name to rows, or to a function
 * `(url, init) => rows | { status, body, headers }`. PostgREST's `limit`,
 * `offset` and `Prefer: count=exact` are honoured over a row array, so paging
 * code is exercised for real. Every call is recorded.
 */
export function fakeKoios(routes) {
  const calls = [];
  const fetch = async (input, init = {}) => {
    const url = new URL(String(input));
    const endpoint = url.pathname.split('/').pop();
    const headers = new Headers(init.headers);
    const body = init.body ? JSON.parse(init.body) : undefined;
    calls.push({ endpoint, url, method: init.method ?? 'GET', headers, body });
    if (init.signal?.aborted) throw Object.assign(new Error('aborted'), { name: 'TimeoutError' });
    const route = routes[endpoint];
    if (route === undefined) return new Response(JSON.stringify({ message: 'no route' }), { status: 404 });
    let answer = typeof route === 'function' ? await route(url, { ...init, body, headers }) : route;
    if (answer instanceof Response) return answer;
    if (answer && !Array.isArray(answer) && typeof answer === 'object' && 'status' in answer) {
      return new Response(answer.body === undefined ? '' : JSON.stringify(answer.body), {
        status: answer.status,
        headers: answer.headers ?? {},
      });
    }
    const rows = Array.isArray(answer) ? answer : [answer];
    const offset = Number(url.searchParams.get('offset') ?? 0);
    const limit = url.searchParams.has('limit') ? Number(url.searchParams.get('limit')) : 1000;
    const page = rows.slice(offset, offset + Math.min(limit, 1000));
    const out = new Headers({ 'content-type': 'application/json' });
    if (headers.get('prefer') === 'count=exact') {
      out.set('content-range', page.length ? `${offset}-${offset + page.length - 1}/${rows.length}` : `*/${rows.length}`);
    } else {
      out.set('content-range', page.length ? `${offset}-${offset + page.length - 1}/*` : '*/*');
    }
    return new Response(JSON.stringify(page), { status: 200, headers: out });
  };
  return { fetch, calls };
}

export function provider(routes, options = {}) {
  const fake = fakeKoios(routes);
  const { chainData } = createKoiosProvider({ network: 'mainnet', fetch: fake.fetch, maxRetries: 0, ...options });
  return { ...fake, chainData };
}

export async function rejectsWith(promiseOrFn, code) {
  const p = typeof promiseOrFn === 'function' ? promiseOrFn() : promiseOrFn;
  assert.ok(p instanceof Promise, 'must return a promise, never throw synchronously');
  await assert.rejects(p, (e) => e.code === code || assert.fail(`expected ${code}, got ${e.code}: ${e.message}`));
}

/* -- ids --------------------------------------------------------------------- */

const enc = (prefix, bytes) => bech32.encode(prefix, bech32.toWords(Buffer.from(bytes)), 1023);
export const hash = (byte) => Buffer.alloc(28, byte).toString('hex');
export const drepId = (byte, script = false) => enc('drep', [script ? 0x23 : 0x22, ...Buffer.alloc(28, byte)]);
export const cip105 = (byte) => enc('drep', Buffer.alloc(28, byte));
export const coldId = (byte, script = false) => enc('cc_cold', [script ? 0x13 : 0x12, ...Buffer.alloc(28, byte)]);
export const hotId = (byte, script = false) => enc('cc_hot', [script ? 0x03 : 0x02, ...Buffer.alloc(28, byte)]);
export const poolId = (byte) => enc('pool', Buffer.alloc(28, byte));
export const stakeAddr = (byte) => enc('stake', [0xe1, ...Buffer.alloc(28, byte)]);
export const txHash = (byte) => Buffer.alloc(32, byte).toString('hex');
export const actionId = (byte, index = 0) => enc('gov_action', [...Buffer.alloc(32, byte), index]);

/* -- chain ------------------------------------------------------------------- */

export const EPOCH = 657;
/** Epoch 657 starts here; 432000 s epochs, as on mainnet. */
export const EPOCH_START = 1790022291;
export const at = (epoch, offset = 1000) => EPOCH_START + (epoch - EPOCH) * 432000 + offset;

export const chainRoutes = {
  tip: [{ hash: 'aa', epoch_no: EPOCH, era: 'Conway', abs_slot: 198699726, epoch_slot: 1000, block_height: 13982865, block_no: 13982865, block_time: at(EPOCH, 5000) }],
  genesis: [
    {
      networkmagic: '764824073',
      networkid: 'Mainnet',
      activeslotcoeff: '0.05',
      updatequorum: '5',
      maxlovelacesupply: '45000000000000000',
      epochlength: '432000',
      systemstart: 1506203091,
      slotsperkesperiod: '129600',
      slotlength: '1',
      maxkesrevolutions: '62',
      securityparam: '2160',
    },
  ],
  epoch_info: [{ epoch_no: EPOCH, start_time: EPOCH_START, end_time: EPOCH_START + 432000, active_stake: '21357778069987000' }],
};

export const paramsRow = (epoch = EPOCH, major = 11) => ({
  epoch_no: epoch,
  protocol_major: major,
  protocol_minor: 0,
  min_fee_a: 44,
  min_fee_b: 155381,
  max_tx_size: 16384,
  max_val_size: 5000,
  key_deposit: '2000000',
  pool_deposit: '500000000',
  coins_per_utxo_size: '4310',
  gov_action_lifetime: 6,
  gov_action_deposit: '100000000000',
  drep_deposit: '500000000',
  drep_activity: 20,
  committee_min_size: 5,
  committee_max_term_length: 146,
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
  pvtpp_security_group: 0.51,
  // Mainnet epoch 657, as Koios serves them.
  min_fee_ref_script_cost_per_byte: 15,
  max_block_size: 90112,
  max_bh_size: 1100,
  max_tx_ex_mem: 16500000,
  max_tx_ex_steps: 10000000000,
  max_block_ex_mem: 72000000,
  max_block_ex_steps: 20000000000,
  collateral_percent: 150,
  max_collateral_inputs: 3,
  price_mem: 0.0577,
  price_step: 7.21e-5,
  cost_models: { PlutusV1: [100788, 420, 1, 1, 1000], PlutusV2: [100788, 420, 1, 1, -900], PlutusV3: [100788, 420, 1, 1, 1000, 173] },
  max_epoch: 18,
  optimal_pool_count: 500,
  influence: 0.3,
  monetary_expand_rate: 0.003,
  treasury_growth_rate: 0.2,
  min_pool_cost: '170000000',
  // Source columns the contract does not carry.
  decentralisation: 0,
  extra_entropy: null,
  min_utxo_value: 0,
  nonce: '45814ea700147322e9747d558f97ee97ff55846b8eb886dcdedc20461e5d9e63',
});

/** Apply the PostgREST filters this provider uses (eq, in, not.is.null, is.null, gte) to rows. */
export function filterRows(rows, url, ignore = []) {
  const skip = new Set(['select', 'order', 'limit', 'offset', 'or', ...ignore]);
  let out = rows;
  for (const [key, raw] of url.searchParams) {
    if (skip.has(key) || key.startsWith('_')) continue;
    out = out.filter((row) => {
      const v = row[key];
      if (raw === 'not.is.null') return v !== null && v !== undefined;
      if (raw === 'is.null') return v === null || v === undefined;
      if (raw.startsWith('eq.')) return String(v) === raw.slice(3);
      if (raw.startsWith('neq.')) return String(v) !== raw.slice(4);
      if (raw.startsWith('gte.')) return Number(v) >= Number(raw.slice(4));
      if (raw.startsWith('lt.')) return Number(v) < Number(raw.slice(3));
      if (raw.startsWith('in.(')) return raw.slice(4, -1).split(',').includes(String(v));
      throw new Error(`fake Koios: unsupported filter ${key}=${raw}`);
    });
  }
  return out;
}
