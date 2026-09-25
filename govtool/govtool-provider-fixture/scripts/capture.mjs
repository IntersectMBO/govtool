/**
 * Capture a frozen slice of mainnet into a contract-shaped fixture.
 *
 * Mapping happens HERE, not in the provider: what lands in data/*.json is
 * already `@govtool/data-providers` shapes, so the provider is a pure reader
 * and a shape bug fails at capture time rather than at read time.
 *
 *   BLOCKFROST_PROJECT_ID=<key> npm run capture
 *
 * Deliberately modest in size — this is a development fixture, not a mirror.
 */

import { writeFileSync, mkdirSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';

const KEY = process.env.BLOCKFROST_PROJECT_ID;
if (!KEY) {
  console.error('BLOCKFROST_PROJECT_ID is not set');
  process.exit(1);
}
const BASE = process.env.BLOCKFROST_URL ?? 'https://cardano-mainnet.blockfrost.io/api/v0';
const OUT = join(dirname(fileURLToPath(import.meta.url)), '..', 'data');

const WANT_DREPS = Number(process.env.CAPTURE_DREPS ?? 30);
const WANT_PROPOSALS = Number(process.env.CAPTURE_PROPOSALS ?? 30);
const WANT_VOTED = Number(process.env.CAPTURE_VOTED_PROPOSALS ?? 12);
const WANT_POOLS = Number(process.env.CAPTURE_POOLS ?? 10);

let calls = 0;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function bf(path, { optional = false } = {}) {
  for (let attempt = 0; ; attempt++) {
    const response = await fetch(`${BASE}${path}`, { headers: { project_id: KEY } });
    calls++;
    if (response.status === 429 && attempt < 5) {
      await sleep(1000 * (attempt + 1));
      continue;
    }
    if (!response.ok) {
      if (optional) return null;
      throw new Error(`${path} -> HTTP ${response.status}`);
    }
    return response.json();
  }
}

/* -- float -> exact Ratio, per SPEC.md §3.2 -------------------------------- */

/**
 * Bounded-denominator continued fractions. Real governance thresholds are small
 * rationals (51/100, 67/100, 2/3), so a cap of 1000 recovers them exactly:
 * 0.67 -> 67/100, 0.666... -> 2/3.
 */
function toRatio(value, maxDenominator = 1000) {
  if (value === null || value === undefined) return null;
  // Some sources already carry the exact rational — Blockfrost does for the
  // committee quorum, because 2/3 has no terminating decimal. Take it as-is.
  if (typeof value === 'object') {
    const n = Number(value.numerator);
    const d = Number(value.denominator);
    return Number.isFinite(n) && Number.isFinite(d) && d !== 0
      ? { numerator: n, denominator: d }
      : null;
  }
  const x = Number(value);
  if (!Number.isFinite(x)) return null;

  // A short decimal is exact as written (7.21e-05 -> 721/10000000), as in the
  // providers' ratio.ts; only a long float rendering needs reconstructing.
  const exact = exactDecimal(String(value));
  if (exact) return exact;

  let [n0, d0, n1, d1] = [0, 1, 1, 0];
  let v = x;
  for (let i = 0; i < 40; i++) {
    const a = Math.floor(v);
    const n2 = a * n1 + n0;
    const d2 = a * d1 + d0;
    if (!Number.isFinite(n2) || !Number.isFinite(d2) || d2 > maxDenominator) break;
    [n0, d0, n1, d1] = [n1, d1, n2, d2];
    const frac = v - a;
    if (frac < 1e-12) break;
    v = 1 / frac;
  }
  if (d1 === 0) return null;

  let a = Math.abs(n1);
  let b = Math.abs(d1);
  while (b !== 0) [a, b] = [b, a % b];
  const k = a || 1;
  return { numerator: n1 / k, denominator: d1 / k };
}

const DECIMAL = /^(-?)(\d+)(?:\.(\d+))?(?:[eE]([+-]?\d+))?$/;

/** A base-10 literal with at most 9 fractional digits, reduced; otherwise null. */
function exactDecimal(text) {
  const m = DECIMAL.exec(text.trim());
  if (!m) return null;
  const [, sign, whole, fraction = '', exp = '0'] = m;
  const shift = fraction.length - Number(exp);
  if (shift > 9) return null;
  let n = BigInt(`${sign}${whole}${fraction}`);
  let d = 1n;
  if (shift >= 0) d = 10n ** BigInt(shift);
  else n *= 10n ** BigInt(-shift);
  let [a, b] = [n < 0n ? -n : n, d];
  while (b !== 0n) [a, b] = [b, a % b];
  const g = a || 1n;
  const [rn, rd] = [n / g, d / g];
  const max = BigInt(Number.MAX_SAFE_INTEGER);
  if (rn > max || rn < -max || rd > max) return null;
  return { numerator: Number(rn), denominator: Number(rd) };
}

/** An integer that must be a safe integer (execution steps are ~2e10), from a number or a digit string. */
function safeInt(value, what) {
  const n = typeof value === 'string' && /^-?\d+$/.test(value) ? Number(value) : value;
  if (typeof n !== 'number' || !Number.isSafeInteger(n)) throw new Error(`${what} is not a safe integer: ${value}`);
  return n;
}

/**
 * Cost models from Blockfrost's `cost_models_raw`: per language, the integer
 * array in ledger parameter order. The named `cost_models` object is never
 * read — its key order is not the ledger's.
 */
function costModelsOf(raw) {
  if (!raw || typeof raw !== 'object') throw new Error('cost_models_raw is missing');
  const out = {};
  for (const language of ['PlutusV1', 'PlutusV2', 'PlutusV3']) {
    if (raw[language] == null) continue;
    out[language] = raw[language].map((c, i) => safeInt(c, `${language}[${i}]`));
  }
  return out;
}

/**
 * A ParameterChange update in the contract's names and shapes, as the
 * providers' body decoders produce it. A key not listed keeps its ledger name.
 */
const PARAM_KEYS = {
  govActionLifetime: ['govActionLifetime', 'int'],
  govActionDeposit: ['govActionDeposit', 'lovelace'],
  dRepDeposit: ['drepDeposit', 'lovelace'],
  dRepActivity: ['drepActivity', 'int'],
  committeeMinSize: ['committeeMinSize', 'int'],
  committeeMaxTermLength: ['committeeMaxTermLength', 'int'],
  dRepVotingThresholds: ['drepThresholds', 'thresholds'],
  poolVotingThresholds: ['poolThresholds', 'thresholds'],
  stakeAddressDeposit: ['keyDeposit', 'lovelace'],
  stakePoolDeposit: ['poolDeposit', 'lovelace'],
  utxoCostPerByte: ['coinsPerUtxoByte', 'lovelace'],
  txFeePerByte: ['minFeeA', 'int'],
  txFeeFixed: ['minFeeB', 'int'],
  maxTxSize: ['maxTxSize', 'int'],
  maxValueSize: ['maxValSize', 'int'],
  protocolVersion: ['protocolVersion', 'plain'],
  maxBlockBodySize: ['maxBlockBodySize', 'int'],
  maxBlockHeaderSize: ['maxBlockHeaderSize', 'int'],
  maxTxExecutionUnits: ['maxTxExecutionUnits', 'exUnits'],
  maxBlockExecutionUnits: ['maxBlockExecutionUnits', 'exUnits'],
  collateralPercentage: ['collateralPercentage', 'int'],
  maxCollateralInputs: ['maxCollateralInputs', 'int'],
  costModels: ['costModels', 'costModels'],
  poolRetireMaxEpoch: ['poolRetireMaxEpoch', 'int'],
  stakePoolTargetNum: ['stakePoolTargetNum', 'int'],
  minPoolCost: ['minPoolCost', 'lovelace'],
  monetaryExpansion: ['monetaryExpansion', 'ratio'],
  treasuryCut: ['treasuryCut', 'ratio'],
  poolPledgeInfluence: ['poolPledgeInfluence', 'ratio'],
  minFeeRefScriptCostPerByte: ['minFeeRefScriptCostPerByte', 'ratio'],
  executionUnitPrices: ['executionUnitPrices', 'prices'],
};

function paramChanges(update) {
  const out = {};
  for (const [key, value] of Object.entries(update ?? {})) {
    if (value === null) continue;
    const [name, conv] = PARAM_KEYS[key] ?? [key, 'plain'];
    switch (conv) {
      case 'int':
        out[name] = safeInt(value, key);
        break;
      case 'lovelace':
        out[name] = BigInt(value).toString();
        break;
      case 'ratio':
        out[name] = toRatio(value);
        break;
      case 'thresholds':
        out[name] = Object.fromEntries(Object.entries(value).map(([k, v]) => [k, toRatio(v)]));
        break;
      case 'exUnits':
        out[name] = { memory: safeInt(value.memory, `${key}.memory`), steps: safeInt(value.steps, `${key}.steps`) };
        break;
      case 'prices':
        out[name] = { memory: toRatio(value.priceMemory), steps: toRatio(value.priceSteps) };
        break;
      case 'costModels':
        out[name] = Object.fromEntries(Object.entries(value).map(([l, c]) => [l, c.map((x, i) => safeInt(x, `${key}.${l}[${i}]`))]));
        break;
      default:
        out[name] = value;
    }
  }
  return out;
}

/* -- bech32, so withdrawal recipients are real stake addresses -------------- */

const CHARSET = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';
const polymod = (values) => {
  const GEN = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3];
  let chk = 1;
  for (const v of values) {
    const b = chk >> 25;
    chk = ((chk & 0x1ffffff) << 5) ^ v;
    for (let i = 0; i < 5; i++) if ((b >> i) & 1) chk ^= GEN[i];
  }
  return chk;
};
const hrpExpand = (hrp) => [
  ...[...hrp].map((c) => c.charCodeAt(0) >> 5),
  0,
  ...[...hrp].map((c) => c.charCodeAt(0) & 31),
];
function toWords(bytes) {
  const words = [];
  let acc = 0;
  let bits = 0;
  for (const b of bytes) {
    acc = (acc << 8) | b;
    bits += 8;
    while (bits >= 5) {
      bits -= 5;
      words.push((acc >> bits) & 31);
    }
  }
  if (bits > 0) words.push((acc << (5 - bits)) & 31);
  return words;
}
function bech32Encode(hrp, bytes) {
  const words = toWords(bytes);
  const chk = polymod([...hrpExpand(hrp), ...words, 0, 0, 0, 0, 0, 0]) ^ 1;
  const checksum = [];
  for (let i = 0; i < 6; i++) checksum.push((chk >> (5 * (5 - i))) & 31);
  return `${hrp}1${[...words, ...checksum].map((w) => CHARSET[w]).join('')}`;
}
/** A reward account: header byte (key 0xe1 / script 0xf1 on mainnet) + hash. */
function stakeAddress(credential) {
  const isScript = credential.scriptHash !== undefined;
  const hash = credential.scriptHash ?? credential.keyHash;
  if (!hash) return '';
  const bytes = [isScript ? 0xf1 : 0xe1, ...hash.match(/../g).map((h) => parseInt(h, 16))];
  return bech32Encode('stake', bytes);
}

/* -- mappers --------------------------------------------------------------- */

const ACTION_TYPE = {
  info_action: 'InfoAction',
  no_confidence: 'NoConfidence',
  parameter_change: 'ParameterChange',
  hard_fork_initiation: 'HardForkInitiation',
  treasury_withdrawals: 'TreasuryWithdrawals',
  new_committee: 'UpdateCommittee',
  new_constitution: 'NewConstitution',
};

function actionStatus(p) {
  if (p.enacted_epoch != null) return 'enacted';
  if (p.ratified_epoch != null) return 'ratified';
  if (p.expired_epoch != null) return 'expired';
  if (p.dropped_epoch != null) return 'dropped';
  return 'live';
}

const stamp = (epoch) => (epoch == null ? null : { epoch });

/**
 * Blockfrost's `governance_description` -> the contract's typed body.
 *
 * `contents[0]` is the PREVIOUS ACTION for every type that has a lineage; the
 * two that do not (`InfoAction`, `TreasuryWithdrawals`) use the slot for their
 * own payload instead. That asymmetry is the ledger's, not Blockfrost's.
 */
function toBody(type, description) {
  const c = description?.contents ?? [];
  switch (type) {
    case 'InfoAction':
      return { type: 'InfoAction' };
    case 'NoConfidence':
      return { type: 'NoConfidence' };
    case 'ParameterChange':
      return { type: 'ParameterChange', changes: paramChanges(c[1]), guardrailsScriptHash: c[2] ?? null };
    case 'HardForkInitiation':
      return {
        type: 'HardForkInitiation',
        protocolVersion: { major: c[1]?.major ?? 0, minor: c[1]?.minor ?? 0 },
      };
    case 'TreasuryWithdrawals': {
      // contents[0] is [[rewardAccount, amount], ...] — no previous action.
      const withdrawals = (c[0] ?? []).map(([account, amount]) => ({
        stakeAddress: stakeAddress(account?.credential ?? {}),
        amount: String(amount),
      }));
      const total = withdrawals.reduce((sum, w) => sum + BigInt(w.amount), 0n);
      return {
        type: 'TreasuryWithdrawals',
        withdrawals,
        totalAmount: total.toString(),
        guardrailsScriptHash: c[1] ?? null,
      };
    }
    case 'UpdateCommittee':
      return {
        type: 'UpdateCommittee',
        // Added members are keyed `keyHash-<hex>` / `scriptHash-<hex>`.
        added: Object.entries(c[2] ?? {}).map(([key, epoch]) => ({
          coldCredential: key,
          termExpiryEpoch: Number(epoch),
        })),
        removed: (c[1] ?? []).map((cred) => ({
          coldCredential: cred?.scriptHash ?? cred?.keyHash ?? '',
        })),
        quorum: toRatio(c[3]) ?? { numerator: 2, denominator: 3 },
      };
    case 'NewConstitution': {
      const anchor = c[1]?.anchor ?? {};
      return {
        type: 'NewConstitution',
        anchor: { url: anchor.url ?? '', dataHash: anchor.dataHash ?? '' },
        guardrailsScriptHash: c[1]?.script ?? null,
      };
    }
    default:
      return { type: 'InfoAction' };
  }
}

/** The lineage head this action builds on, where the type has one. */
function toPreviousAction(type, description) {
  if (type === 'InfoAction' || type === 'TreasuryWithdrawals') return null;
  const prev = description?.contents?.[0];
  if (!prev?.txId) return null;
  return { id: '', txHash: prev.txId, index: prev.govActionIx ?? 0 };
}

/* -- capture --------------------------------------------------------------- */

console.log(`capturing from ${BASE}`);

const [latestBlock, latestEpoch, params, network, committeeRaw, genesis] = await Promise.all([
  bf('/blocks/latest'),
  bf('/epochs/latest'),
  bf('/epochs/latest/parameters'),
  bf('/network'),
  bf('/governance/committee', { optional: true }),
  bf('/genesis'),
]);

const networkInfo = {
  network: 'mainnet',
  era: 'conway',
  tip: {
    epoch: latestEpoch.epoch,
    slot: latestBlock.slot,
    block: latestBlock.height,
    time: new Date(latestBlock.time * 1000).toISOString(),
  },
  currentEpoch: latestEpoch.epoch,
};

const protocolParams = {
  epoch: latestEpoch.epoch,
  protocolVersion: { major: params.protocol_major_ver, minor: params.protocol_minor_ver },
  govActionLifetime: Number(params.gov_action_lifetime),
  govActionDeposit: String(params.gov_action_deposit),
  drepDeposit: String(params.drep_deposit),
  drepActivity: Number(params.drep_activity),
  committeeMinSize: Number(params.committee_min_size),
  committeeMaxTermLength: Number(params.committee_max_term_length),
  drepThresholds: {
    motionNoConfidence: toRatio(params.dvt_motion_no_confidence),
    committeeNormal: toRatio(params.dvt_committee_normal),
    committeeNoConfidence: toRatio(params.dvt_committee_no_confidence),
    updateToConstitution: toRatio(params.dvt_update_to_constitution),
    hardForkInitiation: toRatio(params.dvt_hard_fork_initiation),
    ppNetworkGroup: toRatio(params.dvt_p_p_network_group),
    ppEconomicGroup: toRatio(params.dvt_p_p_economic_group),
    ppTechnicalGroup: toRatio(params.dvt_p_p_technical_group),
    ppGovGroup: toRatio(params.dvt_p_p_gov_group),
    treasuryWithdrawal: toRatio(params.dvt_treasury_withdrawal),
  },
  poolThresholds: {
    motionNoConfidence: toRatio(params.pvt_motion_no_confidence),
    committeeNormal: toRatio(params.pvt_committee_normal),
    committeeNoConfidence: toRatio(params.pvt_committee_no_confidence),
    hardForkInitiation: toRatio(params.pvt_hard_fork_initiation),
    // Blockfrost carries two spellings of this one parameter; pick one.
    ppSecurityGroup: toRatio(params.pvtpp_security_group ?? params.pvt_p_p_security_group),
  },
  keyDeposit: String(params.key_deposit),
  poolDeposit: String(params.pool_deposit),
  coinsPerUtxoByte: String(params.coins_per_utxo_size ?? params.coins_per_utxo_word),
  minFeeA: Number(params.min_fee_a),
  minFeeB: Number(params.min_fee_b),
  minFeeRefScriptCostPerByte: toRatio(params.min_fee_ref_script_cost_per_byte),
  maxBlockBodySize: safeInt(params.max_block_size, 'max_block_size'),
  maxBlockHeaderSize: safeInt(params.max_block_header_size, 'max_block_header_size'),
  maxTxSize: Number(params.max_tx_size),
  maxValSize: Number(params.max_val_size),
  maxTxExecutionUnits: { memory: safeInt(params.max_tx_ex_mem, 'max_tx_ex_mem'), steps: safeInt(params.max_tx_ex_steps, 'max_tx_ex_steps') },
  maxBlockExecutionUnits: {
    memory: safeInt(params.max_block_ex_mem, 'max_block_ex_mem'),
    steps: safeInt(params.max_block_ex_steps, 'max_block_ex_steps'),
  },
  collateralPercentage: safeInt(params.collateral_percent, 'collateral_percent'),
  maxCollateralInputs: safeInt(params.max_collateral_inputs, 'max_collateral_inputs'),
  executionUnitPrices: { memory: toRatio(params.price_mem), steps: toRatio(params.price_step) },
  costModels: costModelsOf(params.cost_models_raw),
  poolRetireMaxEpoch: safeInt(params.e_max, 'e_max'),
  stakePoolTargetNum: safeInt(params.n_opt, 'n_opt'),
  poolPledgeInfluence: toRatio(params.a0),
  monetaryExpansion: toRatio(params.rho),
  treasuryCut: toRatio(params.tau),
  minPoolCost: String(params.min_pool_cost),
};
for (const [k, v] of Object.entries(protocolParams)) if (v === null || v === undefined) throw new Error(`protocol parameter ${k} is missing`);

const genesisParams = {
  networkMagic: safeInt(genesis.network_magic, 'network_magic'),
  networkId: genesis.network_magic === 764824073 ? 'Mainnet' : 'Testnet',
  systemStart: new Date(safeInt(genesis.system_start, 'system_start') * 1000).toISOString().replace('.000Z', 'Z'),
  epochLength: safeInt(genesis.epoch_length, 'epoch_length'),
  slotLength: Number(genesis.slot_length),
  activeSlotsCoefficient: toRatio(genesis.active_slots_coefficient),
  securityParam: safeInt(genesis.security_param, 'security_param'),
  slotsPerKesPeriod: safeInt(genesis.slots_per_kes_period, 'slots_per_kes_period'),
  maxKesEvolutions: safeInt(genesis.max_kes_evolutions, 'max_kes_evolutions'),
  updateQuorum: safeInt(genesis.update_quorum, 'update_quorum'),
  maxLovelaceSupply: String(genesis.max_lovelace_supply),
};

const stakeDistribution = {
  epoch: latestEpoch.epoch,
  totalActiveStake: network.stake.active,
  totalLiveStake: network.stake.live,
};

const treasury = {
  epoch: latestEpoch.epoch,
  balance: network.supply.treasury,
  reserves: network.supply.reserves,
};

/* DReps — take a spread, and fetch each one's anchor. */
console.log('dreps…');
const drepIds = (await bf(`/governance/dreps?count=${WANT_DREPS}`)).map((d) => d.drep_id);
const dreps = [];
for (const id of drepIds) {
  const [info, meta] = await Promise.all([
    bf(`/governance/dreps/${id}`, { optional: true }),
    bf(`/governance/dreps/${id}/metadata`, { optional: true }),
  ]);
  if (!info) continue;
  const anchor = meta?.url ? { url: meta.url, dataHash: meta.hash } : null;
  dreps.push({
    role: 'drep',
    id: info.drep_id,
    isScriptBased: info.has_script,
    // Anonymous is defined by the ABSENCE OF AN ANCHOR — nothing else.
    kind: anchor === null ? 'anonymous' : 'drep',
    anchor,
    registration: {
      latest: {
        txRef: { txHash: '' },
        at: { epoch: info.active_epoch ?? latestEpoch.epoch },
      },
      latestUpdate: null,
    },
    status: info.retired ? 'retired' : info.expired ? 'inactive' : 'active',
    expiryEpoch: info.last_active_epoch ?? undefined,
    votingPower: { amount: info.amount ?? '0', basis: 'active', epoch: latestEpoch.epoch },
    // Captured alongside so the fixture can serve a metadata service too.
    _metadataBody: meta?.json_metadata ?? null,
  });
}

/* Proposals — a spread of statuses, with typed bodies. */
console.log('proposals…');
const proposalRefs = await bf(`/governance/proposals?count=${WANT_PROPOSALS}&order=desc`);
const proposals = [];
for (const ref of proposalRefs) {
  const p = await bf(`/governance/proposals/${ref.tx_hash}/${ref.cert_index}`, { optional: true });
  if (!p) continue;
  const type = ACTION_TYPE[p.governance_type] ?? 'InfoAction';
  proposals.push({
    id: p.id,
    txHash: p.tx_hash,
    index: p.cert_index,
    type,
    body: toBody(type, p.governance_description),
    lifecycle: {
      status: actionStatus(p),
      submitted: { epoch: p.expiration != null ? p.expiration - protocolParams.govActionLifetime : latestEpoch.epoch },
      submittedTx: { txHash: p.tx_hash },
      expires: stamp(p.expiration),
      ratifiedAt: stamp(p.ratified_epoch),
      enactedAt: stamp(p.enacted_epoch),
      droppedAt: stamp(p.dropped_epoch),
      expiredAt: stamp(p.expired_epoch),
    },
    anchor: null,
    previousAction: toPreviousAction(type, p.governance_description),
    deposit: p.deposit != null ? String(p.deposit) : null,
    depositReturnAddress: p.return_address ?? null,
  });
}

/* Votes — only for the newest few, which is where the interesting ones are. */
console.log('votes…');
const votes = {};
for (const p of proposals.slice(0, WANT_VOTED)) {
  const rows = await bf(`/governance/proposals/${p.txHash}/${p.index}/votes?count=100`, { optional: true });
  if (!rows?.length) continue;
  votes[p.id] = rows.map((v) => ({
    voter:
      v.voter_role === 'constitutional_committee'
        ? { role: 'cc', hot: v.voter }
        : { role: v.voter_role === 'spo' ? 'spo' : 'drep', id: v.voter },
    choice: v.vote,
    anchor: null,
    txRef: { txHash: v.tx_hash, index: v.cert_index },
  }));
}

/* Pools */
console.log('pools…');
const poolIds = (await bf(`/pools?count=${WANT_POOLS}`)) ?? [];
const pools = [];
for (const poolId of poolIds) {
  const p = await bf(`/pools/${poolId}`, { optional: true });
  if (!p) continue;
  const meta = await bf(`/pools/${poolId}/metadata`, { optional: true });
  pools.push({
    role: 'spo',
    id: p.pool_id,
    poolId: p.pool_id,
    anchor: meta?.url ? { url: meta.url, dataHash: meta.hash } : null,
    votingPower: { amount: p.active_stake ?? '0', basis: 'active', epoch: latestEpoch.epoch },
    activeStake: p.active_stake,
    liveStake: p.live_stake,
    pledge: p.declared_pledge,
  });
}

/* Committee and constitution */
const committee = committeeRaw
  ? {
      members: (committeeRaw.members ?? []).map((m) => ({
        role: 'cc',
        coldCredential: m.cc_cold_id,
        hotCredential: m.cc_hot_id ?? null,
        termStartEpoch: null,
        termExpiryEpoch: m.expiration_epoch ?? null,
        hasResigned: m.status === 'resigned',
        isScriptBased: m.cc_cold_has_script,
      })),
      quorum: committeeRaw.quorum ?? { numerator: 2, denominator: 3 },
      enactedBy: null,
      isDissolved: committeeRaw.is_dissolved ?? false,
    }
  : null;

/** Derived, not fetched: the latest enacted NewConstitution carries the anchor. */
const enactedConstitution = proposals.find(
  (p) => p.type === 'NewConstitution' && p.lifecycle.status === 'enacted',
);
const constitution = {
  anchor: enactedConstitution?.body.anchor ?? { url: '', dataHash: '' },
  guardrailsScriptHash: null,
  enactedBy: enactedConstitution ? { id: enactedConstitution.id, txHash: enactedConstitution.txHash, index: enactedConstitution.index } : null,
  enactedAt: enactedConstitution?.lifecycle.enactedAt ?? null,
};

/* Accounts — real stake addresses taken from DRep delegator lists. */
console.log('accounts…');
const accounts = [];
const delegators = {};
for (const drep of dreps.slice(0, 8)) {
  const rows = await bf(`/governance/dreps/${drep.id}/delegators?count=10`, { optional: true });
  if (!rows?.length) continue;
  delegators[drep.id] = rows.map((r) => ({
    stakeAddress: r.address,
    activeVotingPower: r.amount ?? '0',
  }));
  for (const r of rows.slice(0, 3)) {
    const acct = await bf(`/accounts/${r.address}`, { optional: true });
    if (!acct) continue;
    accounts.push({
      account: {
        stakeAddress: acct.stake_address,
        stakeKeyHash: '',
        isRegistered: acct.active,
        balance: {
          total: acct.controlled_amount ?? '0',
          utxo: String(BigInt(acct.controlled_amount ?? '0') - BigInt(acct.rewards_sum ?? '0') + BigInt(acct.withdrawals_sum ?? '0')),
          rewards: acct.withdrawable_amount ?? '0',
          rewardsRest: '0',
        },
      },
      delegation: acct.drep_id
        ? { target: { kind: 'drep', drep: { role: 'drep', id: acct.drep_id } }, txRef: null }
        : null,
    });
  }
}

const fixture = {
  capturedAt: new Date().toISOString(),
  source: BASE,
  networkInfo,
  protocolParams,
  genesisParams,
  stakeDistribution,
  treasury,
  dreps,
  drepDelegators: delegators,
  proposals,
  votes,
  pools,
  committee,
  constitution,
  accounts,
};

mkdirSync(OUT, { recursive: true });
writeFileSync(join(OUT, 'mainnet.json'), JSON.stringify(fixture, null, 2));

const byStatus = (xs, key) =>
  xs.reduce((acc, x) => ({ ...acc, [key(x)]: (acc[key(x)] ?? 0) + 1 }), {});

console.log(`
wrote data/mainnet.json  (${calls} API calls)
  dreps        ${dreps.length}  ${JSON.stringify(byStatus(dreps, (d) => d.status))}
  anonymous    ${dreps.filter((d) => d.kind === 'anonymous').length}
  proposals    ${proposals.length}  ${JSON.stringify(byStatus(proposals, (p) => p.lifecycle.status))}
  types        ${JSON.stringify(byStatus(proposals, (p) => p.type))}
  votes on     ${Object.keys(votes).length} proposals, ${Object.values(votes).flat().length} total
  pools        ${pools.length}
  committee    ${committee?.members.length ?? 0} members
  accounts     ${accounts.length}
`);
