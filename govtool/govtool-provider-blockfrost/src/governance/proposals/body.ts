/**
 * The typed body of a governance action, decoded from Blockfrost's
 * `governance_description` — the ledger's own JSON rendering of the action,
 * the same text db-sync stores. SPEC.md §5.2: the body is required for all
 * seven types, so an undecodable description is an internal fault, never an
 * untyped fallback.
 *
 * The description is decoded directly rather than through the `/parameters`
 * and `/withdrawals` sub-resources: it carries everything (the parameter
 * update in ledger names, the withdrawal credentials, the predecessor), so
 * each proposal costs one read instead of three.
 */
import type {
  GovActionBody,
  GovActionLineage,
  GovActionRef,
  GovActionType,
  NetworkId,
  Ratio,
} from '@govtool/data-providers/chain-data';

import { internal } from '../../errors';
import { encodeCommitteeColdId, encodeGovActionId, encodeStakeAddress, isHex } from '../../ids';
import { asArray, asString, isObject, numText, toPlain, type ExactJson } from '../../json';
import { toRatio } from '../../ratio';

/** Blockfrost `governance_type` → contract type. */
export const BF_TO_TYPE: Record<string, GovActionType> = {
  parameter_change: 'ParameterChange',
  hard_fork_initiation: 'HardForkInitiation',
  treasury_withdrawals: 'TreasuryWithdrawals',
  no_confidence: 'NoConfidence',
  new_committee: 'UpdateCommittee',
  new_constitution: 'NewConstitution',
  info_action: 'InfoAction',
};

/** The ledger's tag in `governance_description`, per contract type. */
const LEDGER_TAG: Record<GovActionType, string> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  UpdateCommittee: 'UpdateCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

/**
 * Lineage is by PURPOSE, not type: UpdateCommittee and NoConfidence share the
 * committee lineage.
 */
export const LINEAGE_TYPES: Record<GovActionLineage, readonly GovActionType[]> = {
  pparamUpdate: ['ParameterChange'],
  hardFork: ['HardForkInitiation'],
  committee: ['UpdateCommittee', 'NoConfidence'],
  constitution: ['NewConstitution'],
};

export function toGovActionType(bfType: string): GovActionType {
  const type = BF_TO_TYPE[bfType];
  if (!type) throw internal(`Unknown Blockfrost governance_type ${bfType}`);
  return type;
}

/* ------------------------------------------------------------------------- */
/* Parameter changes                                                          */
/* ------------------------------------------------------------------------- */

type Conv =
  | 'int'
  | 'lovelace'
  | 'ratio'
  | 'drepThresholds'
  | 'poolThresholds'
  | 'prices'
  | 'exUnits'
  | 'costModels'
  | 'version'
  | 'plain';

/**
 * Ledger JSON key → [contract key, conversion], for every parameter the
 * Conway ledger holds. A key the ledger adds later keeps its ledger name and
 * value, so the body still says what changes.
 */
const PARAM_KEYS: Record<string, [string, Conv]> = {
  govActionLifetime: ['govActionLifetime', 'int'],
  govActionDeposit: ['govActionDeposit', 'lovelace'],
  dRepDeposit: ['drepDeposit', 'lovelace'],
  dRepActivity: ['drepActivity', 'int'],
  committeeMinSize: ['committeeMinSize', 'int'],
  committeeMaxTermLength: ['committeeMaxTermLength', 'int'],
  dRepVotingThresholds: ['drepThresholds', 'drepThresholds'],
  poolVotingThresholds: ['poolThresholds', 'poolThresholds'],
  stakeAddressDeposit: ['keyDeposit', 'lovelace'],
  stakePoolDeposit: ['poolDeposit', 'lovelace'],
  utxoCostPerByte: ['coinsPerUtxoByte', 'lovelace'],
  txFeePerByte: ['minFeeA', 'int'],
  txFeeFixed: ['minFeeB', 'int'],
  maxTxSize: ['maxTxSize', 'int'],
  maxValueSize: ['maxValSize', 'int'],
  protocolVersion: ['protocolVersion', 'version'],
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

const DREP_THRESHOLD_KEYS = [
  'motionNoConfidence',
  'committeeNormal',
  'committeeNoConfidence',
  'updateToConstitution',
  'hardForkInitiation',
  'ppNetworkGroup',
  'ppEconomicGroup',
  'ppTechnicalGroup',
  'ppGovGroup',
  'treasuryWithdrawal',
] as const;

const POOL_THRESHOLD_KEYS = [
  'motionNoConfidence',
  'committeeNormal',
  'committeeNoConfidence',
  'hardForkInitiation',
  'ppSecurityGroup',
] as const;

const fail = (what: string): never => {
  throw internal(`Cannot decode governance action body: ${what}`);
};

const ratioOf = (v: ExactJson | undefined, what: string): Ratio =>
  toRatio(isObject(v) ? toPlain(v) : (numText(v) ?? undefined)) ?? fail(`${what} is not a rational`);

const intOf = (v: ExactJson | undefined, what: string): number => {
  const text = numText(v);
  if (text === undefined || !/^-?\d+$/.test(text) || !Number.isSafeInteger(Number(text))) return fail(`${what} is not an integer`);
  return Number(text);
};

/** The ledger's per-language cost models: `{ PlutusV1: [...], ... }`, arrays in parameter order, kept as given. */
function costModelsOf(v: ExactJson, what: string): Record<string, number[]> {
  if (!isObject(v)) return fail(`${what} is not an object`);
  return Object.fromEntries(
    Object.entries(v).map(([language, costs]) => {
      const list = asArray(costs) ?? fail(`${what}.${language} is not an integer array`);
      return [language, list.map((c, i) => intOf(c, `${what}.${language}[${i}]`))];
    }),
  );
}

const lovelaceOf = (v: ExactJson | undefined, what: string): string => {
  const text = numText(v);
  if (text === undefined || !/^\d+$/.test(text)) return fail(`${what} is not lovelace`);
  return BigInt(text).toString();
};

function thresholdGroup(v: ExactJson | undefined, keys: readonly string[], what: string): Record<string, Ratio> {
  if (!isObject(v)) return fail(`${what} is not an object`);
  return Object.fromEntries(keys.map((k) => [k, ratioOf(v[k], `${what}.${k}`)]));
}

function convertParam(conv: Conv, v: ExactJson, key: string): unknown {
  switch (conv) {
    case 'int':
      return intOf(v, key);
    case 'lovelace':
      return lovelaceOf(v, key);
    case 'ratio':
      return ratioOf(v, key);
    case 'drepThresholds':
      return thresholdGroup(v, DREP_THRESHOLD_KEYS, key);
    case 'poolThresholds':
      return thresholdGroup(v, POOL_THRESHOLD_KEYS, key);
    case 'prices':
      if (!isObject(v)) return fail(`${key} is not an object`);
      return { memory: ratioOf(v['priceMemory'], 'priceMemory'), steps: ratioOf(v['priceSteps'], 'priceSteps') };
    case 'exUnits':
      if (!isObject(v)) return fail(`${key} is not an object`);
      return { memory: intOf(v['memory'], `${key}.memory`), steps: intOf(v['steps'], `${key}.steps`) };
    case 'costModels':
      return costModelsOf(v, key);
    case 'version':
      if (!isObject(v)) return fail(`${key} is not an object`);
      return { major: intOf(v['major'], 'major'), minor: intOf(v['minor'], 'minor') };
    case 'plain':
      return toPlain(v);
  }
}

/** The ledger's parameter-update map, in contract names where the contract has them. */
export function mapParamChanges(update: { [key: string]: ExactJson }): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(update)) {
    if (value === null) continue;
    const [name, conv] = PARAM_KEYS[key] ?? [key, 'plain'];
    out[name] = convertParam(conv, value, key);
  }
  return out;
}

/* ------------------------------------------------------------------------- */
/* Parameter groups — which thresholds a ParameterChange is judged by          */
/* ------------------------------------------------------------------------- */

export type DRepParamGroup = 'ppNetworkGroup' | 'ppEconomicGroup' | 'ppTechnicalGroup' | 'ppGovGroup';

/** Conway parameter groups (CIP-1694), by ledger JSON key. */
const DREP_GROUP: Record<string, DRepParamGroup> = {
  maxBlockBodySize: 'ppNetworkGroup',
  maxTxSize: 'ppNetworkGroup',
  maxBlockHeaderSize: 'ppNetworkGroup',
  maxValueSize: 'ppNetworkGroup',
  maxTxExecutionUnits: 'ppNetworkGroup',
  maxBlockExecutionUnits: 'ppNetworkGroup',
  maxCollateralInputs: 'ppNetworkGroup',
  txFeePerByte: 'ppEconomicGroup',
  txFeeFixed: 'ppEconomicGroup',
  stakeAddressDeposit: 'ppEconomicGroup',
  stakePoolDeposit: 'ppEconomicGroup',
  monetaryExpansion: 'ppEconomicGroup',
  treasuryCut: 'ppEconomicGroup',
  minPoolCost: 'ppEconomicGroup',
  utxoCostPerByte: 'ppEconomicGroup',
  executionUnitPrices: 'ppEconomicGroup',
  minFeeRefScriptCostPerByte: 'ppEconomicGroup',
  poolPledgeInfluence: 'ppTechnicalGroup',
  poolRetireMaxEpoch: 'ppTechnicalGroup',
  stakePoolTargetNum: 'ppTechnicalGroup',
  costModels: 'ppTechnicalGroup',
  collateralPercentage: 'ppTechnicalGroup',
  poolVotingThresholds: 'ppGovGroup',
  dRepVotingThresholds: 'ppGovGroup',
  committeeMinSize: 'ppGovGroup',
  committeeMaxTermLength: 'ppGovGroup',
  govActionLifetime: 'ppGovGroup',
  govActionDeposit: 'ppGovGroup',
  dRepDeposit: 'ppGovGroup',
  dRepActivity: 'ppGovGroup',
};

/** The security group: the only parameters stake pools vote on. */
const SECURITY = new Set([
  'maxBlockBodySize',
  'maxTxSize',
  'maxBlockHeaderSize',
  'maxValueSize',
  'maxBlockExecutionUnits',
  'txFeePerByte',
  'txFeeFixed',
  'utxoCostPerByte',
  'govActionDeposit',
  'minFeeRefScriptCostPerByte',
]);

export interface ParamGroups {
  drep: DRepParamGroup[];
  security: boolean;
}

/**
 * Groups a change touches. An unrecognised key is judged by every group, the
 * conservative reading: a threshold shown too high is visibly wrong, one shown
 * too low reads as passing.
 */
export function paramGroups(keys: readonly string[]): ParamGroups {
  const drep = new Set<DRepParamGroup>();
  let security = false;
  for (const key of keys) {
    const group = DREP_GROUP[key];
    if (group) drep.add(group);
    else ['ppNetworkGroup', 'ppEconomicGroup', 'ppTechnicalGroup', 'ppGovGroup'].forEach((g) => drep.add(g as DRepParamGroup));
    if (SECURITY.has(key) || !group) security = true;
  }
  return { drep: [...drep], security };
}

/* ------------------------------------------------------------------------- */
/* Bodies                                                                     */
/* ------------------------------------------------------------------------- */

/** `{ keyHash }` / `{ scriptHash }` → hash and kind. */
function credential(v: ExactJson | undefined, what: string): { hash: string; isScript: boolean } {
  if (!isObject(v)) return fail(`${what} is not a credential`);
  const key = asString(v['keyHash']);
  const script = asString(v['scriptHash']);
  if (key) return { hash: key.toLowerCase(), isScript: false };
  if (script) return { hash: script.toLowerCase(), isScript: true };
  return fail(`${what} is not a credential`);
}

const hashOrNull = (v: ExactJson | undefined): string | null => {
  const s = asString(v);
  return s ? s.toLowerCase() : null;
};

export interface DecodedBody {
  body: GovActionBody;
  /** Previous action in the same lineage; null at its head or for a type with no lineage. */
  previous: GovActionRef | null;
  /** Ledger keys of a ParameterChange, for threshold selection. */
  paramKeys?: string[];
}

/** `{ txId, govActionIx }` → a GovActionRef; null stays null. */
function prevOf(v: ExactJson | undefined): GovActionRef | null {
  if (v === null || v === undefined) return null;
  if (!isObject(v)) return fail('previous action is not an object');
  const txHash = asString(v['txId']);
  const index = intOf(v['govActionIx'], 'previous action index');
  if (!txHash || !isHex(txHash, 32)) return fail('previous action has no transaction id');
  const hash = txHash.toLowerCase();
  return { id: encodeGovActionId(hash, index), txHash: hash, index };
}

export function decodeBody(type: GovActionType, description: ExactJson | undefined, network: NetworkId): DecodedBody {
  if (!isObject(description)) return fail('description is not an object');
  if (asString(description['tag']) !== LEDGER_TAG[type]) {
    return fail(`description tag ${String(asString(description['tag']))} does not match ${type}`);
  }
  if (type === 'InfoAction') return { body: { type }, previous: null };
  if (type === 'NoConfidence') return { body: { type }, previous: prevOf(description['contents']) };

  const contents = asArray(description['contents']) ?? fail(`${type} has no contents`);

  switch (type) {
    case 'ParameterChange': {
      const update = contents[1];
      if (!isObject(update)) return fail('parameter update is not an object');
      return {
        body: {
          type,
          changes: mapParamChanges(update),
          guardrailsScriptHash: hashOrNull(contents[2]),
        },
        previous: prevOf(contents[0]),
        paramKeys: Object.keys(update).filter((k) => update[k] !== null),
      };
    }
    case 'HardForkInitiation': {
      const version = contents[1];
      if (!isObject(version)) return fail('protocol version is not an object');
      return {
        body: { type, protocolVersion: { major: intOf(version['major'], 'major'), minor: intOf(version['minor'], 'minor') } },
        previous: prevOf(contents[0]),
      };
    }
    case 'TreasuryWithdrawals': {
      const pairs = asArray(contents[0]) ?? fail('withdrawals are not a list');
      let total = 0n;
      const withdrawals = pairs.map((pair, i) => {
        const [account, amount] = asArray(pair) ?? fail(`withdrawal ${i} is not a pair`);
        if (!isObject(account)) return fail(`withdrawal ${i} has no reward account`);
        const cred = credential(account['credential'], `withdrawal ${i} credential`);
        const lovelace = lovelaceOf(amount, `withdrawal ${i} amount`);
        total += BigInt(lovelace);
        return { stakeAddress: encodeStakeAddress(cred.hash, cred.isScript, network), amount: lovelace };
      });
      return {
        body: { type, withdrawals, totalAmount: total.toString(), guardrailsScriptHash: hashOrNull(contents[1]) },
        previous: null,
      };
    }
    case 'UpdateCommittee': {
      const removed = (asArray(contents[1]) ?? fail('removed members are not a list')).map((c, i) => {
        const cred = credential(c, `removed member ${i}`);
        return { coldCredential: encodeCommitteeColdId(cred.hash, cred.isScript) };
      });
      const addedMap = contents[2];
      if (!isObject(addedMap)) return fail('added members are not an object');
      const added = Object.entries(addedMap).map(([key, epoch]) => {
        const m = /^(keyHash|scriptHash)-([0-9a-fA-F]{56})$/.exec(key) ?? fail(`added member key ${key}`);
        return {
          coldCredential: encodeCommitteeColdId(m[2]!.toLowerCase(), m[1] === 'scriptHash'),
          termExpiryEpoch: intOf(epoch, `term of ${key}`),
        };
      });
      return { body: { type, added, removed, quorum: ratioOf(contents[3], 'quorum') }, previous: prevOf(contents[0]) };
    }
    case 'NewConstitution': {
      const constitution = contents[1];
      if (!isObject(constitution)) return fail('constitution is not an object');
      const anchor = constitution['anchor'];
      if (!isObject(anchor)) return fail('constitution has no anchor');
      const url = asString(anchor['url']) ?? fail('constitution anchor has no url');
      const dataHash = asString(anchor['dataHash']) ?? fail('constitution anchor has no hash');
      return {
        body: { type, anchor: { url, dataHash: dataHash.toLowerCase() }, guardrailsScriptHash: hashOrNull(constitution['script']) },
        previous: prevOf(contents[0]),
      };
    }
  }
}
