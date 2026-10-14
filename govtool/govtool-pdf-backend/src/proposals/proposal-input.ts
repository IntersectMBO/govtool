// Writable fields of a proposal content (SPEC §8.2 POST /proposals, reused by
// §8.3 POST /proposal-contents). Pure: reads the `data` payload, applies the
// type, length and per-type rules, and returns what the service stores.
// Everything not read here (ids, counters, owners, submission state) is
// ignored, so the server-forced fields cannot be set by a client (Δ3).

import { StakeAddress } from 'libcardano';
import type { DataPayload } from '../common/body';
import { badRequestDetails, validationError } from '../common/errors';
import { readArray, readBool, readIntRef, readString, readText } from '../common/fields';

export const GOV_ACTION_TREASURY = 2;
export const GOV_ACTION_CONSTITUTION = 3;
export const GOV_ACTION_HARD_FORK = 6;

export const MAX_COMPONENTS = 25;

export interface LinkInput {
  link: string;
  text: string | null;
}

export interface WithdrawalInput {
  receivingAddress: string | null;
  amount: number | null;
}

export interface ConstitutionInput {
  constitutionUrl: string | null;
  haveGuardrailsScript: boolean | null;
  guardrailsScriptUrl: string | null;
  guardrailsScriptHash: string | null;
}

export interface HardForkInput {
  previousGaHash: string | null;
  previousGaId: string | null;
  major: string | null;
  minor: string | null;
}

export interface ContentInput {
  govActionTypeId: number;
  name: string;
  abstract: string;
  motivation: string;
  rationale: string;
  isDraft: boolean;
  links: LinkInput[];
  withdrawals: WithdrawalInput[];
  /** Present only for type 3 when the client sent the object. */
  constitution: ConstitutionInput | null;
  /** Present only for type 6 when the create rule says a row is due. */
  hardFork: HardForkInput | null;
}

export interface ContentInputOptions {
  /** `CARDANO_NETWORK_ID`: withdrawal addresses must be on it when set. */
  networkId: 0 | 1 | null;
  /**
   * When the hard-fork row is created: `any-field` (POST /proposals: any
   * non-empty field) or `previous-ga-id` (POST /proposal-contents).
   */
  hardForkRule: 'any-field' | 'previous-ga-id';
}

type Obj = Record<string, unknown>;
const isObj = (v: unknown): v is Obj => typeof v === 'object' && v !== null && !Array.isArray(v);

/**
 * A relation object as pdf-ui may echo it: `{data: {id, attributes: {...}}}`
 * or `{data: null}` (draft restore) is unwrapped; a plain object is kept.
 * Returns undefined when absent or null.
 */
export function unwrapRelation(v: unknown, field: string): Obj | undefined {
  if (v === undefined || v === null) return undefined;
  if (!isObj(v)) throw validationError(`${field} is invalid`);
  if (Object.prototype.hasOwnProperty.call(v, 'data')) {
    const d = v.data;
    if (d === null || d === undefined) return undefined;
    if (!isObj(d)) throw validationError(`${field} is invalid`);
    if (isObj(d.attributes)) return d.attributes;
    return d;
  }
  return v;
}

const emptyToNull = (s: string | null | undefined): string | null => (s === undefined || s === '' ? null : s);

/** A number or numeric string that is finite and ≥ 0, else null. */
export function parseAmount(v: unknown): number | null {
  if (typeof v === 'number') return Number.isFinite(v) && v >= 0 ? v : null;
  if (typeof v === 'string' && v.trim() !== '') {
    const n = Number(v.trim());
    return Number.isFinite(n) && n >= 0 ? n : null;
  }
  return null;
}

/** http:, https: or ipfs: (Δ24). */
export function isAllowedDocumentUrl(v: unknown): boolean {
  if (typeof v !== 'string' || v.trim() === '') return false;
  try {
    const u = new URL(v.trim());
    return u.protocol === 'http:' || u.protocol === 'https:' || u.protocol === 'ipfs:';
  } catch {
    return false;
  }
}

export function isValidStakeAddress(v: unknown, networkId: 0 | 1 | null): boolean {
  if (typeof v !== 'string' || v.trim() === '') return false;
  try {
    const a = StakeAddress.fromBech32(v.trim());
    return networkId === null || a.networkId === networkId;
  } catch {
    return false;
  }
}

function readLinks(d: DataPayload): LinkInput[] {
  const raw = readArray(d, 'proposal_links', MAX_COMPONENTS) ?? [];
  const out: LinkInput[] = [];
  for (const item of raw) {
    if (!isObj(item)) throw validationError('proposal_links is invalid');
    const link = readString(item, 'prop_link', { max: 2048 });
    const text = readString(item, 'prop_link_text');
    // A blank link row carries nothing to show; drop it rather than store ''.
    if (link === undefined || link === null || link.trim() === '') continue;
    // http, https or ipfs only (Δ47): no `javascript:` hrefs.
    if (!isAllowedDocumentUrl(link)) throw validationError('prop_link is invalid');
    out.push({ link, text: text ?? null });
  }
  return out;
}

function readWithdrawals(d: DataPayload): Array<WithdrawalInput & { rawAmount: unknown }> {
  const raw = readArray(d, 'proposal_withdrawals', MAX_COMPONENTS) ?? [];
  return raw.map((item) => {
    if (!isObj(item)) throw validationError('proposal_withdrawals is invalid');
    const receivingAddress = readString(item, 'prop_receiving_address', { max: 200 }) ?? null;
    const rawAmount = item.prop_amount;
    if (
      rawAmount !== undefined &&
      rawAmount !== null &&
      typeof rawAmount !== 'number' &&
      typeof rawAmount !== 'string'
    ) {
      throw validationError('prop_amount is invalid');
    }
    return { receivingAddress, amount: parseAmount(rawAmount), rawAmount };
  });
}

function readConstitution(o: Obj): ConstitutionInput {
  const have = o.prop_have_guardrails_script;
  return {
    constitutionUrl: emptyToNull(readString(o, 'prop_constitution_url', { max: 2048 })),
    // true and false/null are meaningful; any other value is stored as false.
    haveGuardrailsScript: have === true ? true : have === null || have === undefined ? null : false,
    guardrailsScriptUrl: emptyToNull(readString(o, 'prop_guardrails_script_url', { max: 2048 })),
    guardrailsScriptHash: emptyToNull(readString(o, 'prop_guardrails_script_hash')),
  };
}

function validateConstitution(c: ConstitutionInput | null): void {
  if (!c) throw badRequestDetails('proposal_constitution_content is required for Constitution action');
  if (!isAllowedDocumentUrl(c.constitutionUrl)) {
    throw badRequestDetails('prop_constitution_url is required and must be a valid URL (IPFS is allowed)');
  }
  if (c.haveGuardrailsScript === true) {
    if (!isAllowedDocumentUrl(c.guardrailsScriptUrl)) {
      throw badRequestDetails(
        'prop_guardrails_script_url is required and must be a valid URL when prop_have_guardrails_script is true',
      );
    }
    if (!c.guardrailsScriptHash || c.guardrailsScriptHash.trim() === '') {
      throw badRequestDetails(
        'prop_guardrails_script_hash is required when prop_have_guardrails_script is true',
      );
    }
  } else if (c.guardrailsScriptUrl !== null || c.guardrailsScriptHash !== null) {
    throw badRequestDetails(
      'prop_guardrails_script_url and prop_guardrails_script_hash must not be provided when prop_have_guardrails_script is false or null',
    );
  }
}

function readHardFork(o: Obj): HardForkInput {
  return {
    previousGaHash: emptyToNull(readString(o, 'previous_ga_hash')),
    previousGaId: emptyToNull(readString(o, 'previous_ga_id')),
    major: emptyToNull(readString(o, 'major')),
    minor: emptyToNull(readString(o, 'minor')),
  };
}

/**
 * Parse and validate the writable content fields. `gov_action_type_id` is
 * checked for shape here; the caller checks it names a seeded type.
 */
export function readContentInput(d: DataPayload, opts: ContentInputOptions): ContentInput {
  const govActionTypeId = readIntRef(d, 'gov_action_type_id');
  if (govActionTypeId === undefined || govActionTypeId === null) {
    throw validationError('gov_action_type_id is invalid');
  }
  const isDraft = readBool(d, 'is_draft') ?? false;
  const name = readString(d, 'prop_name', { max: 80 }) ?? '';
  // pdf-ui lets a draft be saved before it has a title.
  if (!isDraft && name.trim() === '') throw validationError('prop_name is required');
  const abstract = readText(d, 'prop_abstract', 2500) ?? '';
  const motivation = readText(d, 'prop_motivation', 12000) ?? '';
  const rationale = readText(d, 'prop_rationale', 12000) ?? '';
  const links = readLinks(d);
  const withdrawalsRaw = readWithdrawals(d);

  let constitution: ConstitutionInput | null = null;
  if (govActionTypeId === GOV_ACTION_CONSTITUTION) {
    const o = unwrapRelation(d.proposal_constitution_content, 'proposal_constitution_content');
    constitution = o ? readConstitution(o) : null;
  }

  let hardFork: HardForkInput | null = null;
  if (govActionTypeId === GOV_ACTION_HARD_FORK) {
    const o = unwrapRelation(d.proposal_hard_fork_content, 'proposal_hard_fork_content');
    if (o) {
      const h = readHardFork(o);
      const due =
        opts.hardForkRule === 'previous-ga-id'
          ? h.previousGaId !== null
          : [h.previousGaHash, h.previousGaId, h.major, h.minor].some((x) => x !== null);
      hardFork = due ? h : null;
    }
  }

  if (!isDraft) {
    if (govActionTypeId === GOV_ACTION_TREASURY) {
      if (withdrawalsRaw.length === 0) throw badRequestDetails('Withdrawal parametars not exist');
      for (const w of withdrawalsRaw) {
        const amount = Number(typeof w.rawAmount === 'string' ? w.rawAmount.trim() : w.rawAmount);
        const amountOk =
          (typeof w.rawAmount === 'number' ||
            (typeof w.rawAmount === 'string' && w.rawAmount.trim() !== '')) &&
          Number.isFinite(amount) &&
          amount > 0;
        if (!amountOk || !isValidStakeAddress(w.receivingAddress, opts.networkId)) {
          throw badRequestDetails('Withdrawal addrress or amount parametars not valid');
        }
      }
    }
    if (govActionTypeId === GOV_ACTION_CONSTITUTION) validateConstitution(constitution);
  }

  return {
    govActionTypeId,
    name,
    abstract,
    motivation,
    rationale,
    isDraft,
    links,
    withdrawals: withdrawalsRaw.map(({ receivingAddress, amount }) => ({ receivingAddress, amount })),
    constitution,
    hardFork,
  };
}
