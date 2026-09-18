import type {
  EnactedActionSummary,
  EpochStamp,
  GovAction,
  GovActionBody,
  GovActionLifecycle,
  GovActionRef,
  GovActionStatus,
  GovActionType,
  Ratio,
} from '@govtool/data-providers/chain-data';

import { internal } from '../common/errors';
import { encodeCip129GovActionId, parseGovActionId } from '../common/ids';
import {
  sumLovelace,
  toIsoString,
  toLovelace,
  toNullableLovelace,
  toStrictInteger,
} from '../common/numbers';
import { asRecord, projectGovActionMetadata } from './metadata.mapper';
import type { KoiosProposalType, ProposalRow } from '../rows';

/**
 * Koios kept the pre-ratification name `NewCommittee`; the ledger, and this
 * contract, call the action `UpdateCommittee`. Confusingly Koios' *own*
 * `proposal_description.tag` already says `UpdateCommittee` — only the
 * `proposal_type` column is stale — so the rename is applied on the column
 * and the tag is trusted as-is.
 */
export function toContractType(type: KoiosProposalType): GovActionType {
  return type === 'NewCommittee' ? 'UpdateCommittee' : type;
}

export function toKoiosType(type: GovActionType): KoiosProposalType {
  return type === 'UpdateCommittee' ? 'NewCommittee' : type;
}

export function mapGovActionRef(row: ProposalRow): GovActionRef {
  return {
    id: row.proposal_id,
    txHash: row.proposal_tx_hash,
    index: row.proposal_index,
  };
}

/**
 * Lifecycle. Koios records the epoch each transition happened in but no
 * timestamp for any of them except submission, so every `EpochStamp` after
 * `submitted` carries an epoch and an empty time — which is why `EpochStamp`
 * keeps `epoch` optional and `time` required, and why these are built through
 * one helper rather than inline.
 */
export function mapLifecycle(row: ProposalRow): GovActionLifecycle {
  const submittedTime = toIsoString(row.block_time);
  return {
    status: deriveStatus(row),
    submitted: {
      epoch: toStrictInteger(row.proposed_epoch),
      time: submittedTime,
    },
    submittedTx: {
      txHash: row.proposal_tx_hash,
      index: row.proposal_index,
      at: { epoch: toStrictInteger(row.proposed_epoch), time: submittedTime },
    },
    expires: row.expiration === null ? null : { epoch: row.expiration },
    ratifiedAt: epochStamp(row.ratified_epoch),
    enactedAt: epochStamp(row.enacted_epoch),
    droppedAt: epochStamp(row.dropped_epoch),
    expiredAt: epochStamp(row.expired_epoch),
  };
}

/**
 * Koios records every lifecycle transition after submission as an epoch
 * number and nothing else, so these stamps carry an epoch and no time — which
 * is what `EpochStamp` allows, and why a consumer narrows with `hasTime`
 * before formatting one.
 */
function epochStamp(epoch: number | null): EpochStamp | null {
  return epoch === null ? null : { epoch };
}

export function deriveStatus(row: ProposalRow): GovActionStatus {
  if (row.enacted_epoch !== null) return 'enacted';
  if (row.ratified_epoch !== null) return 'ratified';
  if (row.dropped_epoch !== null) return 'dropped';
  if (row.expired_epoch !== null) return 'expired';
  return 'live';
}

export function mapGovAction(row: ProposalRow): GovAction {
  const action: GovAction = {
    ...mapGovActionRef(row),
    type: toContractType(row.proposal_type),
    lifecycle: mapLifecycle(row),
    deposit: toNullableLovelace(row.deposit),
    depositReturnAddress: row.return_address,
    proposedBy: row.return_address,
    previousAction: mapPreviousAction(row.previous_gov_action_proposal_id),
    metadata: projectGovActionMetadata({
      url: row.meta_url,
      hash: row.meta_hash,
      json: row.meta_json,
      isValid: row.meta_is_valid,
      comment: row.meta_comment,
    }),
    rawBody: row.proposal_description,
  };
  const body = mapBody(row);
  if (body !== undefined) {
    action.body = body;
  }
  return action;
}

export function mapEnactedSummary(row: ProposalRow): EnactedActionSummary {
  const summary: EnactedActionSummary = {
    type: toContractType(row.proposal_type),
    action: mapGovActionRef(row),
    rawBody: row.proposal_description,
  };
  if (row.enacted_epoch !== null) {
    summary.enactedAt = { epoch: row.enacted_epoch };
  }
  summary.submittedTx = {
    txHash: row.proposal_tx_hash,
    index: row.proposal_index,
  };
  const body = mapBody(row);
  if (body !== undefined) {
    summary.body = body;
  }
  return summary;
}

/**
 * `previous_gov_action_proposal_id` is already a CIP-129 id, so the only work
 * is splitting out the tx hash and index the ref also carries.
 */
export function mapPreviousAction(id: string | null): GovActionRef | null {
  if (id === null) return null;
  const parts = parseGovActionId(id);
  return { id, txHash: parts.txHash, index: parts.index };
}

/* ------------------------------------------------------------------------- */
/* Typed bodies                                                               */
/* ------------------------------------------------------------------------- */

/**
 * The typed action body, built from `proposal_description`.
 *
 * This is the single biggest thing Koios gives that db-sync's GovTool SQL
 * does not: `proposal_description` is the ledger's own JSON rendering of the
 * action, so all seven variants can be typed rather than handed to the
 * consumer as an untyped blob. Its shape is a Haskell sum type serialised as
 * `{ tag, contents }`, where `contents` is a positional tuple — hence the
 * index arithmetic below, which is the price of not having named fields.
 *
 * Anything that does not parse returns `undefined` and leaves the caller with
 * `rawBody`, rather than throwing: a body this provider cannot read is a
 * rendering problem for one action, not a failed request.
 */
export function mapBody(row: ProposalRow): GovActionBody | undefined {
  const description = asRecord(row.proposal_description);
  if (description === null) return undefined;

  const tag = description['tag'];
  const contents = description['contents'];
  const tuple = Array.isArray(contents) ? contents : [];

  switch (tag) {
    case 'InfoAction':
      return { type: 'InfoAction' };
    case 'NoConfidence':
      return { type: 'NoConfidence' };
    case 'ParameterChange':
      return mapParameterChange(row, tuple);
    case 'HardForkInitiation':
      return mapHardFork(tuple);
    case 'TreasuryWithdrawals':
      return mapTreasuryWithdrawals(row, tuple);
    case 'UpdateCommittee':
    case 'NewCommittee':
      return mapUpdateCommittee(tuple);
    case 'NewConstitution':
      return mapNewConstitution(tuple);
    default:
      return undefined;
  }
}

/** `contents: [previousAction | null, changes, guardrailsScriptHash | null]` */
function mapParameterChange(
  row: ProposalRow,
  tuple: unknown[],
): GovActionBody | undefined {
  const changes = row.param_proposal ?? asRecord(tuple[1]);
  if (changes === null || changes === undefined) return undefined;
  return {
    type: 'ParameterChange',
    changes,
    guardrailsScriptHash: asHash(tuple[2]),
  };
}

/** `contents: [previousAction | null, { major, minor }]` */
function mapHardFork(tuple: unknown[]): GovActionBody | undefined {
  const version = asRecord(tuple[1]);
  if (version === null) return undefined;
  const major = Number(version['major']);
  const minor = Number(version['minor']);
  if (!Number.isInteger(major) || !Number.isInteger(minor)) return undefined;
  return { type: 'HardForkInitiation', protocolVersion: { major, minor } };
}

/**
 * `contents: [[[rewardAccount, amount], …], guardrailsScriptHash | null]`
 *
 * The tuple's reward accounts are `{ network, credential }` objects, but the
 * row's own `withdrawal` array carries the same targets already encoded as
 * bech32 stake addresses — which is what the contract asks for, so it is used
 * in preference to re-encoding the credentials here.
 */
function mapTreasuryWithdrawals(
  row: ProposalRow,
  tuple: unknown[],
): GovActionBody | undefined {
  const withdrawals = (row.withdrawal ?? []).map((entry) => ({
    stakeAddress: entry.stake_address,
    amount: toLovelace(entry.amount),
  }));
  if (withdrawals.length === 0) return undefined;
  return {
    type: 'TreasuryWithdrawals',
    withdrawals,
    totalAmount: sumLovelace(...withdrawals.map((entry) => entry.amount)),
    guardrailsScriptHash: asHash(tuple[1]),
  };
}

/**
 * `contents: [previousAction | null, [removedCredential, …], { "<type>-<hash>": expiryEpoch }, quorum]`
 *
 * The added members arrive as an object whose *keys* encode both the
 * credential type and its hash — `keyHash-dc0d…` / `scriptHash-349e…` — which
 * is why they are split on the first `-` rather than read from a field.
 */
function mapUpdateCommittee(tuple: unknown[]): GovActionBody | undefined {
  const quorum = asRatio(tuple[3]);
  if (quorum === undefined) return undefined;

  const removed: { coldCredential: string; isScriptBased: boolean }[] = [];
  for (const entry of Array.isArray(tuple[1]) ? tuple[1] : []) {
    const credential = asCredential(entry);
    if (credential !== undefined) removed.push(credential);
  }

  const added: {
    coldCredential: string;
    isScriptBased: boolean;
    termExpiryEpoch: number;
  }[] = [];
  for (const [key, value] of Object.entries(asRecord(tuple[2]) ?? {})) {
    const separator = key.indexOf('-');
    if (separator === -1) continue;
    const epoch = Number(value);
    if (!Number.isInteger(epoch)) continue;
    added.push({
      coldCredential: key.slice(separator + 1),
      isScriptBased: key.slice(0, separator) === 'scriptHash',
      termExpiryEpoch: epoch,
    });
  }

  return { type: 'UpdateCommittee', added, removed, quorum };
}

/** `contents: [previousAction | null, { anchor, script }]` */
function mapNewConstitution(tuple: unknown[]): GovActionBody | undefined {
  const constitution = asRecord(tuple[1]);
  const anchor = asRecord(constitution?.['anchor']);
  const url = anchor?.['url'];
  const dataHash = anchor?.['dataHash'];
  if (typeof url !== 'string' || typeof dataHash !== 'string') return undefined;
  return {
    type: 'NewConstitution',
    anchor: { url, dataHash },
    guardrailsScriptHash: asHash(constitution?.['script']),
  };
}

function asCredential(
  value: unknown,
): { coldCredential: string; isScriptBased: boolean } | undefined {
  const record = asRecord(value);
  if (record === null) return undefined;
  const scriptHash = record['scriptHash'];
  if (typeof scriptHash === 'string') {
    return { coldCredential: scriptHash, isScriptBased: true };
  }
  const keyHash = record['keyHash'];
  if (typeof keyHash === 'string') {
    return { coldCredential: keyHash, isScriptBased: false };
  }
  return undefined;
}

function asRatio(value: unknown): Ratio | undefined {
  const record = asRecord(value);
  if (record === null) return undefined;
  const numerator = Number(record['numerator']);
  const denominator = Number(record['denominator']);
  if (!Number.isInteger(numerator) || !Number.isInteger(denominator)) {
    return undefined;
  }
  return { numerator, denominator };
}

function asHash(value: unknown): string | null {
  return typeof value === 'string' ? value : null;
}

/** Re-exported so `listByTx` can build a ref without a second parse. */
export function refFromParts(txHash: string, index: number): GovActionRef {
  if (!Number.isInteger(index)) {
    throw internal('Koios returned a proposal with a non-integer index.');
  }
  return { id: encodeCip129GovActionId(txHash, index), txHash, index };
}
