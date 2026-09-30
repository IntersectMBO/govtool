/**
 * Proposal records: the index, one proposal's record, its lifecycle and its
 * mapping to a GovAction.
 *
 * A contract GovAction needs three reads per proposal:
 *   /governance/proposals/{tx}/{i}   type, body, deposit, lifecycle epochs
 *   /txs/{tx}                        when it was submitted (the record has no date)
 *   /txs/{tx}/cbor                   its anchor, from the proposal procedure
 * The last two are per TRANSACTION and shared when one transaction carries
 * several proposals. `/metadata` is not used for the anchor: it is the
 * fetched document, and the chain's own procedure is the authority.
 */
import type {
  EpochStamp,
  GovAction,
  GovActionLifecycle,
  GovActionStatus,
  GovActionType,
  NetworkId,
} from '@govtool/data-providers/chain-data';

import { proposalAnchorsOf } from '../../cbor';
import { loadTxCbor, txStamp } from '../../chain';
import type { Session } from '../../context';
import { internal, notFound } from '../../errors';
import { encodeGovActionId } from '../../ids';
import { asString, isObject, numText, type ExactJson } from '../../json';
import { decodeBody, toGovActionType, type DecodedBody } from './body';

export interface BfProposalRef {
  id: string;
  tx_hash: string;
  cert_index: number;
  governance_type: string;
}

export interface IndexEntry {
  id: string;
  txHash: string;
  index: number;
  type: GovActionType;
  /** Position in chain order, oldest first. */
  seq: number;
}

/** Every proposal, oldest first: two requests on mainnet. */
export const loadIndex = (s: Session): Promise<IndexEntry[]> =>
  s.once('proposals:index', async () => {
    const rows = await s.http.getAll<BfProposalRef>('/governance/proposals', { batch: 2 });
    const seen = new Set<string>();
    const out: IndexEntry[] = [];
    for (const row of rows) {
      const id = encodeGovActionId(row.tx_hash, row.cert_index);
      if (row.id !== undefined && row.id !== id) throw internal('Blockfrost proposal id disagrees with its transaction', { id: row.id });
      if (seen.has(id)) continue;
      seen.add(id);
      out.push({ id, txHash: row.tx_hash, index: row.cert_index, type: toGovActionType(row.governance_type), seq: out.length });
    }
    return out;
  });

export interface ProposalRecord {
  id: string;
  txHash: string;
  index: number;
  type: GovActionType;
  description: ExactJson | undefined;
  deposit: string | null;
  returnAddress: string | null;
  ratified: number | null;
  enacted: number | null;
  dropped: number | null;
  expired: number | null;
  expiration: number | null;
}

const epochOf = (v: ExactJson | undefined, what: string): number | null => {
  if (v === null || v === undefined) return null;
  const text = numText(v);
  if (text === undefined || !/^\d+$/.test(text)) throw internal(`Blockfrost proposal ${what} is not an epoch`);
  return Number(text);
};

export function toRecord(raw: ExactJson): ProposalRecord {
  if (!isObject(raw)) throw internal('Blockfrost proposal record is not an object');
  const txHash = asString(raw['tx_hash']);
  const index = epochOf(raw['cert_index'], 'cert_index');
  if (!txHash || index === null) throw internal('Blockfrost proposal record has no transaction');
  const deposit = numText(raw['deposit']);
  return {
    id: encodeGovActionId(txHash, index),
    txHash,
    index,
    type: toGovActionType(asString(raw['governance_type']) ?? ''),
    description: raw['governance_description'],
    deposit: deposit !== undefined && /^\d+$/.test(deposit) ? BigInt(deposit).toString() : null,
    returnAddress: asString(raw['return_address']) ?? null,
    ratified: epochOf(raw['ratified_epoch'], 'ratified_epoch'),
    enacted: epochOf(raw['enacted_epoch'], 'enacted_epoch'),
    dropped: epochOf(raw['dropped_epoch'], 'dropped_epoch'),
    expired: epochOf(raw['expired_epoch'], 'expired_epoch'),
    expiration: epochOf(raw['expiration'], 'expiration'),
  };
}

export const loadRecord = (s: Session, txHash: string, index: number): Promise<ProposalRecord | null> =>
  s.once(`proposal:${txHash}#${index}`, async () => {
    const raw = await s.http.getExactOrNull(`/governance/proposals/${txHash}/${index}`);
    return raw === null ? null : toRecord(raw);
  });

export async function requireRecord(s: Session, entry: { id: string; txHash: string; index: number }): Promise<ProposalRecord> {
  const record = await loadRecord(s, entry.txHash, entry.index);
  if (!record) throw notFound('Governance action not found', { id: entry.id });
  return record;
}

/**
 * Precedence enacted > ratified > expired > dropped > live, as db-sync's
 * provider: Blockfrost reads the same db-sync columns, where an expired
 * action also carries a dropped epoch (the epoch after, when it leaves the
 * ledger state), so `dropped` means dropped WITHOUT expiring.
 */
export function deriveStatus(r: ProposalRecord): GovActionStatus {
  if (r.enacted !== null) return 'enacted';
  if (r.ratified !== null) return 'ratified';
  if (r.expired !== null) return 'expired';
  if (r.dropped !== null) return 'dropped';
  return 'live';
}

const epochOnly = (epoch: number | null): EpochStamp | null => (epoch === null ? null : { epoch });

/**
 * `expires` is Blockfrost's `expiration`, the same db-sync column the db-sync
 * provider reads, so both name the same epoch (docs/api/decisions.md OPEN-76).
 */
export function toLifecycle(r: ProposalRecord, submitted: EpochStamp): GovActionLifecycle {
  const status = deriveStatus(r);
  return {
    status,
    submitted,
    submittedTx: {
      txHash: r.txHash,
      index: r.index,
      ...(submitted.block === undefined ? {} : { block: submitted.block }),
      at: submitted,
    },
    expires: epochOnly(r.expiration),
    ratifiedAt: epochOnly(r.ratified),
    enactedAt: epochOnly(r.enacted),
    droppedAt: status === 'dropped' ? epochOnly(r.dropped) : null,
    expiredAt: epochOnly(r.expired),
  };
}

export const decodeRecord = (r: ProposalRecord, network: NetworkId): DecodedBody => decodeBody(r.type, r.description, network);

export interface Hydrated {
  record: ProposalRecord;
  action: GovAction;
  paramKeys?: string[];
}

/** The full GovAction: record + submission date + anchor. Aggregates are the caller's. */
export async function hydrate(s: Session, record: ProposalRecord, network: NetworkId): Promise<Hydrated> {
  const [submitted, cbor] = await Promise.all([txStamp(s, record.txHash), loadTxCbor(s, record.txHash)]);
  const anchor = proposalAnchorsOf(cbor)[record.index];
  if (!anchor) throw internal('Proposal transaction has no procedure at the proposal index', { id: record.id });
  const { body, previous, paramKeys } = decodeRecord(record, network);
  const action: GovAction = {
    id: record.id,
    txHash: record.txHash,
    index: record.index,
    type: body.type,
    body,
    lifecycle: toLifecycle(record, submitted),
    anchor,
    deposit: record.deposit,
    depositReturnAddress: record.returnAddress,
    previousAction: previous,
  };
  return paramKeys ? { record, action, paramKeys } : { record, action };
}
