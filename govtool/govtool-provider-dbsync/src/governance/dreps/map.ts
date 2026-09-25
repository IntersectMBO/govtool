/**
 * Row -> contract mapping for the DRep area. Pure functions, no database.
 */
import type {
  Anchor,
  DRep,
  DRepStatus,
  DRepVoteRow,
  EpochStamp,
  GovActionType,
  RegistrationEvent,
  VoteChoice,
} from '@govtool/data-providers/chain-data';

import { encodeDRepId, encodeGovActionId } from '../../ids';
import { internal } from '../../errors';
import { toInt, toIso, toLovelace } from '../../numbers';

type DbNumber = number | string | bigint;

/* ------------------------------------------------------------------------- */
/* Rows                                                                        */
/* ------------------------------------------------------------------------- */

export interface ListRow {
  id: DbNumber;
  hash: string;
  has_script: boolean;
  status: string;
  expiry_known: boolean;
  active_until: DbNumber | null;
  amount: DbNumber | null;
  snap_epoch: DbNumber | null;
  total_count?: DbNumber;
}

interface StampColumns {
  block_no: DbNumber | null;
  epoch_no: DbNumber | null;
  slot_no: DbNumber | null;
  time: Date | string | null;
}

export interface DetailRow extends StampColumns {
  event: 'registration' | 'update' | 'retirement';
  drep_hash_id: DbNumber;
  tx_hash: string;
  cert_index: DbNumber;
  deposit: DbNumber | null;
  anchor_url: string | null;
  anchor_hash: string | null;
  current_anchor_url: string | null;
  current_anchor_hash: string | null;
  has_current_anchor: boolean;
}

export interface HistoryRow extends StampColumns {
  tx_hash: string;
  cert_index: DbNumber;
  deposit: DbNumber | null;
  anchor_url: string | null;
  anchor_hash: string | null;
  total_count?: DbNumber;
}

export interface VoteRow extends StampColumns {
  type: string;
  proposal_tx_hash: string;
  proposal_index: DbNumber;
  vote: string | null;
  vote_tx_hash: string | null;
  vote_index: DbNumber | null;
  anchor_url: string | null;
  anchor_hash: string | null;
  title: string | null;
  total_count?: DbNumber;
}

/* ------------------------------------------------------------------------- */
/* Scalars                                                                     */
/* ------------------------------------------------------------------------- */

const STATUSES: readonly DRepStatus[] = ['active', 'inactive', 'retired'];

export function toStatus(value: string): DRepStatus {
  if ((STATUSES as readonly string[]).includes(value)) return value as DRepStatus;
  throw internal(`Unexpected DRep status from db-sync: ${value}`);
}

/** db-sync `govactiontype` -> contract. db-sync calls UpdateCommittee `NewCommittee`. */
const ACTION_TYPES: Record<string, GovActionType> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  NewCommittee: 'UpdateCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

export function toActionType(value: string): GovActionType {
  const mapped = ACTION_TYPES[value];
  if (!mapped) throw internal(`Unexpected governance action type from db-sync: ${value}`);
  return mapped;
}

const CHOICES: Record<string, VoteChoice> = { Yes: 'yes', No: 'no', Abstain: 'abstain' };

export function toChoice(value: string): VoteChoice {
  const mapped = CHOICES[value];
  if (!mapped) throw internal(`Unexpected vote from db-sync: ${value}`);
  return mapped;
}

/** An anchor needs both halves; db-sync's voting_anchor has both NOT NULL. */
export const toAnchor = (url: string | null, hash: string | null): Anchor | null =>
  url === null || hash === null ? null : { url, dataHash: hash };

export function toStamp(row: StampColumns): EpochStamp {
  if (row.epoch_no === null) throw internal('db-sync block has no epoch');
  return {
    epoch: toInt(row.epoch_no),
    ...(row.slot_no === null ? {} : { slot: toInt(row.slot_no) }),
    ...(row.block_no === null ? {} : { block: toInt(row.block_no) }),
    ...(row.time === null ? {} : { time: toIso(row.time) }),
  };
}

/* ------------------------------------------------------------------------- */
/* Entities                                                                    */
/* ------------------------------------------------------------------------- */

function toEvent(row: DetailRow | HistoryRow): RegistrationEvent {
  const at = toStamp(row);
  return {
    txRef: {
      txHash: row.tx_hash,
      index: toInt(row.cert_index),
      ...(at.block === undefined ? {} : { block: at.block }),
    },
    at,
    anchor: toAnchor(row.anchor_url, row.anchor_hash),
    // Only a registration carries a deposit; an update has none.
    deposit: row.deposit === null ? null : toLovelace(row.deposit),
  };
}

export const toHistoryEvent = (row: HistoryRow): RegistrationEvent => toEvent(row);

export interface Extras {
  delegatorCount?: number;
  activity?: { voted: number; votable: number };
}

/**
 * Assemble a DRep from its directory row and its registration facts. Every
 * DRep in the directory has a registration, so a missing one is a database
 * inconsistency, not an absent value.
 */
export function toDRep(row: ListRow, details: readonly DetailRow[], extras: Extras): DRep {
  const registration = details.find((d) => d.event === 'registration');
  if (!registration) throw internal('DRep registration certificate not found');
  const update = details.find((d) => d.event === 'update');
  const retirement = details.find((d) => d.event === 'retirement');
  const status = toStatus(row.status);
  const anchor = registration.has_current_anchor
    ? toAnchor(registration.current_anchor_url, registration.current_anchor_hash)
    : null;
  const isScript = row.has_script;

  const drep: DRep = {
    role: 'drep',
    id: encodeDRepId(row.hash, isScript),
    isScriptBased: isScript,
    // `kind` is derived from the anchor, never independently.
    kind: anchor === null ? 'anonymous' : 'drep',
    anchor,
    registration: {
      latest: toEvent(registration),
      latestUpdate: update ? toEvent(update) : null,
      retiredAt: status === 'retired' && retirement ? toStamp(retirement) : null,
    },
    status,
    ...(row.expiry_known && row.active_until !== null ? { expiryEpoch: toInt(row.active_until) } : {}),
    votingPower:
      row.amount === null || row.snap_epoch === null
        ? null
        : { amount: toLovelace(row.amount), basis: 'active', epoch: toInt(row.snap_epoch) },
    ...(extras.activity ? { activity: extras.activity } : {}),
    ...(extras.delegatorCount === undefined ? {} : { delegatorCount: extras.delegatorCount }),
  };
  return drep;
}

export function toVoteRow(row: VoteRow): DRepVoteRow {
  const action = {
    id: encodeGovActionId(row.proposal_tx_hash, toInt(row.proposal_index)),
    type: toActionType(row.type),
    ...(row.title === null ? {} : { title: row.title }),
  };
  if (row.vote === null || row.vote_tx_hash === null) return { voted: false, action };
  const at = toStamp(row);
  return {
    voted: true,
    action,
    choice: toChoice(row.vote),
    anchor: toAnchor(row.anchor_url, row.anchor_hash),
    txRef: {
      txHash: row.vote_tx_hash,
      ...(row.vote_index === null ? {} : { index: toInt(row.vote_index) }),
      ...(at.block === undefined ? {} : { block: at.block }),
    },
    at,
  };
}
