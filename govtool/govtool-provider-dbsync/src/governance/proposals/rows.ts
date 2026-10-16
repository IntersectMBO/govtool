/**
 * The proposal row: its SQL, status derivation and mapping to GovAction.
 */
import type {
  EpochStamp,
  GovAction,
  GovActionLifecycle,
  GovActionStatus,
  NetworkId,
} from '@govtool/data-providers/chain-data';

import { internal } from '../../errors';
import { encodeGovActionId } from '../../ids';
import { toInt, toIso, toLovelace } from '../../numbers';
import { decodeBody } from './body';

/**
 * The epoch an action's vote aggregate is taken at: the epoch it was
 * ratified, expired or dropped in, or the current epoch while live. `g` is
 * gov_action_proposal; `cur.no` the current epoch.
 */
export const TALLY_EPOCH_SQL = 'COALESCE(g.ratified_epoch, g.expired_epoch, g.dropped_epoch, cur.no)';

/**
 * Status predicates over `g`, keyed by contract status. The same precedence as
 * `deriveStatus`: enacted > ratified > expired > dropped > live. db-sync sets
 * dropped_epoch on expired actions as well (the epoch after they expire,
 * when they leave the ledger state), so `dropped` means dropped WITHOUT
 * expiring: removed because a competing action in its lineage was enacted.
 */
export const STATUS_SQL: Record<GovActionStatus, string> = {
  enacted: 'g.enacted_epoch IS NOT NULL',
  ratified: 'g.enacted_epoch IS NULL AND g.ratified_epoch IS NOT NULL',
  expired: 'g.enacted_epoch IS NULL AND g.ratified_epoch IS NULL AND g.expired_epoch IS NOT NULL',
  dropped:
    'g.enacted_epoch IS NULL AND g.ratified_epoch IS NULL AND g.expired_epoch IS NULL AND g.dropped_epoch IS NOT NULL',
  live: 'g.enacted_epoch IS NULL AND g.ratified_epoch IS NULL AND g.expired_epoch IS NULL AND g.dropped_epoch IS NULL',
};

export interface EpochColumns {
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
}

export function deriveStatus(row: EpochColumns): GovActionStatus {
  if (row.enacted_epoch !== null) return 'enacted';
  if (row.ratified_epoch !== null) return 'ratified';
  if (row.expired_epoch !== null) return 'expired';
  if (row.dropped_epoch !== null) return 'dropped';
  return 'live';
}

/** Columns selected for a GovAction, over `g` (proposal), `t` (its tx), `b` (its block), `cur`. */
export const PROPOSAL_COLUMNS = `
  g.id::text AS id, encode(t.hash, 'hex') AS tx_hash, g.index, g.type::text AS db_type,
  g.description::text AS description, g.deposit::text AS deposit, sa.view AS return_address,
  g.expiration, g.ratified_epoch, g.enacted_epoch, g.dropped_epoch, g.expired_epoch,
  ${TALLY_EPOCH_SQL} AS tally_epoch,
  b.epoch_no AS sub_epoch, b.slot_no AS sub_slot, b.block_no AS sub_block, b.time AS sub_time,
  va.url AS anchor_url, encode(va.data_hash, 'hex') AS anchor_hash,
  encode(pt.hash, 'hex') AS prev_tx_hash, pg.index AS prev_index`;

/** Joins for PROPOSAL_COLUMNS; `g`, `t` and `cur` are bound by the caller. */
export const PROPOSAL_JOINS = `
  JOIN block b ON b.id = t.block_id
  LEFT JOIN stake_address sa ON sa.id = g.return_address
  LEFT JOIN voting_anchor va ON va.id = g.voting_anchor_id
  LEFT JOIN gov_action_proposal pg ON pg.id = g.prev_gov_action_proposal
  LEFT JOIN tx pt ON pt.id = pg.tx_id`;

export interface ProposalRow extends EpochColumns {
  id: string;
  tx_hash: string;
  index: string | number;
  db_type: string;
  description: string;
  deposit: string | null;
  return_address: string | null;
  expiration: number | null;
  tally_epoch: number;
  sub_epoch: number | null;
  sub_slot: string | number | null;
  sub_block: string | number | null;
  sub_time: Date | string;
  anchor_url: string | null;
  anchor_hash: string | null;
  prev_tx_hash: string | null;
  prev_index: string | number | null;
  total_count?: string | number;
}

const epochOnly = (epoch: number | null): EpochStamp | null => (epoch === null ? null : { epoch });

export function toLifecycle(row: ProposalRow): GovActionLifecycle {
  if (row.sub_epoch === null) throw internal('Governance action in a block with no epoch');
  const status = deriveStatus(row);
  const submitted: EpochStamp = {
    epoch: row.sub_epoch,
    ...(row.sub_slot === null ? {} : { slot: toInt(row.sub_slot) }),
    ...(row.sub_block === null ? {} : { block: toInt(row.sub_block) }),
    time: toIso(row.sub_time),
  };
  return {
    status,
    submitted,
    submittedTx: {
      txHash: row.tx_hash,
      index: toInt(row.index),
      ...(submitted.block === undefined ? {} : { block: submitted.block }),
      at: submitted,
    },
    expires: epochOnly(row.expiration),
    ratifiedAt: epochOnly(row.ratified_epoch),
    enactedAt: epochOnly(row.enacted_epoch),
    // An expired action also carries dropped_epoch in db-sync; it was not
    // dropped in the contract's sense, so droppedAt stays null for it.
    droppedAt: status === 'dropped' ? epochOnly(row.dropped_epoch) : null,
    expiredAt: epochOnly(row.expired_epoch),
  };
}

export interface MappedProposal {
  action: GovAction;
  paramKeys?: string[];
}

/** Map a proposal row. Aggregates are attached by the caller. */
export function toGovAction(row: ProposalRow, network: NetworkId): MappedProposal {
  const index = toInt(row.index);
  const { body, paramKeys } = decodeBody(row.db_type, row.description, network);
  const action: GovAction = {
    id: encodeGovActionId(row.tx_hash, index),
    txHash: row.tx_hash,
    index,
    type: body.type,
    body,
    lifecycle: toLifecycle(row),
    anchor: row.anchor_url !== null && row.anchor_hash !== null ? { url: row.anchor_url, dataHash: row.anchor_hash } : null,
    deposit: row.deposit === null ? null : toLovelace(row.deposit),
    depositReturnAddress: row.return_address,
    previousAction:
      row.prev_tx_hash !== null && row.prev_index !== null
        ? {
            id: encodeGovActionId(row.prev_tx_hash, toInt(row.prev_index)),
            txHash: row.prev_tx_hash,
            index: toInt(row.prev_index),
          }
        : null,
  };
  return paramKeys ? { action, paramKeys } : { action };
}
