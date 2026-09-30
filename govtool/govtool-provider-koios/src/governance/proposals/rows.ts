/**
 * A Koios `/proposal_list` row: status derivation, lifecycle, and mapping to
 * the contract's GovAction (aggregates are attached by the caller).
 */
import type {
  EpochStamp,
  GovAction,
  GovActionLifecycle,
  GovActionRef,
  GovActionStatus,
  NetworkId,
} from '@govtool/data-providers/chain-data';

import { internal } from '../../errors';
import { decodeGovActionId, encodeGovActionId, isHex } from '../../ids';
import { toIso, toLovelace, toInt } from '../../numbers';
import type { ProposalRow } from '../../rows';
import { toAnchor } from '../dreps/directory';
import { decodeBody } from './body';

/** Columns every proposal read selects; `proposal_description` is the typed body's source. */
export const PROPOSAL_SELECT =
  'block_time,proposal_id,proposal_tx_hash,proposal_index,proposal_type,proposal_description,' +
  'previous_gov_action_proposal_id,deposit,return_address,proposed_epoch,ratified_epoch,enacted_epoch,' +
  'dropped_epoch,expired_epoch,expiration,meta_url,meta_hash';

/**
 * Status predicates as PostgREST `or=(…)` members, with the same precedence as
 * `deriveStatus`: enacted > ratified > expired > dropped > live. Koios (like
 * db-sync, whose columns it serves) sets `dropped_epoch` on expired actions as
 * well — the epoch after they expire, when they leave the ledger state — so
 * `dropped` means dropped WITHOUT expiring.
 */
export const STATUS_FILTER: Record<GovActionStatus, string> = {
  enacted: 'enacted_epoch.not.is.null',
  ratified: 'and(enacted_epoch.is.null,ratified_epoch.not.is.null)',
  expired: 'and(enacted_epoch.is.null,ratified_epoch.is.null,expired_epoch.not.is.null)',
  dropped: 'and(enacted_epoch.is.null,ratified_epoch.is.null,expired_epoch.is.null,dropped_epoch.not.is.null)',
  live: 'and(enacted_epoch.is.null,ratified_epoch.is.null,expired_epoch.is.null,dropped_epoch.is.null)',
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

/**
 * The epoch the vote aggregate is taken at: ratified, expired or dropped in,
 * or the current epoch while live — Koios' `epoch_of_interest`, and the db-sync
 * provider's tally epoch.
 */
export const tallyEpoch = (row: EpochColumns, currentEpoch: number): number =>
  row.ratified_epoch ?? row.expired_epoch ?? row.dropped_epoch ?? currentEpoch;

const epochOnly = (epoch: number | null): EpochStamp | null => (epoch === null ? null : { epoch });

/**
 * Lifecycle. Koios dates submission by block time and every later transition
 * by epoch alone. `expires` is Koios' `expiration`, which is db-sync's
 * column: the same epoch the db-sync provider reports (Decisions OPEN-76).
 */
export function toLifecycle(row: ProposalRow): GovActionLifecycle {
  const status = deriveStatus(row);
  const submitted: EpochStamp = { epoch: toInt(row.proposed_epoch), time: toIso(row.block_time) };
  return {
    status,
    submitted,
    submittedTx: { txHash: row.proposal_tx_hash, index: row.proposal_index, at: submitted },
    expires: epochOnly(row.expiration),
    ratifiedAt: epochOnly(row.ratified_epoch),
    enactedAt: epochOnly(row.enacted_epoch),
    droppedAt: status === 'dropped' ? epochOnly(row.dropped_epoch) : null,
    expiredAt: epochOnly(row.expired_epoch),
  };
}

/** A CIP-129 action id Koios sent, checked against its own tx hash and index. */
export function refOf(id: string, txHash?: string, index?: number): GovActionRef {
  const parts = decodeGovActionId(id);
  if ((txHash !== undefined && parts.txHash !== txHash.toLowerCase()) || (index !== undefined && parts.index !== index)) {
    throw internal('Koios sent a governance action id that disagrees with its tx hash and index', { id });
  }
  return { id: encodeGovActionId(parts.txHash, parts.index), txHash: parts.txHash, index: parts.index };
}

export interface MappedProposal {
  action: GovAction;
  paramKeys?: string[];
}

export function toGovAction(row: ProposalRow, network: NetworkId): MappedProposal {
  if (!isHex(row.proposal_tx_hash, 32)) throw internal('Koios sent a malformed proposal tx hash');
  const ref = refOf(row.proposal_id, row.proposal_tx_hash, row.proposal_index);
  const { body, paramKeys } = decodeBody(row.proposal_type, row.proposal_description, network);
  const action: GovAction = {
    ...ref,
    type: body.type,
    body,
    lifecycle: toLifecycle(row),
    anchor: toAnchor(row.meta_url, row.meta_hash),
    deposit: row.deposit === null ? null : toLovelace(row.deposit),
    depositReturnAddress: row.return_address,
    previousAction: row.previous_gov_action_proposal_id === null ? null : refOf(row.previous_gov_action_proposal_id),
  };
  return paramKeys ? { action, paramKeys } : { action };
}
