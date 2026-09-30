/**
 * The DRep directory, `/governance/dreps`, read whole.
 *
 * On hosted Blockfrost (v6.8) directory rows are HYDRATED: each carries the
 * DRep's stake, script flag, retired and expired flags, last active epoch and
 * its anchor. So the whole directory — about 1.7k rows, 18 pages on mainnet —
 * is enough to filter, sort, count and total every DRep without a per-DRep
 * read. Only the registration certificates need one (see ./registration).
 *
 * Three things the rows contain that are not DReps, all found on mainnet:
 *   - the two predefined targets, `drep_always_abstain` and
 *     `drep_always_no_confidence`, listed as if they were DReps. Their
 *     amounts are the predefined stake totals; they are never DReps here.
 *   - credentials that never registered: delegated to (legal before protocol
 *     10) but with no registration certificate. They show `last_active_epoch:
 *     null` and an empty `/updates`; 25 of them on mainnet. A registration is
 *     itself activity, so every registered DRep has a last active epoch.
 */
import type { Anchor, DRepStatus } from '@govtool/data-providers/chain-data';

import type { Session } from '../../context';
import { internal } from '../../errors';
import { decodeDRepId, encodeDRepId } from '../../ids';
import { toLovelace } from '../../numbers';

export interface BfDRepRow {
  drep_id: string;
  hex: string;
  amount: string;
  has_script: boolean;
  retired: boolean;
  expired: boolean;
  last_active_epoch: number | null;
  /** Present on directory rows; the anchor with the document Blockfrost fetched, if any. */
  metadata?: { url: string; hash: string } | null;
}

export interface DirectoryDRep {
  /** CIP-129, re-encoded from the credential. */
  id: string;
  hash: string;
  isScript: boolean;
  amount: string;
  status: DRepStatus;
  /**
   * `null` = registered with no anchor. Blockfrost keeps the anchor even when
   * it could not fetch the document (the row then has `json_metadata: null`
   * and an `error`), so a null here is the anchor's absence, not a failed fetch.
   */
  anchor: Anchor | null;
  lastActiveEpoch: number;
}

export interface Directory {
  dreps: DirectoryDRep[];
  alwaysAbstain: string;
  alwaysNoConfidence: string;
  /** Credentials listed that never registered; excluded from `dreps`. */
  neverRegistered: string[];
}

/**
 * Status from the ledger's view as Blockfrost reports it: `retired` wins,
 * then `expired` (current epoch past the DRep's expiry), else active. On
 * mainnet the `expired` flag agreed with last-active + drepActivity for every
 * one of 1,685 registered DReps; the expiry epoch itself is not served, so no
 * `expiryEpoch` is emitted.
 */
export const statusOf = (row: Pick<BfDRepRow, 'retired' | 'expired'>): DRepStatus =>
  row.retired ? 'retired' : row.expired ? 'inactive' : 'active';

/** A row as a DRep, or `undefined` for a credential that never registered. */
export function toDirectoryDRep(row: BfDRepRow, anchorRow?: { url: string; hash: string } | null): DirectoryDRep | undefined {
  if (row.last_active_epoch === null) return undefined;
  const { hash, isScript } = decodeDRepId(row.drep_id);
  if (isScript !== row.has_script || (row.hex && row.hex.slice(2).toLowerCase() !== hash)) {
    throw internal('Blockfrost DRep id disagrees with its hex or script flag', { id: row.drep_id });
  }
  const meta = anchorRow === undefined ? row.metadata : anchorRow;
  return {
    id: encodeDRepId(hash, isScript),
    hash,
    isScript,
    amount: toLovelace(row.amount, 'DRep amount'),
    status: statusOf(row),
    anchor: meta && meta.url && meta.hash ? { url: meta.url, dataHash: meta.hash.toLowerCase() } : null,
    lastActiveEpoch: row.last_active_epoch,
  };
}

export function assembleDirectory(rows: readonly BfDRepRow[]): Directory {
  let alwaysAbstain: string | undefined;
  let alwaysNoConfidence: string | undefined;
  const dreps: DirectoryDRep[] = [];
  const neverRegistered: string[] = [];
  const seen = new Set<string>();
  for (const row of rows) {
    if (row.drep_id === 'drep_always_abstain') {
      alwaysAbstain = toLovelace(row.amount, 'always-abstain amount');
      continue;
    }
    if (row.drep_id === 'drep_always_no_confidence') {
      alwaysNoConfidence = toLovelace(row.amount, 'always-no-confidence amount');
      continue;
    }
    const drep = toDirectoryDRep(row);
    if (!drep) {
      neverRegistered.push(row.drep_id);
      continue;
    }
    // Offset paging can repeat a row if the directory grows mid-read.
    if (seen.has(drep.id)) continue;
    seen.add(drep.id);
    dreps.push(drep);
  }
  if (alwaysAbstain === undefined || alwaysNoConfidence === undefined) {
    throw internal('Blockfrost DRep directory lacks the predefined voting-option rows');
  }
  return { dreps, alwaysAbstain, alwaysNoConfidence, neverRegistered };
}

export const loadDirectory = (s: Session): Promise<Directory> =>
  s.once('directory', async () => assembleDirectory(await s.http.getAll<BfDRepRow>('/governance/dreps')));
