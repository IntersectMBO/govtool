/**
 * Voters: parsing a caller's voter id, the SQL predicate that selects its
 * votes, and mapping a vote row to the contract's VoteRecord.
 */
import type { EpochStamp, VoteCastBy, VoteChoice, VoteRecord } from '@govtool/data-providers/chain-data';

import { internal, invalidInput } from '../../errors';
import {
  decodeCommitteeColdId,
  decodeCommitteeHotId,
  decodeDRepId,
  decodePoolId,
  encodeCommitteeColdId,
  encodeCommitteeHotId,
  encodeDRepId,
  encodePoolId,
} from '../../ids';
import { toInt, toIso } from '../../numbers';

/**
 * A voter as a caller names it. A committee member may be named by its cold
 * credential (the stable id) or by a hot one; a cold id matches the votes of
 * every hot key it has authorised.
 */
export type VoterKey =
  | { kind: 'drep'; hash: string; isScript: boolean }
  | { kind: 'spo'; hash: string }
  | { kind: 'ccCold'; hash: string; isScript: boolean }
  | { kind: 'ccHot'; hash: string; isScript: boolean };

/** Decode by bech32 prefix, then validate fully; anything else is refused. */
export function parseVoterId(id: string): VoterKey {
  if (typeof id !== 'string' || id.length === 0) throw invalidInput('voterId must be a non-empty string');
  if (id.startsWith('drep1')) return { kind: 'drep', ...decodeDRepId(id) };
  if (id.startsWith('pool1')) return { kind: 'spo', hash: decodePoolId(id) };
  if (id.startsWith('cc_cold1')) return { kind: 'ccCold', ...decodeCommitteeColdId(id) };
  if (id.startsWith('cc_hot1')) return { kind: 'ccHot', ...decodeCommitteeHotId(id) };
  throw invalidInput('voterId must be a CIP-129 DRep or committee credential, or a pool1 id', { voterId: id });
}

/**
 * A predicate over `voting_procedure` aliased `vp`, with its parameters
 * appended to `params`. Values are always bound, never interpolated.
 */
export function voterPredicate(voter: VoterKey, params: unknown[]): string {
  const p = (value: unknown) => {
    params.push(value);
    return `$${params.length}`;
  };
  switch (voter.kind) {
    case 'drep':
      return `vp.drep_voter = (SELECT id FROM drep_hash WHERE raw = decode(${p(voter.hash)}, 'hex') AND has_script = ${p(voter.isScript)})`;
    case 'spo':
      return `vp.pool_voter = (SELECT id FROM pool_hash WHERE hash_raw = decode(${p(voter.hash)}, 'hex'))`;
    case 'ccHot':
      return `vp.committee_voter = (SELECT id FROM committee_hash WHERE raw = decode(${p(voter.hash)}, 'hex') AND has_script = ${p(voter.isScript)})`;
    case 'ccCold':
      return `vp.committee_voter IN (SELECT cr.hot_key_id FROM committee_registration cr
                JOIN committee_hash cold ON cold.id = cr.cold_key_id
               WHERE cold.raw = decode(${p(voter.hash)}, 'hex') AND cold.has_script = ${p(voter.isScript)})`;
  }
}

/**
 * The latest vote per voter, from a `voting_procedure` set filtered by
 * `where` (over alias `vp`). A voter may change its vote; only the last one
 * counts. Invalid votes are excluded.
 */
export const latestVotesSql = (where: string) => `
  SELECT DISTINCT ON (vp.gov_action_proposal_id, vp.voter_role, COALESCE(vp.drep_voter, vp.pool_voter, vp.committee_voter))
         vp.*
    FROM voting_procedure vp
   WHERE vp.invalid IS NULL AND (${where})
   ORDER BY vp.gov_action_proposal_id, vp.voter_role, COALESCE(vp.drep_voter, vp.pool_voter, vp.committee_voter),
            vp.tx_id DESC, vp.id DESC`;

/** Columns a VoteRecord needs, joined onto a latest-votes CTE `v`. */
export const VOTE_COLUMNS = `
  v.gov_action_proposal_id AS proposal_id,
  v.voter_role::text AS voter_role, v.vote::text AS vote, v.index AS vote_index,
  encode(vt.hash, 'hex') AS vote_tx_hash, vb.block_no AS vote_block_no, vb.epoch_no AS vote_epoch,
  vb.slot_no AS vote_slot, vb.time AS vote_time,
  encode(dh.raw, 'hex') AS drep_raw, dh.has_script AS drep_script,
  encode(ph.hash_raw, 'hex') AS pool_raw,
  encode(hot.raw, 'hex') AS hot_raw, hot.has_script AS hot_script,
  encode(cold.raw, 'hex') AS cold_raw, cold.has_script AS cold_script,
  va.url AS vote_anchor_url, encode(va.data_hash, 'hex') AS vote_anchor_hash`;

/**
 * Joins for VOTE_COLUMNS. A committee vote carries the HOT credential; the
 * cold one is the authorisation in force when the vote was cast, falling back
 * to the latest one for the hot key.
 */
export const VOTE_JOINS = `
  JOIN tx vt ON vt.id = v.tx_id
  JOIN block vb ON vb.id = vt.block_id
  LEFT JOIN drep_hash dh ON dh.id = v.drep_voter
  LEFT JOIN pool_hash ph ON ph.id = v.pool_voter
  LEFT JOIN committee_hash hot ON hot.id = v.committee_voter
  LEFT JOIN LATERAL (
    SELECT ch.raw, ch.has_script
      FROM committee_registration cr
      JOIN committee_hash ch ON ch.id = cr.cold_key_id
     WHERE cr.hot_key_id = v.committee_voter
     ORDER BY (cr.tx_id <= v.tx_id) DESC, cr.tx_id DESC, cr.id DESC
     LIMIT 1
  ) cold ON v.committee_voter IS NOT NULL
  LEFT JOIN voting_anchor va ON va.id = v.voting_anchor_id`;

export interface VoteRow {
  proposal_id: string | number;
  voter_role: string;
  vote: string;
  vote_index: number;
  vote_tx_hash: string;
  vote_block_no: string | number | null;
  vote_epoch: number | null;
  vote_slot: string | number | null;
  vote_time: Date | string;
  drep_raw: string | null;
  drep_script: boolean | null;
  pool_raw: string | null;
  hot_raw: string | null;
  hot_script: boolean | null;
  cold_raw: string | null;
  cold_script: boolean | null;
  vote_anchor_url: string | null;
  vote_anchor_hash: string | null;
}

const CHOICE: Record<string, VoteChoice> = { Yes: 'yes', No: 'no', Abstain: 'abstain' };

export function toVoteChoice(vote: string): VoteChoice {
  const choice = CHOICE[vote];
  if (!choice) throw internal(`Unknown db-sync vote ${vote}`);
  return choice;
}

function voterOf(row: VoteRow): VoteCastBy {
  switch (row.voter_role) {
    case 'DRep':
      if (!row.drep_raw || row.drep_script === null) break;
      return { role: 'drep', id: encodeDRepId(row.drep_raw, row.drep_script), isScriptBased: row.drep_script };
    case 'SPO':
      if (!row.pool_raw) break;
      return { role: 'spo', id: encodePoolId(row.pool_raw), isScriptBased: false };
    case 'ConstitutionalCommittee':
      if (!row.hot_raw || row.hot_script === null) break;
      return {
        role: 'cc',
        hot: encodeCommitteeHotId(row.hot_raw, row.hot_script),
        ...(row.cold_raw && row.cold_script !== null ? { cold: encodeCommitteeColdId(row.cold_raw, row.cold_script) } : {}),
        isScriptBased: row.hot_script,
      };
  }
  throw internal(`Vote row with unresolvable ${row.voter_role} voter`);
}

export function toVoteRecord(row: VoteRow): VoteRecord {
  if (row.vote_epoch === null) throw internal('Vote in a block with no epoch');
  const at: EpochStamp = {
    epoch: row.vote_epoch,
    ...(row.vote_slot === null ? {} : { slot: toInt(row.vote_slot) }),
    ...(row.vote_block_no === null ? {} : { block: toInt(row.vote_block_no) }),
    time: toIso(row.vote_time),
  };
  return {
    voter: voterOf(row),
    choice: toVoteChoice(row.vote),
    anchor:
      row.vote_anchor_url !== null && row.vote_anchor_hash !== null
        ? { url: row.vote_anchor_url, dataHash: row.vote_anchor_hash }
        : null,
    txRef: {
      txHash: row.vote_tx_hash,
      index: row.vote_index,
      ...(row.vote_block_no === null ? {} : { block: toInt(row.vote_block_no) }),
    },
    at,
  };
}
