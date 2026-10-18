/**
 * Voters and votes: parsing a caller's voter id, reading votes off Koios'
 * `/vote_list`, keeping each voter's latest vote, and mapping to VoteRecord.
 *
 * `/vote_list` is used rather than `/proposal_votes` because only it carries
 * the vote's transaction hash (a VoteRecord requires one) and its epoch and
 * block. It lists every vote ever cast, re-votes included, so the latest per
 * voter is picked here — by block time, then block height; a voter voting
 * twice in one block is broken by transaction hash, which is deterministic but
 * cannot see the order inside the block.
 */
import type { VoteCastBy, VoteChoice, VoteRecord } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../../context';
import { internal, invalidInput, unsupported } from '../../errors';
import { chunkForFilter, inList } from '../../http';
import {
  decodeCommitteeHotId,
  decodeDRepId,
  decodePoolId,
  encodeCommitteeHotId,
  encodeDRepId,
  encodePoolId,
} from '../../ids';
import { toIso } from '../../numbers';
import type { KoiosVoterRole, VoteListRow } from '../../rows';
import { toAnchor } from '../dreps/directory';

/** A voter as Koios names it on `/vote_list`: the canonical bech32 id. */
export interface VoterKey {
  role: KoiosVoterRole;
  id: string;
  isScript: boolean;
}

/**
 * Decode by bech32 prefix, then validate fully. A committee member is named by
 * its HOT credential here: Koios can map a cold credential only to the hot key
 * authorised now, and a vote cast under an earlier hot key would then read as
 * "not voted" — a fabricated answer — so a cold id is refused instead.
 */
export function parseVoterId(id: unknown): VoterKey {
  if (typeof id !== 'string' || id.trim() === '') throw invalidInput('voterId must be a non-empty string');
  const v = id.trim();
  if (v.startsWith('drep1')) {
    const { hash, isScript } = decodeDRepId(v);
    return { role: 'DRep', id: encodeDRepId(hash, isScript), isScript };
  }
  if (v.startsWith('pool1')) return { role: 'SPO', id: encodePoolId(decodePoolId(v)), isScript: false };
  if (v.startsWith('cc_hot1')) {
    const { hash, isScript } = decodeCommitteeHotId(v);
    return { role: 'ConstitutionalCommittee', id: encodeCommitteeHotId(hash, isScript), isScript };
  }
  if (v.startsWith('cc_cold1')) {
    throw unsupported(
      'a committee cold credential as voterId',
      'Koios resolves a cold credential only to its current hot key; pass the hot credential',
    );
  }
  throw invalidInput('voterId must be a CIP-129 DRep id, a cc_hot id, or a pool1 id', { voterId: id });
}

export const VOTE_SELECT =
  'vote_tx_hash,voter_role,voter_id,proposal_id,proposal_tx_hash,proposal_index,proposal_type,epoch_no,block_height,block_time,vote,meta_url,meta_hash';

const later = (a: VoteListRow, b: VoteListRow) =>
  a.block_time - b.block_time || (a.block_height ?? 0) - (b.block_height ?? 0) || (a.vote_tx_hash > b.vote_tx_hash ? 1 : a.vote_tx_hash < b.vote_tx_hash ? -1 : 0);

/** Each voter's latest vote per action, newest first. */
export function latestVotes(rows: readonly VoteListRow[]): VoteListRow[] {
  const best = new Map<string, VoteListRow>();
  for (const row of rows) {
    const key = `${row.proposal_id}|${row.voter_role}|${row.voter_id}`;
    const current = best.get(key);
    if (!current || later(row, current) > 0) best.set(key, row);
  }
  return [...best.values()].sort((a, b) => later(b, a));
}

/** Every vote matching `query` on `/vote_list`, latest per voter and action. */
export async function readVotes(ctx: Ctx, query: Record<string, string>): Promise<VoteListRow[]> {
  const rows = await ctx.http.getAll<VoteListRow>('vote_list', query, { select: VOTE_SELECT, order: 'block_time.desc,vote_tx_hash.desc' });
  return latestVotes(rows);
}

/** Votes on several actions, the action ids split across short `in.(…)` filters. */
export async function readVotesOn(ctx: Ctx, proposalIds: readonly string[], extra: Record<string, string> = {}): Promise<VoteListRow[]> {
  if (proposalIds.length === 0) return [];
  const parts = await Promise.all(
    chunkForFilter(proposalIds).map((chunk) => ctx.http.getAll<VoteListRow>('vote_list', { ...extra, proposal_id: inList(chunk) }, { select: VOTE_SELECT })),
  );
  return latestVotes(parts.flat());
}

const CHOICE: Record<string, VoteChoice> = { Yes: 'yes', No: 'no', Abstain: 'abstain' };

export function toChoice(vote: string): VoteChoice {
  const choice = CHOICE[vote];
  if (!choice) throw internal('Koios sent an unknown vote', { vote });
  return choice;
}

/** The voter of a vote row. `coldOf` maps a hot credential to its cold one, where known. */
export function voterOf(row: VoteListRow, coldOf: ReadonlyMap<string, string> = new Map()): VoteCastBy {
  switch (row.voter_role) {
    case 'DRep': {
      const { hash, isScript } = decodeDRepId(row.voter_id);
      return { role: 'drep', id: encodeDRepId(hash, isScript), isScriptBased: isScript };
    }
    case 'SPO':
      return { role: 'spo', id: encodePoolId(decodePoolId(row.voter_id)), isScriptBased: false };
    case 'ConstitutionalCommittee': {
      const { hash, isScript } = decodeCommitteeHotId(row.voter_id);
      const hot = encodeCommitteeHotId(hash, isScript);
      const cold = coldOf.get(hot);
      return { role: 'cc', hot, ...(cold ? { cold } : {}), isScriptBased: isScript };
    }
  }
  throw internal('Koios sent a vote with an unknown voter role', { role: row.voter_role });
}

export function toVoteRecord(row: VoteListRow, coldOf?: ReadonlyMap<string, string>): VoteRecord {
  const at = {
    epoch: row.epoch_no,
    ...(row.block_height === null ? {} : { block: row.block_height }),
    time: toIso(row.block_time),
  };
  return {
    voter: voterOf(row, coldOf),
    choice: toChoice(row.vote),
    anchor: toAnchor(row.meta_url, row.meta_hash),
    txRef: { txHash: row.vote_tx_hash, ...(row.block_height === null ? {} : { block: row.block_height }) },
    at,
  };
}
