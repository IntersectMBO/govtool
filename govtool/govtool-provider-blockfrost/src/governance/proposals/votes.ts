/**
 * The votes on one proposal, `/governance/proposals/{tx}/{i}/votes`, and the
 * contract's VoteRecord.
 *
 *   counted      A voter can re-vote; Blockfrost returns every vote and marks
 *                all but the one in force `counted: false`. Only counted rows
 *                are votes (mainnet: 24 superseded rows on one action).
 *   voter_role   `drep`, `spo`, `constitutional_committee` — not `cc`.
 *   voter        DRep ids in CIP-129 (CIP-105 accepted and re-encoded), pools
 *                as `pool1…`, committee members by HOT credential: CIP-129
 *                `cc_hot1…` on hosted v6.8, bare hex on blockfrost-ryo 3.1.1.
 *   anchor       not in the row. It is read from the vote transaction's CBOR
 *                (one request per vote transaction), so an absent rationale is
 *                a fact rather than a guess.
 *   at           not served: dating a vote is one more read per transaction.
 */
import type { VoteCastBy, VoteChoice, VoteRecord } from '@govtool/data-providers/chain-data';

import { votesOf, type CborVote } from '../../cbor';
import { loadTxCbor } from '../../chain';
import type { Session } from '../../context';
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
  fromBlockfrostDRepId,
  fromBlockfrostHotId,
  type Credential,
} from '../../ids';
import { credentialKey, loadCommittee } from '../committee';

export interface BfProposalVote {
  tx_hash: string;
  cert_index: number;
  voter_role: string;
  voter: string;
  vote: string;
  counted?: boolean;
}

export type VoterKind = 'drep' | 'spo' | 'cc';

/** A normalised voter: `key` identifies it across Blockfrost's encodings. */
export interface Voter {
  kind: VoterKind;
  hash: string;
  isScript: boolean;
  key: string;
}

export interface Vote {
  voter: Voter;
  choice: VoteChoice;
  txHash: string;
  certIndex: number;
}

const ROLES: Record<string, VoterKind> = { drep: 'drep', spo: 'spo', constitutional_committee: 'cc', cc: 'cc' };
const CHOICES: Record<string, VoteChoice> = { yes: 'yes', no: 'no', abstain: 'abstain' };

export const voterKey = (kind: VoterKind, c: Credential) => `${kind}:${credentialKey(c)}`;

export function toVoter(kind: VoterKind, c: Credential): Voter {
  return { kind, hash: c.hash, isScript: c.isScript, key: voterKey(kind, c) };
}

/** A Blockfrost vote row's voter; committee hex is resolved against the current committee. */
async function voterOf(s: Session, row: BfProposalVote): Promise<Voter> {
  const kind = ROLES[row.voter_role];
  if (!kind) throw internal(`Blockfrost reported an unknown voter_role '${row.voter_role}'`);
  if (kind === 'drep') {
    const c = fromBlockfrostDRepId(row.voter);
    if (!c) throw internal('Blockfrost reported a vote by a predefined voting option', { voter: row.voter });
    return toVoter('drep', c);
  }
  if (kind === 'spo') {
    try {
      return toVoter('spo', { hash: decodePoolId(row.voter), isScript: false });
    } catch {
      throw internal('Blockfrost reported a pool voter that is not a pool1 id', { voter: row.voter });
    }
  }
  const known = /^[0-9a-fA-F]+$/.test(row.voter) ? (await loadCommittee(s)).hotCredentials : new Map<string, Credential>();
  const c = fromBlockfrostHotId(row.voter, known);
  if (!c) throw internal('Blockfrost reported a committee voter as bare hex that no current member holds', { voter: row.voter });
  return toVoter('cc', c);
}

/** The votes in force on a proposal, oldest first. */
export const loadProposalVotes = (s: Session, txHash: string, index: number): Promise<Vote[]> =>
  s.once(`votes:${txHash}#${index}`, async () => {
    const rows = await s.http.getAll<BfProposalVote>(`/governance/proposals/${txHash}/${index}/votes`);
    const out: Vote[] = [];
    const seen = new Set<string>();
    for (const row of rows) {
      if (row.counted === false) continue;
      const choice = CHOICES[row.vote];
      if (!choice) throw internal(`Blockfrost reported an unknown vote '${row.vote}'`);
      const voter = await voterOf(s, row);
      // One counted vote per voter; a second would mean `counted` is not what it is taken to be.
      if (seen.has(voter.key)) throw internal('Blockfrost counts two votes by one voter on one action', { voter: row.voter });
      seen.add(voter.key);
      out.push({ voter, choice, txHash: row.tx_hash, certIndex: row.cert_index });
    }
    return out;
  });

const CBOR_KIND: Record<CborVote['voter']['kind'], VoterKind> = { ccHot: 'cc', drep: 'drep', spo: 'spo' };

/** The voting procedure behind a vote row, from its transaction's CBOR. */
export async function procedureOf(
  s: Session,
  vote: { voter: Voter; txHash: string },
  action: { txHash: string; index: number },
): Promise<CborVote> {
  const procedures = votesOf(await loadTxCbor(s, vote.txHash));
  const hit = procedures.find(
    (p) =>
      voterKey(CBOR_KIND[p.voter.kind], p.voter) === vote.voter.key &&
      p.action.txHash === action.txHash &&
      p.action.index === action.index,
  );
  if (!hit) throw internal('Vote transaction holds no such voting procedure', { txHash: vote.txHash });
  return hit;
}

/** The voter as the contract names it. A committee vote carries its hot key; the cold one when the current committee resolves it. */
export async function castBy(s: Session, voter: Voter): Promise<VoteCastBy> {
  switch (voter.kind) {
    case 'drep':
      return { role: 'drep', id: encodeDRepId(voter.hash, voter.isScript), isScriptBased: voter.isScript };
    case 'spo':
      return { role: 'spo', id: encodePoolId(voter.hash), isScriptBased: false };
    case 'cc': {
      const member = (await loadCommittee(s)).byHot.get(credentialKey(voter));
      return {
        role: 'cc',
        hot: encodeCommitteeHotId(voter.hash, voter.isScript),
        ...(member ? { cold: member.coldCredential } : {}),
        isScriptBased: voter.isScript,
      };
    }
  }
}

export async function toVoteRecord(s: Session, vote: Vote, action: { txHash: string; index: number }): Promise<VoteRecord> {
  const [procedure, voter] = await Promise.all([procedureOf(s, vote, action), castBy(s, vote.voter)]);
  if (procedure.vote !== vote.choice) throw internal('Vote transaction disagrees with Blockfrost on the vote', { txHash: vote.txHash });
  return {
    voter,
    choice: vote.choice,
    anchor: procedure.anchor,
    txRef: { txHash: vote.txHash, index: vote.certIndex },
  };
}

/**
 * A caller's voter id → the voter keys whose votes are that voter's. A
 * committee member may be named by its cold credential (the stable id) or a
 * hot one. A cold id is resolved through the CURRENT committee to the hot key
 * it has authorised now: Blockfrost keeps no history of hot-key rotations, so
 * a vote cast under a hot key the member has since rotated away from is not
 * attributed to it. Anything else is refused.
 */
export async function parseVoterId(s: Session, id: unknown): Promise<{ keys: Set<string> }> {
  if (typeof id !== 'string' || id.length === 0) throw invalidInput('voterId must be a non-empty string');
  if (id.startsWith('drep1')) return { keys: new Set([voterKey('drep', decodeDRepId(id))]) };
  if (id.startsWith('pool1')) return { keys: new Set([voterKey('spo', { hash: decodePoolId(id), isScript: false })]) };
  if (id.startsWith('cc_hot1')) return { keys: new Set([voterKey('cc', decodeCommitteeHotId(id))]) };
  if (id.startsWith('cc_cold1')) {
    const cold = decodeCommitteeColdId(id);
    const coldId = encodeCommitteeColdId(cold.hash, cold.isScript);
    const { byHot } = await loadCommittee(s);
    const keys = new Set<string>();
    for (const [hotKey, member] of byHot) if (member.coldCredential === coldId) keys.add(`cc:${hotKey}`);
    return { keys };
  }
  throw invalidInput('voterId must be a CIP-129 DRep or committee credential, or a pool1 id', { voterId: id });
}
