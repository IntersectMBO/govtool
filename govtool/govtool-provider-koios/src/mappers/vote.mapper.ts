import type {
  RoleTally,
  VoteChoice,
  VoteRecord,
  VoterRef,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { invalidInput } from '../common/errors';
import {
  bech32CredentialHash,
  encodeCip105DRepId,
  isScriptCredential,
  isScriptDRepId,
  parseGovActionId,
  toDRepHash,
} from '../common/ids';
import { toIsoString, toLovelace } from '../common/numbers';
import { projectVoteRationale } from './metadata.mapper';
import type {
  KoiosVote,
  KoiosVoterRole,
  ProposalVotingSummaryRow,
  VoteListRow,
} from '../rows';

export function toVoteChoice(vote: KoiosVote): VoteChoice {
  switch (vote) {
    case 'Yes':
      return 'yes';
    case 'No':
      return 'no';
    case 'Abstain':
      return 'abstain';
  }
}

export function toKoiosVote(vote: VoteChoice): KoiosVote {
  switch (vote) {
    case 'yes':
      return 'Yes';
    case 'no':
      return 'No';
    case 'abstain':
      return 'Abstain';
  }
}

export function toVoterRole(role: KoiosVoterRole): VoterRole {
  switch (role) {
    case 'DRep':
      return 'drep';
    case 'SPO':
      return 'spo';
    case 'ConstitutionalCommittee':
      return 'cc';
  }
}

export function toKoiosVoterRole(role: VoterRole): KoiosVoterRole {
  switch (role) {
    case 'drep':
      return 'DRep';
    case 'spo':
      return 'SPO';
    case 'cc':
      return 'ConstitutionalCommittee';
    case 'direct':
      throw invalidInput(
        'Koios does not model a direct voter as a distinct role',
        { role },
      );
  }
}

/**
 * A voter reference from the id and role alone.
 *
 * Koios gives `voter_id` as CIP-129 bech32 and `voter_hex` as the raw
 * credential, so no decoding is needed for the identity. `cip105Id` is only
 * meaningful for DReps — a pool id and a CC credential have no CIP-105 form.
 */
export function toVoterRef(
  role: KoiosVoterRole,
  id: string,
  hex?: string | null,
): VoterRef {
  const contractRole = toVoterRole(role);
  if (contractRole === 'drep') {
    const hash = hex ?? toDRepHash(id);
    const isScriptBased = isScriptDRepId(id);
    return {
      role: 'drep',
      id,
      hash,
      isScriptBased,
      cip105Id: encodeCip105DRepId(hash, isScriptBased),
    };
  }
  // SPO and committee voters. `/vote_list` returns `voter_hex: null` for
  // every role, so the hash has to come out of the bech32 id: a `pool1…` is
  // a bare 28-byte key hash, a `cc_hot1…` is CIP-129 with a header byte.
  return {
    role: contractRole,
    id,
    hash: hex ?? bech32CredentialHash(id),
    // A stake-pool credential is always key-based; a committee credential
    // may be script-based, and its header says so.
    isScriptBased: contractRole === 'spo' ? false : isScriptCredential(id),
  };
}

/**
 * A vote from `/vote_list`.
 *
 * `votingPower` is always `null`: Koios records who voted and how, but never
 * the power that was applied at the moment the vote was counted. The per-role
 * totals on `/proposal_voting_summary` are the only power figures it has, and
 * they cannot be attributed back to individual voters.
 *
 * `isCurrent` is supplied by the caller, because a single row cannot know
 * whether a later vote replaced it — see `markSuperseded`.
 */
export function mapVoteRecord(
  row: VoteListRow,
  isCurrent: boolean,
): VoteRecord {
  const parts = parseGovActionId(row.proposal_id);
  return {
    proposal: {
      id: row.proposal_id,
      txHash: row.proposal_tx_hash,
      index: row.proposal_index,
    },
    voter: toVoterRef(row.voter_role, row.voter_id),
    vote: toVoteChoice(row.vote),
    txRef: {
      txHash: row.vote_tx_hash,
      block: row.block_height ?? undefined,
      at: { epoch: row.epoch_no, time: toIsoString(row.block_time) },
    },
    at: { epoch: row.epoch_no, time: toIsoString(row.block_time) },
    votingPower: null,
    rationale: projectVoteRationale({
      url: row.meta_url,
      hash: row.meta_hash,
      json: row.meta_json,
    }),
    isCurrent: isCurrent && parts.index === row.proposal_index,
  };
}

/**
 * Marks every vote but the latest per (voter, proposal) as superseded.
 *
 * This is only correct over a set that contains *all* of a voter's votes on
 * the proposals involved, which is true when the caller scoped the read to
 * one proposal or one voter, and false for an arbitrary page of the global
 * feed. `governance.votes.list#isCurrent` is declared `partial` for that
 * reason.
 */
export function markSuperseded(rows: VoteListRow[]): Map<VoteListRow, boolean> {
  const latest = new Map<string, VoteListRow>();
  for (const row of rows) {
    const key = `${row.proposal_id}:${row.voter_id}`;
    const held = latest.get(key);
    if (held === undefined || row.block_time > held.block_time) {
      latest.set(key, row);
    }
  }
  return new Map(
    rows.map((row) => [
      row,
      latest.get(`${row.proposal_id}:${row.voter_id}`) === row,
    ]),
  );
}

/**
 * Per-role tallies from `/proposal_voting_summary`.
 *
 * Koios reports two power figures per choice — `*_active_*_vote_power` and
 * `*_vote_power`. The first is the stake that actually voted that way; the
 * second is the CIP-1694 counting figure, which for `no` folds in
 * non-voters and always-no-confidence delegation. `stake` carries the first,
 * because it is the only one that means "this much stake voted this way";
 * the difference between them is surfaced as `notVotedStake`.
 *
 * `threshold` and `passing` are absent. The thresholds live on
 * `/epoch_params` as floating-point numbers and the contract's `Ratio` is
 * exact — see `mapProtocolParams`. Koios does publish a `*_pct`, but a
 * percentage is a rendering of a comparison this contract expects the
 * consumer to make from the ratio, so it is not smuggled in here.
 */
export function mapTallies(row: ProposalVotingSummaryRow): RoleTally[] {
  const drep: RoleTally = {
    role: 'drep',
    stake: {
      yes: toLovelace(row.drep_active_yes_vote_power ?? '0'),
      no: toLovelace(row.drep_active_no_vote_power ?? '0'),
      abstain: toLovelace(row.drep_active_abstain_vote_power ?? '0'),
    },
    count: {
      yes: row.drep_yes_votes_cast,
      no: row.drep_no_votes_cast,
      abstain: row.drep_abstain_votes_cast,
    },
  };
  const drepNotVoted = difference(
    row.drep_no_vote_power,
    row.drep_active_no_vote_power,
  );
  if (drepNotVoted !== undefined) {
    drep.notVotedStake = drepNotVoted;
  }

  const spo: RoleTally = {
    role: 'spo',
    stake: {
      yes: toLovelace(row.pool_active_yes_vote_power ?? '0'),
      no: toLovelace(row.pool_active_no_vote_power ?? '0'),
      abstain: toLovelace(row.pool_active_abstain_vote_power ?? '0'),
    },
    count: {
      yes: row.pool_yes_votes_cast,
      no: row.pool_no_votes_cast,
      abstain: row.pool_abstain_votes_cast,
    },
  };
  const spoNotVoted = difference(
    row.pool_no_vote_power,
    row.pool_active_no_vote_power,
  );
  if (spoNotVoted !== undefined) {
    spo.notVotedStake = spoNotVoted;
  }

  // The committee votes by head, so it has counts and no stake at all.
  const cc: RoleTally = {
    role: 'cc',
    count: {
      yes: row.committee_yes_votes_cast,
      no: row.committee_no_votes_cast,
      abstain: row.committee_abstain_votes_cast,
    },
  };

  return [drep, spo, cc];
}

function difference(
  total: string | null,
  active: string | null,
): string | undefined {
  if (total === null || active === null) return undefined;
  const gap = BigInt(total) - BigInt(active);
  return gap > 0n ? String(gap) : '0';
}
