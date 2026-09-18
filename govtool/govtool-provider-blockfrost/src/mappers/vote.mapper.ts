import type {
  GovActionRef,
  RoleTally,
  VoteChoice,
  VoteRecord,
  VoterRef,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { internal } from '../common/errors';
import { encodeCip105DRepId } from '../common/ids';
import type { BfProposalVote } from '../http/types';
import { bech32 } from 'bech32';

const BECH32_LIMIT = 1023;

const CHOICES: ReadonlySet<string> = new Set(['yes', 'no', 'abstain']);

/**
 * Blockfrost's `voter_role` values, which are its own names rather than the
 * contract's: it spells the committee `constitutional_committee`. Observed
 * across 2,327 votes on mainnet: `drep`, `spo`, `constitutional_committee`.
 *
 * `direct` has no Blockfrost spelling — a direct voter registers a DRep
 * credential and votes as `drep` — so it is absent here by design.
 */
const ROLE_BY_BLOCKFROST_NAME: Record<string, VoterRole> = {
  drep: 'drep',
  spo: 'spo',
  constitutional_committee: 'cc',
  // Accepted defensively: Blockfrost's OpenAPI has used the short form too.
  cc: 'cc',
};

function toVoteChoice(value: string): VoteChoice {
  if (!CHOICES.has(value)) {
    throw internal(`Unexpected vote value from Blockfrost: ${value}`);
  }
  return value as VoteChoice;
}

function toVoterRole(value: string): VoterRole {
  const role = ROLE_BY_BLOCKFROST_NAME[value];
  if (role === undefined) {
    throw internal(`Unexpected voter_role from Blockfrost: ${value}`);
  }
  return role;
}

/**
 * Blockfrost identifies a voter differently per role, verified on mainnet:
 *
 *  - `drep` → CIP-129 bech32 (`drep1…`), 29 bytes with the credential-type
 *    header, so both the hash and script-ness come out of it;
 *  - `spo` → bech32 `pool1…`, a plain 28-byte key hash with no script form;
 *  - `constitutional_committee` → **raw hex**, not bech32 at all, and with no
 *    indication of whether it is the hot or cold credential or script-based.
 *
 * So `id` is whatever Blockfrost reported (it is the only identifier a caller
 * can feed back in), and `hash` is derived where the encoding allows.
 */
export function toVoterRef(role: VoterRole, voter: string): VoterRef {
  let bytes: Buffer | null = null;
  try {
    bytes = Buffer.from(
      bech32.fromWords(bech32.decode(voter, BECH32_LIMIT).words),
    );
  } catch {
    bytes = null;
  }

  // Committee members arrive as bare hex; a 28-byte credential is the hash.
  if (
    bytes === null &&
    /^[0-9a-fA-F]+$/.test(voter) &&
    voter.length % 2 === 0
  ) {
    return { role, id: voter, hash: voter.toLowerCase(), isScriptBased: false };
  }

  if (bytes === null) {
    return { role, id: voter, hash: '', isScriptBased: false };
  }

  if (bytes.length === 29) {
    // A CIP-129 credential: header byte then the 28-byte hash. The odd
    // header values (0x23 for DRep, 0x13/0x03 for committee) mark a script.
    const header = bytes[0] as number;
    const hash = bytes.subarray(1).toString('hex');
    const isScript = header === 0x23 || header === 0x13 || header === 0x03;
    const ref: VoterRef = { role, id: voter, hash, isScriptBased: isScript };
    if (role === 'drep') {
      ref.cip105Id = encodeCip105DRepId(hash, isScript);
    }
    return ref;
  }

  return {
    role,
    id: voter,
    hash: bytes.toString('hex'),
    // A stake-pool credential is always key-based.
    isScriptBased: false,
  };
}

/**
 * One vote on a known proposal.
 *
 * `at` is omitted: Blockfrost dates a vote only through its transaction, and
 * resolving that is a read per vote. `votingPower` is `null` for the same
 * reason — the power a vote carried is the voter's stake at the time, which
 * Blockfrost does not report alongside the vote.
 *
 * `isCurrent` is `true` for every row, because this endpoint already returns
 * the voter's current vote on the action; a re-vote replaces the earlier one.
 */
export function mapProposalVote(
  proposal: GovActionRef,
  row: BfProposalVote,
): VoteRecord {
  const role = toVoterRole(row.voter_role);
  return {
    proposal,
    voter: toVoterRef(role, row.voter),
    vote: toVoteChoice(row.vote),
    txRef: { txHash: row.tx_hash, index: row.cert_index },
    votingPower: null,
    rationale: null,
    isCurrent: true,
  };
}

/**
 * Tallies by headcount, per role.
 *
 * Blockfrost returns each vote individually with no voting power attached, so
 * only `count` can be filled — never `stake`. For DReps and SPOs the ledger
 * decides by stake, so a DRep or SPO tally from this provider is a *turnout*
 * figure and cannot be compared against a threshold. `RoleTally.threshold`
 * and `passing` are therefore left unset rather than computed from counts.
 */
export function tallyByRole(votes: readonly BfProposalVote[]): RoleTally[] {
  const byRole = new Map<VoterRole, Record<VoteChoice, number>>();

  for (const row of votes) {
    const role = toVoterRole(row.voter_role);
    const choice = toVoteChoice(row.vote);
    const counts =
      byRole.get(role) ??
      ({ yes: 0, no: 0, abstain: 0 } as Record<VoteChoice, number>);
    counts[choice] += 1;
    byRole.set(role, counts);
  }

  return [...byRole.entries()].map(([role, count]) => ({ role, count }));
}
