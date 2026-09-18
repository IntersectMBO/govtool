import type {
  VoteChoice,
  VoteRecord,
} from '@govtool/data-providers/chain-data';
import type {
  MetadataProjection,
  VoteRationaleBody,
} from '@govtool/data-providers/metadata';

import { encodeCip129DRepId, encodeCip129GovActionId } from '../common/ids';
import { toInteger, toIsoString } from '../common/numbers';
import { internal } from '../common/errors';
import type { DRepVoteTuple } from '../rows';
import { projectMetadata } from './metadata.mapper';

const VOTE_CHOICES: ReadonlySet<string> = new Set(['yes', 'no', 'abstain']);

function toVoteChoice(value: string): VoteChoice {
  if (!VOTE_CHOICES.has(value)) {
    throw internal(`Unexpected vote value returned from database: ${value}`);
  }
  return value as VoteChoice;
}

function rationale(
  url: string | null,
  docHash: string | null,
): MetadataProjection<VoteRationaleBody> | null {
  if (url === null) {
    return null;
  }
  return projectMetadata<VoteRationaleBody>({
    anchor: { url, dataHash: docHash ?? '' },
    standard: 'CIP100',
    body: {},
    hasBodyData: false,
  });
}

/**
 * One positional `get-votes.sql` row → `VoteRecord`. The statement is
 * `DISTINCT ON (proposal, voter)` ordered by newest first, so every row is
 * the voter's current vote on that action.
 *
 * `isScript` is not selected by the statement; the caller knows it from the
 * DRep it asked about, or passes `false` when it does not.
 */
export function mapVoteTuple(row: DRepVoteTuple, isScript = false): VoteRecord {
  const [
    proposalId,
    govActionId,
    drepHash,
    vote,
    url,
    docHash,
    epochNo,
    date,
    voteTxHash,
  ] = row;

  const [txHash, rawIndex] = govActionId.split('#');
  const index = Number(rawIndex);
  if (!txHash || !Number.isInteger(index)) {
    throw internal(
      `Unexpected governance action id from database: ${govActionId}`,
    );
  }

  return {
    proposal: {
      id: encodeCip129GovActionId(txHash, index),
      txHash,
      index,
      providerId: String(proposalId),
    },
    voter: {
      role: 'drep',
      id: encodeCip129DRepId(drepHash, isScript),
      hash: drepHash,
      isScriptBased: isScript,
    },
    vote: toVoteChoice(vote),
    txRef: { txHash: voteTxHash },
    at: { epoch: toInteger(epochNo), time: toIsoString(date) },
    votingPower: null,
    rationale: rationale(url, docHash),
    isCurrent: true,
  };
}
