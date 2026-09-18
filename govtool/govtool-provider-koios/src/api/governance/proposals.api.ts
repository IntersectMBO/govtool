import type {
  EnactedActionSummary,
  Envelope,
  GovAction,
  GovActionActivityEvent,
  GovActionExpand,
  GovActionRef,
  GovActionType,
  PagedEnvelope,
  PageRequest,
  ProposalListQuery,
  ProposalsApi,
  RoleTally,
  VoteListQuery,
  VoteRecord,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import { normalizeGovActionId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { paginateLocally, toKoiosPage, toPage } from '../../common/paging';
import { toIsoString } from '../../common/numbers';
import type { KoiosHttpClient } from '../../http/client';
import {
  mapEnactedSummary,
  mapGovAction,
  toKoiosType,
} from '../../mappers/proposal.mapper';
import { mapTallies, toVoterRef } from '../../mappers/vote.mapper';
import type {
  ProposalRow,
  ProposalVotingSummaryRow,
  VoteListRow,
} from '../../rows';
import type { KoiosVotesApi } from './votes.api';

export class KoiosProposalsApi implements ProposalsApi {
  constructor(
    private readonly http: KoiosHttpClient,
    private readonly votes: KoiosVotesApi,
  ) {}

  /**
   * `/proposal_list` is a PostgREST view, so type, status and ordering are
   * pushed down as filters rather than applied here. Only `search` has no
   * server-side form: the metadata is a JSON column with no text index, and
   * scanning it per page would be both slow and silently partial.
   */
  async list(q?: ProposalListQuery): Promise<PagedEnvelope<GovAction>> {
    if (q?.search !== undefined) {
      throw unsupported(
        'governance.proposals.list{search}',
        'Koios has no text index over proposal metadata',
      );
    }
    if (q?.voterId !== undefined) {
      throw unsupported(
        'governance.proposals.list{voterId}',
        'Koios cannot join a voter onto a proposal listing; read /governance/votes instead',
      );
    }

    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<ProposalRow>(
      'proposal_list',
      { ...typeFilter(q?.type), ...statusFilter(q?.status) },
      { ...page, count: 'estimated', order: sortOrder(q?.sort) },
    );

    const actions = response.rows.map(mapGovAction);
    await this.expand(actions, response.rows, q?.expand);
    return envelope(toPage(response, actions, offset, limit));
  }

  async get(
    id: string,
    q?: { expand?: GovActionExpand[]; voterId?: string },
  ): Promise<Envelope<GovAction>> {
    if (q?.voterId !== undefined) {
      throw unsupported(
        'governance.proposals.get{voterId}',
        'Koios has no per-voter filter on a single proposal read',
      );
    }
    const row = await this.proposalRow(id);
    const action = mapGovAction(row);
    await this.expand([action], [row], q?.expand);
    return envelope(action);
  }

  /**
   * Delegated to `/vote_list` rather than `/proposal_votes`.
   *
   * `/proposal_votes` looks like the right endpoint but omits
   * `vote_tx_hash`, and the contract's `VoteRecord.txRef` is required — a
   * vote the consumer cannot link back to its transaction is not a vote
   * record. `/vote_list` carries the hash and takes the same filter.
   */
  listVotes(id: string, q?: VoteListQuery): Promise<PagedEnvelope<VoteRecord>> {
    return this.votes.list({ ...q, proposalId: id });
  }

  async getTallies(
    id: string,
    q?: { role?: VoterRole },
  ): Promise<Envelope<RoleTally[]>> {
    const tallies = await this.tallies(normalizeGovActionId(id));
    if (tallies === null) {
      throw notFound('Koios has no voting summary for that proposal', { id });
    }
    return envelope(
      q?.role === undefined
        ? tallies
        : tallies.filter((tally) => tally.role === q.role),
    );
  }

  /**
   * Synthesised, not read: Koios has no activity feed. Submission and every
   * lifecycle transition come off the proposal row's epoch columns, and the
   * votes come from `/vote_list`.
   *
   * Only `submitted` and `voted` carry a real timestamp — the rest are epoch
   * transitions Koios records as an epoch number and nothing else.
   */
  async listActivity(
    id: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<GovActionActivityEvent>> {
    const row = await this.proposalRow(id);
    const events: GovActionActivityEvent[] = [
      {
        type: 'submitted',
        at: { epoch: row.proposed_epoch, time: toIsoString(row.block_time) },
        txRef: { txHash: row.proposal_tx_hash, index: row.proposal_index },
      },
    ];

    const votes = await this.http.get<VoteListRow>(
      'vote_list',
      { proposal_id: `eq.${row.proposal_id}` },
      { order: 'block_time.desc' },
    );
    for (const vote of votes.rows) {
      events.push({
        type: 'voted',
        at: { epoch: vote.epoch_no, time: toIsoString(vote.block_time) },
        txRef: { txHash: vote.vote_tx_hash },
        voter: toVoterRef(vote.voter_role, vote.voter_id),
        vote:
          vote.vote === 'Yes' ? 'yes' : vote.vote === 'No' ? 'no' : 'abstain',
      });
    }

    for (const [type, epoch] of [
      ['ratified', row.ratified_epoch],
      ['enacted', row.enacted_epoch],
      ['expired', row.expired_epoch],
      ['dropped', row.dropped_epoch],
    ] as const) {
      if (epoch !== null) {
        events.push({ type, at: { epoch }, txRef: null });
      }
    }

    events.sort((a, b) => (b.at.epoch ?? 0) - (a.at.epoch ?? 0));
    return envelope(paginateLocally(events, q));
  }

  async getEnacted(
    type: GovActionType,
  ): Promise<Envelope<EnactedActionSummary | null>> {
    const response = await this.http.get<ProposalRow>(
      'proposal_list',
      {
        proposal_type: `eq.${toKoiosType(type)}`,
        enacted_epoch: 'not.is.null',
      },
      { order: 'enacted_epoch.desc', limit: 1 },
    );
    const row = response.rows[0];
    return envelope(row === undefined ? null : mapEnactedSummary(row));
  }

  /** The post-submission confirmation path: tx hash in, action ids out. */
  async listByTx(txHash: string): Promise<Envelope<GovActionRef[]>> {
    const response = await this.http.get<ProposalRow>('proposal_list', {
      proposal_tx_hash: `eq.${txHash}`,
    });
    return envelope(
      response.rows.map((row) => ({
        id: row.proposal_id,
        txHash: row.proposal_tx_hash,
        index: row.proposal_index,
      })),
    );
  }

  /* --------------------------------------------------------------------- */

  private async proposalRow(id: string): Promise<ProposalRow> {
    const proposalId = normalizeGovActionId(id);
    const response = await this.http.get<ProposalRow>('proposal_list', {
      proposal_id: `eq.${proposalId}`,
    });
    const row = response.rows[0];
    if (row === undefined) {
      throw notFound('Koios has no such governance action', { id });
    }
    return row;
  }

  /**
   * Tallies are one request per action — `/proposal_voting_summary` takes a
   * single `_proposal_id` — so expanding a page of 20 costs 20 requests. They
   * are issued together, but a caller paging with `expand: ['tallies']` on the
   * public tier should expect to meet the rate limiter.
   */
  private async expand(
    actions: GovAction[],
    rows: ProposalRow[],
    expand: GovActionExpand[] | undefined,
  ): Promise<void> {
    const wanted = new Set(expand ?? []);
    if (wanted.has('myVote')) {
      throw unsupported(
        'governance.proposals.list{expand:myVote}',
        'Koios cannot join a single voter onto a proposal read',
      );
    }
    if (wanted.has('protocolParams')) {
      throw unsupported(
        'governance.proposals.list{expand:protocolParams}',
        'the parameters in force at submission are a separate /epoch_params read; call network.getProtocolParams({ epoch })',
      );
    }
    if (wanted.has('thresholds')) {
      throw unsupported(
        'governance.proposals.list{expand:thresholds}',
        'Koios reports voting thresholds as floating-point numbers, which cannot be returned as an exact Ratio',
      );
    }
    if (!wanted.has('tallies')) return;

    const tallies = await Promise.all(
      rows.map((row) => this.tallies(row.proposal_id)),
    );
    tallies.forEach((tally, index) => {
      const action = actions[index];
      if (action !== undefined && tally !== null) {
        action.tallies = tally;
      }
    });
  }

  private async tallies(proposalId: string): Promise<RoleTally[] | null> {
    const response = await this.http.get<ProposalVotingSummaryRow>(
      'proposal_voting_summary',
      { _proposal_id: proposalId },
    );
    const row = response.rows[0];
    return row === undefined ? null : mapTallies(row);
  }
}

function typeFilter(
  types: GovActionType[] | undefined,
): Record<string, string> {
  if (types === undefined || types.length === 0) return {};
  return { proposal_type: `in.(${types.map(toKoiosType).join(',')})` };
}

/**
 * Status is four nullable epoch columns rather than a column of its own, so
 * only single-status filters translate. A request for several at once would
 * need an `or=` expression per combination; it is refused instead.
 */
function statusFilter(
  statuses: ProposalListQuery['status'],
): Record<string, string> {
  if (statuses === undefined || statuses.length === 0) return {};
  if (statuses.length > 1) {
    throw unsupported(
      'governance.proposals.list{status:multiple}',
      'Koios encodes proposal status as four separate epoch columns; filter one status at a time',
    );
  }
  switch (statuses[0]) {
    case 'enacted':
      return { enacted_epoch: 'not.is.null' };
    case 'ratified':
      return { ratified_epoch: 'not.is.null' };
    case 'expired':
      return { expired_epoch: 'not.is.null' };
    case 'dropped':
      return { dropped_epoch: 'not.is.null' };
    case 'live':
      return {
        enacted_epoch: 'is.null',
        ratified_epoch: 'is.null',
        expired_epoch: 'is.null',
        dropped_epoch: 'is.null',
      };
    default:
      return {};
  }
}

function sortOrder(sort: ProposalListQuery['sort']): string {
  switch (sort) {
    case 'oldest':
      return 'block_time.asc';
    case 'soonestToExpire':
      return 'expiration.asc';
    case 'mostYesVotes':
    case 'highestParticipation':
      throw unsupported(
        `governance.proposals.list{sort:${sort}}`,
        'vote weights live on /proposal_voting_summary, which cannot be joined into the listing',
      );
    case 'newest':
    default:
      return 'block_time.desc';
  }
}
