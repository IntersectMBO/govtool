import type {
  Envelope,
  PagedEnvelope,
  VoteListQuery,
  VoteRecord,
  VotesApi,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import { normalizeGovActionId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { toKoiosPage, toPage } from '../../common/paging';
import type { KoiosHttpClient } from '../../http/client';
import {
  mapVoteRecord,
  markSuperseded,
  toKoiosVote,
  toKoiosVoterRole,
} from '../../mappers/vote.mapper';
import { toKoiosType } from '../../mappers/proposal.mapper';
import type { VoteListRow } from '../../rows';

export class KoiosVotesApi implements VotesApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * `/vote_list` is the one endpoint that carries every field a `VoteRecord`
   * needs, and PostgREST makes it filterable by voter, proposal, role, choice
   * and action type — so the proposal- and DRep-scoped vote routes are all
   * this query with a different filter.
   *
   * What it cannot do is `search`, and what it does not carry is the voting
   * power applied to each vote; both are declared rather than approximated.
   */
  async list(
    q?: VoteListQuery & { voterId?: string; proposalId?: string },
  ): Promise<PagedEnvelope<VoteRecord>> {
    if (q?.search !== undefined) {
      throw unsupported(
        'governance.votes.list{search}',
        'Koios has no text index over vote rationales',
      );
    }

    const { offset, limit, page } = toKoiosPage(q);
    const query: Record<string, string> = {};
    if (q?.proposalId !== undefined) {
      query['proposal_id'] = `eq.${normalizeGovActionId(q.proposalId)}`;
    }
    if (q?.voterId !== undefined) {
      query['voter_id'] = `eq.${q.voterId}`;
    }
    if (q?.role !== undefined && q.role.length > 0) {
      query['voter_role'] = `in.(${q.role.map(toKoiosVoterRole).join(',')})`;
    }
    if (q?.vote !== undefined && q.vote.length > 0) {
      query['vote'] = `in.(${q.vote.map(toKoiosVote).join(',')})`;
    }
    if (q?.proposalType !== undefined && q.proposalType.length > 0) {
      query['proposal_type'] =
        `in.(${q.proposalType.map(toKoiosType).join(',')})`;
    }

    const response = await this.http.get<VoteListRow>('vote_list', query, {
      ...page,
      count: 'estimated',
      order: q?.sort === 'oldest' ? 'block_time.asc' : 'block_time.desc',
    });

    const current = markSuperseded(response.rows);
    const records = response.rows
      .filter(
        (row) => q?.includeSuperseded === true || current.get(row) !== false,
      )
      .map((row) => mapVoteRecord(row, current.get(row) ?? true));

    return envelope(toPage(response, records, offset, limit));
  }

  /**
   * A vote by its transaction. `index` is accepted but never needed: a
   * transaction can hold several voting procedures, and Koios flattens them
   * into rows with no ordinal, so the index cannot be matched against
   * anything. It is rejected rather than ignored.
   */
  async get(
    txHash: string,
    q?: { index?: number },
  ): Promise<Envelope<VoteRecord>> {
    if (q?.index !== undefined) {
      throw unsupported(
        'governance.votes.get{index}',
        'Koios does not number the voting procedures within a transaction',
      );
    }
    const response = await this.http.get<VoteListRow>('vote_list', {
      vote_tx_hash: `eq.${txHash}`,
    });
    const row = response.rows[0];
    if (row === undefined) {
      throw notFound('Koios has no vote in that transaction', { txHash });
    }
    return envelope(mapVoteRecord(row, true));
  }
}
