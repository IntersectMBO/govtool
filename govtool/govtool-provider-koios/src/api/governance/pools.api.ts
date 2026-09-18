import type {
  Envelope,
  PagedEnvelope,
  PageRequest,
  PoolsApi,
  SpoVoter,
  VoteListQuery,
  VoteRecord,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import { envelope } from '../../common/meta';
import { toVoterRef } from '../../mappers/vote.mapper';
import { toKoiosPage, toPage } from '../../common/paging';
import { toIsoString, toLovelace } from '../../common/numbers';
import type { KoiosHttpClient } from '../../http/client';
import { projectVoteRationale } from '../../mappers/metadata.mapper';
import type { PoolInfoRow, PoolListRow, PoolVoteRow } from '../../rows';

export class KoiosPoolsApi implements PoolsApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * `/pool_list` pages cheaply but carries no voting power — that is on
   * `/pool_info`, which takes a POST body of ids. The listing is therefore
   * `votingPower: null` by construction; a caller that needs it reads `get`
   * per pool. Hydrating the page the way `dreps.list` does would mean a
   * second request whose only new field is the power, and unlike DReps a pool
   * listing is usually rendered without it.
   */
  async list(
    q?: PageRequest & { search?: string },
  ): Promise<PagedEnvelope<SpoVoter>> {
    if (q?.search !== undefined) {
      throw unsupported(
        'governance.pools.list{search}',
        'Koios has no index over pool tickers or names',
      );
    }
    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<PoolListRow>(
      'pool_list',
      { pool_status: 'eq.registered' },
      { ...page, count: 'estimated', order: 'pool_id_bech32.asc' },
    );
    return envelope(
      toPage(response, response.rows.map(mapPoolListRow), offset, limit),
    );
  }

  async get(id: string): Promise<Envelope<SpoVoter>> {
    const response = await this.http.post<PoolInfoRow>('pool_info', {
      _pool_bech32_ids: [id],
    });
    const row = response.rows[0];
    if (row === undefined) {
      throw notFound('Koios has no such stake pool', { id });
    }
    return envelope(mapPoolInfoRow(row));
  }

  /** `/pool_votes` carries the vote transaction, unlike `/proposal_votes`. */
  async listVotes(
    id: string,
    q?: VoteListQuery,
  ): Promise<PagedEnvelope<VoteRecord>> {
    const { offset, limit, page } = toKoiosPage(q);
    const response = await this.http.get<PoolVoteRow>(
      'pool_votes',
      { _pool_bech32: id },
      { ...page, count: 'estimated', order: 'block_time.desc' },
    );

    // `/pool_votes` returns no voter columns at all — the pool is the query
    // parameter — so the credential hash is decoded from the id, the same way
    // `/vote_list` rows are. Hand-rolling this left `hash: ''` on every row.
    const voter: VoteRecord['voter'] = toVoterRef('SPO', id);
    const records = response.rows.map((row): VoteRecord => {
      const at = { time: toIsoString(row.block_time) };
      return {
        proposal: {
          id: row.proposal_id,
          txHash: row.proposal_tx_hash,
          index: row.proposal_index,
        },
        voter,
        vote: row.vote === 'Yes' ? 'yes' : row.vote === 'No' ? 'no' : 'abstain',
        txRef: { txHash: row.vote_tx_hash, at },
        at,
        votingPower: null,
        rationale: projectVoteRationale({
          url: row.meta_url,
          hash: row.meta_hash,
          json: undefined,
        }),
        isCurrent: true,
      };
    });

    return envelope(toPage(response, records, offset, limit));
  }
}

function mapPoolListRow(row: PoolListRow): SpoVoter {
  const voter: SpoVoter = {
    role: 'spo',
    id: row.pool_id_bech32,
    poolId: row.pool_id_bech32,
    hash: row.pool_id_hex,
    isScriptBased: false,
    votingPower: null,
  };
  if (row.ticker !== null) voter.ticker = row.ticker;
  if (row.pledge !== null) voter.pledge = toLovelace(row.pledge);
  if (row.active_stake !== null) {
    voter.activeStake = toLovelace(row.active_stake);
  }
  return voter;
}

function mapPoolInfoRow(row: PoolInfoRow): SpoVoter {
  const voter = mapPoolListRow(row);
  if (row.live_stake !== null) voter.liveStake = toLovelace(row.live_stake);
  if (row.meta_json?.name !== undefined) voter.name = row.meta_json.name;
  if (row.meta_json?.ticker !== undefined) voter.ticker = row.meta_json.ticker;
  if (row.voting_power !== null) {
    voter.votingPower = {
      amount: toLovelace(row.voting_power),
      basis: 'active',
    };
  }
  return voter;
}
