import type {
  Envelope,
  PagedEnvelope,
  PageRequest,
  PoolsApi,
  SpoVoter,
  VoteListQuery,
  VoteRecord,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';

/**
 * Not implemented.
 *
 * `/pools` lists pool ids and `/pools/{id}` would carry the stake and
 * metadata, but it times out (504) on this deployment. Even working, an
 * `SpoVoter` needs the pool's voting power, which Blockfrost reports only as
 * live stake — and a pool's votes are only reachable per proposal, not per
 * pool.
 */
export class BlockfrostPoolsApi implements PoolsApi {
  list(
    _q?: PageRequest & { search?: string },
  ): Promise<PagedEnvelope<SpoVoter>> {
    return Promise.reject(
      unsupported(
        'governance.pools.list',
        '/pools/{id} times out on this deployment, so a pool cannot be hydrated',
      ),
    );
  }

  get(_id: string): Promise<Envelope<SpoVoter>> {
    return Promise.reject(
      unsupported(
        'governance.pools.get',
        '/pools/{id} times out on this deployment',
      ),
    );
  }

  listVotes(
    _id: string,
    _q?: VoteListQuery,
  ): Promise<PagedEnvelope<VoteRecord>> {
    return Promise.reject(
      unsupported(
        'governance.pools.listVotes',
        'Blockfrost indexes votes per proposal, not per pool',
      ),
    );
  }
}
