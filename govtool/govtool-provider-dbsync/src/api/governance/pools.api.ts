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
 * Not implemented. The legacy SQL touches `pool_stat` only to weight an
 * action's SPO tally; there is no statement that reads a pool as a voter.
 */
export class DbSyncPoolsApi implements PoolsApi {
  list(
    _q?: PageRequest & { search?: string },
  ): Promise<PagedEnvelope<SpoVoter>> {
    return Promise.reject(unsupported('governance.pools.list'));
  }

  get(_id: string): Promise<Envelope<SpoVoter>> {
    return Promise.reject(unsupported('governance.pools.get'));
  }

  listVotes(
    _id: string,
    _q?: VoteListQuery,
  ): Promise<PagedEnvelope<VoteRecord>> {
    return Promise.reject(unsupported('governance.pools.listVotes'));
  }
}
