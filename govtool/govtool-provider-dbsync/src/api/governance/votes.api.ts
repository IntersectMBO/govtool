import type {
  Envelope,
  PagedEnvelope,
  VoteListQuery,
  VoteRecord,
  VotesApi,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';

/**
 * Not implemented. `get-votes.sql` is keyed on a DRep credential
 * (`WHERE drep_hash.raw = decode($1,'hex')`), so it cannot answer a
 * cross-cutting feed or a lookup by vote transaction. A DRep's own record is
 * available at `governance.dreps.listVotes`.
 */
export class DbSyncVotesApi implements VotesApi {
  list(
    _q?: VoteListQuery & { voterId?: string; proposalId?: string },
  ): Promise<PagedEnvelope<VoteRecord>> {
    return Promise.reject(unsupported('governance.votes.list'));
  }

  get(_txHash: string, _q?: { index?: number }): Promise<Envelope<VoteRecord>> {
    return Promise.reject(unsupported('governance.votes.get'));
  }
}
