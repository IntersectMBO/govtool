import type {
  Envelope,
  GovernanceApi,
  PagedEnvelope,
  PageRequest,
  Voter,
  VoterRef,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';
import type { Queryable } from '../../db/queryable';
import { DbSyncCommitteeApi } from './committee.api';
import { DbSyncDRepsApi } from './dreps.api';
import { DbSyncMetricsApi } from './metrics.api';
import { DbSyncPoolsApi } from './pools.api';
import { DbSyncProposalsApi } from './proposals.api';
import { DbSyncVotesApi } from './votes.api';

export class DbSyncGovernanceApi implements GovernanceApi {
  readonly dreps: DbSyncDRepsApi;
  readonly pools: DbSyncPoolsApi;
  readonly proposals: DbSyncProposalsApi;
  readonly votes: DbSyncVotesApi;
  readonly committee: DbSyncCommitteeApi;
  readonly metrics: DbSyncMetricsApi;

  /**
   * Role-agnostic voter lookup is not implemented: it would have to resolve
   * SPO and committee voters, and neither has a statement here.
   */
  readonly voters = {
    resolve: (_id: string): Promise<Envelope<Voter>> =>
      Promise.reject(unsupported('governance.voters.resolve')),
    list: (
      _q?: PageRequest & { role?: VoterRole[]; search?: string },
    ): Promise<PagedEnvelope<VoterRef>> =>
      Promise.reject(unsupported('governance.voters.list')),
  };

  constructor(db: Queryable) {
    this.proposals = new DbSyncProposalsApi(db);
    this.dreps = new DbSyncDRepsApi(db, this.proposals);
    this.pools = new DbSyncPoolsApi();
    this.votes = new DbSyncVotesApi();
    this.committee = new DbSyncCommitteeApi();
    this.metrics = new DbSyncMetricsApi(db);
  }
}

export { DbSyncCommitteeApi, DbSyncDRepsApi, DbSyncMetricsApi };
export { DbSyncPoolsApi, DbSyncProposalsApi, DbSyncVotesApi };
