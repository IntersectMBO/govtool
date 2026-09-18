import type {
  Envelope,
  GovernanceApi,
  PagedEnvelope,
  PageRequest,
  Voter,
  VoterRef,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../../common/errors';
import type { KoiosHttpClient } from '../../http/client';
import { KoiosCommitteeApi } from './committee.api';
import { KoiosDRepsApi } from './dreps.api';
import { KoiosMetricsApi } from './metrics.api';
import { KoiosPoolsApi } from './pools.api';
import { KoiosProposalsApi } from './proposals.api';
import { KoiosVotesApi } from './votes.api';

export { KoiosCommitteeApi } from './committee.api';
export { KoiosDRepsApi } from './dreps.api';
export { KoiosMetricsApi, UNCOMPUTABLE_METRICS } from './metrics.api';
export { KoiosPoolsApi } from './pools.api';
export { KoiosProposalsApi } from './proposals.api';
export { KoiosVotesApi } from './votes.api';

export class KoiosGovernanceApi implements GovernanceApi {
  readonly dreps: KoiosDRepsApi;
  readonly pools: KoiosPoolsApi;
  readonly proposals: KoiosProposalsApi;
  readonly votes: KoiosVotesApi;
  readonly committee: KoiosCommitteeApi;
  readonly metrics: KoiosMetricsApi;

  constructor(http: KoiosHttpClient) {
    this.dreps = new KoiosDRepsApi(http);
    this.pools = new KoiosPoolsApi(http);
    this.votes = new KoiosVotesApi(http);
    this.proposals = new KoiosProposalsApi(http, this.votes);
    this.committee = new KoiosCommitteeApi(http);
    this.metrics = new KoiosMetricsApi(http);
  }

  /**
   * Role-agnostic voter lookup.
   *
   * `resolve` dispatches on the bech32 prefix, which is enough to tell the
   * three roles apart — `drep1…`, `pool1…`, `cc_hot1…`/`cc_cold1…` — so a
   * vote row can resolve its voter without the caller knowing which
   * sub-resource owns it.
   *
   * `list` has no implementation: a combined, paged listing across DReps,
   * pools and committee members would have to page three unrelated endpoints
   * in lockstep and invent a stable ordering across them.
   */
  readonly voters = {
    resolve: async (id: string): Promise<Envelope<Voter>> => {
      if (id.startsWith('pool1')) {
        return this.pools.get(id) as Promise<Envelope<Voter>>;
      }
      if (id.startsWith('cc_hot') || id.startsWith('cc_cold')) {
        return this.committee.getMember(id) as Promise<Envelope<Voter>>;
      }
      if (id.startsWith('drep')) {
        return this.dreps.get(id) as Promise<Envelope<Voter>>;
      }
      throw notFound('Not a DRep, pool or committee credential', { id });
    },

    list: (
      _q?: PageRequest & { role?: VoterRole[]; search?: string },
    ): Promise<PagedEnvelope<VoterRef>> =>
      Promise.reject(
        unsupported(
          'governance.voters.list',
          'Koios has no combined voter index; page /drep_list, /pool_list and /committee_info separately',
        ),
      ),
  };
}
