import type {
  Envelope,
  GovernanceApi,
  PagedEnvelope,
  PageRequest,
  Voter,
  VoterRef,
  VoterRole,
} from '@govtool/data-providers/chain-data';

import type { EpochTimeResolver } from '../../common/epoch-time';
import { unsupported } from '../../common/errors';
import type { BlockfrostClient } from '../../http/client';
import { BlockfrostCommitteeApi } from './committee.api';
import { BlockfrostDRepsApi } from './dreps.api';
import { BlockfrostMetricsApi } from './metrics.api';
import { BlockfrostPoolsApi } from './pools.api';
import { BlockfrostProposalsApi } from './proposals.api';
import { BlockfrostVotesApi } from './votes.api';

export class BlockfrostGovernanceApi implements GovernanceApi {
  readonly dreps: BlockfrostDRepsApi;
  readonly pools: BlockfrostPoolsApi;
  readonly proposals: BlockfrostProposalsApi;
  readonly votes: BlockfrostVotesApi;
  readonly committee: BlockfrostCommitteeApi;
  readonly metrics: BlockfrostMetricsApi;

  /** Resolving a voter role-agnostically needs the two roles this provider has no resource for. */
  readonly voters = {
    resolve: (_id: string): Promise<Envelope<Voter>> =>
      Promise.reject(
        unsupported(
          'governance.voters.resolve',
          'resolving SPO and committee voters needs resources Blockfrost does not serve',
        ),
      ),
    list: (
      _q?: PageRequest & { role?: VoterRole[]; search?: string },
    ): Promise<PagedEnvelope<VoterRef>> =>
      Promise.reject(unsupported('governance.voters.list')),
  };

  constructor(client: BlockfrostClient, epochs: EpochTimeResolver) {
    this.dreps = new BlockfrostDRepsApi(client, epochs);
    this.proposals = new BlockfrostProposalsApi(client, epochs);
    this.pools = new BlockfrostPoolsApi();
    this.votes = new BlockfrostVotesApi();
    this.committee = new BlockfrostCommitteeApi();
    this.metrics = new BlockfrostMetricsApi();
  }
}

export {
  BlockfrostCommitteeApi,
  BlockfrostDRepsApi,
  BlockfrostMetricsApi,
  BlockfrostPoolsApi,
  BlockfrostProposalsApi,
  BlockfrostVotesApi,
};
