import type {
  Envelope,
  EpochNo,
  GovernanceMetrics,
  MetricsApi,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';

/**
 * Not implemented.
 *
 * Every counter on `GovernanceMetrics` is an aggregate over a whole
 * collection — unique delegators, registered DReps, total votes — and
 * Blockfrost has no aggregate endpoint for any of them. Computing one means
 * paging the entire collection: `totalRegisteredDReps` alone is the whole
 * DRep directory, and `uniqueDelegators` is every delegator of every DRep.
 *
 * This is the sharpest difference between the two providers so far: db-sync
 * answers the whole metrics dashboard in one query, and a per-entity HTTP API
 * cannot answer it at all.
 */
export class BlockfrostMetricsApi implements MetricsApi {
  get(_q?: { epoch?: EpochNo }): Promise<Envelope<GovernanceMetrics>> {
    return Promise.reject(
      unsupported(
        'governance.metrics.get',
        'every counter is a collection-wide aggregate; Blockfrost has no aggregate endpoint',
      ),
    );
  }
}
