/**
 * Chain Data API — `/governance/*`
 */

import type { CommitteeApi } from './committee';
import type { DRepsApi } from './dreps';
import type { PoolsApi } from './pools';
import type { ProposalsApi } from './proposals';

export * from './committee';
export * from './dreps';
export * from './pools';
export * from './proposals';
export * from './votes';

/**
 * Every namespace here is required. What varies between providers is which
 * OPTIONAL METHODS each namespace carries, and which option values each
 * declares — not whether the namespace exists.
 */
export interface GovernanceApi {
  dreps: DRepsApi;
  proposals: ProposalsApi;
  pools: PoolsApi;
  committee: CommitteeApi;
}
