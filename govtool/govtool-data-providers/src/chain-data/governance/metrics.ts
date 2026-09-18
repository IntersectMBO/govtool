/**
 * Chain Data API — `/governance/metrics`
 *
 * Aggregate counters. These are derived counts rather than ledger state, so
 * they are the most expensive values in the contract to compute and the most
 * likely to be reported `partial` by a provider other than db-sync.
 */

import type { Envelope, EpochNo, Lovelace, Ratio } from '../common';

export interface GovernanceMetrics {
  /** Optional: a single aggregate query may not carry the epoch it ran against. */
  epoch?: EpochNo;
  /** Delegation */
  uniqueDelegators: number;
  totalDelegations: number;
  /** Total stake in the current DRep distribution, predefined options included. */
  totalDRepDistribution?: Lovelace;
  /** DReps */
  totalRegisteredDReps: number;
  totalActiveDReps: number;
  totalInactiveDReps: number;
  /** Active DReps whose metadata carries a CIP-119 `givenName`. */
  totalActiveCip119CompliantDReps: number;
  totalRegisteredDirectVoters: number;
  /** Actions & votes */
  totalGovernanceActions: number;
  totalLiveGovernanceActions?: number;
  totalDRepVotes: number;
  totalSpoVotes?: number;
  totalCcVotes?: number;
  /** Committee */
  committee: { size: number; quorum: Ratio };
  /** Treasury, mirrored here for the dashboard tile. */
  treasury?: { balance: Lovelace; reserves: Lovelace };
}

export interface MetricsApi {
  /** `GET /governance/metrics` */
  get(q?: { epoch?: EpochNo }): Promise<Envelope<GovernanceMetrics>>;
}
