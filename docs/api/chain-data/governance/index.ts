/**
 * Chain Data API — `/governance/*` namespace.
 */

import type { Envelope, PagedEnvelope, PageRequest } from "../common";
import type { CommitteeApi, CommitteeMember } from "./committee";
import type { DRep, DRepsApi } from "./dreps";
import type { MetricsApi } from "./metrics";
import type { PoolsApi, SpoVoter } from "./pools";
import type { ProposalsApi } from "./proposals";
import type { VotesApi } from "./votes";
import type { VoterRef, VoterRole } from "../refs";

export * from "./committee";
export * from "./dreps";
export * from "./metrics";
export * from "./pools";
export * from "./proposals";
export * from "./votes";

/** Any entity that can cast a governance vote. */
export type Voter = DRep | SpoVoter | CommitteeMember;

export interface GovernanceApi {
  dreps: DRepsApi;
  pools: PoolsApi;
  proposals: ProposalsApi;
  votes: VotesApi;
  committee: CommitteeApi;
  metrics: MetricsApi;

  /**
   * `GET /governance/voters` — role-agnostic lookup, so a vote row can resolve
   * its voter without the caller knowing which sub-resource owns it.
   */
  voters: {
    resolve(id: string): Promise<Envelope<Voter>>;
    list(q?: PageRequest & { role?: VoterRole[]; search?: string }): Promise<
      PagedEnvelope<VoterRef>
    >;
  };
}
