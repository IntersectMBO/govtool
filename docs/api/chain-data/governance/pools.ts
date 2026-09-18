/**
 * Chain Data API — `/governance/pools/*`
 *
 * Stake pools as governance voters. Only the fields governance surfaces need;
 * full pool metrics belong to a pool explorer, not here.
 */

import type {
  Bech32,
  Envelope,
  Lovelace,
  PagedEnvelope,
  PageRequest,
  VotingPower,
} from "../common";
import type { VoterRef } from "../refs";
import type { VoteListQuery, VoteRecord } from "./votes";

export interface SpoVoter extends VoterRef {
  role: "spo";
  poolId: Bech32;
  ticker?: string;
  name?: string;
  liveStake?: Lovelace;
  activeStake?: Lovelace;
  pledge?: Lovelace;
  /** `basis: "active"` — the snapshot SPO votes are weighted by. */
  votingPower: VotingPower | null;
}

export interface PoolsApi {
  /** `GET /governance/pools` */
  list(q?: PageRequest & { search?: string }): Promise<PagedEnvelope<SpoVoter>>;
  /** `GET /governance/pools/{id}` */
  get(id: string): Promise<Envelope<SpoVoter>>;
  /** `GET /governance/pools/{id}/votes` */
  listVotes(id: string, q?: VoteListQuery): Promise<PagedEnvelope<VoteRecord>>;
}
