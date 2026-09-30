/**
 * Chain Data API — `/governance/pools/*`
 *
 * Stake pools as governance voters. Ticker and name are NOT here: pool metadata
 * is anchored by the pool registration certificate and is resolved by the
 * metadata service like any other document.
 */

import type {
  Anchor,
  Bech32,
  Envelope,
  Lovelace,
  PageRequest,
  PagedEnvelope,
  VotingPower,
} from '../common';
import type { VoterRef } from '../refs';
import type { VoteRecord } from './votes';

export interface SpoVoter extends VoterRef {
  role: 'spo';
  poolId: Bech32;
  /** The pool registration certificate's metadata anchor. */
  anchor: Anchor | null;
  /** The snapshot SPO votes are weighted by. */
  votingPower: VotingPower | null;
  activeStake?: Lovelace;
  liveStake?: Lovelace;
  pledge?: Lovelace;
}

export interface PoolsApi {
  list(q: PageRequest & { search?: string }): Promise<PagedEnvelope<SpoVoter>>;
  get(id: string): Promise<Envelope<SpoVoter>>;

  /**
   * Optional, like every individual-vote listing — only the per-action
   * aggregate is required. This is what a pool's page renders, and what the
   * index provider makes findable.
   */
  listVotes?(id: string, q: PageRequest): Promise<PagedEnvelope<VoteRecord>>;
}
