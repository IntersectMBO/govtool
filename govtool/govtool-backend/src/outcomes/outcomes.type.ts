import type { ApiInteger } from 'src/common/integer';
import type { LegacyDescription } from 'src/common/legacy-description';
import type { LegacyParamProposal } from 'src/epoch/epoch.type';

/**
 * The wire shapes of the governance outcomes routes, which the
 * `@intersect.mbo/govtool-outcomes-pillar-ui` package reads (its
 * `src/types/api.ts`). Snake_case and db-sync-flavoured because that package
 * was written against a db-sync-backed service; the names stop here.
 */

export const outcomeSortOptions = [
  'newestFirst',
  'oldestFirst',
  'highestYesVotes',
] as const;
export type OutcomeSort = (typeof outcomeSortOptions)[number];

/** Status words the list filter takes; anything else is an action type. */
export const outcomeStatusFilters = [
  'expired',
  'ratified',
  'enacted',
  'live',
] as const;

export type OutcomeStatus = {
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
};

export type OutcomeStatusTimes = {
  ratified_time: string | null;
  enacted_time: string | null;
  dropped_time: string | null;
  expired_time: string | null;
};

export type OutcomeDescription = LegacyDescription;

export type OutcomeListRow = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  yes_votes: ApiInteger;
  no_votes: ApiInteger;
  abstain_votes: ApiInteger;
  description: OutcomeDescription;
  expiry_date: string | null;
  expiration: number | null;
  time: string | null;
  epoch_no: number;
  url: string | null;
  data_hash: string | null;
  proposal_params: LegacyParamProposal | null;
  title: string | null;
  abstract: string | null;
  status: OutcomeStatus;
  status_times: OutcomeStatusTimes;
};

export type OutcomeDetailRow = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  description: OutcomeDescription;
  expiry_date: string | null;
  expiration: number | null;
  time: string | null;
  epoch_no: number;
  url: string | null;
  data_hash: string | null;
  proposal_params: LegacyParamProposal | null;
  json_metadata: Record<string, unknown> | null;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
  yes_votes: ApiInteger;
  no_votes: ApiInteger;
  abstain_votes: ApiInteger;
  pool_yes_votes: ApiInteger;
  pool_no_votes: ApiInteger;
  pool_abstain_votes: ApiInteger;
  cc_yes_votes: ApiInteger;
  cc_no_votes: ApiInteger;
  cc_abstain_votes: ApiInteger;
  /**
   * A decimal string, as the outcomes service sent it: the UI tests it for
   * truthiness before linking the previous action, so a number 0 would hide
   * the link (D146).
   */
  prev_gov_action_index: string | null;
  prev_gov_action_tx_hash: string | null;
  used_epoch_no: number;
  status: OutcomeStatus;
  status_times: OutcomeStatusTimes;
};

export type OutcomeNetworkMetrics = {
  epoch_no: number;
  /** Lovelace, as a decimal string. */
  total_stake_controlled_by_active_dreps: string;
  total_stake_controlled_by_stake_pools: string;
  always_abstain_voting_power: string;
  spos_abstain_voting_power: string;
  always_no_confidence_voting_power: string;
  spos_no_confidence_voting_power: string;
  no_of_committee_members: number;
  quorum_numerator: number;
  quorum_denominator: number;
};

export type OutcomeMetadataResponse = {
  metadataStatus: string | null;
  metadataValid: boolean;
  data?: Record<string, unknown>;
};

export type SignatureVerificationResult = {
  isValid: boolean;
  message?: string;
  error?: string;
};
