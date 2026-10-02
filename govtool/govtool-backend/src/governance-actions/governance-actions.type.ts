import type { ApiInteger } from 'src/common/integer';
import type { LegacyDescription } from 'src/common/legacy-description';
import type { VoteAggregate } from '@govtool/data-providers/chain-data';
import type { LegacyParamProposal } from 'src/epoch/epoch.type';

/** Governance action response shapes, retaining existing field names. */

export const governanceActionSortOptions = [
  'newestFirst',
  'oldestFirst',
  'highestYesVotes',
] as const;
export type GovernanceActionSort = (typeof governanceActionSortOptions)[number];

/** Status words the list filter takes; anything else is an action type. */
export const governanceActionStatusFilters = [
  'expired',
  'ratified',
  'enacted',
  'live',
] as const;

export type GovernanceActionStatus = {
  ratified_epoch: number | null;
  enacted_epoch: number | null;
  dropped_epoch: number | null;
  expired_epoch: number | null;
};

export type GovernanceActionStatusTimes = {
  ratified_time: string | null;
  enacted_time: string | null;
  dropped_time: string | null;
  expired_time: string | null;
};

export type GovernanceActionDescription = LegacyDescription;

export type GovernanceActionListRow = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  yes_votes: ApiInteger | null;
  no_votes: ApiInteger | null;
  abstain_votes: ApiInteger | null;
  description: GovernanceActionDescription;
  expiry_date: string | null;
  expiration: number | null;
  time: string | null;
  epoch_no: number;
  url: string | null;
  data_hash: string | null;
  proposal_params: LegacyParamProposal | null;
  title: string | null;
  abstract: string | null;
  status: GovernanceActionStatus;
  status_times: GovernanceActionStatusTimes;
};

export type GovernanceActionDetailRow = {
  id: string;
  tx_hash: string;
  index: number;
  type: string;
  description: GovernanceActionDescription;
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
  yes_votes: ApiInteger | null;
  no_votes: ApiInteger | null;
  abstain_votes: ApiInteger | null;
  pool_yes_votes: ApiInteger | null;
  pool_no_votes: ApiInteger | null;
  pool_abstain_votes: ApiInteger | null;
  cc_yes_votes: ApiInteger | null;
  cc_no_votes: ApiInteger | null;
  cc_abstain_votes: ApiInteger | null;
  /** Complete per-role tallies at this action's tally epoch; absent roles are unavailable. */
  vote_aggregates: VoteAggregate[];
  /**
   * A decimal string, as the governance action service sent it: the UI tests it for
   * truthiness before linking the previous action, so a number 0 would hide
   * the link (D146).
   */
  prev_gov_action_index: string | null;
  prev_gov_action_tx_hash: string | null;
  used_epoch_no: number;
  status: GovernanceActionStatus;
  status_times: GovernanceActionStatusTimes;
};

export type GovernanceActionNetworkMetrics = {
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

export type GovernanceActionMetadataResponse = {
  metadataStatus: string | null;
  metadataValid: boolean;
  data?: Record<string, unknown>;
};

export type SignatureVerificationResult = {
  isValid: boolean;
  message?: string;
  error?: string;
};
