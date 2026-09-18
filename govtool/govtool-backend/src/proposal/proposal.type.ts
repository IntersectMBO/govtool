import type { ApiInteger } from 'src/common/integer';

/**
 * Legacy wire shapes for the `/proposal/*` routes.
 *
 * The raw db-sync row types that used to live here moved to
 * `@govtool/provider-dbsync` (`src/rows`): this package no longer sees a
 * database row, only the Chain Data contract's entities.
 */

export const governanceActionTypes = [
  'ParameterChange',
  'HardForkInitiation',
  'TreasuryWithdrawals',
  'NoConfidence',
  'NewCommittee',
  'NewConstitution',
  'InfoAction',
] as const;
export type GovernanceActionType = (typeof governanceActionTypes)[number];

export const governanceActionSortModes = [
  'SoonestToExpire',
  'NewestCreated',
  'MostYesVotes',
] as const;
export type GovernanceActionSortMode =
  (typeof governanceActionSortModes)[number];

export type ProposalResponse = {
  id: string;
  txHash: string;
  index: number;
  type: GovernanceActionType;
  details: unknown;
  expiryDate: string | null;
  expiryEpochNo: number | null;
  createdDate: string;
  createdEpochNo: number;
  // LEFT JOINed on voting_anchor, so both are absent for an action with no anchor.
  url: string | null;
  metadataHash: string | null;
  protocolParams: unknown;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
  dRepYesVotes: ApiInteger;
  dRepNoVotes: ApiInteger;
  dRepAbstainVotes: ApiInteger;
  poolYesVotes: ApiInteger;
  poolNoVotes: ApiInteger;
  poolAbstainVotes: ApiInteger;
  ccYesVotes: ApiInteger;
  ccNoVotes: ApiInteger;
  ccAbstainVotes: ApiInteger;
  prevGovActionIndex: number | null;
  prevGovActionTxHash: string | null;
  json: unknown;
  authors: unknown;
};

export type ListProposalsResponse = {
  page: number;
  pageSize: number;
  total: number;
  elements: ProposalResponse[];
};

export type GetProposalResponse = {
  vote: unknown;
  proposal: ProposalResponse;
};

export type EnactedProposalDetailsResponse = {
  // Null under a provider with no internal row ids; see `toLegacyRowId`.
  id: number | null;
  txId: number | null;
  index: number;
  description: unknown;
  hash: string;
};
