import type { ApiInteger } from 'src/common/integer';

/**
 * Legacy wire shapes for the `/drep/*` routes.
 *
 * The raw db-sync row types that used to live here moved to
 * `@govtool/provider-dbsync` (`src/rows`).
 */

import { ProposalResponse } from 'src/proposal/proposal.type';
export type DRepVotingPowerListResponse = {
  view: string;
  /** NULL for the predefined options, which have no credential hash. */
  hashRaw: string | null;
  votingPower: ApiInteger;
  givenName: string | null;
};

export const drepStatuses = ['Active', 'Inactive', 'Retired'] as const;
export type DRepStatus = (typeof drepStatuses)[number];
export type DRepType = 'DRep' | 'SoleVoter';

export const drepListSorts = [
  'Random',
  'VotingPower',
  'Activity',
  'RegistrationDate',
  'Status',
] as const;
export type DRepListSort = (typeof drepListSorts)[number];

export type DRepInfoResponse = {
  isScriptBased: boolean;
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: ApiInteger | null;
  url: string | null;
  dataHash: string | null;
  votingPower: ApiInteger | null;
  dRepRegisterTxHash: string | null;
  dRepRetireTxHash: string | null;
  soleVoterRegisterTxHash: string | null;
  soleVoterRetireTxHash: string | null;
  paymentAddress: string | null;
  givenName: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  imageUrl: string | null;
  imageHash: string | null;
};

export type DRepListItem = {
  isScriptBased: boolean;
  drepId: string;
  view: string;
  url: string | null;
  metadataHash: string | null;
  deposit: ApiInteger;
  votingPower: ApiInteger | null;
  status: DRepStatus;
  type: DRepType;
  latestTxHash: string | null;
  latestRegistrationDate: string;
  metadataError: string | null;
  paymentAddress: string | null;
  givenName: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  imageUrl: string | null;
  imageHash: string | null;
  votesLastYear: number | null;
  identityReferences: unknown;
  linkReferences: unknown;
};

export type DRepListResponse = {
  page: number;
  pageSize: number;
  total: number;
  elements: DRepListItem[];
};

export type VoteParams = {
  proposalId: string;
  drepId: string;
  vote: string;
  url: string | null;
  metadataHash: string | null;
  epochNo: number;
  date: string;
  txHash: string;
};

export type VoteResponse = {
  vote: VoteParams;
  proposal: ProposalResponse;
};

export type DRepListParams = {
  page?: number;
  pageSize?: number;
  search?: string;
  status: DRepStatus[];
  sort?: DRepListSort;
  seed?: string;
};
