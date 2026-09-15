export type GovernanceActionType =
  | 'ParameterChange'
  | 'HardForkInitiation'
  | 'TreasuryWithdrawals'
  | 'NoConfidence'
  | 'NewCommittee'
  | 'NewConstitution'
  | 'InfoAction';

export type GovernanceActionSortMode =
  | 'SoonestToExpire'
  | 'NewestCreated'
  | 'MostYesVotes';

export type Proposal = {
  id: number | string;
  tx_hash: string;
  index: number | string;
  type: GovernanceActionType | string;
  description: unknown;
  expiry_date: Date | string | null;
  expiration: number | string | null;
  time: Date | string;
  epoch_no: number | string;
  url: string;
  data_hash: string;
  proposal_params: unknown | null;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
  yes_votes: number | string;
  no_votes: number | string;
  abstain_votes: number | string;
  pool_yes_votes: number | string;
  pool_no_votes: number | string;
  pool_abstain_votes: number | string;
  cc_yes_votes: number | string;
  cc_no_votes: number | string;
  cc_abstain_votes: number | string;
  prev_gov_action_index: number | string | null;
  prev_gov_action_tx_hash: string | null;
  json_content: unknown | null;
  authors: unknown | null;
};

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
  url: string;
  metadataHash: string;
  protocolParams: unknown | null;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
  prevGovActionIndex: number | null;
  prevGovActionTxHash: string | null;
  json: unknown | null;
  authors: unknown | null;
};

export type ListProposalsResponse = {
  page: number;
  pageSize: number;
  total: number;
  elements: ProposalResponse[];
};

export type GetProposalResponse = {
  vote: unknown | null;
  proposal: ProposalResponse;
};

export type EnactedProposalDetailsRow = {
  id: number | string;
  tx_id: number | string;
  index: number | string;
  description: unknown | null;
  hash: string;
};

export type EnactedProposalDetailsResponse = {
  id: number;
  txId: number;
  index: number;
  description: unknown | null;
  hash: string;
};
