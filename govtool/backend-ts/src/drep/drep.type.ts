import { ProposalResponse } from 'src/proposal/proposal.type';
export type DRepVotingPower = {
    amount: number | string;
};

export type DRepVotingPowerList = {
    view: string;
    hash_raw: string;
    voting_power: number | string;
    given_name: string | null;
};

export type DRepVotingPowerListResponse = {
  view: string;
  hashRaw: string;
  votingPower: number;
  givenName: string | null;
};

export type DRepStatus = 'Active' | 'Inactive' | 'Retired';
export type DRepType = 'DRep' | 'SoleVoter';

export type DRepListSort = 'Random' | 'VotingPower' | 'Activity' | 'RegistrationDate' | 'Status';


export type DRepInfo = {
  is_script_based: boolean;
  is_registered_as_drep: boolean | null;
  was_registered_as_drep: boolean | null;
  is_registered_as_sole_voter: boolean | null;
  was_registered_as_sole_voter: boolean | null;
  deposit: number | string | null;
  url: string | null;
  data_hash: string | null;
  voting_power: number | string | null;
  drep_register_tx_hash: string | null;
  drep_retire_tx_hash: string | null;
  sole_voter_register_tx_hash: string | null;
  sole_voter_retire_tx_hash: string | null;
  payment_address: string | null;
  given_name: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  image_url: string | null;
  image_hash: string | null;
};

export type DRepInfoResponse = {
  isScriptBased: boolean;
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: number | null;
  url: string | null;
  dataHash: string | null;
  votingPower: number | null;
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

export type DRepList = {
  drep_hash: string;
  view: string;
  has_script: boolean;
  url: string | null;
  metadata_hash: string | null;
  deposit: number | string;
  amount: number | string | null;
  active: boolean;
  tx_hash: string | null;
  last_register_time: Date | string;
  latest_deposit: number | string;
  has_non_deregister_voting_anchor: boolean | null;
  fetch_error: string | null;
  payment_address: string | null;
  given_name: string | null;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  image_url: string | null;
  image_hash: string | null;
  votes_last_year: number | string | null;
  identity_references: unknown | null;
  link_references: unknown | null;
};

export type DRepListItem = {
  isScriptBased: boolean;
  drepId: string;
  view: string;
  url: string | null;
  metadataHash: string | null;
  deposit: number;
  votingPower: number | null;
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
  identityReferences: unknown | null;
  linkReferences: unknown | null;
};

export type DRepListResponse = {
  page: number;
  pageSize: number;
  total: number;
  elements: DRepListItem[];
};

export type DRepVoteRow = {
  proposal_id: number | string;
  gov_action_id: string;
  drep_id: string;
  vote: string;
  url: string | null;
  doc_hash: string | null;
  epoch_no: number | string;
  date: Date | string;
  vote_tx_hash: string;
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
