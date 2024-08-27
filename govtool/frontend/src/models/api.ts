import { MetadataValidationStatus } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";

export type EpochParams = {
  block_id: number;
  coins_per_utxo_size: number;
  collateral_percent: number;
  committee_max_term_length: number;
  committee_min_size: number;
  cost_model_id: number;
  decentralisation: number;
  drep_activity: number;
  drep_deposit: number;
  dvt_committee_no_confidence: number;
  dvt_committee_normal: number;
  dvt_hard_fork_initiation: number;
  dvt_motion_no_confidence: number;
  dvt_pp_economic_group: number;
  dvt_pp_gov_group: number;
  dvt_pp_network_group: number;
  dvt_pp_technical_group: number;
  dvt_treasury_withdrawal: number;
  dvt_update_to_constitution: number;
  epoch_no: number;
  extra_entropy: null;
  gov_action_deposit: number;
  gov_action_lifetime: number;
  id: number;
  influence: number;
  key_deposit: number;
  max_bh_size: number;
  max_block_ex_mem: number;
  max_block_ex_steps: number;
  max_block_size: number;
  max_collateral_inputs: number;
  max_epoch: number;
  max_tx_ex_mem: number;
  max_tx_ex_steps: number;
  max_tx_size: number;
  max_val_size: number;
  min_fee_a: number;
  min_fee_b: number;
  min_fee_ref_script_cost_per_byte: number;
  min_pool_cost: number;
  min_utxo_value: number;
  monetary_expand_rate: number;
  nonce: string;
  optimal_pool_count: number;
  pool_deposit: number;
  price_mem: number;
  price_step: number;
  protocol_major: number;
  protocol_minor: number;
  pvt_committee_no_confidence: number;
  pvt_committee_normal: number;
  pvt_hard_fork_initiation: number;
  pvt_motion_no_confidence: number;
  pvtpp_security_group: number;
  treasury_growth_rate: number;
};

export type NetworkMetrics = {
  currentTime: string;
  currentEpoch: number;
  currentBlock: number;
  uniqueDelegators: number;
  totalDelegations: number;
  totalGovernanceActions: number;
  totalDRepVotes: number;
  totalRegisteredDReps: number;
  alwaysAbstainVotingPower: number;
  alwaysNoConfidenceVotingPower: number;
  networkName: string;
};

export type VoterInfo = {
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: number;
  dRepRegisterTxHash: string | null;
  dRepRetireTxHash: string | null;
  soleVoterRegisterTxHash: string | null;
  soleVoterRetireTxHash: string | null;
};

export enum DRepStatus {
  Active = "Active",
  Inactive = "Inactive",
  Retired = "Retired",
  Yourself = "Yourself",
}

export enum DRepListSort {
  VotingPower = "VotingPower",
  RegistrationDate = "RegistrationDate",
  Status = "Status",
}

export type DrepDataDTO = {
  deposit: number;
  drepId: string;
  latestRegistrationDate: string;
  latestTxHash?: string;
  metadataHash?: string;
  status: DRepStatus;
  type: "DRep" | "SoleVoter";
  url?: string;
  view: string;
  votingPower?: number;
};

export type DRepData = DrepDataDTO & {
  paymentAddress: string | null;
  givenName: string;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  references: string[];
  doNotList: boolean;
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
};

export type Vote = "yes" | "no" | "abstain";

export type ProposalVote = {
  date: string;
  drepId: string;
  epochNo: number;
  metadataHash: string;
  proposalId: string;
  txHash: string;
  url: string;
  vote: Vote;
};

export type ProposalDataDTO = {
  createdDate: string;
  createdEpochNo: number;
  details?: ActionDetailsType;
  expiryDate?: string;
  expiryEpochNo?: number;
  id: string;
  index: number;
  metadataHash: string;
  txHash: string;
  type: GovernanceActionType;
  url: string;
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  prevGovActionIndex: number | null;
  prevGovActionTxHash: string | null;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: string[];
  title?: string;
  protocolParams: EpochParams | null;
};

export type ProposalData = ProposalDataDTO & {
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
};

export type VotedProposalDTO = {
  vote: ProposalVote;
  proposal: ProposalDataDTO;
};

export type VotedProposal = {
  vote: ProposalVote;
  proposal: ProposalData;
};

export type CurrentDelegation = {
  dRepHash: string | null;
  dRepView: string | null;
  txHash: string | null;
} | null;

export type Infinite<T> = {
  elements: T[];
  page: number;
  pageSize: number;
  total: number;
};
