import { MetadataValidationStatus } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";

export type EpochParams = {
  block_id: number | null;
  coins_per_utxo_size: number | null;
  collateral_percent: number | null;
  committee_max_term_length: number | null;
  committee_min_size: number | null;
  cost_model_id: number | null;
  decentralisation: number | null;
  drep_activity: number | null;
  drep_deposit: number | null;
  dvt_committee_no_confidence: number | null;
  dvt_committee_normal: number | null;
  dvt_hard_fork_initiation: number | null;
  dvt_motion_no_confidence: number | null;
  dvt_p_p_economic_group: number | null;
  dvt_p_p_gov_group: number | null;
  dvt_p_p_network_group: number | null;
  dvt_p_p_technical_group: number | null;
  dvt_treasury_withdrawal: number | null;
  dvt_update_to_constitution: number | null;
  epoch_no: number | null;
  extra_entropy: null;
  gov_action_deposit: number | null;
  gov_action_lifetime: number | null;
  id: number;
  influence: number | null;
  key_deposit: number | null;
  max_bh_size: number | null;
  max_block_ex_mem: number | null;
  max_block_ex_steps: number | null;
  max_block_size: number | null;
  max_collateral_inputs: number | null;
  max_epoch: number | null;
  max_tx_ex_mem: number | null;
  max_tx_ex_steps: number | null;
  max_tx_size: number | null;
  max_val_size: number | null;
  min_fee_a: number | null;
  min_fee_b: number | null;
  min_fee_ref_script_cost_per_byte: number | null;
  min_pool_cost: number | null;
  min_utxo_value: number | null;
  monetary_expand_rate: number | null;
  nonce: string | null;
  optimal_pool_count: number | null;
  pool_deposit: number | null;
  price_mem: number | null;
  price_step: number | null;
  protocol_major: number | null;
  protocol_minor: number | null;
  pvt_committee_no_confidence: number | null;
  pvt_committee_normal: number | null;
  pvt_hard_fork_initiation: number | null;
  pvt_motion_no_confidence: number | null;
  pvtpp_security_group: number | null;
  treasury_growth_rate: number | null;
};

export type TransactionStatus = {
  transactionConfirmed: boolean;
  votingProcedure:
    | {
        committee_voter: number | null;
        drep_voter: number | null;
        gov_action_proposal_id: number;
        id: number;
        index: number;
        invalid: boolean | null;
        pool_voter: number | null;
        tx_id: number;
        vote: Vote;
        voter_role: "DRep" | "Pool" | "Committee";
        voting_anchor_id: number | null;
      }[]
    | [];
};

export enum Network {
  samchonet = "sanchonet",
  preview = "preview",
  testnet = "testnet",
  preprod = "preprod",
  mainnet = "mainnet",
}

export type NetworkInfo = {
  currentTime: string;
  currentEpoch: number;
  currentBlock: number;
  networkName: Network;
};

export type NetworkTotalStake = {
  totalStakeControlledByDReps: number;
  totalStakeControlledBySPOs: number;
  alwaysAbstainVotingPower: number;
  alwaysNoConfidenceVotingPower: number;
};

export type NetworkMetrics = {
  uniqueDelegators: number;
  totalDelegations: number;
  totalGovernanceActions: number;
  totalDRepVotes: number;
  totalRegisteredDReps: number;
  totalDRepDistr: number;
  totalActiveDReps: number;
  totalInactiveDReps: number;
  totalActiveCIP119CompliantDReps: number;
  totalRegisteredDirectVoters: number;
  noOfCommitteeMembers: number;
  quorumNumerator: number;
  quorumDenominator: number;
};

export type VoterInfo = {
  dRepRegisterTxHash: string | null;
  dRepRetireTxHash: string | null;
  deposit: number;
  givenName: string | null;
  imageHash: string | null;
  imageUrl: string | null;
  isRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  motivations: string | null;
  objectives: string | null;
  paymentAddress: string | null;
  qualifications: string | null;
  soleVoterRegisterTxHash: string | null;
  soleVoterRetireTxHash: string | null;
  url: string | null;
  votingPower: number | null;
  wasRegisteredAsDRep: boolean;
  wasRegisteredAsSoleVoter: boolean;
};

export enum DRepStatus {
  Active = "Active",
  Inactive = "Inactive",
  Retired = "Retired",
  Yourself = "Yourself",
}

export enum DRepListSort {
  Random = "Random",
  VotingPower = "VotingPower",
  RegistrationDate = "RegistrationDate",
  Status = "Status",
}

export type DrepDataDTO = {
  deposit: number;
  drepId: string;
  isScriptBased: boolean;
  latestRegistrationDate: string;
  latestTxHash?: string;
  metadataHash?: string;
  status: DRepStatus;
  type: "DRep" | "SoleVoter";
  url?: string;
  view: string;
  votingPower?: number;
  imageUrl: string | null;
  // either base64 for IPFS image or URL for regular image
  image: string | null;
};

export type DRepData = DrepDataDTO & {
  paymentAddress: string | null;
  givenName: string;
  objectives: string | null;
  motivations: string | null;
  qualifications: string | null;
  references: Reference[];
  doNotList: boolean;
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
  imageUrl: string | null;
  // either base64 for IPFS image or URL for regular image
  image: string | null;
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

export type SubmittedVotesData = {
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  type: GovernanceActionType;
  protocolParams: EpochParams | null;
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
  prevGovActionIndex: number | null;
  prevGovActionTxHash: string | null;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: Reference[];
  title?: string;
  protocolParams: EpochParams | null;
} & SubmittedVotesData;

export type ProposalData = ProposalDataDTO & {
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
};

export type NewConstitutionAnchor = {
  dataHash: string;
  url: string;
};

export type VotedProposalDTO = {
  vote: ProposalVote | null;
  proposal: ProposalDataDTO;
};

export type VotedProposal = {
  vote: ProposalVote | null;
  proposal: ProposalData;
};

export type CurrentDelegation = {
  dRepHash: string | null;
  dRepView: string | null;
  isDRepScriptBased: boolean;
  txHash: string | null;
} | null;

export type Infinite<T> = {
  elements: T[];
  page: number;
  pageSize: number;
  total: number;
};

type DRepVotingPower = {
  view: string;
  hashRaw: string;
  votingPower: number;
  givenName: string | null;
};

export type DRepVotingPowerListResponse = DRepVotingPower[];
