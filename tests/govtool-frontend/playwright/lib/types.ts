import { CardanoTestWalletJson } from "@cardanoapi/cardano-test-wallet/types";

export type StaticWallet = CardanoTestWalletJson & {
  dRepId: string;
  address: string;
};

export type KuberValue = {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
};

export interface IProposal {
  id: string;
  txHash: string;
  index: number;
  type: string;
  details: any;
  expiryDate: string;
  expiryEpochNo: number;
  createdDate: string;
  createdEpochNo: number;
  url: string;
  metadataHash: string;
  protocolParams: EpochParams | null;
  title: string | null;
  about: string | null;
  motivation: string | null;
  rationale: string | null;
  metadata: any;
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
  prevGovActionIndex: null | number;
  prevGovActionTxHash: null | string;
}

export type IVote = {
  drepId: string;
  metadataHash: string;
  url: string;
  proposalId: string;
  vote: string; // You might want to consider using a more specific type, like 'VoteType' enum
};

export type IVotedProposal = {
  proposal: IProposal;
  vote: IVote;
};

export type IDRepInfo = {
  name: string;
  objectives?: string;
  motivations?: string;
  qualifications?: string;
  paymentAddress?: string;
  identityReferenceLinks?: LinkType[];
  linksReferenceLinks?: LinkType[];
  donNotList?: boolean;
};

export type LinkType = {
  url: string;
  description: string;
};

export enum ProposalType {
  info = "Info",
  treasury = "Treasury",
}

export enum GovernanceActionType {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  TreasuryWithdrawal = "TreasuryWithdrawals",
  HardFork = "HardForkInitiation",
  NoConfidence = "NoConfidence",
  NewCommittee = "NewCommittee",
  UpdatetotheConstitution = "NewConstitution",
}

export enum FullGovernanceDRepVoteActionsType {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  TreasuryWithdrawal = "TreasuryWithdrawals",
  HardFork = "HardForkInitiation",
}

export enum BootstrapGovernanceActionType {
  InfoAction = "InfoAction",
}

export type DRepStatus = "Active" | "Inactive" | "Retired";

export type IDRep = {
  drepId: string;
  view: string;
  url: string;
  metadataHash: string;
  deposit: number;
  votingPower: number;
  status: DRepStatus;
  type: string;
  latestTxHash: string;
  latestRegistrationDate: string;
};

export type ProtocolParams = {
  dRepDeposit: number;
  govActionDeposit: number;
  protocolVersion: ProtocolVersionType;
};

type Comment = {
  proposal_id: string;
  comment_text: string;
};

export type StaticProposal = {
  id: number;
  comments?: Comment[];
  title: string;
};

export type CommentResponse = {
  id: number;
  attributes: {
    proposal_id: string;
    comment_parent_id: null | string;
    user_id: string;
    comment_text: string;
    createdAt: string;
    updatedAt: string;
    user_govtool_username: string;
    subcommens_number: number;
  };
};

export type ProposalLink = {
  prop_link: string;
  prop_link_text: string;
};

export type ProposalCreateRequest = {
  proposal_links: Array<ProposalLink>;
  gov_action_type_id: number;
  prop_name: string;
  prop_abstract: string;
  prop_motivation: string;
  prop_rationale: string;
  prop_receiving_address?: string;
  prop_amount?: string;
  is_draft: boolean;
};

export type ProposedGovAction = {
  id: number;
  attributes: {
    gov_action_type_name: string;
    createdAt: string;
    updatedAt: string;
  };
};

export type WalletAndAnchorType = {
  url: string;
  dataHash: string;
  wallet: StaticWallet;
};

export type ProtocolVersionType = {
  major: number;
  minor: number;
};

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
