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
  title: string | null;
  about: string | null;
  motivation: string | null;
  rationale: string | null;
  metadata: any;
  references: any;
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
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
  email?: string;
  bio?: string;
  extraContentLinks?: string[];
};

export enum ProposalType {
  info = "Info",
  treasury = "Treasury",
}

export enum FilterOption {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  TreasuryWithdrawal = "TreasuryWithdrawals",
  HardFork = "HardForkInitiation",
  NoConfidence = "NoConfidence",
  NewCommittee = "NewCommittee",
  UpdatetotheConstitution = "NewConstitution",
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
