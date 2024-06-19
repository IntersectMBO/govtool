import { CardanoTestWallet } from "@cardanoapi/cardano-test-wallet/types";

export type StaticWallet = CardanoTestWallet & {
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

export type IProposalForm = {
  title: string;
  abstract: string;
  motivation: string;
  rationale: string;
  extraContentLinks?: string[];
  type: ProposalType;
  receivingAddress?: string;
  amount?: string;
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

export type ProposalCreationPayload = {
  proposal_links: Array<ProposalLinksType>;
  gov_action_type_id: number;
  prop_name: string;
  prop_abstract: string;
  prop_motivation: string;
  prop_rationale: string;
  prop_receiving_address: string;
  prop_amount: string;
  is_draft: boolean;
};

type ProposalLinksType = {
  prop_link: string;
  prop_link_text: string;
};

export type ProposalCreationResponse = {
  data: {
    attributes: {
      proposal_id: number;
      proposal_content_id: number;
    };
  };
  meta: Object;
};

export type AddPollPayload = {
  data: {
    proposal_id: string;
    poll_start_dt: string;
    is_poll_active: boolean;
  };
};
