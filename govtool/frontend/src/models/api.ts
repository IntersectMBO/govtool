import { MetadataValidationStatus } from "@models";

export interface VoterInfo {
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: number;
  dRepRegisterTxHash: string | null;
  dRepRetireTxHash: string | null;
  soleVoterRegisterTxHash: string | null;
  soleVoterRetireTxHash: string | null;
}

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

export interface DRepData {
  drepId: string;
  view: string;
  url: string;
  metadataHash: string;
  deposit: number;
  votingPower: number;
  status: DRepStatus;
  type: "DRep" | "DirectVoter";
}
export type InfinityDRepData = {
  elements: DRepData[];
  page: number;
  pageSize: number;
  total: number;
};

export type Vote = "yes" | "no" | "abstain";

export type InfinityProposals = {
  elements: ProposalData[];
  page: number;
  pageSize: number;
  total: number;
};

type ProposalVote = {
  date: string;
  drepId: string;
  epochNo: number;
  metadataHash: string;
  proposalId: string;
  txHash: string;
  url: string;
  vote: Vote;
};

type ProposalData = {
  id: string;
  type: string;
  details?: ActionDetailsType;
  expiryDate: string;
  expiryEpochNo: number;
  createdDate: string;
  createdEpochNo: number;
  url: string;
  metadataHash: string;
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
  txHash: string;
  index: number;
  title?: string;
  about?: string;
  motivation?: string;
  rationale?: string;
};
export interface VotedProposal {
  vote: ProposalVote;
  proposal: ProposalData;
}
export type VotedProposalToDisplay = {
  vote: ProposalVote;
  proposal: ProposalData & {
    isDataMissing: boolean | MetadataValidationStatus;
  };
};

export type CurrentDelegation = {
  dRepHash: string | null;
  dRepView: string | null;
  txHash: string | null;
} | null;
