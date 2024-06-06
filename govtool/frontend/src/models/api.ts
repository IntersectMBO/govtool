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
  type: "DRep" | "SoleVoter";
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

export type ProposalData = {
  abstainVotes: number;
  createdDate: string;
  createdEpochNo: number;
  expiryDate: string;
  expiryEpochNo: number;
  id: string;
  index: number;
  metadataValid: boolean;
  noVotes: number;
  txHash: string;
  type: string;
  yesVotes: number;
  abstract?: string;
  details?: ActionDetailsType;
  metadataHash?: string;
  metadataStatus?: MetadataValidationStatus;
  motivation?: string;
  rationale?: string;
  references?: string[];
  title?: string;
  url?: string;
};
export interface VotedProposal {
  vote: ProposalVote;
  proposal: ProposalData;
}

export type CurrentDelegation = {
  dRepHash: string | null;
  dRepView: string | null;
  txHash: string | null;
} | null;
