import { MetadataValidationStatus } from "@models";

export interface VoterInfo {
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: number;
}

export enum DRepStatus {
  Active = "Active",
  Inactive = "Inactive",
  Retired = "Retired",
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

export type Vote = "yes" | "no" | "abstain";

export type InfinityProposals = {
  elements: ProposalData[];
  page: number;
  pageSize: number;
  total: number;
};

type ProposalVote = {
  proposalId: string;
  drepId: string;
  vote: Vote;
  url: string;
  metadataHash: string;
  date: string;
  epochNo: number;
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
