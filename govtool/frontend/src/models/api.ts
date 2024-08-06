import { MetadataValidationStatus } from "@models";

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

export interface DrepDataDTO {
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
}

export interface DRepData extends DrepDataDTO {
  bio: string | null;
  dRepName: string | null;
  email: string | null;
  references: string[];
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
}

export type Vote = "yes" | "no" | "abstain";

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

export type ProposalDataDTO = {
  abstainVotes: number;
  createdDate: string;
  createdEpochNo: number;
  details?: ActionDetailsType;
  expiryDate?: string;
  expiryEpochNo?: number;
  id: string;
  index: number;
  metadataHash: string;
  noVotes: number;
  txHash: string;
  type: string;
  url: string;
  yesVotes: number;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: string[];
  title?: string;
};

export type ProposalData = ProposalDataDTO & {
  metadataStatus: MetadataValidationStatus | null;
  metadataValid: boolean;
}

export type VotedProposalDTO = {
  vote: ProposalVote;
  proposal: ProposalDataDTO;
}

export type VotedProposal = {
  vote: ProposalVote;
  proposal: ProposalData;
}

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
}
