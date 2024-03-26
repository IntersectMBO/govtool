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
  type: 'DRep' | 'SoleVoter';
}

export type Vote = "yes" | "no" | "abstain";

export interface VotedProposal {
  vote: {
    proposalId: string;
    drepId: string;
    vote: Vote;
    url: string;
    metadataHash: string;
  };
  proposal: {
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
    title: string | null;
    about: string | null;
    motivation: string | null;
    rationale: string | null;
  };
}
