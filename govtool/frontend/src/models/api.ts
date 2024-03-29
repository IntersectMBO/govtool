export interface VoterInfo {
  isRegisteredAsDRep: boolean;
  wasRegisteredAsDRep: boolean;
  isRegisteredAsSoleVoter: boolean;
  wasRegisteredAsSoleVoter: boolean;
  deposit: number;
}

export interface DRepData {
  drepId: string;
  url: string;
  metadataHash: string;
  deposit: number;
}

export type Vote = "yes" | "no" | "abstain";

export interface VotedProposal {
  vote: {
    proposalId: string;
    drepId: string;
    vote: Vote;
    url: string;
    metadataHash: string;
    date: string;
    epochNo: number;
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
