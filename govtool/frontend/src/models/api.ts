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
  };
  proposal: {
    id: string;
    type: string;
    details: string;
    expiryDate: string;
    createdDate: string;
    url: string;
    metadataHash: string;
    yesVotes: number;
    noVotes: number;
    abstainVotes: number;
    txHash: string;
    index: number;
  };
}
