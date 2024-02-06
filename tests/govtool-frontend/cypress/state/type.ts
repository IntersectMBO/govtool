export interface IProposal {
  id: string;
  txHash: string;
  index: number;
  type: string;
  details: string;
  expiryDate: string;
  createdDate: string;
  url: string;
  metadataHash: string;
  yesVotes: number;
  noVote: number;
  abstainVotes: number;
}

export interface IVotedProposal {
  proposal: IProposal;
  vote: {
    proposalId: string;
    drepId: string;
    vote: string;
    url: string | null;
    metadataHash: string | null;
  };
}
