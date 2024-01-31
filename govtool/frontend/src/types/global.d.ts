export {};

declare global {
  type VoteType = "yes" | "no" | "abstain";

  type ActionTypeFromAPI = {
    id: string;
    type: string;
    details: string;
    expiryDate: string;
    url: string;
    metadataHash: string;
  };

  type ActionType = {
    id: string;
    type: string;
    details?: string;
    expiryDate: string;
    createdDate: string;
    url?: string;
    metadataHash?: string;
    yesVotes: number;
    noVotes: number;
    abstainVotes: number;
    index: number;
    txHash: string;
  };

  interface ActionVotedOnType extends ActionType {
    vote: VoteType;
  }

  type VotedOnDataType = {
    title: string;
    actions: ActionVotedOnType[];
  }[];

  type ToVoteDataType = {
    title: string;
    actions: ActionType[];
  }[];
}
