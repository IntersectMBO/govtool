import { CardanoTestWallet } from "@mock/cardano-test-wallet/types";

export type StaticWallet = CardanoTestWallet & {
  dRepId: string;
  address: string;
};

export type KuberValue = {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
};

export type IProposal = {
  id: string;
  txHash: string;
  index: number;
  type: string;
  details: string;
  createdDate: string;
  expiryDate: string;
  abstainVotes: number;
  noVote: number;
  metadataHash: string;
  url: string;
  yesVotes: number;
};

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
