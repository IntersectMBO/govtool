import { CardanoTestWallet } from "@cardanoapi/cardano-test-wallet/types";

export type StaticWallet = CardanoTestWallet & {
  dRepId: string;
  address: string;
};

export type KuberValue = {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
};

export interface IProposal {
  id: string;
  txHash: string;
  index: number;
  type: string;
  details: any;
  expiryDate: string;
  expiryEpochNo: number;
  createdDate: string;
  createdEpochNo: number;
  url: string;
  metadataHash: string;
  title: string | null;
  about: string | null;
  motivation: string | null;
  rationale: string | null;
  metadata: any;
  references: any;
  yesVotes: number;
  noVotes: number;
}

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

export type IDRepInfo = {
  name: string;
  email?: string;
  bio?: string;
  extraContentLinks?: string[];
};
