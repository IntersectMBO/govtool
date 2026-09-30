import { ProposalType } from "@types";

/** The test wallet that saves drafts of this proposal type. */
export const getDraftProposalWalletName = (proposalType: string) => {
  switch (proposalType) {
    case ProposalType.info:
      return "proposal05";
    case ProposalType.treasury:
      return "proposal07";
    case ProposalType.updatesToTheConstitution:
      return "proposal08";
    case ProposalType.motionOfNoConfedence:
      return "proposal09";
    case ProposalType.hardFork:
      return "proposal10";
  }
};
