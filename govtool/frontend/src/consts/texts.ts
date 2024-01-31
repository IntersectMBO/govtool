export const tooltips = {
  submissionDate: {
    heading: "Submission Date",
    paragraphOne: "The date when the governance action was submitted on-chain.",
  },
  expiryDate: {
    heading: "Expiry Date",
    paragraphOne:
      "The date when the governance action will expiry if it doesn’t reach ratification thresholds.",
    paragraphTwo:
      "IMPORTANT: If the governance action is ratified before the expiry date it will be considered ratified and it will not be available to vote on afterwards.",
  },
  votingPower: {
    heading: "DRep Voting Power",
    paragraphOne:
      "This is the voting power delegated to you as a DRep and it is calculated at the end of every epoch for the epoch that just ended.",
    paragraphTwo:
      "IMPORTANT: When voting, the voting power provides an indication and not the exact number.",
  },
  delegateTodRep: {
    toMyself: {
      heading: "Delegate to myself",
      paragraphOne:
        "If you are registered as DRep you can delegate your voting power on yourself.",
    },
    todRep: {
      heading: "Delegation to DRep",
      paragraphOne:
        "DReps are representatives of the ada holders that can vote on governance actions.",
    },
    noConfidence: {
      heading: "No confidence",
      paragraphOne:
        "If you don’t have trust in the current constitutional committee you signal ‘No-confidence’. By voting ‘No’ means you don’t want governance actions to be ratified.",
    },
    abstain: {
      heading: "Abstaining",
      paragraphOne:
        "Select this to signal no confidence in the current constitutional committee by voting NO on every proposal and voting YES to no-confidence proposals.",
    },
  },
};
