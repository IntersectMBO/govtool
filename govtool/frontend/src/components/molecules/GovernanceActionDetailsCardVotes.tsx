import { Dispatch, SetStateAction, useCallback } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import { VoteActionForm, VotesSubmitted } from "@molecules";
import { useFeatureFlag } from "@/context";
import { ProposalData, ProposalVote } from "@/models";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@/consts";

type GovernanceActionCardVotesProps = {
  setIsVoteSubmitted: Dispatch<SetStateAction<boolean>>;
  isOneColumn: boolean;
  isDashboard?: boolean;
  isInProgress?: boolean;
  isVoter?: boolean;
  vote?: ProposalVote;
  proposal: ProposalData;
};

export const GovernanceActionDetailsCardVotes = ({
  setIsVoteSubmitted,
  isOneColumn,
  isVoter,
  vote,
  isDashboard,
  isInProgress,
  proposal,
}: GovernanceActionCardVotesProps) => {
  const { areDRepVoteTotalsDisplayed } = useFeatureFlag();
  const { screenWidth } = useScreenDimension();
  const isSecurityGroup = useCallback(
    () =>
      Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
        (paramKey) =>
          proposal.protocolParams?.[
            paramKey as keyof typeof proposal.protocolParams
          ] !== null,
      ),
    [proposal.protocolParams],
  );
  const isModifiedPadding =
    (isDashboard && screenWidth < 1368) ?? screenWidth < 1100;

  return (
    <Box
      sx={{
        borderRadius: isOneColumn ? "0 0 20px 20px" : "0 20px 20px 0",
        bgcolor: "rgba(255, 255, 255, 0.60)",
        p: `40px ${isModifiedPadding ? "24px" : "80px"}`,
      }}
    >
      {isVoter &&
      areDRepVoteTotalsDisplayed(proposal.type, isSecurityGroup()) ? (
        <VoteActionForm
          setIsVoteSubmitted={setIsVoteSubmitted}
          proposal={proposal}
          previousVote={vote}
          isInProgress={isInProgress}
        />
      ) : (
        <VotesSubmitted votes={proposal} />
      )}
    </Box>
  );
};
