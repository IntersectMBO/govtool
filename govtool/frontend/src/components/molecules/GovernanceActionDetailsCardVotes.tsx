import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import { VoteActionForm, VotesSubmitted } from "@molecules";
import { useFeatureFlag } from "@/context";
import { ProposalData, ProposalVote } from "@/models";

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
  proposal: {
    dRepAbstainVotes,
    dRepNoVotes,
    dRepYesVotes,
    poolAbstainVotes,
    poolNoVotes,
    poolYesVotes,
    ccAbstainVotes,
    ccNoVotes,
    ccYesVotes,
    expiryDate,
    expiryEpochNo,
    type,
  },
}: GovernanceActionCardVotesProps) => {
  const { isVotingOnGovernanceActionEnabled } = useFeatureFlag();
  const { screenWidth } = useScreenDimension();

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
      {isVoter && isVotingOnGovernanceActionEnabled(type) ? (
        <VoteActionForm
          setIsVoteSubmitted={setIsVoteSubmitted}
          expiryDate={expiryDate}
          expiryEpochNo={expiryEpochNo}
          previousVote={vote}
          dRepAbstainVotes={dRepAbstainVotes}
          dRepNoVotes={dRepNoVotes}
          dRepYesVotes={dRepYesVotes}
          isInProgress={isInProgress}
        />
      ) : (
        <VotesSubmitted
          dRepAbstainVotes={dRepAbstainVotes}
          dRepNoVotes={dRepNoVotes}
          dRepYesVotes={dRepYesVotes}
          poolAbstainVotes={poolAbstainVotes}
          poolNoVotes={poolNoVotes}
          poolYesVotes={poolYesVotes}
          ccAbstainVotes={ccAbstainVotes}
          ccNoVotes={ccNoVotes}
          ccYesVotes={ccYesVotes}
        />
      )}
    </Box>
  );
};
