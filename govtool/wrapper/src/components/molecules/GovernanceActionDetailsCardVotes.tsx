import { Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import { VoteActionForm, VotesSubmitted } from "@molecules";

type GovernanceActionCardVotesProps = {
  setIsVoteSubmitted: Dispatch<SetStateAction<boolean>>;
  abstainVotes: number;
  noVotes: number;
  yesVotes: number;
  isOneColumn: boolean;
  expiryDate: string;
  expiryEpochNo: number;
  isVoter?: boolean;
  voteFromEP?: string;
  voteUrlFromEP?: string;
  voteDateFromEP?: string;
  voteEpochNoFromEP?: number;
  isDashboard?: boolean;
  isInProgress?: boolean;
};

export const GovernanceActionDetailsCardVotes = ({
  setIsVoteSubmitted,
  abstainVotes,
  noVotes,
  yesVotes,
  isOneColumn,
  expiryDate,
  expiryEpochNo,
  isVoter,
  voteFromEP,
  voteUrlFromEP,
  voteDateFromEP,
  voteEpochNoFromEP,
  isDashboard,
  isInProgress,
}: GovernanceActionCardVotesProps) => {
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
      {isVoter ? (
        <VoteActionForm
          setIsVoteSubmitted={setIsVoteSubmitted}
          expiryDate={expiryDate}
          expiryEpochNo={expiryEpochNo}
          voteFromEP={voteFromEP ? voteFromEP.toLowerCase() : undefined}
          voteUrlFromEP={voteUrlFromEP}
          voteDateFromEP={voteDateFromEP}
          voteEpochNoFromEP={voteEpochNoFromEP}
          yesVotes={yesVotes}
          noVotes={noVotes}
          abstainVotes={abstainVotes}
          isInProgress={isInProgress}
        />
      ) : (
        <VotesSubmitted
          yesVotes={yesVotes}
          noVotes={noVotes}
          abstainVotes={abstainVotes}
        />
      )}
    </Box>
  );
};
