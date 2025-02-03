import { useState } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import {
  GovernanceActionCardStatePill,
  GovernanceActionDetailsCardVotes,
} from "@molecules";
import { GovernanceActionDetailsCardData } from "@organisms";
import { MetadataValidationStatus, ProposalData, ProposalVote } from "@models";

type GovernanceActionDetailsCardProps = {
  isDashboard?: boolean;
  isDataMissing: null | MetadataValidationStatus;
  isInProgress?: boolean;
  isVoter?: boolean;
  vote?: ProposalVote | null;
  proposal: ProposalData;
};

export const GovernanceActionDetailsCard = ({
  isDashboard,
  isDataMissing,
  isInProgress,
  isVoter,
  vote,
  proposal,
}: GovernanceActionDetailsCardProps) => {
  const [isVoteSubmitted, setIsVoteSubmitted] = useState<boolean>(false);
  const { screenWidth, isMobile } = useScreenDimension();

  const isOneColumn = (isDashboard && screenWidth < 1200) ?? isMobile;

  return (
    <Box
      sx={{
        borderRadius: "20px",
        display: "grid",
        gridTemplateColumns: isOneColumn ? undefined : "0.6fr 0.4fr",
        mt: "12px",
        width: "100%",
        position: "relative",
        boxShadow: isInProgress
          ? "2px 2px 20px 0px rgba(245, 90, 0, 0.20)"
          : isVoteSubmitted && !isDataMissing
          ? "2px 2px 20px 0px rgba(98, 188, 82, 0.20)"
          : "2px 2px 20px 0px rgba(47, 98, 220, 0.20)",
        ...(isDataMissing && {
          border: "1px solid #F6D5D5",
        }),
      }}
      data-testid="governance-action-details-card"
    >
      {(isVoteSubmitted || isInProgress) && (
        <GovernanceActionCardStatePill
          variant={isVoteSubmitted ? "voteSubmitted" : "inProgress"}
        />
      )}
      <GovernanceActionDetailsCardData
        isDashboard={isDashboard}
        isDataMissing={isDataMissing}
        isInProgress={isInProgress}
        isOneColumn={isOneColumn}
        isSubmitted={isVoteSubmitted}
        proposal={proposal}
      />
      <GovernanceActionDetailsCardVotes
        setIsVoteSubmitted={setIsVoteSubmitted}
        isVoter={isVoter}
        vote={vote}
        isDashboard={isDashboard}
        isOneColumn={isOneColumn}
        isInProgress={isInProgress}
        proposal={proposal}
      />
    </Box>
  );
};
