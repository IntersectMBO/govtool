import { Box } from "@mui/material";
import {
  GovernanceActionCardStatePill,
  GovernanceActionDetailsCardVotes,
} from "@molecules";
import { useScreenDimension } from "@hooks";
import { GovernanceActionDetailsCardData } from "@organisms";
import { useState } from "react";

type GovernanceActionDetailsCardProps = {
  abstainVotes: number;
  createdDate: string;
  // details: unknown;
  expiryDate: string;
  noVotes: number;
  type: string;
  // url: string;
  yesVotes: number;
  govActionId: string;
  isDataMissing: boolean;
  isDashboard?: boolean;
  isVoter?: boolean;
  voteFromEP?: string;
  isInProgress?: boolean;
};

export const GovernanceActionDetailsCard = ({
  abstainVotes,
  createdDate,
  // details,
  expiryDate,
  noVotes,
  type,
  // url,
  yesVotes,
  isDashboard,
  isVoter,
  voteFromEP,
  govActionId,
  isInProgress,
  isDataMissing,
}: GovernanceActionDetailsCardProps) => {
  const [isVoteSubmitted, setIsVoteSubmitted] = useState<boolean>(false);
  const { screenWidth, isMobile } = useScreenDimension();

  const isOneColumn = (isDashboard && screenWidth < 1036) ?? isMobile;

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
        type={type}
        govActionId={govActionId}
        createdDate={createdDate}
        expiryDate={expiryDate}
        isDataMissing={isDataMissing}
        isDashboard={isDashboard}
        isOneColumn={isOneColumn}
      />
      <GovernanceActionDetailsCardVotes
        setIsVoteSubmitted={setIsVoteSubmitted}
        abstainVotes={abstainVotes}
        noVotes={noVotes}
        yesVotes={yesVotes}
        isVoter={isVoter}
        voteFromEP={voteFromEP}
        isDashboard={isDashboard}
        isOneColumn={isOneColumn}
        isInProgress={isInProgress}
      />
    </Box>
  );
};
