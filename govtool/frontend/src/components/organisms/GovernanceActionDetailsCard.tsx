import { useState } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import {
  GovernanceActionCardStatePill,
  GovernanceActionDetailsCardVotes,
} from "@molecules";
import { GovernanceActionDetailsCardData } from "@organisms";
import { MetadataValidationStatus } from "@models";

type GovernanceActionDetailsCardProps = {
  abstainVotes: number;
  createdDate: string;
  createdEpochNo: number;
  expiryDate: string;
  expiryEpochNo: number;
  noVotes: number;
  type: string;
  details?: ActionDetailsType;
  url: string;
  title?: string;
  about?: string;
  motivation?: string;
  rationale?: string;
  yesVotes: number;
  links?: GovernanceActionLink[];
  govActionId: string;
  isDataMissing: boolean | MetadataValidationStatus;
  isDashboard?: boolean;
  isVoter?: boolean;
  voteFromEP?: string;
  voteUrlFromEP?: string;
  voteDateFromEP?: string;
  voteEpochNoFromEP?: number;
  isInProgress?: boolean;
};

export const GovernanceActionDetailsCard = ({
  abstainVotes,
  createdDate,
  createdEpochNo,
  expiryDate,
  expiryEpochNo,
  noVotes,
  type,
  details,
  url,
  title,
  links,
  about,
  motivation,
  rationale,
  yesVotes,
  isDashboard,
  isVoter,
  voteFromEP,
  voteUrlFromEP,
  voteDateFromEP,
  voteEpochNoFromEP,
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
        about={about}
        createdDate={createdDate}
        createdEpochNo={createdEpochNo}
        details={details}
        expiryDate={expiryDate}
        expiryEpochNo={expiryEpochNo}
        govActionId={govActionId}
        isDashboard={isDashboard}
        isDataMissing={isDataMissing}
        isInProgress={isInProgress}
        isOneColumn={isOneColumn}
        isSubmitted={isVoteSubmitted}
        links={links}
        motivation={motivation}
        rationale={rationale}
        title={title}
        type={type}
        url={url}
      />
      <GovernanceActionDetailsCardVotes
        setIsVoteSubmitted={setIsVoteSubmitted}
        abstainVotes={abstainVotes}
        noVotes={noVotes}
        yesVotes={yesVotes}
        expiryDate={expiryDate}
        expiryEpochNo={expiryEpochNo}
        isVoter={isVoter}
        voteFromEP={voteFromEP}
        voteUrlFromEP={voteUrlFromEP}
        voteDateFromEP={voteDateFromEP}
        voteEpochNoFromEP={voteEpochNoFromEP}
        isDashboard={isDashboard}
        isOneColumn={isOneColumn}
        isInProgress={isInProgress}
      />
    </Box>
  );
};
