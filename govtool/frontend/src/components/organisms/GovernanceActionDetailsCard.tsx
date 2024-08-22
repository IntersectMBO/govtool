import { useState } from "react";
import { Box } from "@mui/material";

import { useScreenDimension } from "@hooks";
import {
  GovernanceActionCardStatePill,
  GovernanceActionDetailsCardVotes,
} from "@molecules";
import { GovernanceActionDetailsCardData } from "@organisms";
import { EpochParams, MetadataValidationStatus } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";

type GovernanceActionDetailsCardProps = {
  createdDate: string;
  createdEpochNo: number;
  expiryDate: string;
  expiryEpochNo: number;
  type: GovernanceActionType;
  label: string;
  details?: ActionDetailsType;
  url: string;
  title?: string;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  links?: string[];
  govActionId: string;
  isDataMissing: null | MetadataValidationStatus;
  isDashboard?: boolean;
  isVoter?: boolean;
  voteFromEP?: string;
  voteUrlFromEP?: string;
  voteDateFromEP?: string;
  voteEpochNoFromEP?: number;
  isInProgress?: boolean;
  protocolParams: EpochParams | null;
  dRepAbstainVotes: number;
  dRepNoVotes: number;
  dRepYesVotes: number;
};

export const GovernanceActionDetailsCard = ({
  dRepAbstainVotes,
  createdDate,
  createdEpochNo,
  expiryDate,
  expiryEpochNo,
  dRepNoVotes,
  details,
  url,
  type,
  label,
  title,
  links,
  abstract,
  motivation,
  rationale,
  dRepYesVotes,
  isDashboard,
  isVoter,
  voteFromEP,
  voteUrlFromEP,
  voteDateFromEP,
  voteEpochNoFromEP,
  govActionId,
  isInProgress,
  isDataMissing,
  protocolParams,
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
        abstract={abstract}
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
        label={label}
        url={url}
        type={type}
        protocolParams={protocolParams}
      />
      <GovernanceActionDetailsCardVotes
        setIsVoteSubmitted={setIsVoteSubmitted}
        abstainVotes={dRepAbstainVotes}
        noVotes={dRepNoVotes}
        yesVotes={dRepYesVotes}
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
        type={type}
      />
    </Box>
  );
};
