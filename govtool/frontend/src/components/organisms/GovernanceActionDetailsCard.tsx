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
  abstract?: string;
  ccAbstainVotes: number;
  ccNoVotes: number;
  ccYesVotes: number;
  createdDate: string;
  createdEpochNo: number;
  dRepAbstainVotes: number;
  dRepNoVotes: number;
  dRepYesVotes: number;
  details?: ActionDetailsType;
  expiryDate: string;
  expiryEpochNo: number;
  govActionId: string;
  prevGovActionId: string | null;
  isDashboard?: boolean;
  isDataMissing: null | MetadataValidationStatus;
  isInProgress?: boolean;
  isVoter?: boolean;
  label: string;
  links?: string[];
  motivation?: string;
  poolAbstainVotes: number;
  poolNoVotes: number;
  poolYesVotes: number;
  protocolParams: EpochParams | null;
  rationale?: string;
  title?: string;
  type: GovernanceActionType;
  url: string;
  voteDateFromEP?: string;
  voteEpochNoFromEP?: number;
  voteFromEP?: string;
  voteUrlFromEP?: string;
};

export const GovernanceActionDetailsCard = ({
  abstract,
  ccAbstainVotes,
  ccNoVotes,
  ccYesVotes,
  createdDate,
  createdEpochNo,
  dRepAbstainVotes,
  dRepNoVotes,
  dRepYesVotes,
  details,
  expiryDate,
  expiryEpochNo,
  govActionId,
  prevGovActionId,
  isDashboard,
  isDataMissing,
  isInProgress,
  isVoter,
  label,
  links,
  motivation,
  poolAbstainVotes,
  poolNoVotes,
  poolYesVotes,
  protocolParams,
  rationale,
  title,
  type,
  url,
  voteDateFromEP,
  voteEpochNoFromEP,
  voteFromEP,
  voteUrlFromEP,
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
        prevGovActionId={prevGovActionId}
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
        dRepAbstainVotes={dRepAbstainVotes}
        dRepNoVotes={dRepNoVotes}
        dRepYesVotes={dRepYesVotes}
        poolAbstainVotes={poolAbstainVotes}
        poolNoVotes={poolNoVotes}
        poolYesVotes={poolYesVotes}
        ccAbstainVotes={ccAbstainVotes}
        ccNoVotes={ccNoVotes}
        ccYesVotes={ccYesVotes}
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
