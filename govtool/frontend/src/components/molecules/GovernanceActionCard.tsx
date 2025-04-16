import { FC } from "react";
import { Box, Skeleton } from "@mui/material";

import { Button } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionCardHeader,
  GovernanceActionCardStatePill,
  GovernanceActionsDatesBox,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  encodeCIP129Identifier,
  getFullGovActionId,
  getProposalTypeLabel,
  getProposalTypeNoEmptySpaces,
} from "@utils";
import { ProposalData } from "@models";

type ActionTypeProps = Omit<
  ProposalData,
  | "yesVotes"
  | "noVotes"
  | "abstainVotes"
  | "id"
  | "details"
  | "rationale"
  | "motivation"
> & {
  onClick?: () => void;
  inProgress?: boolean;
  isValidating?: boolean;
  metadataStatus?: MetadataValidationStatus;
};

export const GovernanceActionCard: FC<ActionTypeProps> = ({
  abstract,
  type,
  inProgress = false,
  expiryDate,
  expiryEpochNo,
  onClick,
  createdDate,
  createdEpochNo,
  txHash,
  index,
  title,
  isValidating,
  metadataStatus,
}) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const govActionId = getFullGovActionId(txHash, index);
  const cip129GovernanceActionId = encodeCIP129Identifier({
    txID: txHash,
    index: index.toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });

  return (
    <Box
      sx={{
        width: screenWidth < 420 ? 290 : isMobile ? 324 : 350,
        height: "100%",
        position: "relative",
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        boxShadow: "0px 4px 15px 0px #DDE3F5",
        borderRadius: "20px",
        backgroundColor: metadataStatus
          ? "rgba(251, 235, 235, 0.50)"
          : "rgba(255, 255, 255, 0.3)",
        ...(!!metadataStatus && {
          border: "1px solid #F6D5D5",
        }),
        ...(inProgress && {
          border: "1px solid #FFCBAD",
        }),
      }}
      data-testid={`govaction-${getProposalTypeNoEmptySpaces(type)}-card`}
    >
      {inProgress && <GovernanceActionCardStatePill variant="inProgress" />}
      <Box
        sx={{
          padding: "40px 24px 0",
        }}
      >
        <GovernanceActionCardHeader
          title={title}
          isDataMissing={metadataStatus}
          isValidating={isValidating}
        />
        {!!metadataStatus && (
          <GovernanceActionCardElement
            label={t("govActions.abstract")}
            text={abstract}
            textVariant="twoLines"
            dataTestId="governance-action-abstract"
            isSliderCard
            isMarkdown
            isValidating={isValidating}
          />
        )}
        <GovernanceActionCardElement
          label={t("govActions.governanceActionType")}
          text={getProposalTypeLabel(type)}
          textVariant="pill"
          dataTestId={`${getProposalTypeNoEmptySpaces(type)}-type`}
          isSliderCard
          isValidating={isValidating}
        />
        <GovernanceActionsDatesBox
          createdDate={createdDate}
          expiryDate={expiryDate}
          expiryEpochNo={expiryEpochNo}
          createdEpochNo={createdEpochNo}
          isSliderCard
          isValidating={isValidating}
        />
        <GovernanceActionCardElement
          label={t("govActions.cip129GovernanceActionId")}
          text={cip129GovernanceActionId}
          dataTestId={`${cip129GovernanceActionId}-id`}
          isCopyButton
          isSliderCard
          isValidating={isValidating}
        />
        <GovernanceActionCardElement
          label={t("govActions.governanceActionId")}
          text={govActionId}
          dataTestId={`${govActionId}-id`}
          isCopyButton
          isSliderCard
          isSemiTransparent
          isValidating={isValidating}
        />
      </Box>
      <Box
        sx={{
          boxShadow: "0px 4px 15px 0px #DDE3F5",
          borderBottomLeftRadius: 20,
          borderBottomRightRadius: 20,
          padding: 3,
          bgcolor: "white",
        }}
      >
        {isValidating ? (
          <Skeleton width="100%" height="40px" sx={{ borderRadius: "20px" }} />
        ) : (
          <Button
            onClick={onClick}
            variant={inProgress ? "outlined" : "contained"}
            size="large"
            sx={{
              width: "100%",
            }}
            data-testid={`govaction-${govActionId}-view-detail`}
          >
            {t(
              inProgress
                ? "govActions.viewDetails"
                : "govActions.viewDetailsAndVote",
            )}
          </Button>
        )}
      </Box>
    </Box>
  );
};
