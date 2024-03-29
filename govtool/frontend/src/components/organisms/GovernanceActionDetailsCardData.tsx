import { Box } from "@mui/material";

import { ExternalModalButton } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionDetailsCardLinks,
  DataMissingInfoBox,
  GovernanceActionDetailsCardHeader,
  GovernanceActionsDatesBox,
  GovernanceActionDetailsCardOnChainData,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { GAMetedataErrors, getProposalTypeNoEmptySpaces } from "@utils";

type GovernanceActionDetailsCardDataProps = {
  type: string;
  govActionId: string;
  createdDate: string;
  createdEpochNo: number;
  expiryDate: string;
  expiryEpochNo: number;
  details?: ActionDetailsType;
  url: string;
  title?: string;
  about?: string;
  motivation?: string;
  rationale?: string;
  isDataMissing: boolean | GAMetedataErrors;
  isOneColumn: boolean;
  isDashboard?: boolean;
  isInProgress?: boolean;
  isSubmitted?: boolean;
};

export const GovernanceActionDetailsCardData = ({
  type,
  govActionId,
  createdDate,
  createdEpochNo,
  expiryDate,
  expiryEpochNo,
  details,
  url,
  title,
  about,
  motivation,
  rationale,
  isDataMissing,
  isOneColumn,
  isDashboard,
  isInProgress,
  isSubmitted,
}: GovernanceActionDetailsCardDataProps) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();

  const isModifiedPadding =
    (isDashboard && screenWidth < 1168) ?? screenWidth < 900;

  return (
    <Box
      sx={{
        borderRadius: isOneColumn ? "20px 20px 0 0" : "20px 0 0 20px",
        bgcolor: "rgba(255, 255, 255, 0.30)",
        p: `40px ${isModifiedPadding ? "24px" : "40px"}`,
        overflow: "hidden",
      }}
    >
      <GovernanceActionDetailsCardHeader
        title={title}
        isDataMissing={isDataMissing}
      />
      <DataMissingInfoBox
        isDataMissing={isDataMissing}
        isInProgress={isInProgress}
        isSubmitted={isSubmitted}
      />
      <GovernanceActionCardElement
        label={t("govActions.governanceActionType")}
        text={type}
        textVariant="pill"
        dataTestId={`${getProposalTypeNoEmptySpaces(type)}-type`}
      />
      <GovernanceActionsDatesBox
        createdDate={createdDate}
        expiryDate={expiryDate}
        expiryEpochNo={expiryEpochNo}
        createdEpochNo={createdEpochNo}
      />
      {isDataMissing && (
        <ExternalModalButton
          url={url}
          label={t("govActions.seeExternalData")}
        />
      )}
      <GovernanceActionCardElement
        label={t("govActions.governanceActionId")}
        text={govActionId}
        isCopyButton
        dataTestId={`${govActionId}-id`}
      />
      <GovernanceActionCardElement
        label={t("govActions.about")}
        text={about}
        textVariant="longText"
        dataTestId="about"
      />
      <GovernanceActionCardElement
        label={t("govActions.motivation")}
        text={motivation}
        textVariant="longText"
        dataTestId="motivation"
      />
      <GovernanceActionCardElement
        label={t("govActions.rationale")}
        text={rationale}
        textVariant="longText"
        dataTestId="rationale"
      />
      {details && Object.keys(details).length !== 0 && (
        <GovernanceActionDetailsCardOnChainData data={details} />
      )}
      <GovernanceActionDetailsCardLinks />
    </Box>
  );
};
