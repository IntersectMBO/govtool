import { Box } from "@mui/material";

import { ExternalModalButton } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionDetailsCardLinks,
  DataMissingInfoBox,
  DataMissingHeader,
  GovernanceActionsDatesBox,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { getProposalTypeNoEmptySpaces, testIdFromLabel } from "@utils";
import { MetadataValidationStatus } from "@models";
import { useLocation } from "react-router-dom";

type GovernanceActionDetailsCardDataProps = {
  abstract?: string;
  createdDate: string;
  createdEpochNo: number;
  details?: ActionDetailsType;
  expiryDate: string;
  expiryEpochNo: number;
  govActionId: string;
  isDashboard?: boolean;
  isDataMissing: MetadataValidationStatus | null;
  isInProgress?: boolean;
  isOneColumn: boolean;
  isSubmitted?: boolean;
  links?: string[];
  motivation?: string;
  rationale?: string;
  title?: string;
  label: string;
  url: string;
};

export const GovernanceActionDetailsCardData = ({
  abstract,
  createdDate,
  createdEpochNo,
  details,
  expiryDate,
  expiryEpochNo,
  govActionId,
  isDashboard,
  isDataMissing,
  isInProgress,
  isOneColumn,
  isSubmitted,
  links,
  motivation,
  rationale,
  title,
  label,
  url,
}: GovernanceActionDetailsCardDataProps) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();

  const isModifiedPadding =
    (isDashboard && screenWidth < 1168) ?? screenWidth < 900;

  const { pathname, hash } = useLocation();

  const govActionLinkToShare = `${window.location.protocol}//${
    window.location.hostname
  }${window.location.port ? `:${window.location.port}` : ""}${pathname}${
    hash ?? ""
  }`;

  return (
    <Box
      sx={{
        borderRadius: isOneColumn ? "20px 20px 0 0" : "20px 0 0 20px",
        bgcolor: "rgba(255, 255, 255, 0.30)",
        p: `40px ${isModifiedPadding ? "24px" : "40px"}`,
        overflow: "hidden",
      }}
    >
      <DataMissingHeader
        isDataMissing={isDataMissing}
        shareLink={govActionLinkToShare}
        title={title}
      />
      <DataMissingInfoBox
        isDataMissing={isDataMissing}
        isInProgress={isInProgress}
        isSubmitted={isSubmitted}
      />
      <GovernanceActionCardElement
        label={t("govActions.governanceActionType")}
        text={label}
        textVariant="pill"
        dataTestId={`${getProposalTypeNoEmptySpaces(label)}-type`}
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
        label={t("govActions.abstract")}
        text={abstract}
        textVariant="longText"
        dataTestId="abstract"
        isMarkdown
      />
      <GovernanceActionCardElement
        label={t("govActions.motivation")}
        text={motivation}
        textVariant="longText"
        dataTestId="motivation"
        isMarkdown
      />
      <GovernanceActionCardElement
        label={t("govActions.rationale")}
        text={rationale}
        textVariant="longText"
        dataTestId="rationale"
        isMarkdown
      />
      {details &&
        Object.keys(details).length !== 0 &&
        Object.entries(details).map(([detailLabel, content]) => (
          <GovernanceActionCardElement
            isCopyButton={detailLabel.toLowerCase().includes("address")}
            label={detailLabel}
            text={content}
            dataTestId={testIdFromLabel(detailLabel)}
          />
        ))}
      <GovernanceActionDetailsCardLinks links={links} />
    </Box>
  );
};
