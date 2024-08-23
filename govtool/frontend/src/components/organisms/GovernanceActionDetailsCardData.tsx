import { useMemo, useState } from "react";
import { Box, Tabs, Tab, styled } from "@mui/material";
import { useLocation } from "react-router-dom";
import ReactDiffViewer from "react-diff-viewer";

import { ExternalModalButton } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionDetailsCardLinks,
  DataMissingInfoBox,
  DataMissingHeader,
  GovernanceActionsDatesBox,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  getProposalTypeNoEmptySpaces,
  testIdFromLabel,
  replaceNullValues,
} from "@utils";
import { EpochParams, MetadataValidationStatus } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";
import { useAppContext } from "@/context";

type TabPanelProps = {
  children?: React.ReactNode;
  index: number;
  value: number;
};

const CustomTabPanel = ({ children, value, index }: TabPanelProps) =>
  value === index && (
    <Box sx={value === index && index === 1 ? { overflow: "scroll" } : {}}>
      {children}
    </Box>
  );

type StyledTabProps = {
  label: string;
};

const StyledTab = styled((props: StyledTabProps) => (
  <Tab disableRipple {...props} />
))(() => ({
  textTransform: "none",
  fontWeight: 600,
  fontSize: 16,
  color: "rgba(36, 34, 50, 0.5)",
  "&.Mui-selected": {
    color: "rgba(38, 37, 45, 1)",
  },
}));

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
  type: GovernanceActionType;
  protocolParams: EpochParams | null;
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
  type,
  protocolParams,
}: GovernanceActionDetailsCardDataProps) => {
  const { epochParams } = useAppContext();
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();
  const { isMobile } = useScreenDimension();
  const showTabs =
    type === GovernanceActionType.ParameterChange &&
    !!protocolParams &&
    !!epochParams &&
    !isDataMissing;

  const isModifiedPadding =
    (isDashboard && screenWidth < 1168) ?? screenWidth < 900;

  const [tab, setTab] = useState<number>(0);

  const { pathname, hash } = useLocation();

  const govActionLinkToShare = `${window.location.protocol}//${
    window.location.hostname
  }${window.location.port ? `:${window.location.port}` : ""}${pathname}${
    hash ?? ""
  }`;

  const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
    setTab(newValue);
  };

  const reasoningTabContent = useMemo(
    () => (
      <>
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
      </>
    ),
    [abstract, motivation, rationale],
  );

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

      {showTabs && protocolParams ? (
        <>
          <Tabs
            sx={{
              marginY: 4,
              display: "flex",
              fontSize: 16,
              fontWeight: 500,
            }}
            value={tab}
            indicatorColor="secondary"
            onChange={handleChange}
            aria-label="basic tabs example"
          >
            <StyledTab
              data-testid="reasoning-tab"
              label="Reasoning"
              sx={{
                textTransform: "none",
                width: !isMobile ? "auto" : "50%",
              }}
            />
            <StyledTab
              data-testid="parameters-tab"
              label="Parameters"
              sx={{
                textTransform: "none",
                width: !isMobile ? "auto" : "50%",
              }}
            />
          </Tabs>

          <CustomTabPanel value={tab} index={0}>
            {reasoningTabContent}
          </CustomTabPanel>
          <CustomTabPanel value={tab} index={1}>
            <ReactDiffViewer
              oldValue={JSON.stringify(epochParams, null, 2)}
              newValue={JSON.stringify(
                replaceNullValues(epochParams, protocolParams),
                null,
                2,
              )}
              showDiffOnly
            />
          </CustomTabPanel>
        </>
      ) : (
        reasoningTabContent
      )}

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
