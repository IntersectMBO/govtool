import { useMemo, useState } from "react";
import { Box, Tabs, Tab, styled } from "@mui/material";

import { CopyButton, ExternalModalButton, Typography } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionDetailsCardLinks,
  DataMissingInfoBox,
  DataMissingHeader,
  GovernanceActionsDatesBox,
  GovernanceActionDetailsDiffView,
  GovernanceActionNewCommitteeDetailsTabContent,
  GovernanceActionCardTreasuryWithdrawalElement,
  GovernanceActionNewConstitutionDetailsTabContent,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  getProposalTypeNoEmptySpaces,
  getProposalTypeLabel,
  filterUpdatableProtocolParams,
  filterOutNullParams,
  getFullGovActionId,
  mapArrayToObjectByKeys,
  encodeCIP129Identifier,
} from "@utils";
import { MetadataValidationStatus, ProposalData } from "@models";
import { GovernanceActionType } from "@/types/governanceAction";
import { useAppContext } from "@/context";

type TabPanelProps = {
  children?: React.ReactNode;
  index: number;
  value: number;
};

const CustomTabPanel = ({ children, value, index }: TabPanelProps) =>
  value === index && <Box sx={{ overflow: "auto" }}>{children}</Box>;

type StyledTabProps = {
  label: string;
  isMobile: boolean;
};

const StyledTab = styled((props: StyledTabProps) => (
  <Tab disableRipple {...props} />
))(({ isMobile }) => ({
  textTransform: "none",
  fontWeight: 600,
  fontSize: 16,
  width: !isMobile ? "auto" : "50%",

  color: "rgba(36, 34, 50, 0.5)",
  "&.Mui-selected": {
    color: "rgba(38, 37, 45, 1)",
  },
}));

type GovernanceActionDetailsCardDataProps = {
  isDashboard?: boolean;
  isDataMissing: MetadataValidationStatus | null;
  isInProgress?: boolean;
  isOneColumn: boolean;
  isSubmitted?: boolean;
  proposal: ProposalData;
};

export const GovernanceActionDetailsCardData = ({
  isDashboard,
  isDataMissing,
  isInProgress,
  isOneColumn,
  isSubmitted,
  proposal: {
    abstract,
    createdDate,
    createdEpochNo,
    details,
    expiryDate,
    expiryEpochNo,
    index,
    motivation,
    prevGovActionIndex,
    prevGovActionTxHash,
    rationale,
    references,
    title,
    txHash,
    url,
    type,
    protocolParams,
  },
}: GovernanceActionDetailsCardDataProps) => {
  const { epochParams } = useAppContext();
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();
  const { isMobile } = useScreenDimension();

  const mappedArraysToObjectsProtocolParams = useMemo(
    () =>
      mapArrayToObjectByKeys(protocolParams, [
        "PlutusV1",
        "PlutusV2",
        "PlutusV3",
      ]),
    [protocolParams],
  );

  const updatableProtocolParams = useMemo(
    () =>
      filterUpdatableProtocolParams(
        epochParams,
        mappedArraysToObjectsProtocolParams,
        ["id", "registered_tx_id", "key"],
      ),
    [epochParams, mappedArraysToObjectsProtocolParams],
  );

  const nonNullProtocolParams = useMemo(
    () =>
      filterOutNullParams(mappedArraysToObjectsProtocolParams, [
        "id",
        "registered_tx_id",
        "key",
      ]),
    [updatableProtocolParams, mappedArraysToObjectsProtocolParams],
  );

  const isModifiedPadding =
    (isDashboard && screenWidth < 1168) ?? screenWidth < 900;

  const [selectedTab, setSelectedTab] = useState<number>(0);

  const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
    setSelectedTab(newValue);
  };

  const label = getProposalTypeLabel(type);
  const govActionId = getFullGovActionId(txHash, index);
  const cip129GovernanceActionId = encodeCIP129Identifier({
    txID: txHash,
    index: index.toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });
  const prevGovActionId =
    prevGovActionIndex && prevGovActionTxHash
      ? getFullGovActionId(prevGovActionTxHash, prevGovActionIndex)
      : null;

  const tabs = useMemo(
    () =>
      [
        {
          label: "Reasoning",
          dataTestId: "reasoning-tab",
          content: (
            <ReasoningTabContent
              abstract={abstract}
              motivation={motivation}
              rationale={rationale}
            />
          ),
          visible:
            !isDataMissing && (!!abstract || !!motivation || !!rationale),
        },
        {
          label: "Parameters",
          dataTestId: "parameters-tab",
          content: (
            <GovernanceActionDetailsDiffView
              oldJson={updatableProtocolParams}
              newJson={nonNullProtocolParams}
            />
          ),
          visible:
            (type === GovernanceActionType.ParameterChange ||
              type === GovernanceActionType.NewConstitution) &&
            !!protocolParams &&
            !!epochParams,
        },
        {
          label: "Details",
          dataTestId: "hardfork-details-tab",
          content: (
            <HardforkDetailsTabContent
              details={details}
              prevGovActionId={prevGovActionId}
            />
          ),
          visible:
            type === GovernanceActionType.HardForkInitiation && !!details,
        },
        {
          label: "Parameters",
          dataTestId: "parameters-tab",
          content: (
            <GovernanceActionNewCommitteeDetailsTabContent details={details} />
          ),
          visible: type === GovernanceActionType.NewCommittee && !!details,
        },
        {
          label: "Details",
          dataTestId: "parameters-tab",
          content: (
            <GovernanceActionNewConstitutionDetailsTabContent
              details={details}
            />
          ),
          visible:
            type === GovernanceActionType.NewConstitution &&
            !!details &&
            !!details?.anchor,
        },
      ].filter((tab) => tab.visible),
    [
      abstract,
      motivation,
      rationale,
      epochParams,
      protocolParams,
      type,
      isDataMissing,
    ],
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
      <DataMissingHeader isDataMissing={isDataMissing} title={title} />
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
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
      <GovernanceActionCardElement
        label={t("govActions.cip129GovernanceActionId")}
        text={cip129GovernanceActionId}
        dataTestId={`${cip129GovernanceActionId}-id`}
        isCopyButton
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />

      {tabs?.length === 1 ? (
        tabs[0].content
      ) : (
        <>
          <Tabs
            sx={{
              marginY: 4,
              display: "flex",
              fontSize: 16,
              fontWeight: 500,
            }}
            value={selectedTab}
            indicatorColor="secondary"
            onChange={handleChange}
            aria-label="basic tabs example"
          >
            {tabs.map((tab) => (
              <StyledTab
                key={tab.label}
                data-testid={tab.dataTestId}
                label={tab.label}
                isMobile={isMobile}
              />
            ))}
          </Tabs>

          {tabs.map((tab, tabIndex) => (
            <CustomTabPanel
              key={tab.label}
              value={selectedTab}
              index={tabIndex}
            >
              {tab.content}
            </CustomTabPanel>
          ))}
        </>
      )}
      {details &&
        type === GovernanceActionType.TreasuryWithdrawals &&
        Array.isArray(details) &&
        details.map((withdrawal) => (
          <GovernanceActionCardTreasuryWithdrawalElement
            key={withdrawal.receivingAddress}
            receivingAddress={withdrawal.receivingAddress}
            amount={withdrawal.amount}
          />
        ))}
      <GovernanceActionDetailsCardLinks links={references} />
    </Box>
  );
};

const ReasoningTabContent = ({
  abstract,
  motivation,
  rationale,
}: Pick<ProposalData, "abstract" | "motivation" | "rationale">) => {
  const { t } = useTranslation();

  return (
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
  );
};

const HardforkDetailsTabContent = ({
  details,
  prevGovActionId,
}: Pick<ProposalData, "details"> & { prevGovActionId: string | null }) => {
  const { epochParams } = useAppContext();
  const { t } = useTranslation();

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3, pb: 3 }}>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
        <Typography variant="body2">
          {t("govActions.hardforkDetails.currentVersion")}
        </Typography>
        <Typography variant="body2">
          {epochParams
            ? `${epochParams.protocol_major}.${epochParams.protocol_minor}`
            : "-"}
        </Typography>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
        <Typography variant="body2">
          {t("govActions.hardforkDetails.proposedVersion")}
        </Typography>
        <Typography variant="body2">
          {details ? `${details.major}.${details.minor}` : "-"}
        </Typography>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
        <Typography variant="body2">
          {t("govActions.hardforkDetails.previousGAId")}
        </Typography>
        {prevGovActionId ? (
          <Box sx={{ display: "flex", gap: 1 }}>
            <Typography
              variant="body1"
              sx={{
                fontWeight: 400,
                maxWidth: 283,
                whiteSpace: "nowrap",
                overflow: "hidden",
                textOverflow: "ellipsis",
                color: "primaryBlue",
              }}
            >
              {prevGovActionId}
            </Typography>
            <CopyButton text={prevGovActionId} variant="blueThin" />
          </Box>
        ) : (
          <Typography variant="body2">-</Typography>
        )}
      </Box>
    </Box>
  );
};
