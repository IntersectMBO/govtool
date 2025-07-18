import { useMemo, useState, useEffect } from "react";
import { Box, Tabs, Tab, styled, Skeleton, Link } from "@mui/material";
import { useLocation } from "react-router-dom";
import CheckCircleOutlineIcon from "@mui/icons-material/CheckCircleOutline";
import CancelOutlinedIcon from "@mui/icons-material/CancelOutlined";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { CopyButton, ExternalModalButton, Tooltip, Typography } from "@atoms";
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
  validateSignature,
} from "@utils";
import { MetadataValidationStatus, ProposalData } from "@models";
import { Trans } from "react-i18next";
import { errorRed, successGreen } from "@/consts";
import { GovernanceActionType } from "@/types/governanceAction";
import { useAppContext } from "@/context";
import { theme } from "@/theme";

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

// eslint-disable-next-line @typescript-eslint/no-unused-vars
const StyledTab = styled(({ isMobile, ...props }: StyledTabProps) => (
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
  isDataMissing?: MetadataValidationStatus;
  isInProgress?: boolean;
  isOneColumn: boolean;
  isSubmitted?: boolean;
  isValidating?: boolean;
  proposal: ProposalData;
};

export const GovernanceActionDetailsCardData = ({
  isDashboard,
  isDataMissing,
  isInProgress,
  isOneColumn,
  isSubmitted,
  isValidating,
  proposal: {
    abstract,
    authors,
    json: jsonContent,
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
    metadataHash,
  },
}: GovernanceActionDetailsCardDataProps) => {
  const { epochParams } = useAppContext();
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();
  const { isMobile } = useScreenDimension();
  const { pathname, hash } = useLocation();

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

  const govActionLinkToShare = `${window.location.protocol}//${
    window.location.hostname
  }${window.location.port ? `:${window.location.port}` : ""}${pathname}${
    hash ?? ""
  }`;

  const isItMLabsWithdrawal =
    type === GovernanceActionType.TreasuryWithdrawals &&
    cip129GovernanceActionId ===
      "gov_action18nefry4qacd80xzs2srjahxm2e4vz3c8wvrr03rrtk8mdqfuknysq66459t";

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
      <DataMissingHeader
        isDataMissing={isDataMissing}
        isValidating={isValidating}
        title={title}
        shareLink={govActionLinkToShare}
      />
      <DataMissingInfoBox
        isDataMissing={isDataMissing}
        isValidating={isValidating}
        isInProgress={isInProgress}
        isSubmitted={isSubmitted}
      />
      <GovernanceActionCardElement
        label={t("govActions.governanceActionType")}
        text={label}
        textVariant="pill"
        dataTestId={`${getProposalTypeNoEmptySpaces(label)}-type`}
        isValidating={isValidating}
      />
      <GovernanceActionsDatesBox
        createdDate={createdDate}
        expiryDate={expiryDate}
        expiryEpochNo={expiryEpochNo}
        createdEpochNo={createdEpochNo}
        isValidating={isValidating}
      />
      {isDataMissing &&
        (isValidating ? (
          <Skeleton height="24px" width="128px" variant="text" />
        ) : (
          <ExternalModalButton
            url={url}
            label={t("govActions.seeExternalData")}
          />
        ))}
      <GovernanceActionCardElement
        label={t("govActions.cip129GovernanceActionId")}
        text={cip129GovernanceActionId}
        dataTestId={`${cip129GovernanceActionId}-id`}
        isCopyButton
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
        isValidating={isValidating}
      />
      <GovernanceActionCardElement
        label={t("govActions.governanceActionId")}
        text={govActionId}
        isCopyButton
        dataTestId={`${govActionId}-id`}
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
        isSemiTransparent
        isValidating={isValidating}
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
      {!isValidating &&
        details &&
        type === GovernanceActionType.TreasuryWithdrawals &&
        Array.isArray(details) &&
        details.map((withdrawal) => (
          <GovernanceActionCardTreasuryWithdrawalElement
            key={withdrawal.receivingAddress}
            receivingAddress={withdrawal.receivingAddress}
            amount={withdrawal.amount}
          />
        ))}
      {/* NewConstitution metadata hash and url is visible in details tab */}
      {!isValidating && type !== GovernanceActionType.NewConstitution && (
        <>
          <GovernanceActionCardElement
            label={t("govActions.anchorURL")}
            text={url}
            textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
            dataTestId="anchor-url"
            isLinkButton
            isValidating={isValidating}
          />
          <GovernanceActionCardElement
            label={t("govActions.anchorHash")}
            text={metadataHash}
            textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
            dataTestId="anchor-hash"
            isCopyButton
            isValidating={isValidating}
          />
        </>
      )}
      <GovernanceActionCardElement
        label={t("govActions.authors.title")}
        textVariant="longText"
        dataTestId="authors"
      >
        <Box sx={{ display: "flex", gap: 2, flexWrap: "wrap" }}>
          {(authors ?? []).length <= 0
            ? t("govActions.authors.noDataAvailable")
            : (authors ?? []).map((author) => (
              <Box
                key={author.publicKey}
                sx={{ display: "flex", gap: 0.5, alignItems: "center" }}
              >
                <AuthorSignatureStatus
                  signature={author.signature}
                  publicKey={author.publicKey}
                  algorithm={author.witnessAlgorithm}
                  jsonContent={jsonContent}
                  forceValidStatus={isItMLabsWithdrawal}
                />
                <span>{author.name}</span>
                <Tooltip
                  heading={`${t("govActions.authors.witnessAlgorithm")}: ${
                      author.witnessAlgorithm
                    }`}
                  paragraphOne={`${t("govActions.authors.publicKey")}: ${
                      author.publicKey
                    }`}
                  paragraphTwo={`${t("govActions.authors.signature")}: ${
                      author.signature
                    }`}
                  placement="bottom-end"
                  arrow
                >
                  <InfoOutlinedIcon fontSize="small" />
                </Tooltip>
              </Box>
              ))}
          {isItMLabsWithdrawal && authors && authors.length > 0 && <AuthorsVerificationInfoBox />}
        </Box>
      </GovernanceActionCardElement>

      <GovernanceActionDetailsCardLinks links={references} />
    </Box>
  );
};

const ReasoningTabContent = ({
  abstract,
  motivation,
  rationale,
  isValidating,
}: Pick<ProposalData, "abstract" | "motivation" | "rationale"> & {
  isValidating?: boolean;
}) => {
  const { t } = useTranslation();

  return (
    <>
      <GovernanceActionCardElement
        label={t("govActions.abstract")}
        text={abstract}
        textVariant="longText"
        dataTestId="abstract"
        isMarkdown
        isValidating={isValidating}
      />
      <GovernanceActionCardElement
        label={t("govActions.motivation")}
        text={motivation}
        textVariant="longText"
        dataTestId="motivation"
        isMarkdown
        isValidating={isValidating}
      />
      <GovernanceActionCardElement
        label={t("govActions.rationale")}
        text={rationale}
        textVariant="longText"
        dataTestId="rationale"
        isMarkdown
        isValidating={isValidating}
      />
    </>
  );
};

const HardforkDetailsTabContent = ({
  details,
  prevGovActionId,
}: Pick<ProposalData, "details"> & {
  prevGovActionId: string | null;
}) => {
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

const AuthorSignatureStatus = ({
  algorithm,
  publicKey,
  signature,
  jsonContent,
  forceValidStatus = false,
}: {
  algorithm?: string;
  publicKey?: string;
  signature?: string;
  jsonContent?: Record<string, unknown>;
  forceValidStatus: boolean;
}) => {
  const { t } = useTranslation();

  const [isSignatureValid, setIsSignatureValid] = useState<boolean | null>(
    null,
  );

  useEffect(() => {
    let cancelled = false;
    async function checkSignature() {
      const args = {
        jsonContent,
        algorithm,
        publicKey,
        signature,
      };
      const result = await validateSignature(args);
      if (!cancelled) setIsSignatureValid(result);
    }
    if (forceValidStatus) {
      return setIsSignatureValid(true);
    }
    checkSignature();
    return () => {
      cancelled = true;
    };
  }, [algorithm, jsonContent, publicKey, signature]);

  if (isSignatureValid === null) {
    return <Skeleton variant="text" width={16} />;
  }
  return (
    <Tooltip
      heading={
        isSignatureValid
          ? t("govActions.authors.singatureVerified")
          : t("govActions.authors.signatureNotVerified")
      }
      placement="bottom-end"
      arrow
    >
      {isSignatureValid ? (
        <CheckCircleOutlineIcon
          sx={{ color: successGreen.c500 }}
          fontSize="small"
        />
      ) : (
        <CancelOutlinedIcon sx={{ color: errorRed.c500 }} fontSize="small" />
      )}
    </Tooltip>
  );
};

const AuthorsVerificationInfoBox = () => {
  const { t } = useTranslation();
  const {
    palette: { lightBlue, secondaryBlue },
  } = theme;

  return (
    <Box
      sx={{
        display: "flex",
        gap: 1,
        backgroundColor: lightBlue,
        p: 2,
        borderRadius: "5px",
      }}
    >
      <InfoOutlinedIcon fontSize="small" style={{ color: secondaryBlue }} />
      <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <Typography variant="body2">
          {t("govActions.safeModeInfoBox.title")}
        </Typography>
        <Typography variant="body2">
          {t("govActions.safeModeInfoBox.line1")}
        </Typography>
        <Typography variant="body2">
          <Trans
            i18nKey="govActions.safeModeInfoBox.line2"
            components={[<span style={{ fontWeight: 700 }} key="0" />]}
          />
        </Typography>
        <Link
          href="https://docs.gov.tools/cardano-govtool/faqs/how-was-the-author-of-withdraw-ara45-217-for-mlabs-core...-ga-verified"
          target="_blank"
          rel="noopener noreferrer"
          sx={{ fontSize: "14px" }}
        >
          {t("govActions.safeModeInfoBox.link")}
        </Link>
      </Box>
    </Box>
  );
};
