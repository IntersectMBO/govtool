import { useMemo, useState } from "react";
import {
  Box,
  CircularProgress,
  Skeleton,
  styled,
  Tab,
  Tabs,
} from "@mui/material";

import { Typography } from "@atoms";
import { useFeatureFlag } from "@context";
import { GOV_ACTION_HISTORY_PATHS, GOV_ACTION_HISTORY_TYPE_FILTERS, primaryBlue } from "@consts";
import {
  useGetGovernanceActionMetadata,
  useGetGovernanceActionRecordQuery,
  useGetGovernanceActionEpochParams,
  useGetGovernanceActionProposalDiscussionQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import {
  Breadcrumbs,
  DataMissingInfoBox,
  GovernanceActionCardElement,
  GovernanceActionCardTreasuryWithdrawalElement,
  GovernanceActionDetailsCardLinks,
  GovernanceActionDetailsDiffView,
  GovernanceActionNewConstitutionDetailsTabContent,
  GovernanceActionAuthors,
  GovernanceActionDatesBox,
  GovernanceActionStatus,
  GovernanceActionHardForkDetails,
  GovernanceActionNewCommitteeDetails,
  GovernanceActionProposalDiscussion,
  GovernanceActionHistoryEmptyState,
  GovernanceActionStatusChip,
  Share,
} from "@molecules";
import { GovernanceActionRecord } from "@models";
import {
  filterOutNullParams,
  filterUpdatableProtocolParams,
  getFullGovActionId,
  getMetadataDataMissingStatusTranslation,
  getGovernanceActionCIP129Id,
  mapArrayToObjectByKeys,
} from "@utils";
import { GovernanceActionType } from "@/types/governanceAction";

import { GovernanceActionVoting } from "./GovernanceActionVoting";

import "./governanceActionHistory.css";

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

const cardSx = {
  backgroundColor: "white",
  borderRadius: "16px",
  boxShadow: "0px 4px 15px 0px #DDE3F5",
  paddingX: 2,
  paddingY: 2.75,
};

const PROTOCOL_PARAMS_IGNORED_KEYS = ["id", "registered_tx_id", "key"];

const CenteredBox = ({ children }: { children: React.ReactNode }) => (
  <Box
    data-testid="single-action-page-circular-loader"
    sx={{
      alignItems: "center",
      display: "flex",
      flex: 1,
      justifyContent: "center",
      minHeight: "75vh",
      width: "100%",
    }}
  >
    {children}
  </Box>
);

type GovernanceActionHistoryDetailsContentProps = {
  governanceAction: GovernanceActionRecord;
};

const GovernanceActionHistoryDetailsContent = ({
  governanceAction,
}: GovernanceActionHistoryDetailsContentProps) => {
  const { isMobile } = useScreenDimension();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { t } = useTranslation();
  const [selectedTab, setSelectedTab] = useState(0);

  const { metadata, metadataValid, isMetadataLoading } =
    useGetGovernanceActionMetadata(governanceAction);
  const { proposal, isProposalLoading } = useGetGovernanceActionProposalDiscussionQuery(
    governanceAction.tx_hash,
  );
  const { epochParams } = useGetGovernanceActionEpochParams(governanceAction);

  const title = governanceAction.title || metadata?.data?.title;
  const abstract = governanceAction.abstract || metadata?.data?.abstract;
  const motivation = governanceAction.motivation || metadata?.data?.motivation;
  const rationale = governanceAction.rationale || metadata?.data?.rationale;
  const references: Reference[] =
    governanceAction.json_metadata?.body?.references ||
    metadata?.data?.references ||
    [];
  const authors = governanceAction.json_metadata
    ? governanceAction.json_metadata.authors
    : metadata?.data?.authors;

  const hasAnyContent = !!(abstract || motivation || rationale);
  const isDataMissing = metadata?.metadataStatus;

  const idCIP129 = getGovernanceActionCIP129Id(governanceAction);
  const fullGovActionId = getFullGovActionId(
    governanceAction.tx_hash,
    governanceAction.index,
  );
  const typeLabel =
    GOV_ACTION_HISTORY_TYPE_FILTERS.find(
      (filter) => filter.value === governanceAction.type,
    )?.label || governanceAction.type;

  // A string index ("0" is valid); the link is shown only when it is set.
  const prevGovActionId =
    governanceAction.prev_gov_action_index &&
    governanceAction.prev_gov_action_tx_hash
      ? getFullGovActionId(
          governanceAction.prev_gov_action_tx_hash,
          governanceAction.prev_gov_action_index,
        )
      : null;

  const proposedParams = useMemo(
    () =>
      mapArrayToObjectByKeys(governanceAction.proposal_params, [
        "PlutusV1",
        "PlutusV2",
        "PlutusV3",
      ]),
    [governanceAction.proposal_params],
  );
  const updatableParams = useMemo(
    () =>
      filterUpdatableProtocolParams(
        epochParams,
        proposedParams,
        PROTOCOL_PARAMS_IGNORED_KEYS,
      ),
    [epochParams, proposedParams],
  );
  const nonNullParams = useMemo(
    () => filterOutNullParams(proposedParams, PROTOCOL_PARAMS_IGNORED_KEYS),
    [proposedParams],
  );

  const { type, description } = governanceAction;

  const tabs = [
    {
      label: t("actionRecord.tabs.reasoning"),
      dataTestId: "reasoning-tab",
      visible: !isDataMissing && hasAnyContent,
      content: (
        <Box display="flex" flexDirection="column" gap={3}>
          {[
            [t("actionRecord.abstract"), abstract, "governance-action-abstract"],
            [
              t("actionRecord.motivation"),
              motivation,
              "governance-action-motivation",
            ],
            [t("actionRecord.rationale"), rationale, "governance-action-rationale"],
          ].map(([label, text, dataTestId]) => (
            <GovernanceActionCardElement
              key={dataTestId}
              label={label as string}
              text={text ?? ""}
              dataTestId={dataTestId}
              textVariant="longText"
              isMarkdown
              marginBottom={0}
            />
          ))}
        </Box>
      ),
    },
    {
      label: t("actionRecord.tabs.parameters"),
      dataTestId: "parameters-tab",
      visible:
        (type === GovernanceActionType.ParameterChange ||
          type === GovernanceActionType.NewConstitution) &&
        !!governanceAction.proposal_params &&
        !!epochParams,
      content: (
        <GovernanceActionDetailsDiffView
          oldJson={updatableParams}
          newJson={nonNullParams}
        />
      ),
    },
    {
      label: t("actionRecord.tabs.details"),
      dataTestId: "hardfork-details-tab",
      visible:
        type === GovernanceActionType.HardForkInitiation && !!description,
      content: (
        <GovernanceActionHardForkDetails
          action={governanceAction}
          prevGovActionId={prevGovActionId}
        />
      ),
    },
    {
      label: t("actionRecord.tabs.parameters"),
      dataTestId: "new-committee-tab",
      visible: type === GovernanceActionType.NewCommittee && !!description,
      content: <GovernanceActionNewCommitteeDetails description={description} />,
    },
    {
      label: t("actionRecord.tabs.details"),
      dataTestId: "new-constitution-tab",
      visible:
        type === GovernanceActionType.NewConstitution && !!description?.anchor,
      content: (
        <Box display="flex" flexDirection="column" gap={3}>
          <GovernanceActionNewConstitutionDetailsTabContent
            details={description ?? undefined}
          />
          <GovernanceActionAuthors
            authors={authors}
            metadataUrl={governanceAction.url}
          />
        </Box>
      ),
    },
  ].filter((tab) => tab.visible);

  return (
    <Box
      className="outcome-container"
      data-testid={`single-action-${idCIP129}-page`}
      display="flex"
      flex={1}
      flexDirection="column"
    >
      <Breadcrumbs
        elementOne={t("governanceActionHistoryList.title")}
        elementOnePath={GOV_ACTION_HISTORY_PATHS.governanceActionHistory}
        elementTwo={title ?? ""}
        isMetadataLoading={isMetadataLoading || (!isDataMissing && !title)}
        isDataMissing={isDataMissing ?? null}
      />
      <Box className="outcome-details-grid" mt={0.5}>
        <Box
          className="outcome-details"
          data-testid={`single-action-${idCIP129}-description`}
          sx={{
            ...cardSx,
            ...(!metadataValid && { border: "1px solid #F6D5D5" }),
          }}
        >
          <Box display="flex" flexDirection="column" overflow="hidden" gap={3}>
            <Box
              data-testid="single-action-header"
              display="flex"
              justifyContent="space-between"
              alignItems="center"
              gap={1}
            >
              {isMetadataLoading ? (
                <Skeleton variant="rounded" width="75%" height={32} />
              ) : (
                <Typography
                  data-testid="single-action-title"
                  sx={{
                    fontSize: 22,
                    fontWeight: 600,
                    lineHeight: "24px",
                    wordBreak: "break-word",
                    ...(isDataMissing && { color: "errorRed" }),
                  }}
                >
                  {(isDataMissing &&
                    getMetadataDataMissingStatusTranslation(isDataMissing)) ||
                    title}
                </Typography>
              )}
              <Share link={window.location.href} />
            </Box>

            <DataMissingInfoBox isDataMissing={isDataMissing} sx={{ mb: 0 }} />

            <Box
              data-testid="single-action-type"
              display="flex"
              flexDirection="column"
              gap={0.5}
            >
              <Typography
                sx={{ color: "neutralGray", fontWeight: 600, fontSize: 14 }}
              >
                {t("actionRecord.governanceActionType")}
              </Typography>
              <Box display="inline-flex">
                <GovernanceActionStatusChip
                  label={typeLabel}
                  bgColor={primaryBlue.c100}
                />
              </Box>
            </Box>
            <GovernanceActionDatesBox action={governanceAction} />
            <GovernanceActionStatus
              status={governanceAction.status}
              actionId={idCIP129}
              isCard={false}
            />
            <GovernanceActionCardElement
              label={t("actionRecord.governanceActionId105")}
              text={fullGovActionId}
              dataTestId="single-action-CIP-105-id"
              textVariant="longText"
              isCopyButton
              marginBottom={0}
            />
            <GovernanceActionCardElement
              label={t("actionRecord.governanceActionId129")}
              text={idCIP129}
              dataTestId="single-action-CIP-129-id"
              textVariant="longText"
              isCopyButton
              marginBottom={0}
            />

            {!hasAnyContent && isMetadataLoading && (
              <>
                <Skeleton variant="rounded" width="20%" height={15} />
                <Skeleton variant="rounded" width="100%" height={400} />
              </>
            )}

            {tabs.length === 1 && tabs[0].content}
            {tabs.length > 1 && (
              <Box>
                <Tabs
                  sx={{ display: "flex", fontSize: 16, fontWeight: 500, mb: 3 }}
                  value={Math.min(selectedTab, tabs.length - 1)}
                  indicatorColor="secondary"
                  onChange={(_event, newValue: number) =>
                    setSelectedTab(newValue)
                  }
                  aria-label="Governance action content description"
                >
                  {tabs.map((tab) => (
                    <StyledTab
                      key={tab.dataTestId}
                      data-testid={tab.dataTestId}
                      label={tab.label}
                      isMobile={isMobile}
                    />
                  ))}
                </Tabs>
                {tabs[Math.min(selectedTab, tabs.length - 1)].content}
              </Box>
            )}

            {type === GovernanceActionType.TreasuryWithdrawals &&
              Array.isArray(description) &&
              (
                description as unknown as {
                  receivingAddress: string;
                  amount: number;
                }[]
              ).map((withdrawal) => (
                <Box key={withdrawal.receivingAddress}>
                  <GovernanceActionCardTreasuryWithdrawalElement
                    receivingAddress={withdrawal.receivingAddress}
                    amount={withdrawal.amount}
                  />
                </Box>
              ))}

            {type !== GovernanceActionType.NewConstitution && (
              <>
                <GovernanceActionCardElement
                  label={t("actionRecord.metadataLink")}
                  text={governanceAction.url}
                  dataTestId="metadata-anchor-link"
                  textVariant="longText"
                  isLinkButton
                  marginBottom={0}
                />
                <GovernanceActionCardElement
                  label={t("actionRecord.metadataHash")}
                  text={governanceAction.data_hash}
                  dataTestId="metadata-anchor-hash"
                  textVariant="longText"
                  isCopyButton
                  marginBottom={0}
                />
                <GovernanceActionAuthors
                  authors={authors}
                  metadataUrl={governanceAction.url}
                />
              </>
            )}

            {metadataValid && references.length > 0 && (
              <Box>
                <GovernanceActionDetailsCardLinks links={references} />
              </Box>
            )}

            {isProposalDiscussionForumEnabled && (
              <GovernanceActionProposalDiscussion
                proposal={proposal}
                isLoading={isProposalLoading}
              />
            )}
          </Box>
        </Box>

        <Box
          className="outcome-votes"
          data-testid="single-action-voting-numbers"
          sx={cardSx}
        >
          <GovernanceActionVoting action={governanceAction} />
        </Box>
      </Box>
    </Box>
  );
};

type GovernanceActionHistoryDetailsProps = {
  /** CIP-105 `txHash#index` or CIP-129 `gov_action1…`. */
  id: string;
};

export const GovernanceActionHistoryDetails = ({ id }: GovernanceActionHistoryDetailsProps) => {
  const { t } = useTranslation();
  const { governanceAction, isGovernanceActionLoading, governanceActionError } =
    useGetGovernanceActionRecordQuery(id);

  if (isGovernanceActionLoading) {
    return (
      <CenteredBox>
        <CircularProgress />
      </CenteredBox>
    );
  }

  if (governanceActionError || !governanceAction) {
    return (
      <CenteredBox>
        <GovernanceActionHistoryEmptyState
          title={t("actionRecord.noResults.title")}
          description={t("actionRecord.noResults.description")}
        />
      </CenteredBox>
    );
  }

  return <GovernanceActionHistoryDetailsContent governanceAction={governanceAction} />;
};
