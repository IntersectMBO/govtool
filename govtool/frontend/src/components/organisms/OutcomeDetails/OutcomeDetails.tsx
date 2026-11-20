import { ReactNode, useState } from "react";
import { Link as RouterLink, useLocation, useNavigate, useSearchParams } from "react-router";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase, Menu, MenuItem, Skeleton } from "@mui/material";
import { ChevronRight, EllipsisVertical } from "lucide-react";

import { ShareDialog } from "@/components/organisms/DRepDashboard/ShareDRepDialog";
import { DataMissingInfoBox } from "@/components/molecules/DataMissingInfoBox";
import { GOV_ACTION_HISTORY_TYPE_FILTERS } from "@/consts/governanceActionHistory";
import { GOV_ACTION_HISTORY_PATHS, PATHS } from "@/consts/paths";
import { ds } from "@/consts/designTokens";
import { useCardano } from "@/context/wallet";
import { useSnackbar } from "@/context/snackbar";
import type { EpochParams } from "@/models/api";
import type { GovernanceActionRecord } from "@/models/governanceActions";
import { copyToClipboard } from "@/utils/clipboard";
import { getFullGovActionId } from "@/utils/getGovActionId";
import { getGovernanceActionCIP129Id } from "@/utils/governanceActions";
import { getMetadataDataMissingStatusTranslation } from "@/utils/getMetadataDataMissingStatusTranslation";
import { openInNewTab } from "@/utils/openInNewTab";
import { lifecycleEvents, outcomeKey, treasuryWithdrawals, voterGroups } from "@/utils/outcomeDetails";

import { ActionInformationCard } from "./ActionInformationCard";
import { ActivityTab, AdditionalInfoCard, DiscussionsTab, VotesTab, type ForumProposal } from "./OtherTabs";
import { OverviewTab, type Reference } from "./OverviewTab";
import {
  dateWithEpoch,
  HEADER_TINTS,
  MonoTag,
  StatusPill,
  VerdictBadge,
  type LifecycleKind,
} from "./parts";
import { VotesCard } from "./VotesCard";

const TABS = ["overview", "discussions", "votes", "activity"] as const;
type Tab = (typeof TABS)[number];

type Props = {
  action: GovernanceActionRecord;
  title?: string;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references: Reference[];
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  authors?: any;
  isMetadataLoading: boolean;
  metadataStatus?: Parameters<typeof getMetadataDataMissingStatusTranslation>[0];
  epochParams?: EpochParams;
  proposal?: ForumProposal;
  isProposalLoading: boolean;
};

const DateItem = ({ label, value }: { label: string; value: string }) => (
  <Box sx={{ minWidth: 0 }}>
    <Box sx={{ color: ds.gray["600"], fontSize: 11, fontWeight: 500, letterSpacing: "0.3px", textTransform: "uppercase" }}>{label}</Box>
    <Box sx={{ color: ds.gray["900"], fontSize: 13, mt: 0.25 }}>{value}</Box>
  </Box>
);

/** A concluded (or live) governance action: header, four tabs and the side cards. */
export const OutcomeDetails = ({
  action,
  title,
  abstract,
  motivation,
  rationale,
  references,
  authors,
  isMetadataLoading,
  metadataStatus,
  epochParams,
  proposal,
  isProposalLoading,
}: Props) => {
  const { t } = useTranslation();
  const { isEnabled } = useCardano();
  const { addErrorAlert, addSuccessAlert } = useSnackbar();
  const [params] = useSearchParams();
  const navigate = useNavigate();
  const { hash } = useLocation();
  const [menuAnchor, setMenuAnchor] = useState<HTMLElement | null>(null);
  const requested = params.get("tab") as Tab | null;
  const tab: Tab = requested && TABS.includes(requested) ? requested : "overview";
  const idCIP129 = getGovernanceActionCIP129Id(action);
  const outcome = outcomeKey(action);
  const groups = voterGroups(action);
  const typeLabel =
    GOV_ACTION_HISTORY_TYPE_FILTERS.find((filter) => filter.value === action.type)?.label ??
    action.type;
  // The date of the status the pills lead with, not simply the latest one: an
  // action can expire and be dropped an epoch later.
  const ended = lifecycleEvents(action).find((event) => event.key === outcome);
  const withdrawals = treasuryWithdrawals(action);
  // What the More menu copies: both id forms, then each receiving address.
  const copyItems = [
    { label: t("outcome.copyId"), testId: "outcome-copy-id", value: getFullGovActionId(action.tx_hash, action.index) },
    { label: t("outcome.copyId129"), testId: "outcome-copy-cip129-id", value: idCIP129 },
    ...withdrawals.map((withdrawal, index) => ({
      label: withdrawals.length > 1 ? t("outcome.copyAddressNumbered", { n: index + 1 }) : t("outcome.copyAddress"),
      testId: `outcome-copy-address-${index}`,
      value: withdrawal.receivingAddress,
    })),
  ];
  const copy = async (value: string) => {
    setMenuAnchor(null);
    if (await copyToClipboard(value)) addSuccessAlert(t("alerts.copiedToClipboard"));
    else addErrorAlert(t("alerts.copyFailed"));
  };
  const shownTitle = (metadataStatus && getMetadataDataMissingStatusTranslation(metadataStatus)) || title || t("govActions.card.untitled");

  // The hash is the action's index (txHash#index), so it must survive.
  const setTab = (next: Tab) => {
    const nextParams = new URLSearchParams(params);
    if (next === "overview") nextParams.delete("tab");
    else nextParams.set("tab", next);
    const search = nextParams.toString();
    navigate({ search: search ? `?${search}` : "", hash }, { replace: true });
  };

  // Ratified then Enacted, or how else it ended.
  const lifecycle: LifecycleKind[] = [];
  if (action.status.ratified_epoch !== null) lifecycle.push("ratified");
  if (action.status.enacted_epoch !== null) lifecycle.push("enacted");
  if (outcome === "expired" || outcome === "dropped" || outcome === "live") lifecycle.push(outcome);
  const lifecyclePills: ReactNode[] = lifecycle.map((kind) => (
    <StatusPill key={kind} kind={kind} label={t(`outcome.statuses.${kind}`)} />
  ));

  const side = tab === "votes" ? (
    <>
      <AdditionalInfoCard action={action} proposal={proposal} />
      <VotesCard action={action} />
    </>
  ) : (
    <>
      <VotesCard action={action} />
      <ActionInformationCard action={action} authors={authors} />
    </>
  );

  return (
    <Box data-testid={`single-action-${idCIP129}-page`} sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 2.5, isolation: "isolate", position: "relative" }}>
      {/* The header's full-width tint: green once passed, red when it failed. */}
      <Box
        aria-hidden
        data-testid="outcome-header-tint"
        sx={{
          background: `linear-gradient(180deg, ${HEADER_TINTS[outcome]} 0%, rgba(255, 255, 255, 0) 100%)`,
          height: 380,
          left: "50%",
          position: "absolute",
          top: { xxs: -24, md: -40 },
          transform: "translateX(-50%)",
          width: "100vw",
          zIndex: -1,
        }}
      />
      <Box aria-label="Breadcrumb" component="nav" sx={{ alignItems: "center", color: ds.gray["600"], display: "flex", flexWrap: "wrap", fontSize: 12, gap: 0.75 }}>
        <Box component={RouterLink} sx={{ color: "inherit", textDecoration: "none", "&:hover": { textDecoration: "underline" } }} to={isEnabled ? PATHS.dashboardGovernanceActions : PATHS.governanceActions}>
          {t("outcome.breadcrumbRoot")}
        </Box>
        <ChevronRight aria-hidden size={12} />
        <Box component={RouterLink} data-testid="outcome-breadcrumb" sx={{ color: "inherit", textDecoration: "none", "&:hover": { textDecoration: "underline" } }} to={GOV_ACTION_HISTORY_PATHS.governanceActionHistory}>
          {t("outcome.breadcrumbOutcome")}
        </Box>
        <ChevronRight aria-hidden size={12} />
        <Box aria-current="page" component="span" sx={{ color: ds.gray["900"], fontWeight: 600, maxWidth: 420, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
          {isMetadataLoading ? "" : shownTitle}
        </Box>
      </Box>

      <Box data-testid="single-action-header" sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
        <Box data-testid="outcome-status-pills" sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>
          {action.type !== "InfoAction" &&
            groups.filter((group) => group.applicable).map((group) => (
              <VerdictBadge
                key={group.role}
                aggregate={group.aggregate}
                applicable
                role={group.role}
              />
            ))}
          {lifecyclePills}
        </Box>
        <MonoTag testId="single-action-type">{typeLabel}</MonoTag>
        <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
          {isMetadataLoading ? (
            <Skeleton height={36} variant="rounded" width="70%" />
          ) : (
            <Box
              component="h1"
              data-testid="single-action-title"
              sx={{ color: metadataStatus ? ds.error["700"] : ds.gray["900"], fontSize: { xxs: 24, md: 28 }, fontWeight: 600, letterSpacing: "-0.5px", lineHeight: { xxs: "30px", md: "36px" }, m: 0, wordBreak: "break-word" }}
            >
              {shownTitle}
            </Box>
          )}
          <Box sx={{ alignItems: "center", display: "flex", flexShrink: 0, gap: 1 }}>
            <ShareDialog
              buttonTestId="share-button"
              copyTestId="copy-link-from-share-button"
              displayId={idCIP129}
              kind="action"
              link={window.location.href}
            />
            <ButtonBase
              aria-controls={menuAnchor ? "outcome-more-menu" : undefined}
              aria-expanded={Boolean(menuAnchor)}
              aria-haspopup="menu"
              aria-label={t("outcome.more")}
              data-testid="outcome-more-button"
              onClick={(event) => setMenuAnchor(event.currentTarget)}
              sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: "50%", color: ds.gray["800"], height: 36, width: 36, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
            >
              <EllipsisVertical aria-hidden size={16} />
            </ButtonBase>
            <Menu anchorEl={menuAnchor} id="outcome-more-menu" onClose={() => setMenuAnchor(null)} open={Boolean(menuAnchor)}>
              {copyItems.map((item) => (
                <MenuItem
                  key={item.testId}
                  data-testid={item.testId}
                  onClick={() => copy(item.value)}
                >
                  {item.label}
                </MenuItem>
              ))}
              {action.url && (
                <MenuItem
                  onClick={() => {
                    openInNewTab(action.url);
                    setMenuAnchor(null);
                  }}
                >
                  {t("outcome.openMetadata")}
                </MenuItem>
              )}
            </Menu>
          </Box>
        </Box>
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: { xxs: 2, md: 0 }, "& > *": { pr: { md: 3 } }, "& > * + *": { borderLeft: { md: `1px solid ${ds.gray["200"]}` }, pl: { md: 3 } } }}>
          <DateItem label={t("outcome.submitted")} value={dateWithEpoch(t, action.time, action.epoch_no)} />
          <DateItem label={t("outcome.votingEnds")} value={dateWithEpoch(t, action.expiry_date, action.expiration)} />
          {ended && (
            <DateItem
              label={t({ enacted: "outcome.enactedOn", ratified: "outcome.ratifiedOn", expired: "outcome.expiredOn", dropped: "outcome.droppedOn" }[ended.key as "enacted"])}
              value={dateWithEpoch(t, ended.time, ended.epoch)}
            />
          )}
        </Box>
        {metadataStatus && <DataMissingInfoBox isDataMissing={metadataStatus} sx={{ mb: 0 }} />}
      </Box>

      <Box aria-label={t("outcome.tabs.label")} role="tablist" sx={{ borderBottom: `1px solid ${ds.gray["200"]}`, display: "flex", gap: 1, overflowX: "auto" }}>
        {TABS.map((key) => (
          <ButtonBase
            key={key}
            aria-selected={tab === key}
            data-testid={`outcome-tab-${key}`}
            onClick={() => setTab(key)}
            role="tab"
            sx={{ borderBottom: `2px solid ${tab === key ? ds.primary["600"] : "transparent"}`, color: tab === key ? ds.primary["700"] : ds.gray["600"], flexShrink: 0, fontFamily: "inherit", fontSize: 14, fontWeight: tab === key ? 600 : 500, mb: "-1px", px: 1.5, py: 1.25, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            {t(`outcome.tabs.${key}`)}
          </ButtonBase>
        ))}
      </Box>

      <Box sx={{ alignItems: "start", display: "grid", gap: 2.5, gridTemplateColumns: { xxs: "minmax(0, 1fr)", lg: "minmax(0, 1fr) 340px" } }}>
        <Box data-testid={`single-action-${idCIP129}-description`} role="tabpanel" sx={{ minWidth: 0 }}>
          {tab === "overview" && (
            <OverviewTab
              abstract={abstract}
              action={action}
              epochParams={epochParams}
              motivation={motivation}
              rationale={rationale}
              references={references}
            />
          )}
          {tab === "discussions" && (
            <DiscussionsTab
              isLoading={isProposalLoading}
              proposal={proposal}
              references={references}
            />
          )}
          {tab === "votes" && <VotesTab action={action} />}
          {tab === "activity" && <ActivityTab action={action} />}
        </Box>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>{side}</Box>
      </Box>
    </Box>
  );
};
