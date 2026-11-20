import { ReactNode, useEffect, useMemo, useRef, useState } from "react";
import { useQueryClient } from "@tanstack/react-query";
import { Box, ButtonBase } from "@mui/material";
import { Link as RouterLink, useSearchParams } from "react-router";
import { useTranslation } from "react-i18next";
import { CircleCheck, UserRoundX } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PageTabs } from "@/components/molecules/PageTabs";
import { PATHS } from "@/consts/paths";
import { QUERY_KEYS } from "@/consts/queryKeys";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import type { PendingTransaction } from "@/context/pendingTransaction";
import { useGetDRepDetailsQuery } from "@/hooks/queries/useGetDrepDetailsQuery";
import {
  useDRepDashboardActions,
  useDRepDashboardVotes,
} from "@/hooks/queries/useDRepDashboard";
import type { VoterInfo } from "@/models";
import { actionKey, epochAnchors, registrationEpoch } from "@/utils/drepDashboard";

import { formatLongDate } from "./parts";
import type { DashboardData } from "./types";
import { OverviewTab } from "./OverviewTab";
import { GovernanceTab } from "./GovernanceTab";
import { DiscussionsTab } from "./DiscussionsTab";
import { ActivityReportTab } from "./ActivityReportTab";
import { HistoryLogTab } from "./HistoryLogTab";
import { ShareDRepDialog } from "./ShareDRepDialog";
import { useFeatureFlag } from "@/context/featureFlag";

const TABS = ["overview", "governance", "discussions", "activity", "history"] as const;
export type DashboardTab = (typeof TABS)[number];

const NEW_DREP_DAYS = 30;

const welcomeKey = (dRepID: string) => `govtool_drep_welcome_dismissed_${dRepID}`;

const readDismissed = (dRepID: string) => {
  try {
    return window.localStorage.getItem(welcomeKey(dRepID)) === "1";
  } catch {
    return false;
  }
};

type Props = {
  voter: VoterInfo;
  dRepID: string;
  stakeKey?: string;
  pendingTransaction: PendingTransaction;
  /** The Phase 5 wallet card, built by DashboardCards. */
  walletCard: ReactNode;
  /** Delegate, direct voter and propose cards, built by DashboardCards. */
  actionCards: ReactNode;
};

/** A registered DRep's dashboard: a header and five tabs. */
export const DRepDashboard = ({
  voter,
  dRepID,
  stakeKey,
  pendingTransaction,
  walletCard,
  actionCards,
}: Props) => {
  const { t } = useTranslation();
  const { epochParams, cExplorerBaseUrl } = useAppContext();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const [params] = useSearchParams();
  const [isWelcomeDismissed, setWelcomeDismissed] = useState(() => readDismissed(dRepID));

  const queryClient = useQueryClient();
  const { dRep, isLoading: dRepLoading } = useGetDRepDetailsQuery(dRepID);

  // Nothing refetches the profile when a metadata update confirms, so do it
  // here once the pending update clears.
  const pendingUpdate = pendingTransaction.updateMetaData?.transactionHash;
  const lastUpdate = useRef(pendingUpdate);
  useEffect(() => {
    if (lastUpdate.current && !pendingUpdate) {
      queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.useGetDRepInfoKey] });
      queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.useGetDRepListInfiniteKey] });
    }
    lastUpdate.current = pendingUpdate;
  }, [pendingUpdate, queryClient]);
  const votesQuery = useDRepDashboardVotes(dRepID);
  const actionsQuery = useDRepDashboardActions();
  const currentEpoch =
    typeof epochParams?.epoch_no === "number" ? epochParams.epoch_no : undefined;

  const data: DashboardData = useMemo(() => {
    const { actions } = actionsQuery;
    const actionsByKey = new Map(
      (actions ?? []).map((record) => [actionKey(record.tx_hash, record.index), record]),
    );
    const anchors = epochAnchors(actions ?? [], votesQuery.votes ?? [], currentEpoch);
    return {
      voter,
      dRep,
      dRepLoading,
      dRepID,
      stakeKey,
      currentEpoch,
      votes: votesQuery.votes,
      votesLoading: votesQuery.isLoading,
      votesError: votesQuery.isError,
      retryVotes: () => { votesQuery.refetch(); },
      actions,
      actionsByKey,
      actionsLoading: actionsQuery.isLoading,
      actionsError: actionsQuery.isError,
      retryActions: () => { actionsQuery.refetch(); },
      registeredEpoch: registrationEpoch(dRep?.latestRegistrationDate, anchors),
      updatedAt: Math.max(votesQuery.dataUpdatedAt, actionsQuery.dataUpdatedAt),
      profileLink: dRep?.view
        ? `${window.location.origin}${PATHS.dRepDirectoryDRep.replace(":dRepId", encodeURIComponent(dRep.view))}`
        : undefined,
    };
  }, [
    voter,
    dRep,
    dRepLoading,
    dRepID,
    stakeKey,
    currentEpoch,
    votesQuery.votes,
    votesQuery.isLoading,
    votesQuery.isError,
    votesQuery.dataUpdatedAt,
    actionsQuery.actions,
    actionsQuery.isLoading,
    actionsQuery.isError,
    actionsQuery.dataUpdatedAt,
  ]);

  const tabs = TABS.filter((tab) => tab !== "discussions" || isProposalDiscussionForumEnabled);
  const requested = params.get("tab") as DashboardTab | null;
  const tab: DashboardTab = requested && tabs.includes(requested) ? requested : "overview";

  // New: registered in the last 30 days and not voted yet.
  const registeredAt = new Date(dRep?.latestRegistrationDate ?? "").getTime();
  const isNew =
    votesQuery.votes !== undefined &&
    votesQuery.votes.length === 0 &&
    Date.now() - registeredAt < NEW_DREP_DAYS * 24 * 60 * 60 * 1000;
  const name = voter.givenName || dRep?.givenName;
  const pending = pendingTransaction.retireAsDrep ?? pendingTransaction.updateMetaData;
  const isBusy = !!(pendingTransaction.retireAsDrep || pendingTransaction.updateMetaData);

  return (
    <Box data-testid="drep-dashboard" sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ alignItems: { md: "flex-start" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 1, minWidth: 0 }}>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.25 }}>
            <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 26, md: 30 }, fontWeight: 600, letterSpacing: "-0.6px", lineHeight: { xxs: "32px", md: "38px" }, m: 0, overflowWrap: "anywhere" }}>
              {name ? t("drepDashboard.welcome", { name }) : t("drepDashboard.welcomeAnonymous")}
            </Box>
            <Box
              component="span"
              data-testid="dashboard-role-pill"
              sx={{ alignItems: "center", backgroundColor: PILL_TONES.primary.bg, borderRadius: `${RADIUS.pill}px`, color: PILL_TONES.primary.text, display: "inline-flex", fontSize: 12, fontWeight: 600, gap: 0.625, letterSpacing: "0.3px", lineHeight: "18px", px: 1, textTransform: "uppercase" }}
            >
              <Box aria-hidden component="span" sx={{ backgroundColor: "currentColor", borderRadius: "50%", height: 6, width: 6 }} />
              {isNew ? t("drepDashboard.role.newDrep") : t("drepDashboard.role.drep")}
            </Box>
          </Box>
          <Box sx={{ color: ds.gray["500"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 1 }}>
            {dRep?.latestRegistrationDate && (
              <span>
                {t("drepDashboard.registered")}{" "}
                <Box component="span" sx={{ color: ds.gray["800"], fontWeight: 500 }}>{formatLongDate(dRep.latestRegistrationDate)}</Box>
              </span>
            )}
            {dRep?.latestRegistrationDate && currentEpoch !== undefined && (
              <span aria-hidden>•</span>
            )}
            {currentEpoch !== undefined && (
              <span>
                {t("drepDashboard.currentEpoch")}{" "}
                <Box component="span" sx={{ color: ds.gray["800"], fontWeight: 500 }}>{currentEpoch}</Box>
              </span>
            )}
          </Box>
        </Box>
        <Box sx={{ alignItems: "center", display: "flex", flexShrink: 0, flexWrap: "wrap", gap: 1.5 }}>
          <Button component={RouterLink} dataTestId="edit-drep-profile-button" disabled={isBusy} size="medium" to={PATHS.editDrepMetadata} variant="outlined">
            {t("drepDashboard.editProfile")}
          </Button>
          <Button
            component={RouterLink}
            dataTestId="retire-button"
            disabled={isBusy}
            size="medium"
            startIcon={<UserRoundX aria-hidden size={16} />}
            sx={{
              backgroundColor: ds.error["600"],
              boxShadow: SHADOWS.innerError,
              "&:hover": { backgroundColor: ds.error["700"], boxShadow: SHADOWS.innerError },
            }}
            to={PATHS.retireAsDrep}
          >
            {t("drepDashboard.retire")}
          </Button>
          {data.profileLink && <ShareDRepDialog dRepId={dRep?.view} link={data.profileLink} />}
        </Box>
      </Box>

      {isBusy && pending && (
        <Box role="status" sx={{ alignItems: "center", backgroundColor: ds.primary["25"], border: `1px solid ${ds.primary["100"]}`, borderRadius: `${RADIUS.md}px`, color: ds.primary["700"], display: "flex", flexWrap: "wrap", fontSize: 14, gap: 1, justifyContent: "space-between", px: 2, py: 1.25 }}>
          <span>{pendingTransaction.retireAsDrep ? t("drepDashboard.pending.retire") : t("drepDashboard.pending.update")}</span>
          <Box component="a" href={`${cExplorerBaseUrl}/tx/${pending.transactionHash}`} rel="noopener noreferrer" sx={{ color: ds.primary["700"], fontWeight: 600 }} target="_blank">
            {t("drepDashboard.pending.viewTx")}
          </Box>
        </Box>
      )}

      {isNew && !isWelcomeDismissed && (
        <Box data-testid="drep-dashboard-welcome" role="status" sx={{ alignItems: "center", backgroundColor: ds.success["50"], border: `1px solid ${ds.success["200"]}`, borderLeft: `3px solid ${ds.success["600"]}`, borderRadius: `${RADIUS.md}px`, color: ds.success["700"], display: "flex", fontSize: 14, fontWeight: 500, gap: 1.25, px: 2, py: 1.25 }}>
          <CircleCheck aria-hidden size={18} style={{ flexShrink: 0 }} />
          <Box sx={{ flex: 1 }}>{t("drepDashboard.welcomeBanner")}</Box>
          <ButtonBase
            data-testid="drep-dashboard-welcome-dismiss"
            onClick={() => {
              setWelcomeDismissed(true);
              try {
                window.localStorage.setItem(welcomeKey(dRepID), "1");
              } catch {
                // Storage can be off; the banner then returns on the next visit.
              }
            }}
            sx={{ borderRadius: "4px", color: ds.success["700"], fontSize: 14, fontWeight: 600, textDecoration: "underline", "&:focus-visible": { outline: `2px solid ${ds.success["700"]}` } }}
          >
            {t("drepDashboard.dismiss")}
          </ButtonBase>
        </Box>
      )}

      <PageTabs
        label={t("drepDashboard.tabs.label")}
        tabs={tabs.map((value) => ({
          dataTestId: `drep-dashboard-tab-${value}`,
          isActive: value === tab,
          label: t(`drepDashboard.tabs.${value}`),
          to: value === "overview" ? PATHS.dashboard : `${PATHS.dashboard}?tab=${value}`,
        }))}
      />

      <Box data-testid={`drep-dashboard-panel-${tab}`}>
        {tab === "overview" && <OverviewTab actionCards={actionCards} data={data} walletCard={walletCard} />}
        {tab === "governance" && <GovernanceTab data={data} />}
        {tab === "discussions" && <DiscussionsTab data={data} walletCard={walletCard} />}
        {tab === "activity" && <ActivityReportTab data={data} />}
        {tab === "history" && <HistoryLogTab data={data} />}
      </Box>
    </Box>
  );
};
