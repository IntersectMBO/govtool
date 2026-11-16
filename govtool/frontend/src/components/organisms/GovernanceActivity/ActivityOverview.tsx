import { ReactNode } from "react";
import { Box } from "@mui/material";
import { formatDistanceToNowStrict } from "date-fns";
import { BadgeCheck, BookOpen, CircleCheck, Clock3, Landmark, RefreshCw } from "lucide-react";

import { GOV_ACTION_HISTORY_PATHS, PATHS } from "@consts";
import {
  useGetDecidedActionsQuery,
  useGetLiveProposalsQuery,
  useTranslation,
} from "@hooks";
import { LINKS } from "@/consts/links";
import { DECIDED_ACTIONS_LIMIT } from "@/hooks/queries/useGetDecidedActionsQuery";
import { ds, RADIUS } from "@/consts/designTokens";
import { activityCounts, formatUtc } from "@/utils/governanceActivity";

import { ActivityBand, IconTile, SectionHeading, TextLink } from "./activityParts";

const OverviewCard = ({
  id,
  icon,
  value,
  valueLabel,
  title,
  detail,
  link,
}: {
  id: string;
  icon: ReactNode;
  value: string;
  /** Said instead of the figure when it is a dash. */
  valueLabel?: string;
  title: string;
  detail: string;
  link: ReactNode;
}) => (
  <Box
    component="li"
    data-testid={`activity-stat-${id}`}
    sx={{
      backgroundColor: ds.gray["25"],
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: "14px",
      boxSizing: "border-box",
      display: "flex",
      flexDirection: "column",
      gap: 2.5,
      justifyContent: "space-between",
      minHeight: { xxs: 0, sm: 176 },
      minWidth: 0,
      p: 2.5,
    }}
  >
    <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between" }}>
      {icon}
      <Box
        aria-label={valueLabel}
        component="span"
        data-testid={`activity-stat-${id}-value`}
        role={valueLabel ? "img" : undefined}
        title={valueLabel}
        sx={{ color: ds.gray["900"], fontSize: 28, fontWeight: 600, lineHeight: "31px" }}
      >
        {value}
      </Box>
    </Box>
    <Box sx={{ display: "flex", flexDirection: "column", gap: 0.375 }}>
      <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, letterSpacing: "-0.09px", lineHeight: "20px", m: 0 }}>
        {title}
      </Box>
      <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, letterSpacing: "-0.04px", lineHeight: "19.5px", m: 0 }}>
        {detail}
      </Box>
    </Box>
    {link}
  </Box>
);

/** The page title, its freshness and the four headline figures. */
export const ActivityOverview = () => {
  const { t } = useTranslation();
  const { liveProposals, liveProposalsTotal, areLiveProposalsLoading, liveProposalsUpdatedAt } =
    useGetLiveProposalsQuery();
  const { decidedActions, areDecidedActionsLoading } = useGetDecidedActionsQuery();
  const counts = activityCounts(
    liveProposals,
    liveProposalsTotal,
    decidedActions,
    new Date(),
    DECIDED_ACTIONS_LIMIT,
  );
  const cappedLabel = (value: number | undefined) =>
    (!areDecidedActionsLoading && value === undefined
      ? t("activity.overview.capped")
      : undefined);
  const figure = (value: number | undefined, isLoading: boolean) =>
    (isLoading || value === undefined ? "—" : String(value));

  const updatedAgo =
    liveProposalsUpdatedAt && Date.now() - liveProposalsUpdatedAt < 60_000
      ? t("activity.justNow")
      : liveProposalsUpdatedAt &&
        formatDistanceToNowStrict(liveProposalsUpdatedAt, { addSuffix: true });

  return (
    <ActivityBand sx={{ display: "flex", flexDirection: "column", gap: { xxs: 4, md: 5 }, pb: { xxs: 5, md: 6 }, pt: { xxs: 4, md: 6.75 } }}>
      <Box
        sx={{
          alignItems: { md: "flex-end" },
          display: "flex",
          flexDirection: { xxs: "column", md: "row" },
          gap: 3,
          justifyContent: "space-between",
        }}
      >
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2, maxWidth: 840, minWidth: 0 }}>
          <Box
            component="h1"
            sx={{ color: ds.gray["900"], fontSize: { xxs: 32, md: 40 }, fontWeight: 600, letterSpacing: "-0.89px", lineHeight: { xxs: "40px", md: "50px" }, m: 0 }}
          >
            {t("activity.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["500"], fontSize: { xxs: 16, md: 18 }, lineHeight: { xxs: "24px", md: "27px" }, m: 0, maxWidth: 760 }}>
            {t("activity.description")}
          </Box>
        </Box>
        <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: "column", flexShrink: 0, gap: 1.75 }}>
          <Box
            component="a"
            data-testid="how-governance-works-link"
            href={LINKS.VIEW_GOVERNANCE_ACTIONS}
            rel="noopener noreferrer"
            target="_blank"
            sx={{
              alignItems: "center",
              alignSelf: "flex-start",
              backgroundColor: "#FFFFFF",
              border: `1.5px solid ${ds.gray["200"]}`,
              borderRadius: `${RADIUS.pill}px`,
              color: ds.gray["900"],
              display: "inline-flex",
              fontSize: 14,
              fontWeight: 600,
              gap: 0.75,
              letterSpacing: "-0.09px",
              lineHeight: "20px",
              px: 2,
              py: 1.25,
              textDecoration: "none",
              "&:hover": { borderColor: ds.gray["300"] },
              "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
            }}
          >
            <BookOpen aria-hidden color={ds.primary["600"]} size={18} strokeWidth={1.5} />
            {t("activity.howGovernanceWorks")}
          </Box>
          {!!liveProposalsUpdatedAt && (
            <Box
              component="p"
              data-testid="activity-updated"
              sx={{ alignItems: "center", color: ds.gray["500"], display: "flex", fontSize: 12, gap: 0.875, letterSpacing: "0.01em", lineHeight: "18px", m: 0 }}
            >
              <RefreshCw aria-hidden size={14} strokeWidth={1.25} />
              {t("activity.updated", {
                ago: updatedAgo,
                at: formatUtc(new Date(liveProposalsUpdatedAt).toISOString(), "dateTime"),
              })}
            </Box>
          )}
        </Box>
      </Box>

      <Box aria-labelledby="activity-overview-title" component="section" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <SectionHeading
          description={t("activity.overview.description")}
          id="activity-overview-title"
          link={
            <TextLink dataTestId="activity-view-all-link" to={PATHS.governanceActions}>
              {t("activity.overview.viewAll")}
            </TextLink>
          }
          size="md"
          title={t("activity.overview.title")}
        />
        <Box
          component="ul"
          sx={{
            display: "grid",
            gap: 2,
            gridTemplateColumns: { xxs: "1fr", sm: "repeat(2, minmax(0, 1fr))", lg: "repeat(4, minmax(0, 1fr))" },
            listStyle: "none",
            m: 0,
            p: 0,
          }}
        >
          <OverviewCard
            detail={t("activity.overview.openDetail")}
            icon={<IconTile background={ds.primary["50"]} color={ds.primary["600"]}><Landmark size={18} strokeWidth={1.5} /></IconTile>}
            id="open"
            link={<TextLink to={PATHS.governanceActions}>{t("activity.overview.openLink")}</TextLink>}
            title={t("activity.overview.open")}
            value={figure(counts.open, areLiveProposalsLoading)}
          />
          <OverviewCard
            detail={t("activity.overview.deadlinesDetail")}
            icon={<IconTile background={ds.warning["100"]} color={ds.warning["600"]}><Clock3 size={18} strokeWidth={1.5} /></IconTile>}
            id="deadlines"
            link={<TextLink anchor="active-actions">{t("activity.overview.deadlinesLink")}</TextLink>}
            title={t("activity.overview.deadlines")}
            value={figure(counts.deadlines, areLiveProposalsLoading)}
            valueLabel={
              !areLiveProposalsLoading && counts.deadlines === undefined
                ? t("activity.overview.unknown")
                : undefined
            }
          />
          <OverviewCard
            detail={t("activity.overview.ratifiedDetail")}
            icon={<IconTile background={ds.success["100"]} color={ds.success["600"]}><BadgeCheck size={18} strokeWidth={1.5} /></IconTile>}
            id="ratified"
            link={
              <TextLink to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}?status=ratified`}>
                {t("activity.overview.ratifiedLink")}
              </TextLink>
            }
            title={t("activity.overview.ratified")}
            value={figure(counts.ratified, areDecidedActionsLoading)}
            valueLabel={cappedLabel(counts.ratified)}
          />
          <OverviewCard
            detail={t("activity.overview.enactedDetail")}
            icon={<IconTile background={ds.gray["100"]} color={ds.gray["500"]}><CircleCheck size={18} strokeWidth={1.5} /></IconTile>}
            id="enacted"
            link={
              <TextLink to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}?status=enacted`}>
                {t("activity.overview.enactedLink")}
              </TextLink>
            }
            title={t("activity.overview.enacted")}
            value={figure(counts.enacted, areDecidedActionsLoading)}
            valueLabel={cappedLabel(counts.enacted)}
          />
        </Box>
      </Box>
    </ActivityBand>
  );
};
