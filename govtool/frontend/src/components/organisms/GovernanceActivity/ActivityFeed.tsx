import { ReactNode, useEffect, useRef, useState } from "react";
import { Box, ButtonBase } from "@mui/material";
import { formatDistanceToNowStrict } from "date-fns";
import { BadgeCheck, ChevronDown, CircleCheckBig, CircleX, Vote } from "lucide-react";

import {
  useGetDecidedActionsQuery,
  useGetGovernanceActionMetadata,
  useGetLiveProposalsQuery,
  useTranslation,
} from "@hooks";
import type { GovernanceActionRecord } from "@models";
import { getProposalTypeLabel } from "@utils";
import { ds } from "@/consts/designTokens";
import {
  activityEvents,
  formatUtc,
  groupByUtcDay,
  type ActivityEvent,
  type ActivityKind,
} from "@/utils/governanceActivity";

import {
  ActivityBand,
  ActivityCard,
  Badge,
  BadgeTone,
  Category,
  IconTile,
  SectionHeading,
  TextLink,
} from "./activityParts";
import { GovernanceTimeline } from "./GovernanceTimeline";

const PAGE = 10;

const KIND_STYLE: Record<
  ActivityKind,
  { tone: BadgeTone; background: string; color: string; icon: ReactNode }
> = {
  votingOpened: {
    tone: "primary",
    background: ds.primary["100"],
    color: ds.primary["700"],
    icon: <Vote size={18} strokeWidth={1.5} />,
  },
  ratified: {
    tone: "success",
    background: ds.success["100"],
    color: ds.success["600"],
    icon: <BadgeCheck size={18} strokeWidth={1.5} />,
  },
  enacted: {
    tone: "success",
    background: ds.success["100"],
    color: ds.success["600"],
    icon: <CircleCheckBig size={18} strokeWidth={1.5} />,
  },
  expired: {
    tone: "error",
    background: ds.error["50"],
    color: ds.error["600"],
    icon: <CircleX size={18} strokeWidth={1.5} />,
  },
};

const EventRow = ({
  event,
  record,
}: {
  event: ActivityEvent;
  record?: GovernanceActionRecord;
}) => {
  const { t } = useTranslation();
  const { metadata } = useGetGovernanceActionMetadata(record);
  const style = KIND_STYLE[event.kind];
  const typeLabel = getProposalTypeLabel(event.type);
  const title = event.title ?? metadata?.data?.title ?? typeLabel;

  return (
    <Box
      component="li"
      data-testid="activity-event"
      sx={{
        borderTop: `1px solid ${ds.gray["200"]}`,
        display: "flex",
        gap: 2,
        py: 2.5,
      }}
    >
      <IconTile background={style.background} color={style.color}>
        {style.icon}
      </IconTile>
      <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 1.125, minWidth: 0 }}>
        <Box
          sx={{
            alignItems: { sm: "center" },
            display: "flex",
            flexDirection: { xxs: "column", sm: "row" },
            gap: 1,
            justifyContent: "space-between",
          }}
        >
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1, minWidth: 0 }}>
            <Badge size="md" tone={style.tone}>
              {t(`activity.feed.kinds.${event.kind}`)}
            </Badge>
            <Category>{typeLabel}</Category>
          </Box>
          <Box
            component="time"
            dateTime={event.time}
            sx={{ color: ds.gray["500"], flexShrink: 0, fontSize: 12, letterSpacing: "0.01em", lineHeight: "18px" }}
          >
            {t("activity.feed.ago", {
              ago: formatDistanceToNowStrict(new Date(event.time), { addSuffix: true }),
              time: formatUtc(event.time, "time"),
            })}
          </Box>
        </Box>
        <Box
          component="h4"
          sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, letterSpacing: "-0.18px", lineHeight: "24px", m: 0, overflowWrap: "anywhere" }}
        >
          {title}
        </Box>
        <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, letterSpacing: "-0.09px", lineHeight: "20px", m: 0 }}>
          {t(`activity.feed.details.${event.kind}`)}
        </Box>
        <TextLink label={`${t("activity.feed.viewAction")}: ${title}`} to={event.path}>
          {t("activity.feed.viewAction")}
        </TextLink>
      </Box>
    </Box>
  );
};

/** "What's happening now": the stage explainer and the activity feed. */
export const ActivityFeed = () => {
  const { t } = useTranslation();
  const [shown, setShown] = useState(PAGE);
  const feedRef = useRef<HTMLDivElement>(null);
  // Set by "Load earlier activity": the row whose link takes focus next, as
  // the button may disappear once everything is shown.
  const focusRow = useRef<number | null>(null);
  useEffect(() => {
    if (focusRow.current === null) return;
    const rows = feedRef.current?.querySelectorAll<HTMLElement>("[data-testid=activity-event]");
    rows?.[focusRow.current]?.querySelector<HTMLElement>("a")?.focus();
    focusRow.current = null;
  }, [shown]);
  const { liveProposals, areLiveProposalsLoading } = useGetLiveProposalsQuery();
  const { decidedActions, areDecidedActionsLoading } = useGetDecidedActionsQuery();
  const events = activityEvents(liveProposals, decidedActions);
  const groups = groupByUtcDay(events.slice(0, shown), new Date());
  const records = new Map(
    decidedActions.map((action) => [`${action.tx_hash}#${action.index}`, action]),
  );
  const isLoading = areLiveProposalsLoading || areDecidedActionsLoading;

  return (
    <ActivityBand
      background="#FCFCFA"
      labelledBy="activity-feed-heading"
      sx={{ display: "flex", flexDirection: "column", gap: 4 }}
    >
      <SectionHeading
        description={t("activity.timeline.headingDescription")}
        eyebrow={t("activity.timeline.eyebrow")}
        id="activity-feed-heading"
        title={t("activity.timeline.heading")}
      />
      <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
        <GovernanceTimeline />
        <ActivityCard dataTestId="activity-feed" sx={{ py: 1 }}>
          <Box ref={feedRef} aria-busy={isLoading} aria-label={t("activity.feed.label")} role="region">
            {!isLoading && events.length === 0 && (
              <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, m: 0, py: 4, textAlign: "center" }}>
                {t("activity.feed.empty")}
              </Box>
            )}
            {groups.map((group) => (
              <Box key={group.day}>
                <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", pb: 1.25, pt: 2.25 }}>
                  <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, letterSpacing: "-0.09px", lineHeight: "20px", m: 0 }}>
                    {group.label
                      ? t(`activity.feed.${group.label}`)
                      : formatUtc(`${group.day}T00:00:00Z`, "longDate")}
                  </Box>
                  {group.label && (
                    <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>
                      {formatUtc(`${group.day}T00:00:00Z`, "longDate")}
                    </Box>
                  )}
                </Box>
                <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
                  {group.events.map((event) => (
                    <EventRow key={event.key} event={event} record={records.get(event.actionId)} />
                  ))}
                </Box>
              </Box>
            ))}
          </Box>
          {events.length > shown && (
            <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, display: "flex", justifyContent: "center", py: 2 }}>
              <ButtonBase
                data-testid="load-earlier-activity"
                onClick={() => {
                  focusRow.current = shown;
                  setShown(shown + PAGE);
                }}
                sx={{
                  borderRadius: "4px",
                  color: ds.primary["600"],
                  fontFamily: "inherit",
                  fontSize: 14,
                  fontWeight: 600,
                  gap: 0.75,
                  lineHeight: "20px",
                  "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
                }}
              >
                {t("activity.feed.loadMore")}
                <ChevronDown aria-hidden size={16} />
              </ButtonBase>
            </Box>
          )}
        </ActivityCard>
      </Box>
    </ActivityBand>
  );
};
