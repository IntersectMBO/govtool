import { ReactNode, useMemo, useState } from "react";
import { Box, Skeleton } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { BarChart3, Flame, Users } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PATHS } from "@/consts/paths";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";
import { useDRepVotingPowerHistory } from "@/hooks/queries/useDRepDashboard";
import {
  averageResponseDays,
  categoryPattern,
  epochActivity,
  participation,
  voteCalendar,
  votingStreak,
  type CalendarDay,
} from "@/utils/drepDashboard";

import { ErrorState } from "./GovernanceTab";
import { sortedVotes } from "./OverviewTab";
import {
  ArrowLink,
  EmptyState,
  formatAda,
  formatShortDate,
  Panel,
  TableFrame,
  td,
  th,
  VOTE_PILL_TONES,
} from "./parts";
import type { DashboardData } from "./types";

const Tile = ({
  label,
  value,
  detail,
  lead,
  testId,
}: {
  label: string;
  value: string;
  detail?: ReactNode;
  lead?: ReactNode;
  testId: string;
}) => (
  <Box data-testid={testId} sx={{ alignItems: "center", backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 2, minWidth: 0, p: { xxs: 2, md: 2.5 } }}>
    {lead}
    <Box sx={{ minWidth: 0 }}>
      <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{label}</Box>
      <Box sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, letterSpacing: "-0.4px", mt: 0.5 }}>{value}</Box>
      {detail && <Box sx={{ color: ds.gray["600"], fontSize: 12, mt: 0.5 }}>{detail}</Box>}
    </Box>
  </Box>
);

const Donut = ({ rate }: { rate: number }) => {
  const radius = 20;
  const length = 2 * Math.PI * radius;
  return (
    <Box aria-hidden component="svg" height={52} sx={{ flexShrink: 0 }} viewBox="0 0 52 52" width={52}>
      <circle cx={26} cy={26} fill="none" r={radius} stroke={ds.orange["100"]} strokeWidth={6} />
      <circle
        cx={26}
        cy={26}
        fill="none"
        r={radius}
        stroke={ds.orange["500"]}
        strokeDasharray={`${(rate / 100) * length} ${length}`}
        strokeLinecap="round"
        strokeWidth={6}
        transform="rotate(-90 26 26)"
      />
    </Box>
  );
};

const Stats = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const votes = data.votes ?? [];
  const current = data.currentEpoch;
  const rate = current !== undefined
    ? participation(votes, data.actions ?? [], data.registeredEpoch, current)
    : undefined;
  const response = averageResponseDays(votes);
  const streak = current !== undefined ? votingStreak(votes, current) : 0;
  return (
    <Box sx={{ display: "grid", gap: 2, gridTemplateColumns: { xxs: "minmax(0, 1fr)", md: "repeat(3, minmax(0, 1fr))" } }}>
      <Tile
        detail={rate && rate.votable > 0 ? t("drepDashboard.activity.participationDetail", { voted: rate.voted, votable: rate.votable }) : undefined}
        label={t("drepDashboard.activity.participation")}
        lead={<Donut rate={rate?.rate ?? 0} />}
        testId="drep-activity-participation"
        value={`${rate?.rate ?? 0}%`}
      />
      <Tile
        detail={response !== null ? t("drepDashboard.activity.responseDetail") : undefined}
        label={t("drepDashboard.activity.responseTime")}
        testId="drep-activity-response"
        value={response === null ? t("drepDashboard.activity.noValue") : t("drepDashboard.activity.responseValue", { days: response })}
      />
      <Tile
        detail={streak > 0 ? (
          <Box component="span" sx={{ alignItems: "center", color: ds.success["700"], display: "inline-flex", fontWeight: 500, gap: 0.5 }}>
            <Flame aria-hidden color={ds.orange["600"]} size={14} />
            {t("drepDashboard.activity.streakDetail")}
          </Box>
        ) : undefined}
        label={t("drepDashboard.activity.streak")}
        testId="drep-activity-streak"
        value={streak > 0 ? t("drepDashboard.activity.streakValue", { count: streak }) : "0"}
      />
    </Box>
  );
};

const LEVEL_COLORS: Record<CalendarDay["level"], string> = {
  0: ds.gray["100"],
  1: ds.primary["200"],
  2: ds.primary["400"],
  3: ds.primary["600"],
  4: ds.primary["800"],
};

const WEEKDAYS = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"] as const;

const Heatmap = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const weeks = useMemo(() => voteCalendar(data.votes ?? [], new Date()), [data.votes]);
  const total = weeks.flat().reduce((sum, day) => sum + day.count, 0);
  const months = weeks.map((week, index) => {
    const month = new Date(`${week[0].date}T00:00:00Z`).getUTCMonth();
    const previous = index ? new Date(`${weeks[index - 1][0].date}T00:00:00Z`).getUTCMonth() : -1;
    return month !== previous
      ? new Date(`${week[0].date}T00:00:00Z`).toLocaleDateString("en-US", { month: "short", timeZone: "UTC" })
      : "";
  });
  return (
    <Panel
      testId="drep-activity-heatmap"
      title={data.currentEpoch !== undefined ? t("drepDashboard.activity.heatmap.title", { epoch: data.currentEpoch }) : t("drepDashboard.activity.heatmap.titleNoEpoch")}
      titleId="drep-heatmap-title"
    >
      <Box sx={{ color: ds.gray["600"], fontSize: 13, mt: -1 }}>{t("drepDashboard.activity.heatmap.summary", { count: total })}</Box>
      <Box sx={{ overflowX: "auto" }}>
        <Box sx={{ display: "grid", gap: "4px", gridTemplateColumns: "repeat(53, minmax(12px, 1fr)) 32px", minWidth: 760 }}>
          {WEEKDAYS.map((weekday, row) => (
            <Box key={weekday} sx={{ display: "contents" }}>
              {weeks.map((week) => {
                const day = week[row];
                return (
                  <Box
                    key={day.date}
                    aria-hidden
                    sx={{ aspectRatio: "1", backgroundColor: day.future ? "transparent" : LEVEL_COLORS[day.level], border: day.future ? `1px solid ${ds.gray["100"]}` : "none", borderRadius: "3px" }}
                    title={day.future ? undefined : t("drepDashboard.activity.heatmap.day", { count: day.count, date: formatShortDate(day.date) })}
                  />
                );
              })}
              <Box aria-hidden sx={{ alignSelf: "center", color: ds.gray["500"], fontSize: 11, pl: 0.5 }}>{t(`drepDashboard.activity.heatmap.weekdays.${weekday}`)}</Box>
            </Box>
          ))}
          {months.map((label, index) => (
            // eslint-disable-next-line react/no-array-index-key
            <Box key={`m-${index}`} aria-hidden sx={{ color: ds.gray["500"], fontSize: 11, overflow: "visible", whiteSpace: "nowrap" }}>{label}</Box>
          ))}
          <Box aria-hidden />

        </Box>
      </Box>
      <Box aria-hidden sx={{ alignItems: "center", alignSelf: "flex-end", color: ds.gray["500"], display: "flex", fontSize: 11, gap: 0.5 }}>
        {t("drepDashboard.activity.heatmap.less")}
        {([0, 1, 2, 3, 4] as const).map((level) => <Box key={level} sx={{ backgroundColor: LEVEL_COLORS[level], borderRadius: "3px", height: 12, width: 12 }} />)}
        {t("drepDashboard.activity.heatmap.more")}
      </Box>
    </Panel>
  );
};

const Pattern = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const rows = data.currentEpoch !== undefined
    ? categoryPattern(data.votes ?? [], data.actions ?? [], data.registeredEpoch, data.currentEpoch)
    : [];
  return (
    <Panel testId="drep-activity-pattern" title={t("drepDashboard.activity.pattern.title")} titleId="drep-pattern-title">
      <Box component="ul" sx={{ display: "flex", flexDirection: "column", gap: 2, listStyle: "none", m: 0, p: 0 }}>
        {rows.map(({ key, voted, votable }) => {
          const pct = votable ? Math.round((voted / votable) * 100) : 0;
          return (
            <Box key={key} component="li" data-testid={`drep-pattern-${key}`}>
              <Box sx={{ display: "flex", fontSize: 14, justifyContent: "space-between", mb: 0.75 }}>
                <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 500 }}>{t(`drepDashboard.activity.pattern.categories.${key}`)}</Box>
                <Box component="span" sx={{ color: ds.gray["600"] }}>{t("drepDashboard.activity.pattern.value", { pct, voted, votable })}</Box>
              </Box>
              <Box aria-hidden sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.pill}px`, height: 6, overflow: "hidden" }}>
                <Box sx={{ backgroundColor: ds.primary["600"], height: "100%", width: `${pct}%` }} />
              </Box>
            </Box>
          );
        })}
      </Box>
    </Panel>
  );
};

/** Delegated ADA per epoch; a single series, delegators in each point's label. */
const Growth = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const {
    isServed, data: points, isLoading, isError, refetch,
  } = useDRepVotingPowerHistory(data.dRepID);
  const [hover, setHover] = useState<number | null>(null);
  if (!isServed) return null;
  let body: ReactNode;
  const series = points ?? [];
  if (isLoading) body = <Skeleton height={220} variant="rounded" />;
  else if (isError) body = <ErrorState message={t("drepDashboard.activity.growth.error")} onRetry={() => { refetch(); }} />;
  else if (!series.length || series.every((point) => !Number(point.votingPower))) {
    body = <EmptyState body={t("drepDashboard.activity.growth.emptyBody")} icon={<Users size={20} />} title={t("drepDashboard.activity.growth.emptyTitle")} />;
  } else {
    const width = 1000;
    const height = 200;
    const values = series.map((point) => Number(point.votingPower));
    const max = Math.max(...values) || 1;
    const x = (index: number) =>
      (series.length === 1 ? width / 2 : (index / (series.length - 1)) * width);
    const y = (value: number) => height - 12 - (value / max) * (height - 24);
    const last = series[series.length - 1];
    const shown = hover === null ? undefined : series[hover];
    body = (
      <>
        <Box sx={{ color: ds.gray["700"], fontSize: 14 }}>
          {last.delegators === null
            ? t("drepDashboard.activity.growth.summaryNoDelegators", { ada: formatAda(last.votingPower, 0) })
            : t("drepDashboard.activity.growth.summary", { ada: formatAda(last.votingPower, 0), delegators: last.delegators.toLocaleString("en-US") })}
        </Box>
        <Box sx={{ position: "relative" }}>
          <Box aria-label={t("drepDashboard.activity.growth.ada")} component="svg" preserveAspectRatio="none" role="img" sx={{ display: "block", height: 200, width: "100%" }} viewBox={`0 0 ${width} ${height}`}>
            {[0.25, 0.5, 0.75].map((line) => <line key={line} stroke={ds.gray["100"]} strokeWidth={1} x1={0} x2={width} y1={height * line} y2={height * line} />)}
            <polyline fill="none" points={values.map((value, index) => `${x(index)},${y(value)}`).join(" ")} stroke={ds.primary["600"]} strokeLinejoin="round" strokeWidth={2} vectorEffect="non-scaling-stroke" />
            {series.map((point, index) => (
              <rect
                key={point.epoch}
                fill="transparent"
                height={height}
                onMouseEnter={() => setHover(index)}
                onMouseLeave={() => setHover(null)}
                width={width / series.length}
                x={x(index) - width / series.length / 2}
                y={0}
              />
            ))}
          </Box>
          {shown && (
            <Box role="status" sx={{ backgroundColor: ds.gray["900"], borderRadius: `${RADIUS.sm}px`, color: "#FFFFFF", fontSize: 12, left: `${(x(hover ?? 0) / width) * 100}%`, position: "absolute", px: 1, py: 0.5, top: 0, transform: "translateX(-50%)", whiteSpace: "nowrap" }}>
              {t("drepDashboard.activity.growth.point", { epoch: shown.epoch, ada: formatAda(shown.votingPower, 0) })}
              {shown.delegators !== null ? ` · ${shown.delegators}` : ""}
            </Box>
          )}
          <Box aria-hidden sx={{ color: ds.gray["500"], display: "flex", fontSize: 11, justifyContent: "space-between", mt: 1 }}>
            <span>{series[0].epoch}</span>
            <span>{last.epoch}</span>
          </Box>
        </Box>
      </>
    );
  }
  return (
    <Panel testId="drep-activity-growth" title={t("drepDashboard.activity.growth.title")} titleId="drep-growth-title">
      {body}
    </Panel>
  );
};

const RecentlyVoted = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const rows = sortedVotes(data).filter((row) => row.vote).slice(0, 5);
  return (
    <Panel
      action={rows.length ? <ArrowLink to={`${PATHS.dashboard}?tab=governance`}>{t("drepDashboard.activity.recent.viewAll")}</ArrowLink> : undefined}
      testId="drep-activity-recent"
      title={(
        <Box component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 1 }}>
          {t("drepDashboard.activity.recent.title")}
          <Box component="span" sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.pill}px`, color: ds.gray["700"], fontSize: 12, fontWeight: 500, px: 1 }}>{t("drepDashboard.activity.recent.badge")}</Box>
        </Box>
      )}
      titleId="drep-recent-title"
    >
      {!rows.length ? (
        <EmptyState body={t("drepDashboard.activity.recent.emptyBody")} framed={false} icon={<BarChart3 size={20} />} title={t("drepDashboard.activity.recent.emptyTitle")} />
      ) : (
        <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
          {rows.map(({ proposal, vote }) => (
            <Box key={`${proposal.txHash}#${proposal.index}`} component="li" sx={{ borderTop: `1px solid ${ds.gray["200"]}`, py: 1.75, "&:first-of-type": { borderTop: "none", pt: 0 } }}>
              <Box sx={{ alignItems: "flex-start", display: "flex", gap: 1, justifyContent: "space-between" }}>
                <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{proposal.title || proposal.type}</Box>
                {vote && (
                  <Box
                    component="span"
                    sx={{
                      backgroundColor: VOTE_PILL_TONES[vote.vote].bg,
                      borderRadius: `${RADIUS.pill}px`,
                      color: VOTE_PILL_TONES[vote.vote].text,
                      flexShrink: 0,
                      fontSize: 12,
                      fontWeight: 600,
                      px: 1,
                    }}
                  >
                    {t(`drepDashboard.vote.${vote.vote}`)}
                  </Box>
                )}
              </Box>
              {proposal.abstract && (
                <Box sx={{ color: ds.gray["600"], fontSize: 13, mt: 0.5, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{proposal.abstract}</Box>
              )}
              <Box sx={{ color: ds.gray["500"], fontSize: 12, mt: 0.5 }}>{formatShortDate(vote?.date)}</Box>
            </Box>
          ))}
        </Box>
      )}
    </Panel>
  );
};

const EpochTable = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const rows = data.currentEpoch !== undefined
    ? epochActivity(
      data.votes ?? [],
      data.actions ?? [],
      data.currentEpoch,
      data.registeredEpoch,
      8,
    )
    : [];
  const hasActivity = rows.some((row) => row.votes > 0);
  return (
    <Panel testId="drep-activity-epochs" title={t("drepDashboard.activity.epochs.title")} titleId="drep-epochs-title">
      {!hasActivity ? (
        <EmptyState
          actions={<Button component={RouterLink} size="medium" to={PATHS.governanceActions} variant="outlined">{t("drepDashboard.activity.epochs.viewActions")}</Button>}
          body={t("drepDashboard.activity.epochs.emptyBody")}
          framed={false}
          icon={<BarChart3 size={20} />}
          title={t("drepDashboard.activity.epochs.emptyTitle")}
        />
      ) : (
        <TableFrame label={t("drepDashboard.activity.epochs.title")} minWidth={480}>
          <thead>
            <tr>
              {["epoch", "available", "votes", "participation", "status"].map((key) => (
                <Box key={key} component="th" scope="col" sx={th}>{t(`drepDashboard.activity.epochs.${key}`)}</Box>
              ))}
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row.epoch} data-testid="drep-epoch-row">
                <Box component="td" sx={{ ...td, fontWeight: 500, whiteSpace: "nowrap" }}>{row.isCurrent ? t("drepDashboard.activity.epochs.current", { epoch: row.epoch }) : row.epoch}</Box>
                <Box component="td" sx={{ ...td, color: ds.gray["600"], whiteSpace: "nowrap" }}>{t("drepDashboard.activity.epochs.proposals", { count: row.available })}</Box>
                <Box component="td" sx={td}>{row.votes}</Box>
                <Box component="td" sx={td}>{row.participation === null ? "–" : `${row.participation}%`}</Box>
                <Box component="td" sx={td}>
                  <Box component="span" sx={{ backgroundColor: row.isCurrent ? PILL_TONES.warning.bg : PILL_TONES.success.bg, borderRadius: `${RADIUS.pill}px`, color: row.isCurrent ? PILL_TONES.warning.text : PILL_TONES.success.text, fontSize: 12, fontWeight: 600, px: 1, py: 0.25, whiteSpace: "nowrap" }}>
                    {row.isCurrent ? t("drepDashboard.activity.epochs.inProgress") : t("drepDashboard.activity.epochs.completed")}
                  </Box>
                </Box>
              </tr>
            ))}
          </tbody>
        </TableFrame>
      )}
    </Panel>
  );
};

export const ActivityReportTab = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  // Participation needs the registration date, from the DRep record.
  if (data.votesLoading || data.actionsLoading || data.dRepLoading) {
    return (
      <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
        <Skeleton height={110} variant="rounded" />
        <Skeleton height={260} variant="rounded" />
      </Box>
    );
  }
  if (data.votesError || data.actionsError) {
    return (
      <ErrorState
        message={t("drepDashboard.activity.error")}
        onRetry={() => {
          if (data.votesError) data.retryVotes();
          if (data.actionsError) data.retryActions();
        }}
      />
    );
  }
  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Stats data={data} />
      <Heatmap data={data} />
      <Pattern data={data} />
      <Growth data={data} />
      <Box sx={{ alignItems: "start", display: "grid", gap: 2.5, gridTemplateColumns: { xxs: "minmax(0, 1fr)", lg: "repeat(2, minmax(0, 1fr))" } }}>
        <RecentlyVoted data={data} />
        <EpochTable data={data} />
      </Box>
    </Box>
  );
};
