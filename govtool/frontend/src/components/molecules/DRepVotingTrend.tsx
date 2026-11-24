import { useMemo, useState } from "react";
import { Box, ButtonBase, Tooltip } from "@mui/material";
import { format, startOfDay, subDays } from "date-fns";

import { useTranslation } from "@hooks";
import type { VotedProposal } from "@models";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

const RANGES = [7, 30, 90] as const;
type Range = (typeof RANGES)[number];

const SERIES = [
  { key: "yes", color: ds.success["500"] },
  { key: "no", color: ds.error["500"] },
  { key: "abstain", color: ds.warning["500"] },
] as const;

type Day = { date: Date; yes: number; no: number; abstain: number };

/** One bucket per day for the last `range` days, oldest first. */
export const groupVotesByDay = (
  votes: VotedProposal[],
  range: number,
  today = new Date(),
): Day[] => {
  const end = startOfDay(today);
  const days: Day[] = Array.from({ length: range }, (_, i) => ({
    date: subDays(end, range - 1 - i),
    yes: 0,
    no: 0,
    abstain: 0,
  }));
  const byTime = new Map(days.map((day) => [day.date.getTime(), day]));
  votes.forEach(({ vote }) => {
    if (!vote?.date) return;
    const time = startOfDay(new Date(vote.date)).getTime();
    const day = byTime.get(time);
    if (day && (vote.vote === "yes" || vote.vote === "no" || vote.vote === "abstain")) {
      day[vote.vote] += 1;
    }
  });
  return days;
};

type Props = {
  votes?: VotedProposal[];
  isLoading?: boolean;
  isError?: boolean;
};

/** Daily yes / no / abstain counts of a DRep's votes over 7, 30 or 90 days. */
export const DRepVotingTrend = ({ votes = [], isLoading, isError }: Props) => {
  const { t } = useTranslation();
  const [range, setRange] = useState<Range>(30);
  const days = useMemo(() => groupVotesByDay(votes, range), [votes, range]);
  const totals = days.reduce(
    (sum, day) => ({
      yes: sum.yes + day.yes,
      no: sum.no + day.no,
      abstain: sum.abstain + day.abstain,
    }),
    { yes: 0, no: 0, abstain: 0 },
  );
  const total = totals.yes + totals.no + totals.abstain;
  const max = Math.max(1, ...days.map((day) => day.yes + day.no + day.abstain));
  const labelDays = [days[0], days[Math.floor(days.length / 2)], days[days.length - 1]];

  return (
    <Box
      component="section"
      data-testid="drep-voting-trend"
      sx={{
        backgroundColor: "#FFFFFF",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 2,
        p: { xxs: 2, md: 2.75 },
      }}
    >
      <Box sx={{ alignItems: "flex-start", display: "flex", flexWrap: "wrap", gap: 2, justifyContent: "space-between" }}>
        <Box>
          <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, lineHeight: "24px", m: 0 }}>
            {t("dRepProfile.trend.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["500"], fontSize: 13, m: 0 }}>
            {t("dRepProfile.trend.subtitle", { count: range })}
          </Box>
        </Box>
        <Box
          aria-label={t("dRepProfile.trend.range")}
          role="group"
          sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", gap: 0.25, p: 0.25 }}
        >
          {RANGES.map((option) => (
            <ButtonBase
              key={option}
              aria-pressed={range === option}
              data-testid={`trend-range-${option}`}
              onClick={() => setRange(option)}
              sx={{
                backgroundColor: range === option ? ds.primary["50"] : "transparent",
                borderRadius: `${RADIUS.sm}px`,
                color: range === option ? ds.primary["700"] : ds.gray["600"],
                fontFamily: "inherit",
                fontSize: 12,
                fontWeight: 500,
                px: 1,
                py: 0.5,
              }}
            >
              {t("dRepProfile.trend.days", { count: option })}
            </ButtonBase>
          ))}
        </Box>
      </Box>

      <Box
        sx={{
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: `${RADIUS.md}px`,
          display: "grid",
          gridTemplateColumns: "repeat(4, minmax(0, 1fr))",
          "& > *:not(:last-of-type)": { borderRight: `1px solid ${ds.gray["200"]}` },
        }}
      >
        {[
          { label: t("dRepProfile.trend.total"), value: total, color: ds.gray["900"] },
          { label: t("votes.yes"), value: totals.yes, color: ds.success["700"] },
          { label: t("votes.no"), value: totals.no, color: ds.error["700"] },
          { label: t("votes.abstain"), value: totals.abstain, color: ds.warning["700"] },
        ].map((tile) => (
          <Box key={tile.label} sx={{ minWidth: 0, px: 1.5, py: 1.25 }}>
            <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{tile.label}</Box>
            <Box sx={{ color: tile.color, fontSize: 18, fontWeight: 600 }}>{isLoading ? "—" : tile.value}</Box>
          </Box>
        ))}
      </Box>

      <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 2 }}>
        {SERIES.map((series) => (
          <Box key={series.key} sx={{ alignItems: "center", color: ds.gray["700"], display: "inline-flex", fontSize: 12, gap: 0.75 }}>
            <Box component="span" sx={{ backgroundColor: series.color, borderRadius: "50%", height: 8, width: 8 }} />
            {t(`votes.${series.key}`)}
          </Box>
        ))}
      </Box>

      {!isLoading && (isError || total === 0) ? (
        <Box data-testid="drep-voting-trend-empty" sx={{ color: ds.gray["500"], fontSize: 14, py: 4, textAlign: "center" }}>
          {isError ? t("dRepProfile.activity.error") : t("dRepProfile.trend.empty", { count: range })}
        </Box>
      ) : (
        <Box>
          <Box
            aria-hidden={isLoading}
            sx={{
              alignItems: "flex-end",
              borderBottom: `1px solid ${ds.gray["200"]}`,
              display: "flex",
              gap: range === 90 ? "1px" : "3px",
              height: 160,
            }}
          >
            {days.map((day) => {
              const dayTotal = day.yes + day.no + day.abstain;
              // Segments stack abstain on top of no on top of yes; the top one is rounded.
              const topKey = [...SERIES].reverse().find((series) => day[series.key])?.key;
              const label = `${format(day.date, "MMM d, yyyy")}: ${t("dRepProfile.trend.dayTooltip", {
                total: dayTotal,
                yes: day.yes,
                no: day.no,
                abstain: day.abstain,
              })}`;
              return (
                <Tooltip key={day.date.getTime()} arrow title={label}>
                  <Box
                    aria-hidden={!dayTotal}
                    aria-label={dayTotal ? label : undefined}
                    role={dayTotal ? "img" : undefined}
                    sx={{ alignItems: "stretch", display: "flex", flex: 1, flexDirection: "column", height: "100%", justifyContent: "flex-end", minWidth: 0 }}
                    tabIndex={dayTotal ? 0 : -1}
                  >
                    {[...SERIES].reverse().map((series) => {
                      const count = day[series.key];
                      if (!count) return null;
                      return (
                        <Box
                          key={series.key}
                          sx={{
                            backgroundColor: series.color,
                            borderRadius: series.key === topKey ? "4px 4px 0 0" : 0,
                            height: `calc(${(count / max) * 100}% - 2px)`,
                            mt: "2px",
                          }}
                        />
                      );
                    })}
                  </Box>
                </Tooltip>
              );
            })}
          </Box>
          <Box sx={{ color: ds.gray["500"], display: "flex", fontSize: 11, justifyContent: "space-between", mt: 0.75 }}>
            {labelDays.map((day) => (
              <span key={day.date.getTime()}>{format(day.date, "MMM d")}</span>
            ))}
          </Box>
        </Box>
      )}
    </Box>
  );
};
