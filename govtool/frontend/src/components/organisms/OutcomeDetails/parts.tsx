import { ReactNode } from "react";
import { useTranslation } from "react-i18next";
import { Box } from "@mui/material";
import { AlarmClock, Clock3, Minus, ThumbsDown, ThumbsUp, X } from "lucide-react";

import { ds, FONT_FAMILY, RADIUS, SHADOWS } from "@/consts/designTokens";
import type { GovernanceActionVoteAggregate } from "@/models/governanceActions";
import { formatDisplayDate } from "@/utils/formatDate";
import { roleSummary, type VoterRole } from "@/utils/outcomeDetails";
import { voteAggregateResult } from "@/utils/voteAggregate";

export const ROLE_NAMES: Record<VoterRole, string> = {
  drep: "DReps",
  spo: "SPOs",
  cc: "CC",
};

/** ₳ 3.75B, or a member count for the committee. */
export const compactValue = (
  value: number,
  representation: GovernanceActionVoteAggregate["representation"],
) => {
  if (representation === "count") return value.toLocaleString("en-US");
  if (representation === "percent") return `${(value * 100).toFixed(1)}%`;
  const ada = new Intl.NumberFormat("en-US", {
    maximumFractionDigits: 2,
    notation: "compact",
  }).format(value / 1_000_000);
  return `₳ ${ada}`;
};

/** Fri Jul 31, 2026, (Epoch 646) */
export const dateWithEpoch = (
  t: (key: string, values?: Record<string, unknown>) => string,
  date: string | null | undefined,
  epoch: number | null | undefined,
  outputFormat = "EEE MMM d, yyyy",
) => {
  const text = formatDisplayDate(date, outputFormat);
  if (!text) return epoch === null || epoch === undefined ? "—" : `Epoch ${epoch}`;
  if (epoch === null || epoch === undefined) return text;
  return t("outcome.dateEpoch", { date: text, epoch });
};

export const Card = ({
  children,
  testId,
  sx,
}: {
  children: ReactNode;
  testId?: string;
  sx?: object;
}) => (
  <Box
    component="section"
    data-testid={testId}
    sx={{
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      boxShadow: SHADOWS.card,
      minWidth: 0,
      px: { xxs: 2, md: 2.75 },
      py: { xxs: 2, md: 2.5 },
      ...sx,
    }}
  >
    {children}
  </Box>
);

export const CardTitle = ({ children, as = "h2" }: { children: ReactNode; as?: "h2" | "h3" }) => (
  <Box component={as} sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, lineHeight: "24px", m: 0 }}>
    {children}
  </Box>
);

export const MonoTag = ({ children, testId }: { children: ReactNode; testId?: string }) => (
  <Box
    component="span"
    data-testid={testId}
    sx={{
      alignSelf: "flex-start",
      backgroundColor: ds.gray["100"],
      borderRadius: `${RADIUS.xs}px`,
      color: ds.gray["700"],
      fontFamily: FONT_FAMILY.mono,
      fontSize: 11,
      letterSpacing: "0.4px",
      px: 0.75,
      py: 0.25,
      textTransform: "uppercase",
    }}
  >
    {children}
  </Box>
);

export const Divider = ({ dashed = false }: { dashed?: boolean }) => (
  <Box aria-hidden sx={{ borderTop: `1px ${dashed ? "dashed" : "solid"} ${ds.gray["200"]}` }} />
);

type Verdict = "pass" | "fail" | "noVote" | "noThreshold";

/**
 * How a role ended up: passed, failed, did not vote on this type, or voted
 * on an Info action, which has no threshold. Undefined when the tally is
 * missing or does not add up.
 */
export const groupVerdict = (
  applicable: boolean,
  aggregate?: GovernanceActionVoteAggregate,
  isInfo = false,
): Verdict | undefined => {
  if (!applicable) return "noVote";
  if (isInfo) return "noThreshold";
  if (!aggregate) return undefined;
  // Not roleSummary: a tally with nothing left after abstentions still has a
  // verdict (the backend's, or the ledger's zero ratio against the threshold).
  const result = voteAggregateResult(aggregate);
  if (!result) return undefined;
  return result.passing ? "pass" : "fail";
};

type VerdictStyle = { bg: string; border: string; text: string; icon: ReactNode };

const VERDICT_STYLE: Record<Verdict, VerdictStyle> = {
  pass: { bg: ds.success["50"], border: ds.success["100"], text: ds.success["700"], icon: <ThumbsUp size={13} /> },
  fail: { bg: ds.error["50"], border: ds.error["100"], text: ds.error["700"], icon: <ThumbsDown size={13} /> },
  noVote: { bg: "#FFFFFF", border: ds.gray["200"], text: ds.gray["700"], icon: <Minus size={13} /> },
  noThreshold: { bg: "#FFFFFF", border: ds.gray["200"], text: ds.gray["700"], icon: <Minus size={13} /> },
};

const ICON_COLORS: Record<Verdict, string> = {
  pass: ds.success["600"],
  fail: ds.error["600"],
  noVote: ds.gray["700"],
  noThreshold: ds.gray["700"],
};

/**
 * "Pass DReps • ₳3.678B": a role's verdict, name and yes figure. `size`
 * "pill" for the header, "row" for the Overall Outcome list.
 */
// The ids the outcome checks read for each role's result.
const INDICATOR_IDS: Record<VoterRole, string> = {
  drep: "DReps-voting-results-indicator",
  spo: "SPOs-voting-results-indicator",
  cc: "CC-voting-results-indicator",
};

export const VerdictBadge = ({
  role,
  applicable,
  aggregate,
  isInfo = false,
  size = "pill",
}: {
  role: VoterRole;
  applicable: boolean;
  aggregate?: GovernanceActionVoteAggregate;
  isInfo?: boolean;
  size?: "pill" | "row";
}) => {
  const { t } = useTranslation();
  const verdict = groupVerdict(applicable, aggregate, isInfo);
  const style = VERDICT_STYLE[verdict ?? "noVote"];
  const label = verdict ? t(`outcome.${verdict}`) : t("outcome.pending");
  const counted = verdict && verdict !== "noVote" && aggregate && roleSummary(aggregate);
  const figure = !applicable
    ? t("outcome.notEligible")
    : counted
      ? compactValue(Number(aggregate.yes), aggregate.representation).replace("₳ ", "₳")
      : undefined;
  const isRow = size === "row";
  return (
    <Box
      data-testid={isRow ? INDICATOR_IDS[role] : `outcome-pill-${role}`}
      sx={{
        alignItems: "center",
        backgroundColor: style.bg,
        border: `1px solid ${style.border}`,
        borderRadius: isRow ? `${RADIUS.md}px` : `${RADIUS.pill}px`,
        color: style.text,
        display: "flex",
        fontSize: isRow ? 14 : 13,
        gap: 0.75,
        px: isRow ? 1.5 : 1,
        py: isRow ? 1.25 : 0.5,
        whiteSpace: "nowrap",
      }}
    >
      <Box
        aria-hidden
        data-testid={isRow ? "vote-result-icon" : undefined}
        sx={{
          alignItems: "center",
          backgroundColor: ICON_COLORS[verdict ?? "noVote"],
          borderRadius: "50%",
          color: "#FFFFFF",
          display: "flex",
          height: isRow ? 26 : 20,
          justifyContent: "center",
          width: isRow ? 26 : 20,
        }}
      >
        {style.icon}
      </Box>
      <span>{label}</span>
      <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 600 }}>{ROLE_NAMES[role]}</Box>
      {figure && <Box component="span" sx={{ color: ds.gray["600"] }}>{`• ${figure}`}</Box>}
    </Box>
  );
};

export type LifecycleKind = "ratified" | "enacted" | "expired" | "dropped" | "live";

const LIFECYCLE_STYLE: Record<LifecycleKind, {
  bg: string;
  border: string;
  text: string;
  iconBg: string;
  icon: ReactNode;
}> = {
  ratified: { bg: ds.success["50"], border: ds.success["100"], text: ds.success["700"], iconBg: ds.success["600"], icon: <ThumbsUp size={13} /> },
  enacted: { bg: ds.success["700"], border: ds.success["700"], text: "#FFFFFF", iconBg: "rgba(255, 255, 255, 0.22)", icon: <ThumbsUp size={13} /> },
  expired: { bg: ds.error["50"], border: ds.error["100"], text: ds.error["700"], iconBg: ds.error["600"], icon: <AlarmClock size={13} /> },
  dropped: { bg: ds.error["50"], border: ds.error["100"], text: ds.error["700"], iconBg: ds.error["600"], icon: <X size={13} /> },
  live: { bg: ds.primary["50"], border: ds.primary["100"], text: ds.primary["700"], iconBg: ds.primary["600"], icon: <Clock3 size={13} /> },
};

/** The action's own status (Ratified, Enacted, Expired...), with its icon. */
export const StatusPill = ({ kind, label }: { kind: LifecycleKind; label: string }) => {
  const style = LIFECYCLE_STYLE[kind];
  return (
    <Box
      component="span"
      data-testid={`outcome-status-${kind}`}
      sx={{
        alignItems: "center",
        backgroundColor: style.bg,
        border: `1px solid ${style.border}`,
        borderRadius: `${RADIUS.pill}px`,
        color: style.text,
        display: "inline-flex",
        fontSize: 13,
        fontWeight: 600,
        gap: 0.75,
        pl: 0.5,
        pr: 1.25,
        py: 0.5,
      }}
    >
      <Box
        aria-hidden
        sx={{ alignItems: "center", backgroundColor: style.iconBg, borderRadius: "50%", color: "#FFFFFF", display: "flex", height: 20, justifyContent: "center", width: 20 }}
      >
        {style.icon}
      </Box>
      {label}
    </Box>
  );
};

/** The header's tint, after how the action ended. */
export const HEADER_TINTS: Record<LifecycleKind, string> = {
  ratified: ds.success["50"],
  enacted: ds.success["50"],
  expired: ds.error["50"],
  dropped: ds.error["50"],
  live: ds.primary["25"],
};
