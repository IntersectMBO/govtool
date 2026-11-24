import { KeyboardEvent, ReactNode, useRef } from "react";
import { Box, ButtonBase } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { ArrowRight, Copy, Download, ExternalLink } from "lucide-react";
import { formatDistanceToNowStrict } from "date-fns";

import { useTranslation } from "react-i18next";
import { useSnackbar } from "@/context/snackbar";
import { ds, PILL_TONES, RADIUS, SHADOWS, type PillTone } from "@/consts/designTokens";
import type { ActionOutcome } from "@/utils/drepDashboard";

// Small pieces the DRep dashboard tabs share.

export const LOVELACE = 1_000_000;

/** "12,847.32" from lovelace; "0" for nothing. */
export const formatAda = (lovelace: number | string | null | undefined, decimals = 2) => {
  const value = Number(lovelace ?? 0) / LOVELACE;
  if (!Number.isFinite(value)) return "0";
  return value.toLocaleString("en-US", { maximumFractionDigits: decimals });
};

/** "217.90M" style for large lovelace figures. */
export const formatAdaCompact = (lovelace: number | string | null | undefined) => {
  const value = Number(lovelace ?? 0) / LOVELACE;
  if (!Number.isFinite(value)) return "0";
  return value.toLocaleString("en-US", { maximumFractionDigits: 2, notation: "compact" });
};

export const shortId = (value: string, head = 10, tail = 6) =>
  (value.length > head + tail + 1
    ? `${value.slice(0, head)}…${tail ? value.slice(-tail) : ""}`
    : value);

/** "March 15, 2025" in UTC. */
export const formatLongDate = (time?: string | null) => {
  if (!time) return "";
  const date = new Date(time);
  if (Number.isNaN(date.getTime())) return "";
  return date.toLocaleDateString("en-US", { day: "numeric", month: "long", timeZone: "UTC", year: "numeric" });
};

/** "Sep 26, 2026" in UTC. */
export const formatShortDate = (time?: string | null) => {
  if (!time) return "";
  const date = new Date(time);
  if (Number.isNaN(date.getTime())) return "";
  return date.toLocaleDateString("en-US", { day: "2-digit", month: "short", timeZone: "UTC", year: "numeric" });
};

export const timeAgo = (time?: string | number | null) => {
  if (!time) return "";
  const date = new Date(time);
  return Number.isNaN(date.getTime()) ? "" : formatDistanceToNowStrict(date, { addSuffix: true });
};

/** Only http(s) URLs become links. */
export const safeUrl = (value?: string | null) => {
  if (!value) return undefined;
  try {
    const url = new URL(value);
    return url.protocol === "https:" || url.protocol === "http:" ? url.href : undefined;
  } catch {
    return undefined;
  }
};

export const OUTCOME_TONES: Record<ActionOutcome, PillTone> = {
  active: "primary",
  ratified: "success",
  enacted: "success",
  expired: "error",
  dropped: "neutral",
  unknown: "neutral",
};

export const VOTE_COLORS = {
  yes: ds.success["600"],
  no: ds.error["600"],
  abstain: ds.orange["500"],
};

/** Vote labels on a tinted pill, each pair at least 4.5:1. */
export const VOTE_PILL_TONES = {
  yes: PILL_TONES.success,
  no: PILL_TONES.error,
  abstain: PILL_TONES.warning,
};

export const VOTE_TEXT = {
  yes: ds.success["700"],
  no: ds.error["700"],
  abstain: ds.orange["700"],
};

/** A white card section with an optional heading row. */
export const Panel = ({
  title,
  subtitle,
  action,
  children,
  testId,
  titleId,
  padded = true,
  accent = false,
}: {
  title?: ReactNode;
  subtitle?: ReactNode;
  action?: ReactNode;
  children?: ReactNode;
  testId?: string;
  titleId?: string;
  padded?: boolean;
  accent?: boolean;
}) => (
  <Box
    aria-labelledby={title ? titleId : undefined}
    component="section"
    data-testid={testId}
    sx={{
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      borderTop: accent ? `3px solid ${ds.primary["300"]}` : undefined,
      boxShadow: SHADOWS.card,
      display: "flex",
      flexDirection: "column",
      gap: 2,
      minWidth: 0,
      p: padded ? { xxs: 2, md: 3 } : 0,
    }}
  >
    {(title || action) && (
      <Box sx={{ alignItems: "flex-start", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between", px: padded ? 0 : { xxs: 2, md: 3 }, pt: padded ? 0 : { xxs: 2, md: 3 } }}>
        <Box sx={{ minWidth: 0 }}>
          {title && (
            <Box component="h2" id={titleId} sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, lineHeight: "28px", m: 0 }}>
              {title}
            </Box>
          )}
          {subtitle && (
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 0.5 }}>
              {subtitle}
            </Box>
          )}
        </Box>
        {action}
      </Box>
    )}
    {children}
  </Box>
);

/** A round label, with a dot by default: "● Active". */
export const Pill = ({
  label,
  tone,
  dot = true,
  testId,
}: {
  label: string;
  tone: PillTone;
  dot?: boolean;
  testId?: string;
}) => (
  <Box
    component="span"
    data-testid={testId}
    sx={{
      alignItems: "center",
      backgroundColor: PILL_TONES[tone].bg,
      borderRadius: `${RADIUS.pill}px`,
      color: PILL_TONES[tone].text,
      display: "inline-flex",
      fontSize: 12,
      fontWeight: 500,
      gap: 0.625,
      lineHeight: "18px",
      px: 1,
      py: 0.125,
      whiteSpace: "nowrap",
    }}
  >
    {dot && <Box aria-hidden component="span" sx={{ backgroundColor: "currentColor", borderRadius: "50%", height: 6, width: 6 }} />}
    {label}
  </Box>
);

/** A centred empty, unavailable or error state. */
export const EmptyState = ({
  icon,
  title,
  body,
  actions,
  testId,
  tone = "neutral",
  framed = true,
}: {
  icon: ReactNode;
  title: string;
  body?: string;
  actions?: ReactNode;
  testId?: string;
  tone?: "neutral" | "error";
  framed?: boolean;
}) => (
  <Box
    data-testid={testId}
    role={tone === "error" ? "alert" : undefined}
    sx={{
      alignItems: "center",
      backgroundColor: framed ? ds.gray["25"] : undefined,
      border: framed ? `1px dashed ${ds.gray["200"]}` : undefined,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      flexDirection: "column",
      gap: 1,
      px: 3,
      py: { xxs: 5, md: 7 },
      textAlign: "center",
    }}
  >
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        backgroundColor: tone === "error" ? ds.error["100"] : ds.gray["100"],
        borderRadius: "50%",
        boxShadow: `0 0 0 8px ${tone === "error" ? ds.error["50"] : ds.gray["50"]}`,
        color: tone === "error" ? ds.error["600"] : ds.gray["600"],
        display: "flex",
        height: 48,
        justifyContent: "center",
        mb: 1.5,
        width: 48,
      }}
    >
      {icon}
    </Box>
    <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, lineHeight: "28px", m: 0 }}>
      {title}
    </Box>
    {body && (
      <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, maxWidth: 420 }}>
        {body}
      </Box>
    )}
    {actions && <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "center", mt: 1.5 }}>{actions}</Box>}
  </Box>
);

export const AVATAR_TONES = [
  { bg: ds.primary["600"], fg: "#FFFFFF" },
  { bg: "#6941C6", fg: "#FFFFFF" },
  { bg: "#026AA2", fg: "#FFFFFF" },
  { bg: ds.orange["750"], fg: "#FFFFFF" },
  { bg: ds.success["700"], fg: "#FFFFFF" },
];

export const initialsOf = (name: string) =>
  name
    .split(/\s+/)
    .filter(Boolean)
    .slice(0, 2)
    .map((part) => part[0]?.toUpperCase() ?? "")
    .join("") || "?";

/** Initials in a coloured circle; the colour follows the name. */
export const Initials = ({ name, size = 32 }: { name: string; size?: number }) => {
  const sum = [...name].reduce((total, ch) => total + ch.charCodeAt(0), 0);
  const tone = AVATAR_TONES[sum % AVATAR_TONES.length];
  return (
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        backgroundColor: tone.bg,
        borderRadius: "50%",
        color: tone.fg,
        display: "flex",
        flexShrink: 0,
        fontSize: size > 40 ? 18 : 12,
        fontWeight: 600,
        height: size,
        justifyContent: "center",
        width: size,
      }}
    >
      {initialsOf(name)}
    </Box>
  );
};

/** Copies a value and confirms with a snackbar. */
export const CopyButton = ({
  value,
  label,
  testId,
}: {
  value: string;
  label: string;
  testId?: string;
}) => {
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  return (
    <ButtonBase
      aria-label={t("drepDashboard.copy", { label })}
      data-testid={testId}
      onClick={async () => {
        await navigator.clipboard?.writeText(value);
        addSuccessAlert(t("alerts.copiedToClipboard"));
      }}
      sx={{ borderRadius: "4px", color: ds.gray["500"], flexShrink: 0, p: 0.5, "&:hover": { color: ds.gray["800"] }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
    >
      <Copy aria-hidden size={14} />
    </ButtonBase>
  );
};

const linkSx = {
  alignItems: "center",
  borderRadius: "4px",
  color: ds.primary["600"],
  display: "inline-flex",
  fontSize: 14,
  fontWeight: 600,
  gap: 0.5,
  textDecoration: "none",
  "&:hover": { textDecoration: "underline" },
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

/** A primary-coloured "Label →" link inside the app. */
export const ArrowLink = ({
  to,
  children,
  testId,
  state,
}: {
  to: string;
  children: ReactNode;
  testId?: string;
  state?: unknown;
}) => (
  <Box component={RouterLink} data-testid={testId} state={state} sx={linkSx} to={to}>
    {children}
    <ArrowRight aria-hidden size={14} />
  </Box>
);

/** A link that opens in a new tab, or plain text when the URL is unsafe. */
export const ExternalText = ({ href, children }: { href?: string | null; children: ReactNode }) => {
  const url = safeUrl(href);
  if (!url) return <Box component="span" sx={{ color: ds.gray["700"], fontSize: 14, overflowWrap: "anywhere" }}>{children}</Box>;
  return (
    <Box component="a" href={url} rel="noopener noreferrer" sx={{ ...linkSx, fontWeight: 400, maxWidth: "100%", minWidth: 0 }} target="_blank">
      <Box component="span" sx={{ overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{children}</Box>
      <ExternalLink aria-hidden size={14} style={{ flexShrink: 0 }} />
    </Box>
  );
};

/** Underlined tabs that behave as a tablist (arrow keys, Home, End). */
export const UnderlineTabs = ({
  tabs,
  value,
  onChange,
  label,
  idPrefix,
}: {
  tabs: { value: string; label: string }[];
  value: string;
  onChange: (value: string) => void;
  label: string;
  idPrefix: string;
}) => {
  const refs = useRef<Record<string, HTMLButtonElement | null>>({});
  const onKeyDown = (event: KeyboardEvent, index: number) => {
    const moves: Record<string, number> = {
      ArrowRight: index + 1,
      ArrowLeft: index - 1,
      Home: 0,
      End: tabs.length - 1,
    };
    if (!(event.key in moves)) return;
    event.preventDefault();
    const next = tabs[(moves[event.key] + tabs.length) % tabs.length];
    onChange(next.value);
    refs.current[next.value]?.focus();
  };
  return (
    <Box aria-label={label} role="tablist" sx={{ boxShadow: `inset 0 -1px 0 ${ds.gray["200"]}`, display: "flex", gap: 0.5, overflowX: "auto" }}>
      {tabs.map((tab, index) => {
        const selected = tab.value === value;
        return (
          <ButtonBase
            key={tab.value}
            ref={(node) => { refs.current[tab.value] = node; }}
            aria-controls={`${idPrefix}-panel`}
            aria-selected={selected}
            data-testid={`${idPrefix}-tab-${tab.value}`}
            id={`${idPrefix}-tab-${tab.value}`}
            onClick={() => onChange(tab.value)}
            onKeyDown={(event) => onKeyDown(event, index)}
            role="tab"
            sx={{
              borderBottom: `2px solid ${selected ? ds.primary["600"] : "transparent"}`,
              color: selected ? ds.gray["900"] : ds.gray["600"],
              flexShrink: 0,
              fontSize: 14,
              fontWeight: selected ? 600 : 500,
              px: 1.5,
              py: 1.25,
              whiteSpace: "nowrap",
              "&:hover": { color: ds.gray["900"] },
              "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: -2 },
            }}
            tabIndex={selected ? 0 : -1}
          >
            {tab.label}
          </ButtonBase>
        );
      })}
    </Box>
  );
};

/** Saves rows as a CSV file. */
export const downloadCsv = (fileName: string, csv: string) => {
  if (typeof URL.createObjectURL !== "function") return;
  const url = URL.createObjectURL(new Blob([csv], { type: "text/csv;charset=utf-8" }));
  const anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = fileName;
  anchor.click();
  // Revoking at once can cancel the download in some browsers.
  window.setTimeout(() => URL.revokeObjectURL(url), 1000);
};

export const ExportButton = ({
  onClick,
  disabled,
}: {
  onClick: () => void;
  disabled?: boolean;
}) => {
  const { t } = useTranslation();
  return (
    <ButtonBase
      data-testid="drep-dashboard-export-csv"
      disabled={disabled}
      onClick={onClick}
      sx={{
        alignItems: "center",
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.pill}px`,
        color: ds.gray["800"],
        display: "inline-flex",
        fontSize: 14,
        fontWeight: 500,
        gap: 0.75,
        height: 32,
        px: 1.5,
        "&:hover": { backgroundColor: ds.gray["50"] },
        "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` },
        "&.Mui-disabled": { color: ds.gray["400"] },
      }}
    >
      <Download aria-hidden size={14} />
      {t("drepDashboard.exportCsv")}
    </ButtonBase>
  );
};

/** Table header and cell styles shared by the dashboard tables. */
export const th = {
  backgroundColor: ds.gray["50"],
  borderBottom: `1px solid ${ds.gray["200"]}`,
  color: ds.gray["600"],
  fontSize: 12,
  fontWeight: 500,
  px: 2,
  py: 1.25,
  textAlign: "left" as const,
  whiteSpace: "nowrap" as const,
};

export const td = {
  borderBottom: `1px solid ${ds.gray["100"]}`,
  color: ds.gray["800"],
  fontSize: 14,
  px: 2,
  py: 1.5,
  verticalAlign: "middle" as const,
};

export const TableFrame = ({
  children,
  label,
  minWidth = 720,
}: {
  children: ReactNode;
  label: string;
  minWidth?: number;
}) => (
  <Box sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, overflowX: "auto" }}>
    <Box aria-label={label} component="table" sx={{ borderCollapse: "collapse", minWidth, width: "100%", "& tbody tr:last-of-type td": { borderBottom: "none" } }}>
      {children}
    </Box>
  </Box>
);
