import { Box } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { ArrowRight, ArrowUpRight, type LucideIcon } from "lucide-react";
import { formatDistanceToNowStrict } from "date-fns";

import { ds, RADIUS } from "@/consts/designTokens";
import type { GovToolNotification } from "@/models";
import { formatUtc } from "@/utils/governanceActivity";

import { NotificationAction, TONES, Tone } from "./notificationContent";

export const IconTile = ({
  icon: Icon,
  tone,
  size = 36,
}: {
  icon: LucideIcon;
  tone: Tone;
  size?: number;
}) => (
  <Box
    aria-hidden
    sx={{
      alignItems: "center",
      backgroundColor: TONES[tone].bg,
      borderRadius: `${RADIUS.lg}px`,
      color: TONES[tone].fg,
      display: "flex",
      flexShrink: 0,
      height: size,
      justifyContent: "center",
      width: size,
    }}
  >
    <Icon size={18} strokeWidth={1.7} />
  </Box>
);

/** A round status label with a dot, as in "● Ratified". */
export const StatusChip = ({
  label,
  tone,
  dot = true,
}: {
  label: string;
  tone: Tone;
  dot?: boolean;
}) => (
  <Box
    component="span"
    sx={{
      alignItems: "center",
      backgroundColor: TONES[tone].bg,
      borderRadius: `${RADIUS.pill}px`,
      color: TONES[tone].text,
      display: "inline-flex",
      flexShrink: 0,
      fontSize: 12,
      fontWeight: 600,
      gap: 0.625,
      lineHeight: "18px",
      px: 0.75,
      py: 0.25,
      whiteSpace: "nowrap",
    }}
  >
    {dot && <Box aria-hidden component="span" sx={{ backgroundColor: "currentColor", borderRadius: "50%", height: 6, width: 6 }} />}
    {label}
  </Box>
);

export const UnreadDot = () => (
  <Box aria-hidden sx={{ backgroundColor: ds.primary["600"], borderRadius: "50%", flexShrink: 0, height: 7, mt: 0.75, width: 7 }} />
);

const linkSx = (size: "sm" | "md") => ({
  alignItems: "center",
  borderRadius: "4px",
  color: ds.primary["600"],
  display: "inline-flex",
  fontSize: size === "sm" ? 12 : 14,
  fontWeight: 600,
  gap: 0.625,
  lineHeight: size === "sm" ? "18px" : "20px",
  textDecoration: "none",
  "&:hover": { textDecoration: "underline" },
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
});

/** A notification's link: in the app, or a new tab for an external URL. */
export const ActionLink = ({
  action,
  size = "md",
  withArrow = true,
  onFollow,
}: {
  action: NotificationAction;
  size?: "sm" | "md";
  withArrow?: boolean;
  onFollow?: () => void;
}) => {
  const Arrow = size === "sm" ? ArrowUpRight : ArrowRight;
  const content = (
    <>
      {action.label}
      {withArrow && <Arrow aria-hidden size={size === "sm" ? 13 : 16} />}
    </>
  );
  if (action.href) {
    return (
      <Box component="a" href={action.href} onClick={onFollow} rel="noopener noreferrer" sx={linkSx(size)} target="_blank">
        {content}
      </Box>
    );
  }
  return (
    <Box component={RouterLink} onClick={onFollow} sx={linkSx(size)} to={action.path ?? "/"}>
      {content}
    </Box>
  );
};

/** "12 minutes ago · 2 Oct 2026 · 15:24 UTC", or "12 min" in the popover. */
export const notificationTime = (
  item: GovToolNotification,
  format: (key: string, options?: Record<string, unknown>) => string,
  compact = false,
) => {
  const date = new Date(item.createdAt);
  if (Number.isNaN(date.getTime())) return "";
  const ago = formatDistanceToNowStrict(date, { addSuffix: !compact });
  return compact ? ago : format("notifications.time", { ago, at: formatUtc(item.createdAt, "dateTime") });
};
