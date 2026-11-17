import { Component, ReactNode, useState } from "react";
import { Box } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { ArrowRight, ArrowUpRight, PenLine, type LucideIcon } from "lucide-react";

import { formatDistanceToNowStrict } from "date-fns";
import { Button } from "@/components/atoms/Button";

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

/** An empty, error or sign-in state: an icon, a title, a line and an action. */
export const StatePanel = ({
  icon,
  tone,
  title,
  body,
  action,
  testId,
}: {
  icon: ReactNode;
  tone: "neutral" | "error";
  title: string;
  body: string;
  action?: ReactNode;
  testId: string;
}) => (
  <Box
    data-testid={testId}
    role={tone === "error" ? "alert" : "status"}
    sx={{
      alignItems: "center",
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      flexDirection: "column",
      gap: 1.25,
      px: 3,
      py: { xxs: 6, md: 10 },
      textAlign: "center",
    }}
  >
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        backgroundColor: tone === "error" ? ds.error["100"] : ds.gray["100"],
        borderRadius: tone === "error" ? "50%" : `${RADIUS.lg}px`,
        color: tone === "error" ? ds.error["600"] : ds.gray["600"],
        display: "flex",
        height: 52,
        justifyContent: "center",
        width: 52,
      }}
    >
      {icon}
    </Box>
    <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, lineHeight: "27px", m: 0, mt: 1 }}>
      {title}
    </Box>
    <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, maxWidth: 340 }}>
      {body}
    </Box>
    {action && <Box sx={{ mt: 1 }}>{action}</Box>}
  </Box>
);

/** Asks the wallet to sign in to the notifications service. */
export const SignInPanel = ({ signIn }: { signIn: () => Promise<void> }) => {
  const { t } = useTranslation();
  const [failed, setFailed] = useState(false);
  return (
    <StatePanel
      action={(
        <Button
          dataTestId="notifications-sign-in"
          onClick={async () => {
            setFailed(false);
            try {
              await signIn();
            } catch {
              setFailed(true);
            }
          }}
          startIcon={<PenLine aria-hidden size={16} />}
        >
          {t("notifications.signIn")}
        </Button>
      )}
      body={failed ? t("notifications.signInFailed") : t("notifications.signInBody")}
      icon={<PenLine size={22} />}
      testId="notifications-sign-in-panel"
      title={t("notifications.signInTitle")}
      tone="neutral"
    />
  );
};

/**
 * Keeps a notifications failure (say, data the service sent that the screen
 * cannot read) to the notifications UI instead of the whole page.
 */
export class NotificationBoundary extends Component<
  { children: ReactNode; fallback?: ReactNode },
  { hasFailed: boolean }
> {
  constructor(props: { children: ReactNode; fallback?: ReactNode }) {
    super(props);
    this.state = { hasFailed: false };
  }

  static getDerivedStateFromError() {
    return { hasFailed: true };
  }

  render() {
    const { children, fallback = null } = this.props;
    const { hasFailed } = this.state;
    return hasFailed ? fallback : children;
  }
}
