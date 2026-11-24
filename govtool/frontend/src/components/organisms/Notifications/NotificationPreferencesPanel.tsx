import { ReactNode, useEffect, useState } from "react";
import { Box, Switch } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import {
  ArrowLeft,
  Bell,
  Check,
  CircleCheck,
  Landmark,
  Mail,
  Newspaper,
  Send,
  Sparkles,
  UserRoundX,
  type LucideIcon,
} from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PageContainer } from "@/components/atoms/PageContainer";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS } from "@/consts/designTokens";
import { useNotifications } from "@/hooks/useNotifications";
import type { NotificationEvent, NotificationPreferences } from "@/models";
import { DEFAULT_PREFERENCES, NOTIFICATION_EVENTS, ON_FREQUENCY } from "@/utils/notifications";

import { IconTile, SignInPanel } from "./notificationParts";
import { safeHref, type Tone } from "./notificationContent";

const EVENT_ICONS: Record<NotificationEvent, { icon: LucideIcon; tone: Tone }> = {
  drepChange: { icon: UserRoundX, tone: "warning" },
  delegationChange: { icon: Send, tone: "primary" },
  actionActivity: { icon: Landmark, tone: "primary" },
  relevantAction: { icon: Sparkles, tone: "primary" },
  digest: { icon: Newspaper, tone: "neutral" },
};

const Card = ({ title, body, children }: { title: string; body: string; children: ReactNode }) => (
  <Box
    component="section"
    sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, overflow: "hidden" }}
  >
    <Box sx={{ backgroundColor: ds.gray["50"], borderBottom: `1px solid ${ds.gray["200"]}`, px: { xxs: 2, md: 2.5 }, py: 2.25 }}>
      <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, lineHeight: "30px", m: 0 }}>
        {title}
      </Box>
      <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "19.5px", m: 0, mt: 0.5 }}>
        {body}
      </Box>
    </Box>
    <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>{children}</Box>
  </Box>
);

const Row = ({
  icon,
  tone,
  title,
  body,
  checked,
  disabled,
  onChange,
  testId,
}: {
  icon: LucideIcon;
  tone: Tone;
  title: string;
  body: ReactNode;
  checked: boolean;
  disabled?: boolean;
  onChange: (next: boolean) => void;
  testId: string;
}) => {
  const { t } = useTranslation();
  const labelId = `${testId}-label`;
  const descriptionId = `${testId}-description`;
  return (
    <Box
      component="li"
      sx={{
        alignItems: "center",
        borderTop: `1px solid ${ds.gray["200"]}`,
        display: "flex",
        gap: 2,
        px: { xxs: 2, md: 2.5 },
        py: 2,
        "&:first-of-type": { borderTop: "none" },
      }}
    >
      <IconTile icon={icon} tone={disabled ? "neutral" : tone} size={40} />
      <Box sx={{ flex: 1, minWidth: 0 }}>
        <Box component="span" id={labelId} sx={{ color: disabled ? ds.gray["500"] : ds.gray["900"], display: "block", fontSize: 14, fontWeight: 600, lineHeight: "20px" }}>
          {title}
        </Box>
        <Box component="span" id={descriptionId} sx={{ color: ds.gray["600"], display: "block", fontSize: 13, lineHeight: "19.5px", mt: 0.25 }}>
          {body}
        </Box>
      </Box>
      <Box sx={{ alignItems: "center", display: "flex", flexShrink: 0, gap: 0.5 }}>
        <Switch
          checked={checked}
          disabled={disabled}
          inputProps={
            {
              "aria-describedby": descriptionId,
              "aria-labelledby": labelId,
              "data-testid": testId,
            } as object
          }
          onChange={(event) => onChange(event.target.checked)}
          sx={{
            "& .MuiSwitch-switchBase.Mui-checked": { color: "#FFFFFF" },
            "& .MuiSwitch-switchBase.Mui-checked + .MuiSwitch-track": { backgroundColor: ds.primary["600"], opacity: 1 },
          }}
        />
        <Box aria-hidden component="span" sx={{ color: ds.gray["600"], fontSize: 13, minWidth: 22 }}>
          {checked ? t("notifications.preferences.on") : t("notifications.preferences.off")}
        </Box>
      </Box>
    </Box>
  );
};

/** Which notifications show, per channel and event. */
export const NotificationPreferencesPanel = () => {
  const { t } = useTranslation();
  const { preferences, savePreferences, capabilities, needsSignIn, signIn } = useNotifications();
  const [draft, setDraft] = useState<NotificationPreferences>(preferences);
  const [status, setStatus] = useState<"idle" | "saved" | "failed">("idle");

  // Start from the stored preferences once they arrive.
  useEffect(() => setDraft(preferences), [preferences]);

  const setEvent = (event: NotificationEvent, on: boolean) =>
    setDraft((current) => ({
      ...current,
      events: { ...current.events, [event]: on ? ON_FREQUENCY[event] : "off" },
    }));

  const { address, status: emailStatus } = draft.channels.email;
  const setupUrl = safeHref(capabilities.emailSetupUrl);
  let emailBody: ReactNode = t("notifications.preferences.emailUnavailable");
  if (capabilities.email) {
    if (address) {
      emailBody = emailStatus === "pending"
        ? t("notifications.preferences.emailPending", { address })
        : t("notifications.preferences.emailTo", { address });
    } else if (setupUrl) {
      emailBody = (
        <Box component="a" href={setupUrl} rel="noopener noreferrer" sx={{ color: ds.primary["600"], fontWeight: 600 }} target="_blank">
          {t("notifications.preferences.emailAdd")}
        </Box>
      );
    } else {
      emailBody = t("notifications.preferences.emailAddElsewhere");
    }
  }

  const shownEvents = NOTIFICATION_EVENTS.filter((event) => capabilities.events.includes(event));

  const save = async () => {
    try {
      await savePreferences(draft);
      setStatus("saved");
    } catch {
      setStatus("failed");
    }
  };

  return (
    <PageContainer sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box
        component={RouterLink}
        data-testid="notification-preferences-back"
        to={PATHS.notifications}
        sx={{ alignItems: "center", alignSelf: "flex-start", borderRadius: "4px", color: ds.primary["600"], display: "inline-flex", fontSize: 14, fontWeight: 500, gap: 0.75, textDecoration: "none", "&:hover": { textDecoration: "underline" }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
      >
        <ArrowLeft aria-hidden size={16} />
        {t("notifications.preferences.back")}
      </Box>
      <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box>
          <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 28, md: 32 }, fontWeight: 600, letterSpacing: "-0.6px", lineHeight: "40px", m: 0 }}>
            {t("notifications.preferences.title")}
          </Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1 }}>
            {t("notifications.preferences.description")}
          </Box>
        </Box>
        {!needsSignIn && (
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5 }}>
          <Button
            dataTestId="notification-preferences-reset"
            onClick={() => {
              setDraft(DEFAULT_PREFERENCES);
              setStatus("idle");
            }}
            variant="outlined"
          >
            {t("notifications.preferences.reset")}
          </Button>
          <Button dataTestId="notification-preferences-save" onClick={save} startIcon={<Check aria-hidden size={16} />}>
            {t("notifications.preferences.save")}
          </Button>
        </Box>
        )}
      </Box>

      {needsSignIn && <SignInPanel signIn={signIn} />}

      {!needsSignIn && status !== "idle" && (
        <Box
          data-testid="notification-preferences-status"
          role="status"
          sx={{
            alignItems: "center",
            backgroundColor: status === "saved" ? ds.success["50"] : ds.error["50"],
            border: `1px solid ${status === "saved" ? ds.success["200"] : ds.error["200"]}`,
            borderRadius: `${RADIUS.md}px`,
            color: status === "saved" ? ds.success["700"] : ds.error["700"],
            display: "flex",
            fontSize: 14,
            fontWeight: 500,
            gap: 1,
            px: 2,
            py: 1.5,
          }}
        >
          <CircleCheck aria-hidden size={18} />
          {status === "saved" ? t("notifications.preferences.saved") : t("notifications.preferences.saveFailed")}
        </Box>
      )}

      {!needsSignIn && (
      <>
        <Card body={t("notifications.preferences.channelsBody")} title={t("notifications.preferences.channels")}>
          <Row
            body={t("notifications.preferences.inAppBody")}
            checked={draft.channels.inApp}
            icon={Bell}
            onChange={(on) =>
            setDraft((current) => ({
              ...current,
              channels: { ...current.channels, inApp: on },
            }))}
            testId="notification-channel-in-app"
            title={t("notifications.preferences.inApp")}
            tone="primary"
          />
          <Row
            body={emailBody}
            checked={capabilities.email && draft.channels.email.enabled}
            disabled={!capabilities.email}
            icon={Mail}
            onChange={(on) =>
            setDraft((current) => ({
              ...current,
              channels: { ...current.channels, email: { ...current.channels.email, enabled: on } },
            }))
          }
            testId="notification-channel-email"
            title={t("notifications.preferences.email")}
            tone="primary"
          />
        </Card>

        <Card body={t("notifications.preferences.eventsBody")} title={t("notifications.preferences.events")}>
          {shownEvents.map((event) => (
            <Row
              key={event}
              body={t(`notifications.preferences.eventBodies.${event}`)}
              checked={draft.events[event] !== "off"}
              icon={EVENT_ICONS[event].icon}
              onChange={(on) => setEvent(event, on)}
              testId={`notification-event-${event}`}
              title={t(`notifications.preferences.eventNames.${event}`)}
              tone={EVENT_ICONS[event].tone}
            />
        ))}
        </Card>
      </>
      )}
    </PageContainer>
  );
};
