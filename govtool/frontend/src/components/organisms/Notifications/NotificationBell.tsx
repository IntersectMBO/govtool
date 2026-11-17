import { useId, useState } from "react";
import { Badge, Box, ButtonBase, IconButton, Popover } from "@mui/material";
import { useNavigate } from "react-router";
import { ArrowRight, Bell, Settings } from "lucide-react";

import { useTranslation } from "react-i18next";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useNotifications } from "@/hooks/useNotifications";
import { useScreenDimension } from "@/hooks/useScreenDimension";

import { describeNotification } from "./notificationContent";
import {
  ActionLink,
  IconTile,
  NotificationBoundary,
  StatusChip,
  UnreadDot,
  notificationTime,
} from "./notificationParts";

const POPOVER_ITEMS = 4;

const NotificationBellContent = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { isMobile } = useScreenDimension();
  const [anchor, setAnchor] = useState<HTMLElement | null>(null);
  const popoverId = useId();
  const { items, unread, markAllRead, markRead, isLoading, needsSignIn } = useNotifications({
    pageSize: POPOVER_ITEMS,
  });
  const isOpen = Boolean(anchor);
  const close = () => setAnchor(null);
  const goTo = (path: string) => {
    close();
    navigate(path);
  };

  return (
    <>
      <IconButton
        // On a phone the bell is a link to the centre, not a popup trigger.
        aria-controls={!isMobile && isOpen ? popoverId : undefined}
        aria-expanded={isMobile ? undefined : isOpen}
        aria-haspopup={isMobile ? undefined : "dialog"}
        aria-label={unread ? t("notifications.bellUnread", { count: unread }) : t("notifications.bell")}
        data-testid="notifications-bell"
        onClick={(event) =>
          (isMobile ? navigate(PATHS.notifications) : setAnchor(event.currentTarget))}
        sx={{
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          borderRadius: "50%",
          color: ds.gray["700"],
          height: 36,
          width: 36,
          "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
        }}
      >
        <Badge
          badgeContent={unread}
          max={99}
          sx={{
            "& .MuiBadge-badge": {
              backgroundColor: ds.error["600"],
              border: "2px solid #FFFFFF",
              color: "#FFFFFF",
              fontSize: 10,
              fontWeight: 700,
              height: 18,
              minWidth: 18,
              px: 0.5,
              right: -3,
              top: -5,
            },
          }}
        >
          <Bell aria-hidden size={16} strokeWidth={1.75} />
        </Badge>
      </IconButton>
      <Popover
        anchorEl={anchor}
        anchorOrigin={{ horizontal: "right", vertical: "bottom" }}
        onClose={close}
        open={isOpen}
        slotProps={{
          paper: {
            id: popoverId,
            "aria-label": t("notifications.popoverTitle"),
            role: "dialog",
            sx: {
              border: `1px solid ${ds.gray["200"]}`,
              borderRadius: `${RADIUS.lg}px`,
              boxShadow: SHADOWS.header,
              maxWidth: "calc(100vw - 32px)",
              mt: 1,
              width: 470,
            },
          } as object,
        }}
        transformOrigin={{ horizontal: "right", vertical: "top" }}
      >
        <Box data-testid="notifications-popover">
          <Box sx={{ alignItems: "center", borderBottom: `1px solid ${ds.gray["200"]}`, display: "flex", gap: 1, justifyContent: "space-between", p: 2 }}>
            <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
              <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, letterSpacing: "-0.26px", lineHeight: "27px", m: 0 }}>
                {t("notifications.popoverTitle")}
              </Box>
              {unread > 0 && <StatusChip label={t("notifications.unread", { count: unread })} tone="primary" />}
            </Box>
            <Box sx={{ alignItems: "center", display: "flex", gap: 1.75 }}>
              {unread > 0 && (
                <ButtonBase
                  data-testid="notifications-mark-all-read"
                  onClick={() => markAllRead()}
                  sx={{ borderRadius: "4px", color: ds.primary["600"], fontFamily: "inherit", fontSize: 12, fontWeight: 600, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
                >
                  {t("notifications.markAllRead")}
                </ButtonBase>
              )}
              <IconButton
                aria-label={t("notifications.settings")}
                data-testid="notifications-settings-link"
                onClick={() => goTo(PATHS.notificationPreferences)}
                size="small"
                sx={{ color: ds.gray["500"] }}
              >
                <Settings aria-hidden size={18} strokeWidth={1.7} />
              </IconButton>
            </Box>
          </Box>
          {!isLoading && (needsSignIn || items.length === 0) && (
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "19.5px", m: 0, px: 2, py: 3, textAlign: "center" }}>
              {needsSignIn ? t("notifications.signInTitle") : t("notifications.emptyTitle")}
            </Box>
          )}
          <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0 }}>
            {items.slice(0, POPOVER_ITEMS).map((item) => {
              const content = describeNotification(item, t);
              const [action] = content.actions;
              return (
                <Box
                  key={item.id}
                  component="li"
                  data-testid="notifications-popover-item"
                  sx={{
                    backgroundColor: item.read ? "#FFFFFF" : ds.gray["50"],
                    borderBottom: `1px solid ${ds.gray["200"]}`,
                    display: "flex",
                    gap: 1.5,
                    px: 2,
                    py: 1.75,
                  }}
                >
                  <IconTile icon={content.icon} tone={content.tone} />
                  <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 0.625, minWidth: 0 }}>
                    <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
                      <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, fontWeight: 600 }}>
                        {content.short}
                      </Box>
                      {content.status && (
                        <StatusChip label={content.status.label} tone={content.status.tone} />
                      )}
                      <Box component="time" dateTime={item.createdAt} sx={{ color: ds.gray["500"], fontSize: 12 }}>
                        {notificationTime(item, t, true)}
                      </Box>
                    </Box>
                    <Box component="span" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, lineHeight: "20px", overflowWrap: "anywhere" }}>
                      {content.title}
                    </Box>
                    <Box component="span" sx={{ color: ds.gray["500"], fontSize: 13, lineHeight: "19.5px", overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>
                      {content.body}
                    </Box>
                    {action && (
                      <ActionLink
                        action={action}
                        onFollow={() => {
                          markRead([item.id], true);
                          close();
                        }}
                        size="sm"
                      />
                    )}
                  </Box>
                  {!item.read && <UnreadDot />}
                </Box>
              );
            })}
          </Box>
          <ButtonBase
            data-testid="notifications-view-all"
            onClick={() => goTo(PATHS.notifications)}
            sx={{
              backgroundColor: ds.gray["50"],
              color: ds.primary["600"],
              fontFamily: "inherit",
              fontSize: 14,
              fontWeight: 600,
              gap: 0.75,
              py: 2,
              width: "100%",
              "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: -2 },
            }}
          >
            {t("notifications.viewAll")}
            <ArrowRight aria-hidden size={16} />
          </ButtonBase>
        </Box>
      </Popover>
    </>
  );
};

/** The header bell: unread count, and the latest updates in a popover. */
export const NotificationBell = () => (
  <NotificationBoundary>
    <NotificationBellContent />
  </NotificationBoundary>
);
