import { useState } from "react";
import { Box, ButtonBase, Skeleton } from "@mui/material";
import { useNavigate } from "react-router";
import { useTranslation } from "react-i18next";
import { AlertTriangle, ArrowDown, BellOff, RefreshCw, Settings } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PageContainer } from "@/components/atoms/PageContainer";
import { SortSelect } from "@/components/molecules/SortSelect";
import { PATHS } from "@/consts/paths";
import { ds, RADIUS } from "@/consts/designTokens";
import { NOTIFICATION_PAGE_SIZE, useNotifications } from "@/hooks/useNotifications";
import type { GovToolNotification, NotificationFilter } from "@/models";
import { formatUtc } from "@/utils/governanceActivity";
import { NOTIFICATION_FILTERS } from "@/utils/notifications";

import { describeNotification } from "./notificationContent";
import {
  ActionLink,
  IconTile,
  SignInPanel,
  StatePanel,
  StatusChip,
  UnreadDot,
  notificationTime,
} from "./notificationParts";

const DAY = 24 * 60 * 60 * 1000;
const utcDay = (time: Date | string) => new Date(time).toISOString().slice(0, 10);

type Group = { key: "today" | "week" | "older"; detail: string; items: GovToolNotification[] };

/** Today (UTC), the six days before it, and older, in the order given. */
export const groupNotifications = (
  items: GovToolNotification[],
  now: Date,
  format: (key: string, options?: Record<string, unknown>) => string,
): Group[] => {
  const today = utcDay(now);
  const weekStart = utcDay(new Date(now.getTime() - 6 * DAY));
  const keyOf = (item: GovToolNotification): Group["key"] => {
    const day = utcDay(item.createdAt);
    if (day >= today) return "today";
    return day >= weekStart ? "week" : "older";
  };
  const detail = {
    today: formatUtc(now.toISOString(), "longDate"),
    week: `${formatUtc(`${weekStart}T00:00:00Z`, "longDate")} – ${formatUtc(
      new Date(now.getTime() - DAY).toISOString(),
      "longDate",
    )}`,
    older: format("notifications.groups.olderDetail", {
      date: formatUtc(`${weekStart}T00:00:00Z`, "longDate"),
    }),
  };
  const groups: Group[] = [];
  items.forEach((item) => {
    const key = keyOf(item);
    const last = groups[groups.length - 1];
    if (last?.key === key) last.items.push(item);
    else groups.push({ key, detail: detail[key], items: [item] });
  });
  return groups;
};

const NotificationCard = ({
  item,
  onToggleRead,
}: {
  item: GovToolNotification;
  onToggleRead: (read: boolean) => void;
}) => {
  const { t } = useTranslation();
  const content = describeNotification(item, t);
  return (
    <Box
      component="li"
      data-testid="notification-card"
      sx={{
        backgroundColor: item.read ? "#FFFFFF" : ds.gray["50"],
        borderLeft: item.read ? "3px solid transparent" : `3px solid ${ds.primary["600"]}`,
        borderTop: `1px solid ${ds.gray["200"]}`,
        display: "flex",
        gap: 2,
        p: { xxs: 2, md: 2.5 },
        "&:first-of-type": { borderTop: "none" },
      }}
    >
      <IconTile icon={content.icon} tone={content.tone} size={40} />
      <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 1, minWidth: 0 }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
          <Box component="span" sx={{ color: ds.gray["600"], fontSize: 12, fontWeight: 600 }}>
            {content.category}
          </Box>
          {content.status && <StatusChip label={content.status.label} tone={content.status.tone} />}
          {!item.read && <StatusChip label={t("notifications.unreadChip")} tone="primary" />}
          <Box component="time" dateTime={item.createdAt} sx={{ color: ds.gray["500"], fontSize: 12 }}>
            {notificationTime(item, t)}
          </Box>
        </Box>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: { xxs: 16, md: 18 }, fontWeight: 600, letterSpacing: "-0.26px", lineHeight: { xxs: "24px", md: "27px" }, m: 0, overflowWrap: "anywhere" }}>
          {content.title}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>
          {content.body}
        </Box>
        {content.reason && (
          <Box component="p" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px", m: 0 }}>
            {content.reason}
          </Box>
        )}
        {content.context && (
          <Box sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.sm}px`, color: ds.gray["700"], fontSize: 12, lineHeight: "18px", px: 1.25, py: 1 }}>
            {content.context}
          </Box>
        )}
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 2, mt: 0.5 }}>
          {content.actions.map((action, index) => (
            <ActionLink
              key={action.label}
              action={action}
              onFollow={() => !item.read && onToggleRead(true)}
              withArrow={index === 0}
            />
          ))}
          <ButtonBase
            data-testid="notification-toggle-read"
            onClick={() => onToggleRead(!item.read)}
            sx={{ borderRadius: "4px", color: ds.gray["600"], fontFamily: "inherit", fontSize: 12, "&:hover": { color: ds.gray["900"] }, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            {item.read ? t("notifications.markUnread") : t("notifications.markRead")}
          </ButtonBase>
        </Box>
      </Box>
      {!item.read && <UnreadDot />}
    </Box>
  );
};

/** The Notification Center: filters, day groups, read state and paging. */
export const NotificationCenter = () => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const [filter, setFilter] = useState<NotificationFilter>("all");
  const [sort, setSort] = useState<"newest" | "oldest">("newest");
  const feed = useNotifications({ filter, sort });
  const settingsButton = (
    <Button
      dataTestId="notifications-settings-button"
      onClick={() => navigate(PATHS.notificationPreferences)}
      startIcon={<Settings aria-hidden size={16} />}
      variant="outlined"
    >
      {t("notifications.settings")}
    </Button>
  );

  const renderFeed = () => {
    if (feed.needsSignIn) return <SignInPanel signIn={feed.signIn} />;
    if (feed.isLoading) {
      return (
        <Box aria-busy sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          {[0, 1, 2].map((key) => (
            <Skeleton key={key} height={132} sx={{ borderRadius: `${RADIUS.lg}px` }} variant="rectangular" />
          ))}
        </Box>
      );
    }
    if (feed.isError) {
      return (
        <StatePanel
          action={(
            <Button onClick={feed.retry} startIcon={<RefreshCw aria-hidden size={16} />}>
              {t("notifications.retry")}
            </Button>
          )}
          body={t("notifications.errorBody")}
          icon={<AlertTriangle size={22} />}
          testId="notifications-error"
          title={t("notifications.errorTitle")}
          tone="error"
        />
      );
    }
    if (!feed.preferences.channels.inApp) {
      return (
        <StatePanel
          action={settingsButton}
          body={t("notifications.offBody")}
          icon={<BellOff size={22} />}
          testId="notifications-off"
          title={t("notifications.offTitle")}
          tone="neutral"
        />
      );
    }
    if (!feed.items.length) {
      const filtered = filter !== "all";
      return (
        <StatePanel
          action={filtered ? undefined : settingsButton}
          body={filtered ? t("notifications.emptyFilteredBody") : t("notifications.emptyBody")}
          icon={<BellOff size={22} />}
          testId="notifications-empty"
          title={filtered ? t("notifications.emptyFilteredTitle") : t("notifications.emptyTitle")}
          tone="neutral"
        />
      );
    }
    return (
      <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
        {groupNotifications(feed.items, new Date(), t).map((group) => (
          <Box key={group.key} aria-labelledby={`notifications-group-${group.key}`} component="section">
            <Box component="h2" id={`notifications-group-${group.key}`} sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, lineHeight: "30px", m: 0 }}>
              {t(`notifications.groups.${group.key}`)}
            </Box>
            <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, m: 0, mb: 1.5 }}>
              {group.detail}
            </Box>
            <Box
              component="ul"
              sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, listStyle: "none", m: 0, overflow: "hidden", p: 0 }}
            >
              {group.items.map((item) => (
                <NotificationCard
                  key={item.id}
                  item={item}
                  onToggleRead={(read) => feed.markRead([item.id], read)}
                />
              ))}
            </Box>
          </Box>
        ))}
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 2, justifyContent: "space-between" }}>
          <Box component="span" aria-live="polite" data-testid="notifications-showing" sx={{ color: ds.gray["600"], fontSize: 13 }}>
            {t("notifications.showing", { from: 1, to: feed.items.length, total: feed.total })}
          </Box>
          {feed.hasMore && (
            <Button
              dataTestId="notifications-load-more"
              onClick={feed.loadMore}
              startIcon={<ArrowDown aria-hidden size={16} />}
            >
              {t("notifications.loadMore", {
                count: Math.min(NOTIFICATION_PAGE_SIZE, feed.total - feed.items.length),
              })}
            </Button>
          )}
        </Box>
      </Box>
    );
  };

  return (
    <PageContainer sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box sx={{ alignItems: { md: "flex-end" }, display: "flex", flexDirection: { xxs: "column", md: "row" }, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ minWidth: 0 }}>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.25 }}>
            <Box component="h1" sx={{ color: ds.gray["900"], fontSize: { xxs: 28, md: 32 }, fontWeight: 600, letterSpacing: "-0.6px", lineHeight: "40px", m: 0 }}>
              {t("notifications.centerTitle")}
            </Box>
            {feed.unread > 0 && <StatusChip label={t("notifications.unread", { count: feed.unread })} tone="primary" />}
          </Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1 }}>
            {t("notifications.centerDescription")}
          </Box>
          {feed.mode === "local" && (
            <Box component="p" data-testid="notifications-local-note" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px", m: 0, mt: 0.5 }}>
              {t("notifications.localNote")}
            </Box>
          )}
        </Box>
        {settingsButton}
      </Box>

      <Box aria-label={t("notifications.filtersLabel")} role="group" sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}>
        {NOTIFICATION_FILTERS.map((key) => {
          const isActive = key === filter;
          return (
            <ButtonBase
              key={key}
              aria-pressed={isActive}
              data-testid={`notifications-filter-${key}`}
              onClick={() => setFilter(key)}
              sx={{
                alignItems: "center",
                backgroundColor: isActive ? ds.primary["50"] : "#FFFFFF",
                border: `1px solid ${isActive ? ds.primary["600"] : ds.gray["200"]}`,
                borderRadius: `${RADIUS.pill}px`,
                color: isActive ? ds.primary["700"] : ds.gray["700"],
                fontFamily: "inherit",
                fontSize: 14,
                fontWeight: isActive ? 600 : 500,
                gap: 1,
                px: 1.5,
                py: 0.75,
                "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
              }}
            >
              {t(`notifications.filters.${key}`)}
              <Box component="span" sx={{ backgroundColor: isActive ? ds.primary["100"] : ds.gray["100"], borderRadius: `${RADIUS.pill}px`, fontSize: 12, fontWeight: 600, px: 0.75 }}>
                {feed.counts[key]}
              </Box>
            </ButtonBase>
          );
        })}
      </Box>

      <Box sx={{ alignItems: "center", backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between", px: 2, py: 1 }}>
        <ButtonBase
          data-testid="notifications-mark-all"
          disabled={!feed.unread}
          onClick={() => feed.markAllRead()}
          sx={{ borderRadius: "4px", color: feed.unread ? ds.primary["600"] : ds.gray["400"], fontFamily: "inherit", fontSize: 14, fontWeight: 600, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
        >
          {t("notifications.markAllRead")}
        </ButtonBase>
        <SortSelect
          dataTestId="notifications-sort-button"
          onChange={(next) => setSort(next as "newest" | "oldest")}
          options={[
            { key: "newest", label: t("notifications.sortNewest") },
            { key: "oldest", label: t("notifications.sortOldest") },
          ]}
          value={sort}
        />
      </Box>

      {renderFeed()}
    </PageContainer>
  );
};
