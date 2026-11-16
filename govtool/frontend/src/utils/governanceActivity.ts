import { GOV_ACTION_HISTORY_PATHS, PATHS } from "@/consts/paths";
import type { GovernanceActionRecord, ProposalData } from "@/models";

import { getFullGovActionId } from "./getGovActionId";

// Figures and events for the Governance Activity page, from the live list
// and the newest decided actions.

const DAY = 24 * 60 * 60 * 1000;

export type ActivityLive = Pick<
  ProposalData,
  "createdDate" | "expiryDate" | "index" | "title" | "txHash" | "type"
>;

export type ActivityDecided = Pick<
  GovernanceActionRecord,
  "id" | "index" | "status_times" | "title" | "tx_hash" | "type"
>;

const within = (time: string | null | undefined, from: number, to: number) => {
  if (!time) return false;
  const at = new Date(time).getTime();
  return at > from && at <= to;
};

const latestTime = ({ status_times: times }: ActivityDecided) =>
  Math.max(
    ...[times.ratified_time, times.enacted_time, times.expired_time, times.dropped_time]
      .filter((time): time is string => !!time)
      .map((time) => new Date(time).getTime()),
  );

/**
 * The overview figures. A figure is undefined when the list it counts is only
 * part of the whole: `deadlines` when the live list is short of its total,
 * `ratified` and `enacted` when the decided list is full (`decidedLimit`)
 * and nothing in it is older than the 30-day window.
 */
export const activityCounts = (
  live: ActivityLive[],
  liveTotal: number,
  decided: ActivityDecided[],
  now: Date,
  decidedLimit = Infinity,
) => {
  const at = now.getTime();
  const isDecidedWhole =
    decided.length < decidedLimit ||
    decided.some((action) => latestTime(action) <= at - 30 * DAY);
  const recent = (key: "ratified_time" | "enacted_time") =>
    (isDecidedWhole
      ? decided.filter(({ status_times: times }) =>
        within(times[key], at - 30 * DAY, at),
      ).length
      : undefined);
  return {
    open: liveTotal,
    deadlines:
      liveTotal > live.length
        ? undefined
        : live.filter(({ expiryDate }) => within(expiryDate, at, at + 7 * DAY))
            .length,
    ratified: recent("ratified_time"),
    enacted: recent("enacted_time"),
  };
};

export type ActivityKind = "votingOpened" | "ratified" | "enacted" | "expired";

export type ActivityEvent = {
  key: string;
  /** `txHash#index`. */
  actionId: string;
  kind: ActivityKind;
  time: string;
  type: string;
  title?: string;
  path: string;
};

/** Voting openings from the live list and each decision step, newest first. */
export const activityEvents = (
  live: ActivityLive[],
  decided: ActivityDecided[],
): ActivityEvent[] => {
  const opened = live.map((action): ActivityEvent => {
    const id = getFullGovActionId(action.txHash, action.index);
    return {
      key: `votingOpened-${id}`,
      actionId: id,
      kind: "votingOpened",
      time: action.createdDate,
      type: action.type,
      title: action.title,
      path: `${PATHS.governanceActions}/${id}`,
    };
  });

  const decisions = decided.flatMap((action) => {
    const times = action.status_times;
    const steps: [ActivityKind, string | null][] = [
      ["ratified", times.ratified_time],
      ["enacted", times.enacted_time],
      ["expired", times.expired_time ?? times.dropped_time],
    ];
    return steps
      .filter((step): step is [ActivityKind, string] => !!step[1])
      .map(
        ([kind, time]): ActivityEvent => ({
          key: `${kind}-${action.tx_hash}#${action.index}`,
          actionId: `${action.tx_hash}#${action.index}`,
          kind,
          time,
          type: action.type,
          title: action.title,
          path: `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${action.id}`,
        }),
      );
  });

  return [...opened, ...decisions]
    .filter((event) => !Number.isNaN(new Date(event.time).getTime()))
    .sort((a, b) => new Date(b.time).getTime() - new Date(a.time).getTime());
};

const utcDay = (time: string | Date) => new Date(time).toISOString().slice(0, 10);

/** Events grouped by their UTC date, in the order given. */
export const groupByUtcDay = (events: ActivityEvent[], now: Date) => {
  const today = utcDay(now);
  const yesterday = utcDay(new Date(now.getTime() - DAY));
  const groups: {
    day: string;
    label: "today" | "yesterday" | undefined;
    events: ActivityEvent[];
  }[] = [];
  events.forEach((event) => {
    const day = utcDay(event.time);
    const last = groups[groups.length - 1];
    if (last?.day === day) {
      last.events.push(event);
      return;
    }
    groups.push({
      day,
      label:
        (day === today && "today") || (day === yesterday && "yesterday") || undefined,
      events: [event],
    });
  });
  return groups;
};

/** Whole days until a deadline and how urgent that is. */
export const daysLeft = (expiry: string | undefined, now: Date) => {
  if (!expiry) return undefined;
  const remaining = new Date(expiry).getTime() - now.getTime();
  if (!(remaining > 0)) return undefined;
  const days = Math.floor(remaining / DAY);
  const tone: "error" | "warning" | "neutral" =
    // Warning matches the overview's "within 7 days" count.
    (days <= 3 && "error") || (remaining < 7 * DAY && "warning") || "neutral";
  return { days, tone };
};

const utcParts = (date: Date, month: "short" | "long") => {
  const parts = new Intl.DateTimeFormat("en-US", {
    day: "numeric",
    hour: "2-digit",
    hourCycle: "h23",
    minute: "2-digit",
    month,
    timeZone: "UTC",
    year: "numeric",
  }).formatToParts(date);
  const part = (type: Intl.DateTimeFormatPartTypes) =>
    parts.find((item) => item.type === type)?.value ?? "";
  return {
    date: `${part("day")} ${part("month")} ${part("year")}`,
    time: `${part("hour")}:${part("minute")}`,
  };
};

/** "4 Sep 2026", "4 September 2026", "23:05" or "4 Sep 2026 · 23:05 UTC", in UTC. */
export const formatUtc = (
  time: string | undefined | null,
  style: "date" | "longDate" | "time" | "dateTime",
) => {
  const date = time ? new Date(time) : undefined;
  if (!date || Number.isNaN(date.getTime())) return "—";
  const { date: day, time: clock } = utcParts(
    date,
    style === "longDate" ? "long" : "short",
  );
  if (style === "time") return clock;
  if (style === "dateTime") return `${day} · ${clock} UTC`;
  return day;
};
