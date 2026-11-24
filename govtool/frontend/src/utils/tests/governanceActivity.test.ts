import { describe, expect, it } from "vitest";

import { GovernanceActionType } from "@/types/governanceAction";

import { getGovActionCIP129Id } from "../getGovActionId";

import {
  activityCounts,
  activityEvents,
  daysLeft,
  formatUtc,
  groupByUtcDay,
  type ActivityDecided,
  type ActivityLive,
} from "../governanceActivity";

const DAY = 24 * 60 * 60 * 1000;
const now = new Date("2026-10-02T05:00:00Z");
const ago = (ms: number) => new Date(now.getTime() - ms).toISOString();
const ahead = (ms: number) => new Date(now.getTime() + ms).toISOString();

const live = (over: Partial<ActivityLive> = {}): ActivityLive => ({
  createdDate: ago(DAY),
  expiryDate: ahead(10 * DAY),
  index: 0,
  title: "Live action",
  txHash: "aa".repeat(32),
  type: GovernanceActionType.InfoAction,
  ...over,
});

const decided = (
  times: Partial<ActivityDecided["status_times"]>,
  over: Partial<ActivityDecided> = {},
): ActivityDecided => ({
  id: "1",
  index: 0,
  status_times: {
    ratified_time: null,
    enacted_time: null,
    dropped_time: null,
    expired_time: null,
    ...times,
  },
  title: "Decided action",
  tx_hash: "bb".repeat(32),
  type: "ParameterChange",
  ...over,
});

describe("activityCounts", () => {
  it("counts live actions whose deadline falls within the next 7 days", () => {
    const list = [
      live({ expiryDate: ahead(2 * DAY) }),
      live({ expiryDate: ahead(7 * DAY) }),
      live({ expiryDate: ahead(7 * DAY + 60_000) }),
      live({ expiryDate: ago(60_000) }),
      live({ expiryDate: undefined }),
    ];
    const counts = activityCounts(list, list.length, [], now);
    expect(counts.open).toBe(5);
    expect(counts.deadlines).toBe(2);
  });

  it("does not claim a deadline count when the live list is incomplete", () => {
    const list = [live({ expiryDate: ahead(DAY) })];
    expect(activityCounts(list, 51, [], now).deadlines).toBeUndefined();
    expect(activityCounts(list, 51, [], now).open).toBe(51);
  });

  it("counts ratifications and enactments of the past 30 days separately", () => {
    const list = [
      decided({ ratified_time: ago(29 * DAY), enacted_time: ago(DAY) }),
      decided({ ratified_time: ago(31 * DAY), enacted_time: ago(29 * DAY) }),
      decided({ ratified_time: ago(2 * DAY) }),
      decided({ expired_time: ago(DAY) }),
      decided({ ratified_time: ahead(DAY) }),
    ];
    const counts = activityCounts([], 0, list, now);
    expect(counts.ratified).toBe(2);
    expect(counts.enacted).toBe(2);
  });
});

describe("activityCounts with a capped decided list", () => {
  const full = (time: string) =>
    Array.from({ length: 3 }, () => decided({ ratified_time: time, enacted_time: time }));

  it("does not claim exact counts when every loaded decision is recent", () => {
    const counts = activityCounts([], 0, full(ago(DAY)), now, 3);
    expect(counts.ratified).toBeUndefined();
    expect(counts.enacted).toBeUndefined();
  });

  it("counts once the list reaches past the 30-day window", () => {
    const list = [...full(ago(DAY)).slice(0, 2), decided({ expired_time: ago(40 * DAY) })];
    const counts = activityCounts([], 0, list, now, 3);
    expect(counts.ratified).toBe(2);
  });

  it("counts a list shorter than the limit as whole", () => {
    expect(activityCounts([], 0, full(ago(DAY)), now, 20).ratified).toBe(3);
  });
});

describe("activityEvents", () => {
  it("lists voting openings and decisions newest first", () => {
    const events = activityEvents(
      [live({ createdDate: ago(3 * DAY), title: "Opened" })],
      [
        decided(
          { ratified_time: ago(5 * DAY), enacted_time: ago(DAY) },
          { id: "7", title: "Enacted one" },
        ),
        decided({ dropped_time: ago(2 * DAY) }, { id: "8", title: "Dropped" }),
      ],
    );
    expect(events.map((event) => event.kind)).toEqual([
      "enacted",
      "expired",
      "votingOpened",
      "ratified",
    ]);
    expect(events[0].path).toBe("/actions/history/7");
    expect(events[0].actionId).toBe(`${"bb".repeat(32)}#0`);
    expect(events[2].path).toBe(
      `/actions/${getGovActionCIP129Id("aa".repeat(32), 0)}`,
    );
  });

  it("skips actions with no recorded times", () => {
    expect(activityEvents([], [decided({})])).toEqual([]);
  });

  it("gives every event a unique key", () => {
    const events = activityEvents(
      [],
      [decided({ ratified_time: ago(2 * DAY), enacted_time: ago(DAY) })],
    );
    expect(new Set(events.map((event) => event.key)).size).toBe(2);
  });
});

describe("groupByUtcDay", () => {
  it("groups by the UTC date and labels today and yesterday", () => {
    const at = new Date("2026-10-02T00:30:00Z");
    const events = activityEvents(
      [],
      [
        decided({ enacted_time: "2026-10-02T00:10:00Z" }, { id: "a" }),
        decided({ enacted_time: "2026-10-01T23:30:00Z" }, { id: "b" }),
        decided({ enacted_time: "2026-09-28T12:00:00Z" }, { id: "c" }),
      ],
    );
    const groups = groupByUtcDay(events, at);
    expect(groups.map((group) => [group.day, group.label])).toEqual([
      ["2026-10-02", "today"],
      ["2026-10-01", "yesterday"],
      ["2026-09-28", undefined],
    ]);
  });
});

describe("daysLeft", () => {
  it("is urgent at 3 days or fewer", () => {
    expect(daysLeft(ahead(3 * DAY + 60_000), now)).toEqual({ days: 3, tone: "error" });
  });

  it("warns inside the 7 days the deadline count uses", () => {
    expect(daysLeft(ahead(4 * DAY + 60_000), now)?.tone).toBe("warning");
    expect(daysLeft(ahead(7 * DAY - 60_000), now)?.tone).toBe("warning");
  });

  it("is neutral from 7 days out, as the deadline count is", () => {
    expect(daysLeft(ahead(7 * DAY + 60_000), now)?.tone).toBe("neutral");
  });

  it("is undefined once the deadline passed or when there is none", () => {
    expect(daysLeft(ago(1), now)).toBeUndefined();
    expect(daysLeft(undefined, now)).toBeUndefined();
  });
});

describe("formatUtc", () => {
  const time = "2026-09-04T23:05:00Z";

  it("formats in UTC whatever the browser's zone", () => {
    expect(formatUtc(time, "date")).toBe("4 Sep 2026");
    expect(formatUtc(time, "longDate")).toBe("4 September 2026");
    expect(formatUtc(time, "time")).toBe("23:05");
    expect(formatUtc(time, "dateTime")).toBe("4 Sep 2026 · 23:05 UTC");
  });

  it("is a dash for a missing or invalid time", () => {
    expect(formatUtc(undefined, "date")).toBe("—");
    expect(formatUtc("not a date", "date")).toBe("—");
  });
});
