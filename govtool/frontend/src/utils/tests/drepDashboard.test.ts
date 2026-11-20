import { bech32 } from "bech32";
import { describe, expect, it } from "vitest";

import { GovernanceActionType } from "@/types/governanceAction";

import {
  actionOutcome,
  averageResponseDays,
  categoryPattern,
  epochActivity,
  isRetiredDRep,
  participation,
  participationRate,
  pickDelegatorCount,
  registrationEpoch,
  stakeHexToBech32,
  tenureMonths,
  toCsv,
  voteCalendar,
  voteSummary,
  votingStreak,
  type DashboardAction,
  type DashboardVote,
} from "../drepDashboard";

const NO_STATUS = {
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
};
const NO_TIMES = {
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
};

const action = (
  id: string,
  epoch: number,
  over: Partial<DashboardAction> = {},
): DashboardAction => ({
  tx_hash: id,
  index: 0,
  type: GovernanceActionType.InfoAction,
  epoch_no: epoch,
  time: `2026-01-${String((epoch % 28) + 1).padStart(2, "0")}T00:00:00Z`,
  status: NO_STATUS,
  status_times: NO_TIMES,
  ...over,
});

const vote = (
  id: string,
  epoch: number,
  choice: "yes" | "no" | "abstain" = "yes",
  over: { date?: string; createdDate?: string; type?: string } = {},
): DashboardVote => ({
  vote: { vote: choice, epochNo: epoch, date: over.date ?? "2026-03-01T00:00:00Z" },
  proposal: {
    txHash: id,
    index: 0,
    type: (over.type ?? GovernanceActionType.InfoAction) as GovernanceActionType,
    createdDate: over.createdDate ?? "2026-02-27T00:00:00Z",
  },
});

describe("voteSummary", () => {
  it("counts each choice and rounds the shares", () => {
    const votes = [
      ...Array.from({ length: 7 }, (_, i) => vote(`y${i}`, 1, "yes")),
      vote("n", 1, "no"),
      vote("n2", 1, "no"),
      vote("a", 1, "abstain"),
      { vote: null, proposal: vote("x", 1).proposal },
    ];
    expect(voteSummary(votes)).toEqual({
      total: 10, yes: 7, no: 2, abstain: 1, yesPct: 70, noPct: 20, abstainPct: 10,
    });
  });

  it("reports zero shares for no votes", () => {
    expect(voteSummary([])).toMatchObject({ total: 0, yesPct: 0, noPct: 0, abstainPct: 0 });
  });
});

describe("actionOutcome", () => {
  it("prefers enacted, then ratified, dropped and expired", () => {
    expect(actionOutcome(action("a", 1, { status: { ...NO_STATUS, ratified_epoch: 5, enacted_epoch: 6 } }))).toBe("enacted");
    expect(actionOutcome(action("a", 1, { status: { ...NO_STATUS, ratified_epoch: 5 } }))).toBe("ratified");
    expect(actionOutcome(action("a", 1, { status: { ...NO_STATUS, dropped_epoch: 5, expired_epoch: 5 } }))).toBe("dropped");
    expect(actionOutcome(action("a", 1, { status: { ...NO_STATUS, expired_epoch: 5 } }))).toBe("expired");
  });

  it("is active with no status and unknown without a record", () => {
    expect(actionOutcome(action("a", 1))).toBe("active");
    expect(actionOutcome(undefined)).toBe("unknown");
  });
});

describe("registrationEpoch", () => {
  const anchors = [
    { time: "2026-01-01T00:00:00Z", epoch: 500 },
    { time: "2026-01-11T00:00:00Z", epoch: 502 },
    { time: "2026-01-21T00:00:00Z", epoch: 504 },
  ];

  it("takes the latest epoch known to have started by the registration date", () => {
    expect(registrationEpoch("2026-01-15T00:00:00Z", anchors)).toBe(502);
    expect(registrationEpoch("2026-01-21T00:00:00Z", anchors)).toBe(504);
  });

  it("interpolates between the anchors either side of the registration", () => {
    const sparse = [
      { time: "2026-01-01T00:00:00Z", epoch: 500 },
      { time: "2026-02-20T00:00:00Z", epoch: 510 },
    ];
    expect(registrationEpoch("2026-01-26T00:00:00Z", sparse)).toBe(505);
  });

  it("ignores anchors without a time", () => {
    const withNull = [
      { time: null as unknown as string, epoch: 600 },
      { time: "2026-01-01T00:00:00Z", epoch: 500 },
    ];
    expect(registrationEpoch("2026-01-02T00:00:00Z", withNull)).toBe(500);
  });

  it("is undefined before every anchor or without a date", () => {
    expect(registrationEpoch("2025-12-01T00:00:00Z", anchors)).toBeUndefined();
    expect(registrationEpoch(undefined, anchors)).toBeUndefined();
  });
});

describe("participation", () => {
  const actions = [
    action("old", 400, { status: { ...NO_STATUS, expired_epoch: 410 } }),
    action("ratified", 500, { status: { ...NO_STATUS, ratified_epoch: 503 } }),
    action("live", 505),
    action("live2", 506),
  ];

  it("counts the actions open since registration that the DRep voted on", () => {
    const votes = [vote("ratified", 501), vote("live", 506), vote("old", 405)];
    expect(participation(votes, actions, 501, 507)).toEqual({ voted: 2, votable: 3, rate: 66 });
  });

  it("has no rate when nothing was votable", () => {
    expect(participation([], [], 501, 507)).toEqual({ voted: 0, votable: 0, rate: null });
  });

  it("treats an action as closed from its ratification epoch", () => {
    expect(participation([], actions, 503, 507).votable).toBe(2);
  });
});

describe("participationRate", () => {
  const actions = [
    action("old", 400, {
      time: "2025-01-01T00:00:00Z",
      status: { ...NO_STATUS, expired_epoch: 410 },
    }),
    action("new", 500, { time: "2026-01-01T00:00:00Z" }),
  ];
  const votes = [vote("new", 501)];

  it("counts from the epoch the DRep registered in, placed by dated anchors", () => {
    // Registered mid-2025, after the old action closed: one votable, voted.
    expect(participationRate(votes, actions, "2025-06-01T00:00:00Z", 507)).toBe(100);
  });

  it("counts every action when the registration date is unknown", () => {
    expect(participationRate(votes, actions, "", 507)).toBe(50);
  });
});

describe("pickDelegatorCount", () => {
  const route = { isServed: true, total: 12, isLoading: false };

  it("prefers the directory row's count, zero included", () => {
    expect(pickDelegatorCount(920, route)).toEqual({ show: true, count: 920, isLoading: false });
    expect(pickDelegatorCount(0, route)).toEqual({ show: true, count: 0, isLoading: false });
  });

  it("falls back to the delegators route, then to nothing", () => {
    expect(pickDelegatorCount(null, route)).toEqual({ show: true, count: 12, isLoading: false });
    expect(pickDelegatorCount(undefined, { ...route, isServed: false })).toEqual({
      show: false,
      count: undefined,
      isLoading: false,
    });
  });
});

describe("averageResponseDays", () => {
  it("averages the days from submission to vote, to one decimal", () => {
    const votes = [
      vote("a", 1, "yes", { createdDate: "2026-03-01T00:00:00Z", date: "2026-03-02T00:00:00Z" }),
      vote("b", 1, "yes", { createdDate: "2026-03-01T00:00:00Z", date: "2026-03-03T12:00:00Z" }),
    ];
    expect(averageResponseDays(votes)).toBe(1.8);
  });

  it("is null with no dated votes", () => {
    expect(averageResponseDays([])).toBeNull();
  });
});

describe("votingStreak", () => {
  it("counts consecutive epochs with a vote, ending this epoch", () => {
    expect(votingStreak([vote("a", 10), vote("b", 9), vote("c", 8), vote("d", 6)], 10)).toBe(3);
  });

  it("ends at the previous epoch while this one has no vote yet", () => {
    expect(votingStreak([vote("b", 9), vote("c", 8)], 10)).toBe(2);
  });

  it("is zero when the last two epochs have no vote", () => {
    expect(votingStreak([vote("c", 7)], 10)).toBe(0);
  });
});

describe("epochActivity", () => {
  const actions = [
    action("a", 8),
    action("b", 9, { status: { ...NO_STATUS, ratified_epoch: 10 } }),
    action("c", 10),
  ];

  it("lists each epoch since registration, newest first", () => {
    const rows = epochActivity([vote("a", 9), vote("b", 9), vote("c", 10)], actions, 10, 9, 8);
    expect(rows).toEqual([
      { epoch: 10, available: 2, votes: 1, participation: 50, isCurrent: true },
      { epoch: 9, available: 2, votes: 2, participation: 100, isCurrent: false },
    ]);
  });

  it("stops at the row limit", () => {
    expect(epochActivity([], actions, 10, undefined, 2).map((row) => row.epoch)).toEqual([10, 9]);
  });
});

describe("categoryPattern", () => {
  it("groups votable actions into the four design categories", () => {
    const actions = [
      action("t", 5, { type: GovernanceActionType.TreasuryWithdrawals }),
      action("t2", 5, { type: GovernanceActionType.TreasuryWithdrawals }),
      action("p", 5, { type: GovernanceActionType.HardForkInitiation }),
      action("c", 5, { type: GovernanceActionType.NewConstitution }),
    ];
    const rows = categoryPattern([vote("t", 5), vote("p", 5)], actions, 5, 6);
    expect(rows).toEqual([
      { key: "treasury", voted: 1, votable: 2 },
      { key: "protocol", voted: 1, votable: 1 },
      { key: "info", voted: 0, votable: 0 },
      { key: "committee", voted: 0, votable: 1 },
    ]);
  });
});

describe("voteCalendar", () => {
  it("lays out 53 Sunday-first weeks ending with this week, with levels", () => {
    const today = new Date("2026-11-18T10:00:00Z"); // a Wednesday
    const votes = [
      vote("a", 1, "yes", { date: "2026-11-17T08:00:00Z" }),
      vote("b", 1, "yes", { date: "2026-11-17T09:00:00Z" }),
    ];
    const weeks = voteCalendar(votes, today);
    expect(weeks).toHaveLength(53);
    expect(weeks.every((week) => week.length === 7)).toBe(true);
    const last = weeks[52];
    expect(last[0].date).toBe("2026-11-15");
    expect(last[2]).toMatchObject({ date: "2026-11-17", count: 2, level: 2, future: false });
    expect(last[4].future).toBe(true);
  });
});

describe("toCsv", () => {
  it("quotes separators and defuses formula cells", () => {
    expect(toCsv([["Title", "Vote"], ["A, \"B\"", "=1+1"], ["-2", null]])).toBe(
      "Title,Vote\r\n\"A, \"\"B\"\"\",'=1+1\r\n'-2,",
    );
  });
});

describe("stakeHexToBech32", () => {
  const hash = "ab".repeat(28);

  it("encodes a mainnet and a testnet reward address", () => {
    const main = stakeHexToBech32(`e1${hash}`);
    const test = stakeHexToBech32(`E0${hash}`);
    expect(main?.startsWith("stake1")).toBe(true);
    expect(test?.startsWith("stake_test1")).toBe(true);
    const decoded = bech32.decode(main ?? "");
    expect(Buffer.from(bech32.fromWords(decoded.words)).toString("hex")).toBe(`e1${hash}`);
  });

  it("refuses anything that is not a 29-byte reward address", () => {
    expect(stakeHexToBech32(`01${hash}`)).toBeUndefined();
    expect(stakeHexToBech32("e1abc")).toBeUndefined();
    expect(stakeHexToBech32(undefined)).toBeUndefined();
  });
});

describe("isRetiredDRep", () => {
  const voter = {
    isRegisteredAsDRep: false,
    wasRegisteredAsDRep: true,
    isRegisteredAsSoleVoter: false,
  };

  it("is a former DRep that is neither a DRep nor a direct voter now", () => {
    expect(isRetiredDRep(voter, {})).toBe(true);
  });

  it("is not while registering again, nor for a direct voter or a current DRep", () => {
    expect(isRetiredDRep(voter, { registerAsDrep: { transactionHash: "t" } })).toBe(false);
    expect(isRetiredDRep({ ...voter, isRegisteredAsSoleVoter: true }, {})).toBe(false);
    expect(isRetiredDRep({ ...voter, isRegisteredAsDRep: true }, {})).toBe(false);
    expect(isRetiredDRep({ ...voter, wasRegisteredAsDRep: false }, {})).toBe(false);
  });
});

describe("tenureMonths", () => {
  it("counts whole months between two dates", () => {
    expect(tenureMonths("2025-03-15T00:00:00Z", "2026-08-20T00:00:00Z")).toBe(17);
    expect(tenureMonths("2026-03-15T00:00:00Z", "2026-03-20T00:00:00Z")).toBe(0);
  });

  it("is undefined for a missing or reversed range", () => {
    expect(tenureMonths(undefined, "2026-08-20T00:00:00Z")).toBeUndefined();
    expect(tenureMonths("2026-08-20T00:00:00Z", "2025-08-20T00:00:00Z")).toBeUndefined();
  });
});
