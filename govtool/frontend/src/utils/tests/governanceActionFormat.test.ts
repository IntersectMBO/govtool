import { describe, expect, it } from "vitest";

import type { GovernanceActionRecord } from "@/models";

import {
  decidedStatus,
  timeLeft,
  treasuryTotal,
  voteBar,
} from "../governanceActionFormat";

const status = (
  over: Partial<GovernanceActionRecord["status"]>,
): GovernanceActionRecord["status"] => ({
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  ...over,
});

const times = (
  over: Partial<GovernanceActionRecord["status_times"]>,
): GovernanceActionRecord["status_times"] => ({
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
  ...over,
});

describe("timeLeft", () => {
  const now = new Date("2026-11-08T00:00:00Z");

  it("shows days and hours until expiry", () => {
    expect(timeLeft("2026-11-12T01:30:00Z", now)).toBe("4d 1h");
  });

  it("shows hours and minutes on the last day", () => {
    expect(timeLeft("2026-11-08T05:20:00Z", now)).toBe("5h 20m");
  });

  it("is undefined once the action has expired or has no date", () => {
    expect(timeLeft("2026-11-07T00:00:00Z", now)).toBeUndefined();
    expect(timeLeft(undefined, now)).toBeUndefined();
  });
});

describe("treasuryTotal", () => {
  it("sums every withdrawal amount", () => {
    expect(
      treasuryTotal([
        { receivingAddress: "a", amount: 100 },
        { receivingAddress: "b", amount: 50 },
      ]),
    ).toBe(150);
  });

  it("is undefined for actions that withdraw nothing", () => {
    expect(treasuryTotal({ data: { tag: "InfoAction" } })).toBeUndefined();
    expect(treasuryTotal(undefined)).toBeUndefined();
  });
});

describe("voteBar", () => {
  it("gives the yes share of non-abstaining stake and the threshold", () => {
    expect(
      voteBar({
        role: "drep",
        representation: "stake",
        yes: "60",
        no: "20",
        abstain: "10",
        notVoted: "20",
        totalEligible: "110",
        threshold: { numerator: 67, denominator: 100 },
      }),
    ).toEqual({
      yesPercent: 60,
      noPercent: 40,
      votedNoPercent: 20,
      thresholdPercent: 67,
      passing: false,
    });
  });

  it("is undefined for an aggregate that does not add up", () => {
    expect(
      voteBar({
        role: "cc",
        representation: "count",
        yes: "1",
        no: "1",
        abstain: "0",
        notVoted: "0",
        totalEligible: "7",
        threshold: { numerator: 2, denominator: 3 },
      }),
    ).toBeUndefined();
  });
});

describe("decidedStatus", () => {
  it("reports enacted, with its time, before ratified", () => {
    expect(
      decidedStatus({
        status: status({ ratified_epoch: 653, enacted_epoch: 654 }),
        status_times: times({
          ratified_time: "2026-10-01T00:00:00Z",
          enacted_time: "2026-10-06T00:00:00Z",
        }),
      }),
    ).toEqual({ key: "enacted", time: "2026-10-06T00:00:00Z", ratified: true });
  });

  it("reports an expired action as expired and not ratified", () => {
    expect(
      decidedStatus({
        status: status({ expired_epoch: 656, dropped_epoch: 657 }),
        status_times: times({ expired_time: "2026-10-20T00:00:00Z" }),
      }),
    ).toEqual({ key: "expired", time: "2026-10-20T00:00:00Z", ratified: false });
  });

  it("is undefined for a live action", () => {
    expect(
      decidedStatus({ status: status({}), status_times: times({}) }),
    ).toBeUndefined();
  });
});
