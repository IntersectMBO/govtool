import { describe, expect, it } from "vitest";

import { governanceActionStats } from "../governanceActionStats";

const NOW = new Date("2026-11-19T12:00:00Z");
const day = (days: number) => new Date(NOW.getTime() + days * 86_400_000).toISOString();

const action = (enacted: number | null) => ({
  status: {
    ratified_epoch: null,
    enacted_epoch: enacted,
    dropped_epoch: null,
    expired_epoch: null,
  },
});

describe("governanceActionStats", () => {
  it("counts every action, the live ones, those closing within 7 days and the enacted", () => {
    expect(
      governanceActionStats({
        actions: [action(500), action(null), action(510), action(null)],
        live: [{ expiryDate: day(2) }, { expiryDate: day(6.9) }, { expiryDate: day(9) }],
        liveTotal: 3,
        now: NOW,
      }),
    ).toEqual({ total: 4, votingNow: 3, expiringSoon: 2, enacted: 2 });
  });

  it("leaves Expiring Soon out when the live list is only a first page", () => {
    expect(
      governanceActionStats({
        actions: [],
        live: [{ expiryDate: day(1) }],
        liveTotal: 80,
        now: NOW,
      }).expiringSoon,
    ).toBeUndefined();
  });

  it("leaves each figure out until its data has arrived", () => {
    expect(
      governanceActionStats({
        actions: undefined,
        live: undefined,
        liveTotal: undefined,
        now: NOW,
      }),
    ).toEqual({
      total: undefined,
      votingNow: undefined,
      expiringSoon: undefined,
      enacted: undefined,
    });
  });

  it("ignores live actions without an expiry or already past it", () => {
    expect(
      governanceActionStats({
        actions: [],
        live: [{ expiryDate: undefined }, { expiryDate: day(-1) }],
        liveTotal: 2,
        now: NOW,
      }).expiringSoon,
    ).toBe(0);
  });
});
