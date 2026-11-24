import { describe, expect, it, vi } from "vitest";

import {
  lovelaceFromCborValue,
  pendingVoteCount,
  requestProfileMenu,
  subscribeProfileMenu,
} from "../profileMenu";

describe("requestProfileMenu", () => {
  it("is unhandled with no menu listening", () => {
    expect(requestProfileMenu()).toBe(false);
  });

  it("opens the listening menu until it unsubscribes", () => {
    const open = vi.fn();
    const unsubscribe = subscribeProfileMenu(open);
    expect(requestProfileMenu()).toBe(true);
    expect(open).toHaveBeenCalledTimes(1);
    unsubscribe();
    expect(requestProfileMenu()).toBe(false);
  });
});

describe("lovelaceFromCborValue", () => {
  it("reads a coin alone, in each integer width", () => {
    expect(lovelaceFromCborValue("05")).toBe(5);
    expect(lovelaceFromCborValue("1864")).toBe(100);
    expect(lovelaceFromCborValue("1903e8")).toBe(1000);
    expect(lovelaceFromCborValue("1a3cea7b80")).toBe(1022000000);
    expect(lovelaceFromCborValue("1b000000174876e800")).toBe(100000000000);
  });

  it("reads the coin of a value that also holds tokens", () => {
    expect(lovelaceFromCborValue("821a3cea7b80a1")).toBe(1022000000);
  });

  it("is undefined for anything else", () => {
    expect(lovelaceFromCborValue("")).toBeUndefined();
    expect(lovelaceFromCborValue("zz")).toBeUndefined();
    expect(lovelaceFromCborValue("a1")).toBeUndefined();
    expect(lovelaceFromCborValue(undefined)).toBeUndefined();
  });
});

describe("pendingVoteCount", () => {
  const live = [
    { txHash: "a", index: 0 },
    { txHash: "b", index: 0 },
    { txHash: "b", index: 1 },
  ];

  it("counts the live actions without a vote", () => {
    const votes = [{ proposal: { txHash: "b", index: 0 } }];
    expect(pendingVoteCount(live, 3, votes)).toBe(2);
  });

  it("is undefined while the live list is partial or the votes are unknown", () => {
    expect(pendingVoteCount(live, 40, [])).toBeUndefined();
    expect(pendingVoteCount(live, 3, undefined)).toBeUndefined();
  });
});
