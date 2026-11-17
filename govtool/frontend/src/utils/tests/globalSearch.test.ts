import { beforeEach, describe, expect, it } from "vitest";
import {
  BaseAddress,
  Credential,
  Ed25519KeyHash,
  EnterpriseAddress,
  RewardAddress,
} from "@emurgo/cardano-serialization-lib-asmjs";

import {
  RECENT_SEARCHES_KEY,
  classifyQuery,
  clearRecentSearches,
  forumSearchQuery,
  readRecentSearches,
  saveRecentSearch,
  shortId,
  stakeAddressOf,
} from "../globalSearch";

const keyHash = (byte: string) =>
  Credential.from_keyhash(Ed25519KeyHash.from_hex(byte.repeat(28)));
const payment = keyHash("11");
const stake = keyHash("22");
const stakeAddress = RewardAddress.new(1, stake).to_address().to_bech32();
const baseAddress = BaseAddress.new(1, payment, stake).to_address().to_bech32();
const enterpriseAddress = EnterpriseAddress.new(1, payment).to_address().to_bech32();

describe("stakeAddressOf", () => {
  it("returns a stake address as it is", () => {
    expect(stakeAddressOf(stakeAddress)).toBe(stakeAddress);
  });

  it("finds the stake address inside a base payment address", () => {
    expect(stakeAddressOf(baseAddress)).toBe(stakeAddress);
  });

  it("is undefined for an address without a stake part or a malformed one", () => {
    expect(stakeAddressOf(enterpriseAddress)).toBeUndefined();
    expect(stakeAddressOf("addr1notreal")).toBeUndefined();
    expect(stakeAddressOf("treasury")).toBeUndefined();
  });
});

describe("classifyQuery", () => {
  it("treats words as a keyword and offers the one-word query as a handle", () => {
    expect(classifyQuery("  Treasury ")).toEqual({
      text: "Treasury",
      kind: "keyword",
      handle: "treasury",
    });
    expect(classifyQuery("treasury committee")).toEqual({
      text: "treasury committee",
      kind: "keyword",
    });
  });

  it("reads a $ prefix as a handle", () => {
    expect(classifyQuery("$Intersect")).toEqual({
      text: "$Intersect",
      kind: "handle",
      handle: "intersect",
    });
  });

  it("reads pasted addresses and resolves their stake address", () => {
    expect(classifyQuery(baseAddress)).toEqual({
      text: baseAddress,
      kind: "address",
      stakeAddress,
    });
    expect(classifyQuery(enterpriseAddress)).toEqual({
      text: enterpriseAddress,
      kind: "address",
    });
  });

  it("does not offer ids or long words as handles", () => {
    expect(classifyQuery("gov_action1abc").handle).toBeUndefined();
    expect(classifyQuery("abcdefghijklmnop").handle).toBeUndefined();
  });
});

describe("recent searches", () => {
  beforeEach(() => localStorage.clear());

  it("keeps the newest five, without duplicates", () => {
    ["a1", "b2", "c3", "d4", "e5", "f6", "b2"].forEach((query) =>
      saveRecentSearch({ query, kind: "keyword" }),
    );
    expect(readRecentSearches().map((item) => item.query)).toEqual([
      "b2",
      "f6",
      "e5",
      "d4",
      "c3",
    ]);
  });

  it("clears and survives unreadable storage", () => {
    saveRecentSearch({ query: "x", kind: "keyword" });
    clearRecentSearches();
    expect(readRecentSearches()).toEqual([]);
    localStorage.setItem(RECENT_SEARCHES_KEY, "{not json");
    expect(readRecentSearches()).toEqual([]);
  });
});

describe("shortId", () => {
  it("keeps the start and the last characters", () => {
    expect(shortId("gov_action1qabcdefghijklmnopq7w4d")).toBe("gov_action1q…7w4d");
    expect(shortId("short")).toBe("short");
  });
});

describe("forumSearchQuery", () => {
  it("searches proposal names with an encoded phrase and paging", () => {
    expect(forumSearchQuery("a&b #1", 2, 5)).toBe(
      "filters[$and][0][prop_name][$containsi]=a%26b%20%231" +
        "&pagination[page]=2&pagination[pageSize]=5&sort[createdAt]=desc" +
        "&populate[0]=proposal_withdrawals",
    );
  });
});
