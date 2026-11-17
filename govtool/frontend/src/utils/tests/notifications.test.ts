import { beforeEach, describe, expect, it } from "vitest";

import type { GovernanceActionRecord, ProposalData, VotedProposal } from "@/models";
import { DRepStatus } from "@/models";

import {
  DEFAULT_PREFERENCES,
  isValidNotification,
  countByFilter,
  deriveNotifications,
  emptyStore,
  loadPreferences,
  loadStore,
  matchesFilter,
  savePreferences,
  saveStore,
  type DeriveInput,
} from "../notifications";

const DAY = 24 * 60 * 60 * 1000;
const now = new Date("2026-10-02T12:00:00Z");
const ago = (days: number) => new Date(now.getTime() - days * DAY).toISOString();
const ahead = (days: number) => new Date(now.getTime() + days * DAY).toISOString();

const proposal = (n: number, over: Partial<ProposalData> = {}) =>
  ({
    txHash: `${n}`.padStart(64, "a"),
    index: 0,
    type: "InfoAction",
    title: `Action ${n}`,
    createdDate: ago(20),
    expiryDate: ahead(20),
    ...over,
  }) as ProposalData;

const decided = (n: number, times: Partial<GovernanceActionRecord["status_times"]>) =>
  ({
    id: `d${n}`,
    tx_hash: `${n}`.padStart(64, "a"),
    index: 0,
    type: "TreasuryWithdrawals",
    title: `Decided ${n}`,
    time: ago(25),
    status: {
      ratified_epoch: times.ratified_time ? 1 : null,
      enacted_epoch: times.enacted_time ? 2 : null,
      dropped_epoch: null,
      expired_epoch: times.expired_time ? 3 : null,
    },
    status_times: {
      ratified_time: null,
      enacted_time: null,
      dropped_time: null,
      expired_time: null,
      ...times,
    },
  }) as GovernanceActionRecord;

const vote = (n: number, choice: "yes" | "no" | "abstain" = "yes") =>
  ({ vote: { vote: choice }, proposal: proposal(n) }) as unknown as VotedProposal;

const input = (over: Partial<DeriveInput> = {}): DeriveInput => ({
  delegation: { dRepHash: "h", dRepView: "drep1hazel", isDRepScriptBased: false, txHash: null },
  delegatedDRep: { view: "drep1hazel", status: DRepStatus.Active, givenName: "Hazel Pool" },
  votes: [],
  voteRelation: "dRepVoted",
  isVoter: false,
  live: [],
  decided: [],
  votingPower: 1_840_000_000_000,
  ...over,
});

// First visit 10 days ago: older test actions fall before it and make no digest.
const derive = (
  over: Partial<DeriveInput> = {},
  store = { ...emptyStore(ago(10)), lastDelegation: "drep1hazel" },
) =>
  deriveNotifications(input(over), store, DEFAULT_PREFERENCES, now);

describe("deriveNotifications", () => {
  it("says when the delegated DRep has retired or gone inactive", () => {
    const { items } = derive({
      delegatedDRep: { view: "drep1hazel", status: DRepStatus.Retired, givenName: "Hazel Pool" },
    });
    expect(items).toHaveLength(1);
    expect(items[0]).toMatchObject({
      id: "drep-drep1hazel-Retired",
      kind: "drepChange",
      read: false,
      data: { dRepName: "Hazel Pool", change: "retired", votingPower: 1_840_000_000_000 },
    });
  });

  it("keeps the time it first saw a condition, so the item does not move", () => {
    const retired = { view: "drep1hazel", status: DRepStatus.Retired, givenName: "Hazel Pool" };
    const first = derive({ delegatedDRep: retired });
    const later = deriveNotifications(
      input({ delegatedDRep: retired }),
      first.store,
      DEFAULT_PREFERENCES,
      new Date(now.getTime() + DAY),
    );
    expect(later.items[0].createdAt).toBe(first.items[0].createdAt);
  });

  it("records a delegation change once and keeps it in the history", () => {
    const first = derive({
      delegation: { dRepHash: "x", dRepView: "drep1other", isDRepScriptBased: false, txHash: null },
      delegatedDRep: { view: "drep1other", status: DRepStatus.Active, givenName: "Other" },
    });
    expect(first.items[0]).toMatchObject({
      kind: "delegationChange",
      data: { change: "delegated", target: { type: "drep", dRepId: "drep1other", dRepName: "Other" } },
    });
    const again = deriveNotifications(
      input({
        delegation: { dRepHash: "x", dRepView: "drep1other", isDRepScriptBased: false, txHash: null },
        delegatedDRep: { view: "drep1other", status: DRepStatus.Active, givenName: "Other" },
      }),
      first.store,
      DEFAULT_PREFERENCES,
      now,
    );
    expect(again.items.map((item) => item.id)).toEqual([first.items[0].id]);
  });

  it("names the new DRep once its details arrive after the change was recorded", () => {
    const delegation = { dRepHash: "x", dRepView: "drep1other", isDRepScriptBased: false, txHash: null };
    const first = derive({ delegation, delegatedDRep: undefined });
    expect(first.items[0].data).toMatchObject({ target: { dRepName: null } });
    const later = deriveNotifications(
      input({ delegation, delegatedDRep: { view: "drep1other", status: DRepStatus.Active, givenName: "Other" } }),
      first.store,
      DEFAULT_PREFERENCES,
      now,
    );
    expect(later.items[0].data).toMatchObject({ target: { dRepName: "Other" } });
  });

  it("records one change per delegation transaction, however often it is seen", () => {
    const other = { dRepHash: "x", dRepView: "drep1other", isDRepScriptBased: false, txHash: "tx-b" };
    const hazel = { dRepHash: "h", dRepView: "drep1hazel", isDRepScriptBased: false, txHash: "tx-a" };
    const step = (store: ReturnType<typeof emptyStore>, delegation: typeof other) =>
      deriveNotifications(input({ delegation }), store, DEFAULT_PREFERENCES, now).store;
    // A stale read of the old delegation between two reads of the new one.
    let store = step({ ...emptyStore(ago(10)), lastDelegation: "drep1hazel" }, other);
    store = step(store, hazel);
    store = step(store, other);
    const { items } = deriveNotifications(
      input({ delegation: other }),
      store,
      DEFAULT_PREFERENCES,
      now,
    );
    expect(items.filter((item) => item.id === "delegation-tx-b")).toHaveLength(1);
  });

  it("counts outcomes from before the first visit as already read", () => {
    const { items } = deriveNotifications(
      input({ votes: [vote(1)], decided: [decided(1, { ratified_time: ago(5) })] }),
      { ...emptyStore(ago(1)), lastDelegation: "drep1hazel" },
      DEFAULT_PREFERENCES,
      now,
    );
    expect(items[0]).toMatchObject({ kind: "actionActivity", read: true });
  });

  it("does not report the delegation it finds on the first visit", () => {
    const fresh = emptyStore(now.toISOString());
    const { items, store } = deriveNotifications(input(), fresh, DEFAULT_PREFERENCES, now);
    expect(items).toEqual([]);
    expect(store.lastDelegation).toBe("drep1hazel");
  });

  it("reports outcomes of actions voted on in the past 30 days", () => {
    const { items } = derive({
      votes: [vote(1, "yes"), vote(2), vote(3)],
      decided: [
        decided(1, { ratified_time: ago(2) }),
        decided(2, { expired_time: ago(40) }),
        decided(3, { ratified_time: ago(10), enacted_time: ago(1) }),
      ],
    });
    expect(items.map((item) => [item.kind, item.id])).toEqual([
      ["actionActivity", `activity-${"3".padStart(64, "a")}#0-enacted`],
      ["actionActivity", `activity-${"1".padStart(64, "a")}#0-ratified`],
    ]);
    expect(items[1].data).toMatchObject({ vote: "yes", relation: "dRepVoted", title: "Decided 1" });
  });

  it("reminds a voter of actions closing within 7 days that they have not voted on", () => {
    const { items } = derive({
      isVoter: true,
      voteRelation: "voted",
      votes: [vote(2)],
      live: [
        proposal(1, { expiryDate: ahead(5) }),
        proposal(2, { expiryDate: ahead(3) }),
        proposal(3, { expiryDate: ahead(9) }),
      ],
    });
    expect(items.map((item) => item.id)).toEqual([`deadline-${"1".padStart(64, "a")}#0`]);
    expect(items[0].createdAt).toBe(ahead(-2));
  });

  it("groups actions submitted since the first visit into one digest per week", () => {
    const { items } = derive({
      live: [
        proposal(1, { createdDate: "2026-09-29T10:00:00Z" }),
        proposal(2, { createdDate: "2026-10-01T10:00:00Z" }),
        proposal(3, { createdDate: "2026-09-23T10:00:00Z" }),
        proposal(4, { createdDate: "2026-07-01T10:00:00Z" }),
      ],
    });
    const digests = items.filter((item) => item.kind === "digest");
    expect(digests.map((item) => [item.id, (item.data as { total: number }).total])).toEqual([
      ["digest-2026-09-28", 2],
      ["digest-2026-09-21", 1],
    ]);
  });

  it("marks read items and leaves out switched-off events", () => {
    const store = { ...emptyStore(ago(60)), lastDelegation: "drep1hazel", readIds: ["drep-drep1hazel-Retired"] };
    const retired = { view: "drep1hazel", status: DRepStatus.Retired, givenName: "Hazel Pool" };
    const run = (prefs: typeof DEFAULT_PREFERENCES) =>
      deriveNotifications(input({ delegatedDRep: retired }), store, prefs, now).items;
    expect(run(DEFAULT_PREFERENCES)[0].read).toBe(true);
    const off = { ...DEFAULT_PREFERENCES, events: { ...DEFAULT_PREFERENCES.events, drepChange: "off" as const } };
    expect(run(off)).toEqual([]);
  });

  it("shows nothing while in-app notifications are off", () => {
    const off = {
      ...DEFAULT_PREFERENCES,
      channels: { ...DEFAULT_PREFERENCES.channels, inApp: false },
    };
    const retired = { view: "drep1hazel", status: DRepStatus.Retired, givenName: "Hazel Pool" };
    const { items } = deriveNotifications(
      input({ delegatedDRep: retired }),
      emptyStore(ago(60)),
      off,
      now,
    );
    expect(items).toEqual([]);
  });
});

describe("filters", () => {
  it("counts each filter, with overlaps as the design shows", () => {
    const { items } = derive({
      delegatedDRep: { view: "drep1hazel", status: DRepStatus.Inactive, givenName: null },
      votes: [vote(1)],
      decided: [decided(1, { ratified_time: ago(2) })],
    });
    expect(countByFilter(items)).toEqual({
      all: 2,
      unread: 2,
      governanceActions: 1,
      delegation: 0,
      drep: 1,
      myActivity: 1,
    });
    expect(matchesFilter(items[0], "all")).toBe(true);
  });
});

describe("storage", () => {
  beforeEach(() => localStorage.clear());

  it("keeps state and preferences per stake key and survives bad data", () => {
    saveStore("stakeA", { ...emptyStore(ago(1)), readIds: ["x"] });
    expect(loadStore("stakeA", now).readIds).toEqual(["x"]);
    expect(loadStore("stakeB", now).readIds).toEqual([]);
    localStorage.setItem("govtool_notifications_stakeA", "{broken");
    expect(loadStore("stakeA", now).readIds).toEqual([]);
    savePreferences("stakeA", { ...DEFAULT_PREFERENCES, channels: { ...DEFAULT_PREFERENCES.channels, inApp: false } });
    expect(loadPreferences("stakeA").channels.inApp).toBe(false);
    expect(loadPreferences("stakeB").channels.inApp).toBe(true);
  });
});

describe("isValidNotification", () => {
  const base = { id: "n", createdAt: "2026-10-01T00:00:00Z", read: false };

  it("keeps well-formed items of known kinds", () => {
    expect(isValidNotification({ ...base, kind: "system", data: { title: "T", body: "B", url: null } })).toBe(true);
    expect(
      isValidNotification({
        ...base,
        kind: "delegationChange",
        data: { change: "delegated", target: { type: "drep", dRepId: "d", dRepName: null }, votingPower: "5" },
      }),
    ).toBe(true);
  });

  it("drops unknown kinds and items missing what the screen reads", () => {
    expect(isValidNotification({ ...base, kind: "poll", data: {} })).toBe(false);
    expect(isValidNotification({ ...base, kind: "delegationChange", data: { change: "delegated" } })).toBe(false);
    expect(isValidNotification({ ...base, kind: "actionActivity", data: { actionId: 5 } })).toBe(false);
    expect(isValidNotification({ kind: "system", data: { title: "T", body: "B" } })).toBe(false);
    expect(isValidNotification(null)).toBe(false);
  });
});
