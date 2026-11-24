import { describe, expect, it } from "vitest";

import type { GovernanceActionRecord, GovernanceActionVoteAggregate } from "@/models/governanceActions";

import { MetadataValidationStatus } from "@/models/metadataValidation";

import {
  lifecycleEvents,
  outcomeContent,
  outcomeKey,
  roleSummary,
  treasuryWithdrawals,
  voterGroups,
} from "../outcomeDetails";

const NONE = {
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

const aggregate = (
  role: GovernanceActionVoteAggregate["role"],
  over: Partial<GovernanceActionVoteAggregate> = {},
): GovernanceActionVoteAggregate => ({
  role,
  representation: "stake",
  yes: "700",
  no: "100",
  abstain: "200",
  notVoted: "200",
  totalEligible: "1200",
  threshold: { numerator: 2, denominator: 3 },
  ...over,
});

const action = (over: Partial<GovernanceActionRecord> = {}) =>
  ({
    type: "TreasuryWithdrawals",
    time: "2026-07-31T10:00:00Z",
    epoch_no: 646,
    status: NONE,
    status_times: NO_TIMES,
    vote_aggregates: [aggregate("drep"), aggregate("cc", { representation: "count" })],
    proposal_params: null,
    ...over,
  }) as GovernanceActionRecord;

describe("voterGroups", () => {
  it("marks which roles vote on a treasury withdrawal", () => {
    const groups = voterGroups(action());
    expect(groups.map((group) => [group.role, group.applicable])).toEqual([
      ["drep", true],
      ["spo", false],
      ["cc", true],
    ]);
    expect(groups[0].aggregate?.role).toBe("drep");
  });

  it("has SPOs vote on a hard fork and the CC sit out a no confidence", () => {
    expect(voterGroups(action({ type: "HardForkInitiation" }))[1].applicable).toBe(true);
    expect(voterGroups(action({ type: "NoConfidence" }))[2].applicable).toBe(false);
  });
});

describe("roleSummary", () => {
  it("splits the non-abstaining stake into yes and the rest, with the threshold", () => {
    expect(roleSummary(aggregate("drep"))).toEqual({
      yes: 700,
      rest: 300,
      yesPercent: 70,
      restPercent: 30,
      thresholdPercent: 66.7,
      ratification: 1000,
      thresholdValue: "667",
      passing: true,
    });
  });

  it("rounds the yes needed up, so meeting it passes", () => {
    const cc = aggregate("cc", {
      representation: "count",
      yes: "3",
      no: "2",
      abstain: "1",
      notVoted: "0",
      totalEligible: "6",
    });
    // 3 of 5 is 60%, short of two thirds; 4 are needed.
    expect(roleSummary(cc)?.thresholdValue).toBe("4");
    expect(roleSummary(cc)?.passing).toBe(false);
  });

  it("keeps a percent tally's threshold as a fraction", () => {
    const spo = aggregate("spo", {
      representation: "percent",
      yes: "0.5",
      no: "0.3",
      abstain: "0.2",
      notVoted: "0",
      totalEligible: "1",
      threshold: { numerator: 67, denominator: 100 },
    });
    expect(Number(roleSummary(spo)?.thresholdValue)).toBeCloseTo(0.536, 6);
  });

  it("stays exact for stake beyond the safe integer range", () => {
    const drep = aggregate("drep", {
      yes: "9007199254740993",
      no: "0",
      abstain: "0",
      notVoted: "0",
      totalEligible: "9007199254740993",
      threshold: { numerator: 1, denominator: 1 },
    });
    expect(roleSummary(drep)?.thresholdValue).toBe("9007199254740993");
  });

  it("is undefined for an inconsistent tally", () => {
    expect(roleSummary(aggregate("drep", { totalEligible: "5" }))).toBeUndefined();
  });
});

describe("outcomeKey", () => {
  it("prefers enacted, then ratified, expired and dropped; live otherwise", () => {
    expect(outcomeKey(action({ status: { ...NONE, ratified_epoch: 650, enacted_epoch: 651 } }))).toBe("enacted");
    expect(outcomeKey(action({ status: { ...NONE, ratified_epoch: 650 } }))).toBe("ratified");
    expect(outcomeKey(action({ status: { ...NONE, expired_epoch: 653 } }))).toBe("expired");
    expect(outcomeKey(action({ status: { ...NONE, dropped_epoch: 653 } }))).toBe("dropped");
    expect(outcomeKey(action())).toBe("live");
  });
});

describe("lifecycleEvents", () => {
  it("lists submission and each dated status in order", () => {
    const events = lifecycleEvents(
      action({
        status: { ...NONE, ratified_epoch: 650, enacted_epoch: 651 },
        status_times: { ...NO_TIMES, ratified_time: "2026-08-20T00:00:00Z", enacted_time: "2026-08-25T00:00:00Z" },
      }),
    );
    expect(events.map((event) => [event.key, event.epoch])).toEqual([
      ["submitted", 646],
      ["ratified", 650],
      ["enacted", 651],
    ]);
  });
});

describe("treasuryWithdrawals", () => {
  it("lists each payout with its address and skips rows without one", () => {
    const description = [
      { receivingAddress: "stake1a", amount: 5000000 },
      { amount: 1 },
    ] as unknown as GovernanceActionRecord["description"];
    expect(treasuryWithdrawals(action({ description }))).toEqual([
      { receivingAddress: "stake1a", amount: 5000000 },
    ]);
    expect(treasuryWithdrawals(action({ type: "InfoAction", description }))).toEqual([]);
  });
});

describe("outcomeContent", () => {
  const document = {
    title: "Swapped title",
    abstract: "Swapped abstract",
    motivation: "Swapped motivation",
    rationale: "Swapped rationale",
    references: [{ "@type": "Other", label: "Swapped", uri: "https://example.com" }],
    authors: [{ name: "Swapped" }],
  };
  const unresolved = action({ json_metadata: null, title: undefined, abstract: undefined });

  it("shows the document once it passed its check", () => {
    const content = outcomeContent(unresolved, { metadataValid: true, data: document }, true);
    expect(content.abstract).toBe("Swapped abstract");
    expect(content.references).toHaveLength(1);
    expect(content.metadataStatus).toBeUndefined();
  });

  it("shows nothing from a document whose hash does not match", () => {
    const content = outcomeContent(
      unresolved,
      {
        metadataStatus: MetadataValidationStatus.INVALID_HASH,
        metadataValid: false,
        data: document,
      },
      false,
    );
    expect(content).toEqual({
      title: undefined,
      abstract: undefined,
      motivation: undefined,
      rationale: undefined,
      references: [],
      authors: undefined,
      metadataStatus: MetadataValidationStatus.INVALID_HASH,
    });
  });

  it("says the document was not checked when the check itself failed", () => {
    const content = outcomeContent({ ...unresolved, abstract: "From the record" }, undefined, false);
    expect(content.metadataStatus).toBe(MetadataValidationStatus.INTERNAL_ERROR);
    expect(content.abstract).toBe("From the record");
    expect(content.references).toEqual([]);
  });
});
