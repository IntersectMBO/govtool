import { describe, expect, it } from "vitest";
import type { GovernanceActionVoteAggregate } from "@/models";
import {
  formatVoteAggregateValue,
  voteAggregateResult,
} from "./voteAggregate";

const tally: GovernanceActionVoteAggregate = {
  role: "drep",
  representation: "stake",
  yes: "4503599627370496",
  no: "4503599627370497",
  abstain: "0",
  notVoted: "0",
  totalEligible: "9007199254740993",
  threshold: { numerator: 1, denominator: 2 },
};

describe("exact outcome threshold comparisons", () => {
  it("does not pass a tally one lovelace short of half, even when its displayed percent rounds to 50", () => {
    expect(voteAggregateResult(tally)).toMatchObject({
      passing: false,
      yesPercentage: 50,
    });
    expect(
      voteAggregateResult({ ...tally, yes: tally.no, no: tally.yes }),
    ).toMatchObject({ passing: true });
  });
  it("honours a zero threshold instead of substituting a majority", () => {
    expect(
      voteAggregateResult({
        ...tally,
        yes: "0",
        no: tally.totalEligible,
        threshold: { numerator: 0, denominator: 1 },
      }),
    ).toMatchObject({ passing: true });
  });
  it("treats a zero non-abstaining denominator as a zero ratio, as the ledger does", () => {
    const allAbstain = {
      ...tally,
      yes: "0",
      no: "0",
      abstain: tally.totalEligible,
    };
    expect(voteAggregateResult(allAbstain)).toMatchObject({
      passing: false,
      yesPercentage: undefined,
    });
    expect(
      voteAggregateResult({
        ...allAbstain,
        threshold: { numerator: 0, denominator: 1 },
      }),
    ).toMatchObject({ passing: true });
    expect(
      voteAggregateResult({ ...allAbstain, passing: true }),
    ).toMatchObject({ passing: true });
  });
  it("supports fractional percent aggregates without labelling them as stake", () => {
    const percent: GovernanceActionVoteAggregate = {
      ...tally,
      representation: "percent",
      yes: "0.4",
      no: "0.1",
      abstain: "0.2",
      notVoted: "0.3",
      totalEligible: "1",
    };
    expect(voteAggregateResult(percent)).toMatchObject({
      passing: true,
      yesPercentage: 50,
      ratification: "0.8",
    });
    expect(formatVoteAggregateValue("0.4", "percent")).toBe("40.00%");
  });
  it("rejects inconsistent or negative totals and invalid thresholds", () => {
    expect(voteAggregateResult({ ...tally, no: "0" })).toBeUndefined();
    expect(voteAggregateResult({ ...tally, yes: "-1" })).toBeUndefined();
    expect(
      voteAggregateResult({
        ...tally,
        threshold: { numerator: 1, denominator: 0 },
      }),
    ).toBeUndefined();
  });
  it("formats large stake without passing through a floating-point number", () => {
    expect(formatVoteAggregateValue("9007199254740993000001", "stake")).toBe(
      `₳ ${9007199254740994n.toLocaleString()}`,
    );
  });
});
