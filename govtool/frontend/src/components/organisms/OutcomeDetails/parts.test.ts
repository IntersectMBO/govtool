import { describe, expect, it } from "vitest";

import type { GovernanceActionVoteAggregate } from "@/models/governanceActions";

import { groupVerdict } from "./parts";

const allAbstain: GovernanceActionVoteAggregate = {
  role: "drep",
  representation: "stake",
  yes: "0",
  no: "0",
  abstain: "500",
  notVoted: "0",
  totalEligible: "500",
  threshold: { numerator: 2, denominator: 3 },
};

describe("groupVerdict", () => {
  it("keeps the verdict when everyone abstained", () => {
    expect(groupVerdict(true, allAbstain)).toBe("fail");
    expect(groupVerdict(true, { ...allAbstain, passing: true })).toBe("pass");
  });

  it("has no verdict for a tally that does not add up", () => {
    expect(groupVerdict(true, { ...allAbstain, totalEligible: "1" })).toBeUndefined();
  });
});
