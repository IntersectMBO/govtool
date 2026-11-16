import { describe, expect, it } from "vitest";

import "@consts";
import { nextDelegationStep } from "./DelegationReviewDialog";

const dRep = { drepId: "abc", view: "drep1abc" };
const base = {
  isDelegating: false,
  isNowDelegated: false,
  dRep,
};

describe("nextDelegationStep", () => {
  it("moves from signing to waiting once this delegation is pending", () => {
    expect(
      nextDelegationStep({ ...base, step: "signing", isDelegating: true, pendingResourceId: "abc", pendingTxHash: "tx" }),
    ).toBe("waiting");
  });

  it("closes when signing ends with nothing pending (the hook shows the error)", () => {
    expect(nextDelegationStep({ ...base, step: "signing" })).toBe("close");
  });

  it("keeps signing while the wallet is still open", () => {
    expect(nextDelegationStep({ ...base, step: "signing", isDelegating: true })).toBe("signing");
  });

  it("confirms when the pending transaction clears and the delegation matches", () => {
    expect(nextDelegationStep({ ...base, step: "waiting", isNowDelegated: true })).toBe("confirmed");
  });

  it("ends at not confirmed when the pending transaction clears without a match", () => {
    expect(nextDelegationStep({ ...base, step: "waiting" })).toBe("notConfirmed");
    expect(nextDelegationStep({ ...base, step: "takingLonger" })).toBe("notConfirmed");
  });

  it("says taking longer after a minute of waiting", () => {
    const pending = { pendingResourceId: "abc", pendingTxHash: "tx" };
    expect(nextDelegationStep({ ...base, ...pending, step: "waiting", waitedMs: 30_000 })).toBe("waiting");
    expect(nextDelegationStep({ ...base, ...pending, step: "waiting", waitedMs: 61_000 })).toBe("takingLonger");
  });

  it("ignores a pending transaction for another DRep", () => {
    expect(
      nextDelegationStep({ ...base, step: "signing", pendingResourceId: "other", pendingTxHash: "tx" }),
    ).toBe("close");
  });

  it("confirms after taking longer when the delegation shows up", () => {
    expect(nextDelegationStep({ ...base, step: "takingLonger", isNowDelegated: true })).toBe("confirmed");
  });
});
