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

  it("says taking longer when the pending transaction clears without a match", () => {
    expect(nextDelegationStep({ ...base, step: "waiting" })).toBe("takingLonger");
  });

  it("ignores a pending transaction for another DRep", () => {
    expect(
      nextDelegationStep({ ...base, step: "signing", pendingResourceId: "other", pendingTxHash: "tx" }),
    ).toBe("close");
  });

  it("confirms late if the delegation shows up after taking longer", () => {
    expect(nextDelegationStep({ ...base, step: "takingLonger", isNowDelegated: true })).toBe("confirmed");
  });
});
