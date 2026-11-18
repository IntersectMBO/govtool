import { describe, expect, it } from "vitest";

import { walletErrorReason } from "../walletErrorReason";

describe("walletErrorReason", () => {
  it("reads a plain string, a message or a CIP-30 info", () => {
    expect(walletErrorReason("user declined sign tx")).toBe("user declined sign tx");
    expect(walletErrorReason(new Error("not enough funds"))).toBe("not enough funds");
    expect(walletErrorReason({ code: 2, info: "user declined" })).toBe("user declined");
  });

  it("is empty when there is nothing to show", () => {
    expect(walletErrorReason(undefined)).toBe("");
    expect(walletErrorReason({})).toBe("");
  });
});
