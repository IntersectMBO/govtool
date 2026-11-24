import { describe, expect, it } from "vitest";

import "@consts";
import { avatarTone, dRepInitials } from "./DRepCard";

describe("dRepInitials", () => {
  it("takes the first letters of the first two words", () => {
    expect(dRepInitials("Jordan  Dyer")).toBe("JD");
  });

  it("takes two letters of a single word", () => {
    expect(dRepInitials("eternl")).toBe("ET");
  });

  it("falls back to DR without a name", () => {
    expect(dRepInitials(null)).toBe("DR");
    expect(dRepInitials("  ")).toBe("DR");
  });
});

describe("avatarTone", () => {
  it("gives the same DRep the same colours", () => {
    expect(avatarTone("drep1abc")).toEqual(avatarTone("drep1abc"));
  });
});
