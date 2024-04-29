import { bech32 } from "bech32";

import { bech32Validation } from "..";

describe("bech32 validation", () => {
  it("should return true for valid bech32 value", async () => {
    const result = await bech32Validation(bech32.encode("test", [1, 2, 3]));
    expect(result).toBe(true);
  });

  it("should return an error message for invalid bech32 value", async () => {
    const value = "invalidBech32Value";
    const result = await bech32Validation(value);
    expect(result).toBe("Invalid bech32 address");
  });
});
