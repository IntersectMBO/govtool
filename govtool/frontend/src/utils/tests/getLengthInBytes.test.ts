import { getLengthInBytes } from "..";

describe("getLengthInBytes", () => {
  it("returns correct byte length for ASCII characters", () => {
    const asciiStr = "Hello";
    expect(getLengthInBytes(asciiStr)).toBe(5);
  });

  it("returns correct byte length for multibyte characters", () => {
    const multibyteStr = "𩸽";
    expect(getLengthInBytes(multibyteStr)).toBe(4);
  });

  it("returns correct byte length for mixed characters", () => {
    const mixedStr = "Hello, 世界! 👋";
    expect(getLengthInBytes(mixedStr)).toBe(19);
  });

  it("returns 0 for an empty string", () => {
    const emptyStr = "";
    expect(getLengthInBytes(emptyStr)).toBe(0);
  });
});
