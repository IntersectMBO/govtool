import { getShortenedGovActionId, getFullGovActionId } from "..";

describe("getShortenedGovActionId", () => {
  it("should return the correct shortened id for long hashes", () => {
    const txHash = "1234567890abcdef1234567890abcdef";
    const index = 5;
    const result = getShortenedGovActionId(txHash, index);
    expect(result).toBe("1234...cdef#5");
  });

  it("should handle hashes shorter than 6 characters correctly", () => {
    const txHash = "12345";
    const index = 2;
    const result = getShortenedGovActionId(txHash, index);
    expect(result).toBe("12345#2");
  });

  it("should handle hashes exactly 6 characters long by not shortening", () => {
    const txHash = "123456";
    const index = 3;
    const result = getShortenedGovActionId(txHash, index);
    expect(result).toBe("123456#3");
  });

  it("should handle an empty string for txHash correctly", () => {
    const txHash = "";
    const index = 1;
    const result = getShortenedGovActionId(txHash, index);
    expect(result).toBe("#1");
  });
});

describe("getFullGovActionId", () => {
  it("should return the full id with index", () => {
    const txHash = "1234567890abcdef1234567890abcdef";
    const index = 10;
    const result = getFullGovActionId(txHash, index);
    expect(result).toBe("1234567890abcdef1234567890abcdef#10");
  });
});
