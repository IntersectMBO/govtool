import {
  getShortenedGovActionId,
  getFullGovActionId,
  getGovActionCIP129Id,
  getGovActionPath,
  getGovActionsTypePath,
  parseGovActionCIP129Id,
} from "..";

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

  it("should handle a hash starting with 'gov_action' correctly", () => {
    const txHash = "gov_action_1234567890abcdef";
    const index = 7;
    const result = getShortenedGovActionId(txHash, index);
    expect(result).toBe("gov_...cdef");
  });
});

describe("getFullGovActionId", () => {
  it("should return the full id with index", () => {
    const txHash = "1234567890abcdef1234567890abcdef";
    const index = 10;
    const result = getFullGovActionId(txHash, index);
    expect(result).toBe("1234567890abcdef1234567890abcdef#10");
  });

  it("should return the full id without index if txHash starts with 'gov_action'", () => {
    const txHash = "gov_action_1234567890abcdef";
    const index = 5;
    const result = getFullGovActionId(txHash, index);
    expect(result).toBe("gov_action_1234567890abcdef");
  });
});

describe("governance action URLs", () => {
  const TX = "ab".repeat(32);

  it("round-trips a transaction and index through the CIP-129 id", () => {
    const id = getGovActionCIP129Id(TX, 3);
    expect(id.startsWith("gov_action1")).toBe(true);
    expect(parseGovActionCIP129Id(id)).toEqual({ txHash: TX, index: 3 });
  });

  it("refuses what is not a gov_action id", () => {
    expect(parseGovActionCIP129Id(TX)).toBeNull();
    expect(parseGovActionCIP129Id("gov_action1notbech32")).toBeNull();
    expect(parseGovActionCIP129Id("drep1abc")).toBeNull();
  });

  it("builds an action's page and a type's list", () => {
    expect(getGovActionPath(TX, 0)).toBe(
      `/actions/${getGovActionCIP129Id(TX, 0)}`,
    );
    expect(getGovActionsTypePath("InfoAction")).toBe(
      "/actions?type=InfoAction",
    );
  });
});
