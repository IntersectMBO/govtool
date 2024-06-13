import i18n from "@/i18n";
import { isValidURLFormat, isValidHashFormat, isRewardAddress } from "..";

describe("isValidURLFormat", () => {
  it("returns true for valid HTTP URLs", () => {
    const validHttpUrl = "http://example.com";
    expect(isValidURLFormat(validHttpUrl)).toBe(true);
  });

  it("returns true for valid HTTPS URLs", () => {
    const validHttpsUrl = "https://example.com";
    expect(isValidURLFormat(validHttpsUrl)).toBe(true);
  });

  it("returns true for valid URL without protocol", () => {
    const validUrl = "example.com";
    expect(isValidURLFormat(validUrl)).toBe(true);
  });

  it("returns true for valid HTTPS URLs with IP", () => {
    const validHttpsUrl = "http://192.168.0.1/resoruce";
    expect(isValidURLFormat(validHttpsUrl)).toBe(true);
  });

  it("returns true for valid IPFS URLs", () => {
    const validIpfsUrl =
      "ipfs://c94ae10c7bbc2632f051cadcec61e24a954aa1d61173597b21c03534";
    expect(isValidURLFormat(validIpfsUrl)).toBe(true);
  });

  it("returns false for invalid URLs", () => {
    const invalidUrl = "htp:/example.com";
    expect(isValidURLFormat(invalidUrl)).toBe(false);
  });

  it("returns false for invalid URLs without domain", () => {
    const invalidUrl = "https://invalid";
    expect(isValidURLFormat(invalidUrl)).toBe(false);
  });

  it("returns false for strings that are not URLs", () => {
    const notUrl = "Just a string";
    expect(isValidURLFormat(notUrl)).toBe(false);
  });

  it("returns false for empty string", () => {
    const empty = "";
    expect(isValidURLFormat(empty)).toBe(false);
  });
});

describe("isValidHashFormat", () => {
  it("returns true for valid hexadecimal strings", () => {
    const validHash = "1a3B";
    expect(isValidHashFormat(validHash)).toBe(true);
  });

  it("returns false for non-hexadecimal strings", () => {
    const invalidHash = "GHIJKLMNOP";
    expect(isValidHashFormat(invalidHash)).toBe(false);
  });

  it("returns false for hexadecimal strings with spaces", () => {
    const invalidHash = "1a 3B";
    expect(isValidHashFormat(invalidHash)).toBe(false);
  });

  it("returns false for empty string", () => {
    const empty = "";
    expect(isValidHashFormat(empty)).toBe(false);
  });
});

describe("isRewardAddress", () => {
  it("returns true for stake address", async () => {
    const validStake =
      "stake_test1urmj8g9d09pdhrzxcuhhfkx2rxmldnrzyumyt90qvmurrpgg4c5zj";
    const result = await isRewardAddress(validStake);
    expect(result).toBe(true);
  });

  it("returns error for empty string", async () => {
    const invalid = "";
    const result = await isRewardAddress(invalid);
    expect(result).toBe(i18n.t("forms.errors.mustBeStakeAddress"));
  });

  it("returns error for another bech32 string", async () => {
    const invalid = "drep1l8uyy66sm8u82h82gc8hkcy2xu24dl8ffsh58aa0v7d37yp48u8";
    const result = await isRewardAddress(invalid);
    expect(result).toBe(i18n.t("forms.errors.mustBeStakeAddress"));
  });
});
