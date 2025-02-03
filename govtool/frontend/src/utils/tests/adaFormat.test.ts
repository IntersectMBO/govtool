import { vi, beforeAll, afterAll } from "vitest";

import {
  correctAdaFormat,
  correctVoteAdaFormat,
  correctDRepDirectoryFormat,
  correctAdaFormatWithSuffix,
} from "..";

describe("correctAdaFormat", () => {
  const LOVELACE = 1000000;
  const DECIMALS = 6;
  it("converts lovelace to ada for a given number", () => {
    const lovelace = 15000000;
    const expectedAda = 15;
    expect(correctAdaFormat(lovelace)).toBe(expectedAda);
  });

  it("returns 0 for undefined lovelace value", () => {
    const lovelace = undefined;
    expect(correctAdaFormat(lovelace)).toBe(0);
  });

  it("handles large lovelace values correctly", () => {
    const lovelace = 123456789012345;
    const expectedAda = lovelace / LOVELACE;
    expect(correctAdaFormat(lovelace)).toBe(expectedAda);
  });

  it("handles small lovelace values with correct rounding", () => {
    const lovelace = 123;
    const expectedAda = Number.parseFloat(
      (lovelace / LOVELACE).toFixed(DECIMALS),
    );
    expect(correctAdaFormat(lovelace)).toBe(expectedAda);
  });

  it("returns 0 for zero lovelace value", () => {
    const lovelace = 0;
    expect(correctAdaFormat(lovelace)).toBe(0);
  });
});

describe("correctVoteAdaFormat", () => {
  beforeAll(() => {
    vi.spyOn(Intl, "NumberFormat").mockImplementation(
      ((_locales?: string | string[], options?: Intl.NumberFormatOptions) =>
        new Intl.NumberFormat(
          "en-US",
          options,
        )) as unknown as typeof Intl.NumberFormat,
    );
  });

  afterAll(() => {
    vi.restoreAllMocks();
  });
  test("Correctly formats lovelace value to ada format", () => {
    const lovelace = 123456789012345;
    const expectedResult = "123,456,789.012";

    const result = correctVoteAdaFormat(lovelace, "en-US");

    // Normalize spaces in both result and expectedResult for comparison
    expect(result.replace(/\s/g, " ")).toBe(expectedResult);
  });

  test("Returns 0 for undefined lovelace value", () => {
    const lovelace = undefined;
    const expectedResult = "0";
    expect(correctVoteAdaFormat(lovelace, "en-US")).toBe(expectedResult);
  });

  test("Returns 0 for zero lovelace value", () => {
    const lovelace = 0;
    const expectedResult = "0";
    expect(correctVoteAdaFormat(lovelace, "en-US")).toBe(expectedResult);
  });

  test("Returns 0 for small lovelace value", () => {
    const lovelace = 123;
    const expectedResult = "0";
    expect(correctVoteAdaFormat(lovelace, "en-US")).toBe(expectedResult);
  });
});

describe("correctDRepDirectoryFormat", () => {
  test("Correctly formats lovelace value to directory format", () => {
    const lovelace = 143500000000;
    const expectedResult = "143,500";
    expect(correctDRepDirectoryFormat(lovelace)).toBe(expectedResult);
  });

  test("Returns 0 for numbers smaller than one million", () => {
    const lovelace = 1435;
    const expectedResult = "0";
    expect(correctDRepDirectoryFormat(lovelace)).toBe(expectedResult);
  });

  test("Returns result without comma", () => {
    const lovelace = undefined;
    const expectedResult = "0";
    expect(correctDRepDirectoryFormat(lovelace)).toBe(expectedResult);
  });
});

describe("correctAdaFormatWithSuffix", () => {
  test("Correctly formats lovelace value to ada format with suffix (T)", () => {
    const lovelace = 123456789012345;
    const expectedResult = "123.46M";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });

  test("Correctly formats lovelace value to ada format with suffix (B)", () => {
    const lovelace = 123456789012;
    const expectedResult = "123.46k";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });

  test("Correctly formats lovelace value to ada format with suffix (M)", () => {
    const lovelace = 123456789;
    const expectedResult = "123.46";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });

  test("Returns 0 for undefined lovelace value", () => {
    const lovelace = undefined;
    const expectedResult = "0";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });

  test("Returns 0 for zero lovelace value", () => {
    const lovelace = 0;
    const expectedResult = "0";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });

  test("Returns 0 for small lovelace value", () => {
    const lovelace = 123;
    const expectedResult = "0";
    expect(correctAdaFormatWithSuffix(lovelace)).toBe(expectedResult);
  });
});
