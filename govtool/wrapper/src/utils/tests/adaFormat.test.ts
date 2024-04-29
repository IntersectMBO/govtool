import { correctAdaFormat } from "..";

describe("correctAdaFormat", () => {
  const LOVELACE = 1000000;
  const DECIMALS = 6;

  it("converts lovelace to ADA for a given number", () => {
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
