/* eslint-disable @typescript-eslint/no-explicit-any */
import { parseBoolean } from "../parseBoolean";

describe("parseBoolean", () => {
  it("should return true for 'true' (case insensitive)", () => {
    expect(parseBoolean("true")).toBe(true);
    expect(parseBoolean("TRUE")).toBe(true);
    expect(parseBoolean("TrUe")).toBe(true);
  });

  it("should return false for 'false' (case insensitive)", () => {
    expect(parseBoolean("false")).toBe(false);
    expect(parseBoolean("FALSE")).toBe(false);
    expect(parseBoolean("FaLsE")).toBe(false);
  });

  it("should return null for any other string", () => {
    expect(parseBoolean("yes")).toBeNull();
    expect(parseBoolean("no")).toBeNull();
    expect(parseBoolean("1")).toBeNull();
    expect(parseBoolean("0")).toBeNull();
    expect(parseBoolean("")).toBeNull();
  });

  it("should return null for non-string values", () => {
    expect(parseBoolean(null as any)).toBeNull();
    expect(parseBoolean(undefined as any)).toBeNull();
    expect(parseBoolean(123 as any)).toBeNull();
    expect(parseBoolean({} as any)).toBeNull();
  });
});
