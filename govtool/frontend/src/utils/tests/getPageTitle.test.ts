import { describe, expect, it } from "vitest";

import { getPageTitle } from "../getPageTitle";

describe("getPageTitle", () => {
  it("names the history list and its records, not an action's details", () => {
    expect(getPageTitle("/actions/history")).toBe(
      "Historical Actions",
    );
    expect(getPageTitle("/actions/history/abc")).toBe(
      "Historical Actions",
    );
  });

  it("still names a governance action's own page", () => {
    expect(getPageTitle("/actions/abc")).toBe(
      "Governance Action Details",
    );
  });
});
