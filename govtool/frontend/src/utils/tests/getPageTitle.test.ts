import { describe, expect, it } from "vitest";

import { getPageTitle } from "../getPageTitle";

describe("getPageTitle", () => {
  it("names the history list and its records, not an action's details", () => {
    expect(getPageTitle("/governance_actions/history")).toBe(
      "Historical Actions",
    );
    expect(getPageTitle("/governance_actions/history/abc")).toBe(
      "Historical Actions",
    );
  });

  it("still names a governance action's own page", () => {
    expect(getPageTitle("/governance_actions/abc")).toBe(
      "Governance Action Details",
    );
  });
});
