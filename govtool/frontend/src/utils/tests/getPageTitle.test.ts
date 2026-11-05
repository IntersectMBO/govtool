import { describe, expect, it } from "vitest";

import { getPageTitle } from "../getPageTitle";

describe("getPageTitle", () => {
  it("names the history list and its records, not an action's details", () => {
    expect(getPageTitle("/actions/history")).toBe("Historical Actions");
    expect(getPageTitle("/actions/history/abc")).toBe("Historical Actions");
  });

  it("names the new action form and a type's list", () => {
    expect(getPageTitle("/actions/new")).toBe("Create Governance Action");
    expect(getPageTitle("/actions", "?type=InfoAction")).toBe(
      "Governance Action Category",
    );
    expect(getPageTitle("/actions")).toBe("Governance Actions");
  });

  it("still names a governance action's own page", () => {
    expect(getPageTitle("/actions/abc")).toBe("Governance Action Details");
  });
});
