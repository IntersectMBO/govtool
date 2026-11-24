import { findActiveNavTo } from "..";

const GOVERNANCE_ACTIONS = ["/proposals", "/actions", "/actions/history"];

describe("findActiveNavTo", () => {
  it("picks History, not Live Voting, on the history page", () => {
    expect(findActiveNavTo("/actions/history", GOVERNANCE_ACTIONS)).toBe(
      "/actions/history",
    );
    expect(findActiveNavTo("/actions/history/abc", GOVERNANCE_ACTIONS)).toBe(
      "/actions/history",
    );
  });

  it("keeps Live Voting on its list and detail pages", () => {
    expect(findActiveNavTo("/actions", GOVERNANCE_ACTIONS)).toBe("/actions");
    expect(findActiveNavTo("/actions/gov_action1xyz", GOVERNANCE_ACTIONS)).toBe(
      "/actions",
    );
  });

  it("does not match on a shared name prefix", () => {
    expect(findActiveNavTo("/actions_other", GOVERNANCE_ACTIONS)).toBeNull();
  });

  it("matches nothing elsewhere, and ignores empty and external links", () => {
    expect(findActiveNavTo("/dreps", GOVERNANCE_ACTIONS)).toBeNull();
    expect(findActiveNavTo("/faq", ["", "https://docs.gov.tools"])).toBeNull();
  });
});
