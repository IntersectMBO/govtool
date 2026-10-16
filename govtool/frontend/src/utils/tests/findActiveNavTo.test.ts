import { findActiveNavTo } from "..";

const GOVERNANCE_ACTIONS = [
  "/proposal_discussion",
  "/governance_actions",
  "/governance_actions/history",
];

describe("findActiveNavTo", () => {
  it("picks History, not Live Voting, on the history page", () => {
    expect(
      findActiveNavTo("/governance_actions/history", GOVERNANCE_ACTIONS),
    ).toBe("/governance_actions/history");
    expect(
      findActiveNavTo("/governance_actions/history/abc", GOVERNANCE_ACTIONS),
    ).toBe("/governance_actions/history");
  });

  it("keeps Live Voting on its list and detail pages", () => {
    expect(findActiveNavTo("/governance_actions", GOVERNANCE_ACTIONS)).toBe(
      "/governance_actions",
    );
    expect(
      findActiveNavTo("/governance_actions/gov_action1xyz", GOVERNANCE_ACTIONS),
    ).toBe("/governance_actions");
  });

  it("does not match on a shared name prefix", () => {
    expect(
      findActiveNavTo("/governance_actions_other", GOVERNANCE_ACTIONS),
    ).toBeNull();
  });

  it("matches nothing elsewhere, and ignores empty and external links", () => {
    expect(findActiveNavTo("/drep_directory", GOVERNANCE_ACTIONS)).toBeNull();
    expect(findActiveNavTo("/faq", ["", "https://docs.gov.tools"])).toBeNull();
  });
});
