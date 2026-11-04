import { describe, expect, it } from "vitest";

import { getCurrentPath } from "../getCurrentPath";

describe("getCurrentPath", () => {
  it("maps legacy underscore paths to the current ones", () => {
    expect(getCurrentPath("/drep_directory")).toBe("/dreps");
    expect(getCurrentPath("/connected/drep_directory/drep1abc")).toBe(
      "/connected/dreps/drep1abc",
    );
    expect(getCurrentPath("/governance_actions/history/abc")).toBe(
      "/actions/history/abc",
    );
    expect(getCurrentPath("/governance_actions/category/InfoAction")).toBe(
      "/actions/category/InfoAction",
    );
    expect(
      getCurrentPath("/proposal_discussion/proposal_comment_review/hash"),
    ).toBe("/proposals/comment-review/hash");
    expect(getCurrentPath("/budget_discussion/12")).toBe("/budget/12");
    expect(getCurrentPath("/my/votes_and_favorites")).toBe("/my/votes");
    expect(getCurrentPath("/create_governance_action")).toBe("/create-action");
    expect(getCurrentPath("/stake_keys")).toBe("/stake-keys");
  });

  it("keeps ids that contain underscores", () => {
    expect(getCurrentPath("/governance_actions/gov_action1xyz")).toBe(
      "/actions/gov_action1xyz",
    );
  });

  it("returns null for current and unknown paths", () => {
    expect(getCurrentPath("/dreps")).toBeNull();
    expect(getCurrentPath("/actions/gov_action1xyz")).toBeNull();
    expect(getCurrentPath("/no_such_page")).toBeNull();
    expect(getCurrentPath("/constructor")).toBeNull();
  });
});
