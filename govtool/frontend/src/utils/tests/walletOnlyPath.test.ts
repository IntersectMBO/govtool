import { describe, expect, it } from "vitest";

import { isWalletOnlyPath } from "../walletOnlyPath";

describe("isWalletOnlyPath", () => {
  it("is true for the dashboard and intents", () => {
    [
      "/dashboard",
      "/actions/new",
      "/proposals/new",
      "/my/drep/register",
      "/my/stake-key",
      "/My/Drep/Edit/",
    ].forEach((path) => expect(isWalletOnlyPath(path)).toBe(true));
  });

  it("is false for pages with a public layout", () => {
    [
      "/",
      "/actions",
      "/actions/gov_action1x",
      "/actions/history",
      "/dreps/drep1x",
      "/proposals/12",
      "/budget",
      "/my/votes",
    ].forEach((path) => expect(isWalletOnlyPath(path)).toBe(false));
  });
});
