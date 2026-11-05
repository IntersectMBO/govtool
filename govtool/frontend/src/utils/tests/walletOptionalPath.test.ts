import { describe, expect, it } from "vitest";

import { isWalletOptionalPath } from "../walletOptionalPath";

describe("isWalletOptionalPath", () => {
  it("is true for pages that work without a wallet", () => {
    [
      "/",
      "/actions",
      "/actions/gov_action1x",
      "/actions/history",
      "/dreps",
      "/dreps/drep1x",
      "/proposals/12",
      "/budget",
      "/my/votes",
    ].forEach((path) => expect(isWalletOptionalPath(path)).toBe(true));
  });

  it("is false for intents and the dashboard", () => {
    [
      "/dashboard",
      "/actions/new",
      "/proposals/new",
      "/my/drep/register",
      "/my/stake-key",
      "/actionsx",
    ].forEach((path) => expect(isWalletOptionalPath(path)).toBe(false));
  });
});
