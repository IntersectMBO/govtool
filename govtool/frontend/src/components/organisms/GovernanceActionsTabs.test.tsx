import { beforeEach, describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import { GovernanceActionsTabs } from "./GovernanceActionsTabs";

const mocks = vi.hoisted(() => ({
  isForumEnabled: true,
  cardano: { isEnabled: false, stakeKey: undefined as string | undefined },
}));

vi.mock("@context", () => ({
  useCardano: () => mocks.cardano,
  useFeatureFlag: () => ({
    isProposalDiscussionForumEnabled: mocks.isForumEnabled,
  }),
}));

const renderAt = (path: string) =>
  render(
    <MemoryRouter initialEntries={[path]}>
      <GovernanceActionsTabs />
    </MemoryRouter>,
  );

describe("GovernanceActionsTabs", () => {
  beforeEach(() => {
    mocks.isForumEnabled = true;
    mocks.cardano.isEnabled = false;
    mocks.cardano.stakeKey = undefined;
  });

  it("shows all three tabs with the public ids", () => {
    renderAt("/actions");
    expect(
      screen.getByTestId("proposed-governance-actions-link"),
    ).toBeInTheDocument();
    expect(screen.getByTestId("governance-actions-link")).toHaveAttribute(
      "aria-current",
      "page",
    );
  });

  it("hides the Proposed tab while the forum is off", () => {
    mocks.isForumEnabled = false;
    renderAt("/actions");
    expect(screen.queryByTestId("proposed-governance-actions-link")).toBeNull();
  });

  it("uses the connected ids for a connected wallet", () => {
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abc";
    renderAt("/actions");
    expect(
      screen.getByTestId("governance-actions-live-voting-link"),
    ).toHaveAttribute("aria-current", "page");
  });
});
