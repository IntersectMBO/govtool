import { ReactNode } from "react";
import { describe, expect, it } from "vitest";
import { render, renderHook, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import { useGovernanceActionTabs } from "@/hooks/useGovernanceActionTabs";

import { PageTabs } from "./PageTabs";

const at =
  (path: string) =>
  ({ children }: { children: ReactNode }) =>
    <MemoryRouter initialEntries={[path]}>{children}</MemoryRouter>;

describe("PageTabs", () => {
  const tabs = [
    { label: "Proposed", to: "/a", dataTestId: "tab-a", isActive: false },
    { label: "Live Voting", to: "/b", dataTestId: "tab-b", isActive: true },
  ];

  it("marks the active tab in brand blue", () => {
    render(<PageTabs tabs={tabs} />, { wrapper: at("/b") });
    const active = screen.getByTestId("tab-b");
    expect(active).toHaveAttribute("aria-current", "page");
    expect(active).toHaveStyle({ color: "#1447E6", fontWeight: "600" });
    expect(screen.getByTestId("tab-a")).toHaveStyle({ color: "#667085" });
  });

  it("never shows a vertical scrollbar on the tab row", () => {
    render(<PageTabs tabs={tabs} />, { wrapper: at("/b") });
    expect(screen.getByRole("navigation")).toHaveStyle({ overflowY: "hidden" });
  });

  it("names its navigation", () => {
    render(<PageTabs label="Governance Actions" tabs={tabs} />, { wrapper: at("/b") });
    expect(
      screen.getByRole("navigation", { name: "Governance Actions" }),
    ).toBeInTheDocument();
  });

  it("links each tab to its route", () => {
    render(<PageTabs tabs={tabs} />, { wrapper: at("/b") });
    expect(screen.getByTestId("tab-a")).toHaveAttribute("href", "/a");
  });
});

describe("useGovernanceActionTabs", () => {
  it("uses the connected routes and ids for a connected wallet", () => {
    const { result } = renderHook(() => useGovernanceActionTabs(true), {
      wrapper: at("/connected/governance_actions"),
    });
    const [proposed, live, outcomes] = result.current;
    expect(proposed.dataTestId).toBe("proposal-discussion-link");
    expect(live).toMatchObject({
      to: "/connected/governance_actions",
      dataTestId: "governance-actions-live-voting-link",
      isActive: true,
    });
    expect(outcomes.isActive).toBe(false);
  });

  it("uses the public routes and ids without a wallet", () => {
    const { result } = renderHook(() => useGovernanceActionTabs(false), {
      wrapper: at("/governance_actions/history"),
    });
    const [proposed, live, outcomes] = result.current;
    expect(proposed.dataTestId).toBe("proposed-governance-actions-link");
    expect(live).toMatchObject({
      to: "/governance_actions",
      dataTestId: "governance-actions-link",
      isActive: false,
    });
    expect(outcomes).toMatchObject({
      dataTestId: "governance-actions-governance-actions-link",
      isActive: true,
    });
  });

  it("marks Proposed active on a forum page", () => {
    const { result } = renderHook(() => useGovernanceActionTabs(false), {
      wrapper: at("/proposal_discussion/12"),
    });
    expect(result.current[0].isActive).toBe(true);
  });
});
