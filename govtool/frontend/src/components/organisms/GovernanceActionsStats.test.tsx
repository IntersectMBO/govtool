import { render, screen } from "@testing-library/react";
import { describe, expect, it, vi } from "vitest";

import "@/i18n";

import { GovernanceActionsStats } from "./GovernanceActionsStats";

const live = vi.hoisted(() => ({
  current: {
    liveProposals: [] as { expiryDate?: string }[],
    liveProposalsTotal: 0,
    areLiveProposalsLoading: false,
    isLiveProposalsError: false,
  },
}));

vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepDashboardActions: () => ({
    actions: [{ status: { enacted_epoch: 500 } }, { status: { enacted_epoch: null } }],
  }),
}));
vi.mock("@/hooks/queries/useGetLiveProposalsQuery", () => ({
  useGetLiveProposalsQuery: () => live.current,
}));

const value = (id: string) => screen.getByTestId(`governance-actions-stat-${id}`);

describe("GovernanceActionsStats", () => {
  it("shows the counts once the lists arrive", () => {
    live.current = { ...live.current, liveProposalsTotal: 0, isLiveProposalsError: false };
    render(<GovernanceActionsStats />);
    expect(value("total")).toHaveTextContent("2");
    expect(value("enacted")).toHaveTextContent("1");
    expect(value("votingNow")).toHaveTextContent("0");
  });

  it("shows a dash, not zero, when the live list fails", () => {
    live.current = { ...live.current, isLiveProposalsError: true };
    render(<GovernanceActionsStats />);
    expect(value("votingNow")).toHaveTextContent("—");
    expect(value("expiringSoon")).toHaveTextContent("—");
  });
});
