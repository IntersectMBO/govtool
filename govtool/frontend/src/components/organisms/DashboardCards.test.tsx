import { beforeEach, describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { DashboardCards } from "./DashboardCards";

const mocks = vi.hoisted(() => ({
  voter: {} as Record<string, unknown>,
  pending: {} as Record<string, unknown>,
}));
const { stub } = vi.hoisted(() => ({ stub: (name: string) => () => name }));

vi.mock("@context", () => ({
  useAppContext: () => ({ epochParams: { epoch_no: 524 } }),
  useCardano: () => ({ dRepID: "id", stakeKey: "e1ab", pendingTransaction: mocks.pending }),
  useFeatureFlag: () => ({ isFeatureAvailable: () => true }),
}));
vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useGetAdaHolderCurrentDelegationQuery: () => ({ currentDelegation: null }),
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: 0 }),
  useGetVoterInfo: () => ({ voter: mocks.voter }),
}));
vi.mock("./DashboardWalletCard", () => ({ DashboardWalletCard: stub("wallet") }));
vi.mock("./DashboardCards/DelegateDashboardCard", () => ({ DelegateDashboardCard: stub("delegate") }));
vi.mock("./DashboardCards/DRepDashboardCard", () => ({ DRepDashboardCard: stub("drep") }));
vi.mock("./DashboardCards/DirectVoterDashboardCard", () => ({ DirectVoterDashboardCard: stub("direct") }));
vi.mock("./DashboardCards/ListGovActionsDashboardCard", () => ({ ListGovActionsDashboardCards: stub("list") }));
vi.mock("./DashboardCards/ProposeGovActionDashboardCard", () => ({ ProposeGovActionDashboardCard: stub("propose") }));

const base = {
  isRegisteredAsDRep: false,
  wasRegisteredAsDRep: false,
  isRegisteredAsSoleVoter: false,
  votingPower: 0,
};

describe("DashboardCards", () => {
  beforeEach(() => {
    mocks.pending = {};
  });

  it("offers a direct voter to retire from the header", () => {
    mocks.voter = { ...base, isRegisteredAsSoleVoter: true };
    render(<MemoryRouter><DashboardCards /></MemoryRouter>);
    expect(screen.getByTestId("retire-direct-voter-header-button")).toHaveAttribute("href", "/my/direct-voter/retire");
  });

  it("hides it once the retire page reports the retirement", () => {
    mocks.voter = { ...base, isRegisteredAsSoleVoter: true };
    render(
      <MemoryRouter initialEntries={[{ pathname: "/dashboard", state: { retired: "directVoter" } }]}>
        <DashboardCards />
      </MemoryRouter>,
    );
    expect(screen.queryByTestId("retire-direct-voter-header-button")).toBeNull();
  });

  it("hides it while a retirement is pending, and for anyone else", () => {
    mocks.voter = { ...base, isRegisteredAsSoleVoter: true };
    mocks.pending = { retireAsDirectVoter: { transactionHash: "t" } };
    const { unmount } = render(<MemoryRouter><DashboardCards /></MemoryRouter>);
    expect(screen.queryByTestId("retire-direct-voter-header-button")).toBeNull();
    unmount();
    mocks.pending = {};
    mocks.voter = base;
    render(<MemoryRouter><DashboardCards /></MemoryRouter>);
    expect(screen.queryByTestId("retire-direct-voter-header-button")).toBeNull();
  });
});
