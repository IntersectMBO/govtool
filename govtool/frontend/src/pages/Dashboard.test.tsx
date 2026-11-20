import { beforeEach, describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter, Route, Routes } from "react-router";

import "@/i18n";

import { Dashboard } from "./Dashboard";

const mocks = vi.hoisted(() => ({
  cardano: {
    isEnabled: true,
    stakeKey: "stake1u9abcdefghijklmnopqrstuvwxyz" as string | undefined,
    address: "addr1qxyz",
    disconnectWallet: vi.fn(),
  },
  navigate: vi.fn(),
}));

vi.mock("@context", () => ({
  useCardano: () => mocks.cardano,
  useModal: () => ({ openModal: vi.fn() }),
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: true }),
  useChatwoot: () => ({ openFeedbackWindow: vi.fn() }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useGetVoterInfo: () => ({ voter: undefined }),
  useGetDRepVotingPowerQuery: () => ({ dRepVotingPower: undefined }),
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));

// The account popups read through React Query; they have their own tests.
vi.mock("@/components/organisms/ProfileMenu", () => ({
  ProfileMenu: () => <button aria-label="Profile" data-testid="profile-avatar" type="button" />,
}));
vi.mock("@/components/organisms/ConnectedWalletCard", () => ({
  ConnectedWalletCard: () => null,
}));

vi.mock("@/components/organisms/Notifications/NotificationBell", () => ({
  NotificationBell: () => (
    <button aria-label="Notifications" data-testid="notifications-bell" type="button" />
  ),
}));

const renderDashboard = () =>
  render(
    <MemoryRouter initialEntries={["/connected/drep_directory"]}>
      <Routes>
        <Route element={<Dashboard />}>
          <Route path="/connected/drep_directory" element={<div>page</div>} />
        </Route>
      </Routes>
    </MemoryRouter>,
  );

describe("Dashboard layout", () => {
  beforeEach(() => {
    window.innerWidth = 1440;
    window.scrollTo = vi.fn() as unknown as typeof window.scrollTo;
    localStorage.setItem("wallet_data_stake_key", JSON.stringify("stake1u9abc"));
    localStorage.setItem("wallet_data_name", JSON.stringify("demos"));
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    mocks.navigate.mockReset();
  });

  it("renders the top nav and footer around the page, with no sidebar", () => {
    renderDashboard();
    expect(screen.getByText("page")).toBeInTheDocument();
    expect(screen.getByTestId("wallet-menu-button")).toBeInTheDocument();
    expect(screen.getByRole("contentinfo")).toBeInTheDocument();
    expect(screen.getByRole("main")).toBeInTheDocument();
    expect(screen.getAllByTestId("logo-button")).toHaveLength(1);
    expect(screen.queryByTestId("disconnect-button")).toBeNull();
  });

  it("leaves for the public page when the wallet disconnects", () => {
    const { rerender } = renderDashboard();
    localStorage.clear();
    mocks.cardano.isEnabled = false;
    mocks.cardano.stakeKey = undefined;
    rerender(
      <MemoryRouter initialEntries={["/connected/drep_directory"]}>
        <Routes>
          <Route element={<Dashboard />}>
            <Route path="/connected/drep_directory" element={<div>page</div>} />
          </Route>
        </Routes>
      </MemoryRouter>,
    );
    expect(mocks.navigate).toHaveBeenCalled();
  });
});
