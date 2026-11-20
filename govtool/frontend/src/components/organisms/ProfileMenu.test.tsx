import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";

import { requestProfileMenu } from "@/utils/profileMenu";

import { ProfileMenu } from "./ProfileMenu";

const mocks = vi.hoisted(() => ({
  voter: undefined as Record<string, unknown> | undefined,
  dRep: undefined as Record<string, unknown> | undefined,
  username: "",
  votes: [] as { proposal: { txHash: string; index: number } }[],
  live: [] as { txHash: string; index: number }[],
  confirmDisconnect: vi.fn(),
  forum: true,
}));

vi.mock("@/context/wallet", () => ({
  useCardano: () => ({
    stakeKey: `e1${"ab".repeat(28)}`,
    dRepID: "drep1xyz",
    pendingTransaction: {},
  }),
}));
vi.mock("@/context/featureFlag", () => ({
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: mocks.forum }),
}));
vi.mock("@/context/proposalDiscussion", () => ({
  useProposalDiscussion: () => ({ username: mocks.username }),
}));
vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({
  useGetVoterInfo: () => ({ voter: mocks.voter }),
}));
vi.mock("@/hooks/queries/useGetDrepDetailsQuery", () => ({
  useGetDRepDetailsQuery: () => ({ dRep: mocks.dRep }),
}));
vi.mock("@/hooks/queries/useDRepDashboard", () => ({
  useDRepDashboardVotes: (dRepID?: string) => ({ votes: dRepID ? mocks.votes : undefined }),
}));
vi.mock("@/hooks/queries/useGetLiveProposalsQuery", () => ({
  useGetLiveProposalsQuery: () => ({
    liveProposals: mocks.live,
    liveProposalsTotal: mocks.live.length,
    areLiveProposalsLoading: false,
  }),
}));
vi.mock("@/hooks/useConnectedWallet", () => ({
  useDisconnectWallet: () => ({ confirmDisconnect: mocks.confirmDisconnect }),
}));

const NOT_A_VOTER = {
  isRegisteredAsDRep: false,
  wasRegisteredAsDRep: false,
  isRegisteredAsSoleVoter: false,
};

const open = (path = "/dashboard") => {
  render(
    <MemoryRouter initialEntries={[path]}>
      <ProfileMenu />
    </MemoryRouter>,
  );
  fireEvent.click(screen.getByTestId("profile-avatar"));
  return screen.getByTestId("profile-menu");
};

beforeEach(() => {
  mocks.voter = { ...NOT_A_VOTER };
  mocks.dRep = undefined;
  mocks.username = "";
  mocks.votes = [];
  mocks.live = [];
  mocks.forum = true;
  mocks.confirmDisconnect.mockReset();
});

describe("ProfileMenu", () => {
  it("names a DRep, shows its status and links to its dashboard tabs", () => {
    mocks.voter = { ...NOT_A_VOTER, isRegisteredAsDRep: true, givenName: "Jordan Dyer" };
    mocks.dRep = { status: "Active" };
    const menu = open();
    expect(screen.getByTestId("profile-avatar")).toHaveTextContent("JD");
    expect(within(menu).getByTestId("profile-menu-name")).toHaveTextContent("Jordan Dyer");
    expect(within(menu).getByTestId("profile-role-drep")).toHaveTextContent("DRepActive");
    expect(within(menu).queryByTestId("profile-role-direct-voter")).toBeNull();
    expect(within(menu).getByTestId("profile-menu-overview")).toHaveAttribute("aria-current", "page");
    expect(within(menu).getByTestId("profile-menu-voting")).toHaveAttribute("href", "/dashboard?tab=governance");
    expect(within(menu).getByTestId("profile-menu-history")).toHaveAttribute("href", "/dashboard?tab=history");
  });

  it("shows no DRep status until it is known", () => {
    mocks.voter = { ...NOT_A_VOTER, isRegisteredAsDRep: true };
    mocks.dRep = undefined;
    expect(within(open()).getByTestId("profile-role-drep")).not.toHaveTextContent("Active");
  });

  it("closes when a row is chosen, even the current page's", async () => {
    fireEvent.click(within(open()).getByTestId("profile-menu-overview"));
    // The popover fades out before it leaves the page.
    await waitFor(() => expect(screen.queryByTestId("profile-menu")).toBeNull());
  });

  it("marks an inactive DRep", () => {
    mocks.voter = { ...NOT_A_VOTER, isRegisteredAsDRep: true };
    mocks.dRep = { status: "Inactive" };
    expect(within(open()).getByTestId("profile-role-drep")).toHaveTextContent("Inactive");
  });

  it("marks a retired DRep and offers no history log", () => {
    mocks.voter = { ...NOT_A_VOTER, wasRegisteredAsDRep: true };
    const menu = open();
    expect(within(menu).getByTestId("profile-role-drep")).toHaveTextContent("Retired");
    expect(within(menu).queryByTestId("profile-menu-history")).toBeNull();
  });

  it("shows a direct voter's role and what is left to vote on", () => {
    mocks.voter = { ...NOT_A_VOTER, isRegisteredAsSoleVoter: true };
    mocks.live = [{ txHash: "a", index: 0 }, { txHash: "b", index: 0 }, { txHash: "c", index: 0 }];
    mocks.votes = [{ proposal: { txHash: "a", index: 0 } }];
    const menu = open();
    expect(within(menu).getByTestId("profile-role-direct-voter")).toHaveTextContent("Active");
    expect(within(menu).getByTestId("profile-menu-voting")).toHaveAttribute("href", "/connected/governance_actions");
    expect(within(menu).getByRole("img", { name: "2 actions to vote on" })).toHaveTextContent("2");
  });

  it("leaves out the roles for someone who is neither, and names them by forum handle", () => {
    mocks.username = "jordan-d";
    const menu = open();
    expect(within(menu).queryByText("Governance roles")).toBeNull();
    // The forum name is the name; the handle is then the stake address.
    expect(within(menu).getByTestId("profile-menu-name")).toHaveTextContent("jordan-d");
    expect(within(menu).queryByText("@jordan-d")).toBeNull();
    expect(within(menu).getByText(/^stake1/)).toBeInTheDocument();
    expect(within(menu).queryByRole("img", { name: /to vote on/ })).toBeNull();
  });

  it("hides My proposals while the forum is off", () => {
    mocks.forum = false;
    expect(within(open()).queryByTestId("profile-menu-proposals")).toBeNull();
  });

  it("logs out through the disconnect confirmation", () => {
    fireEvent.click(within(open()).getByTestId("profile-menu-logout"));
    expect(mocks.confirmDisconnect).toHaveBeenCalled();
  });

  it("opens when another part of the page asks for the profile", () => {
    render(
      <MemoryRouter>
        <ProfileMenu />
      </MemoryRouter>,
    );
    expect(screen.queryByTestId("profile-menu")).toBeNull();
    act(() => {
      expect(requestProfileMenu()).toBe(true);
    });
    expect(screen.getByTestId("profile-menu")).toBeInTheDocument();
  });
});
