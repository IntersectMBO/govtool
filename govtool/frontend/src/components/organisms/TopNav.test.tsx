import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { MemoryRouter, useLocation } from "react-router";

import "@/i18n";

import { TopNav } from "./TopNav";

const mocks = vi.hoisted(() => ({
  cardano: {
    isEnabled: false,
    stakeKey: undefined as string | undefined,
    address: "addr1qxyz",
    disconnectWallet: vi.fn(),
    isEnableLoading: null as string | null,
  },
  openModal: vi.fn(),
  voter: undefined as
    | { isRegisteredAsDRep?: boolean; isRegisteredAsSoleVoter?: boolean }
    | undefined,
}));

vi.mock("@context", () => ({
  useCardano: () => mocks.cardano,
  useModal: () => ({ openModal: mocks.openModal }),
  useFeatureFlag: () => ({ isProposalDiscussionForumEnabled: true }),
}));

vi.mock("@hooks", async (importOriginal) => ({
  ...(await importOriginal<typeof import("@hooks")>()),
  useGetVoterInfo: () => ({ voter: mocks.voter }),
  useGetDRepVotingPowerQuery: () => ({ dRepVotingPower: 1_234_000_000 }),
}));

// The profile menu and the wallet card read through React Query; they have
// their own tests (ProfileMenu.test.tsx, ConnectedWalletCard.test.tsx).
vi.mock("./ProfileMenu", () => ({
  ProfileMenu: () => <button aria-label="Profile" data-testid="profile-avatar" type="button" />,
}));
vi.mock("@/context/wallet", () => ({ useCardano: () => mocks.cardano }));
vi.mock("@/hooks/useConnectedWallet", () => ({
  useOpenChooserAfterSwitch: () => {},
  useWalletBalance: () => ({ balance: undefined }),
  useDisconnectWallet: () => ({ confirmDisconnect: vi.fn(), switchWallet: vi.fn() }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderVotingPowerQuery", () => ({
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: undefined }),
}));
vi.mock("@/context/snackbar", () => ({
  useSnackbar: () => ({ addSuccessAlert: vi.fn() }),
}));

// The bell reads notifications through React Query; it has its own tests.
vi.mock("./Notifications/NotificationBell", () => ({
  NotificationBell: () => (
    <button aria-label="Notifications" data-testid="notifications-bell" type="button" />
  ),
}));

const renderAt = (path: string) =>
  render(
    <MemoryRouter initialEntries={[path]}>
      <TopNav />
    </MemoryRouter>,
  );

describe("TopNav (desktop)", () => {
  beforeEach(() => {
    window.innerWidth = 1440;
    mocks.cardano.isEnabled = false;
    mocks.cardano.stakeKey = undefined;
    mocks.voter = undefined;
  });

  it("shows the logo, the five nav items and Connect Wallet", () => {
    renderAt("/");
    expect(screen.getByTestId("logo-button")).toBeInTheDocument();
    expect(screen.getByTestId("connect-wallet-button")).toBeInTheDocument();
    [
      "Home",
      "DRep Directory",
      "Governance Actions",
      "Governance Activity",
      "Guides",
    ].forEach(
      (label) => expect(screen.getByText(label)).toBeInTheDocument(),
    );
    expect(screen.queryByTestId("faqs-link")).toBeNull();
  });

  it("starts with a link that moves focus to the main content", () => {
    render(
      <MemoryRouter initialEntries={["/"]}>
        <TopNav />
        <main>content</main>
      </MemoryRouter>,
    );
    const skip = screen.getByRole("link", { name: "Skip to main content" });
    expect(document.querySelectorAll("a, button")[0]).toBe(skip);
    fireEvent.click(skip);
    expect(document.activeElement).toBe(screen.getByRole("main"));
  });

  it("labels its navigation", () => {
    renderAt("/");
    expect(screen.getByRole("navigation", { name: "Main" })).toBeInTheDocument();
  });

  it("opens the wallet chooser from Connect Wallet", () => {
    renderAt("/");
    fireEvent.click(screen.getByTestId("connect-wallet-button"));
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "chooseWallet" });
  });

  it("highlights Governance Actions in orange on its pages", () => {
    renderAt("/actions");
    expect(screen.getByTestId("governance-actions")).toHaveStyle({
      color: "#BF4B20",
    });
    expect(screen.getByTestId("drep-directory-link")).toHaveStyle({
      color: "#667085",
    });
  });

  it("links Governance Activity and highlights only it on its page", () => {
    renderAt("/activity");
    expect(screen.getByTestId("governance-activity-link")).toHaveAttribute(
      "href",
      "/activity",
    );
    expect(screen.getByTestId("governance-activity-link")).toHaveStyle({
      color: "#BF4B20",
    });
    expect(screen.getByTestId("governance-actions")).toHaveStyle({
      color: "#667085",
    });
  });

  it("highlights Governance Actions on forum pages too", () => {
    renderAt("/proposals");
    expect(screen.getByTestId("governance-actions")).toHaveStyle({
      color: "#BF4B20",
    });
  });

  describe("with a wallet connected", () => {
    beforeEach(() => {
      mocks.cardano.isEnabled = true;
      mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    });

    it("shows the wallet pill instead of Connect Wallet", () => {
      renderAt("/dashboard");
      expect(screen.queryByTestId("connect-wallet-button")).toBeNull();
      const pill = screen.getByTestId("wallet-menu-button");
      expect(pill).toHaveTextContent("stake1u…xyz");
    });

    it("shows the profile menu's avatar and no voting power chip", () => {
      renderAt("/dashboard");
      expect(screen.getByTestId("profile-avatar")).toBeInTheDocument();
      expect(screen.queryByTestId("voting-power-chips")).toBeNull();
    });

    it("shows a hex stake key as its stake1 address", () => {
      mocks.cardano.stakeKey = `e1${"ab".repeat(28)}`;
      renderAt("/dashboard");
      expect(screen.getByTestId("wallet-menu-button")).toHaveTextContent(/stake1u…/);
    });

    it("closes the wallet menu when View profile opens the profile", async () => {
      renderAt("/dashboard");
      fireEvent.click(screen.getByTestId("wallet-menu-button"));
      fireEvent.click(screen.getByTestId("view-profile-button"));
      await waitFor(() => expect(screen.queryByTestId("view-profile-button")).toBeNull());
    });

    it("truncates a long label rather than wrapping the nav", () => {
      renderAt("/dashboard");
      expect(screen.getByText("stake1u…xyz")).toHaveStyle({
        maxWidth: "200px",
        textOverflow: "ellipsis",
      });
    });

    it("offers Disconnect from the wallet menu", () => {
      renderAt("/dashboard");
      fireEvent.click(screen.getByTestId("wallet-menu-button"));
      expect(screen.getByTestId("disconnect-button")).toBeInTheDocument();
    });

    it("points the wallet pill at the menu it opens", () => {
      renderAt("/dashboard");
      const pill = screen.getByTestId("wallet-menu-button");
      expect(pill).not.toHaveAttribute("aria-controls");
      fireEvent.click(pill);
      const id = pill.getAttribute("aria-controls");
      expect(id).toBeTruthy();
      expect(document.getElementById(id as string)).toContainElement(
        screen.getByTestId("disconnect-button"),
      );
    });

    it("keeps the wallet connected when the logo is clicked", () => {
      mocks.cardano.disconnectWallet.mockClear();
      renderAt("/dreps");
      const logo = screen.getByTestId("logo-button");
      expect(logo).toHaveAttribute("href", "/dashboard");
      fireEvent.click(logo);
      expect(mocks.cardano.disconnectWallet).not.toHaveBeenCalled();
    });

    it("hides voting power for a wallet that is not a voter", () => {
      renderAt("/dashboard");
      expect(screen.queryByTestId("voting-power-chips")).toBeNull();
    });

    it("shows the notifications bell to a connected wallet", () => {
      renderAt("/dashboard");
      expect(screen.getByTestId("notifications-bell")).toBeInTheDocument();
    });

    it("hides the bell and the search inside a wallet flow", () => {
      render(
        <MemoryRouter initialEntries={["/my/drep/register"]}>
          <TopNav onNavigateAway={vi.fn()} />
        </MemoryRouter>,
      );
      expect(screen.queryByTestId("notifications-bell")).toBeNull();
      expect(screen.queryByTestId("global-search-button")).toBeNull();
    });

    it("uses the connected nav ids", () => {
      renderAt("/dashboard");
      expect(screen.getByTestId("governance-actions-link")).toHaveAttribute(
        "href",
        "/actions",
      );
      expect(screen.getByTestId("dashboard-link")).toHaveAttribute(
        "href",
        "/dashboard",
      );
      expect(screen.getByTestId("governance-activity-link")).toHaveAttribute(
        "href",
        "/activity",
      );
    });
  });
});

describe("TopNav (mobile)", () => {
  beforeEach(() => {
    window.innerWidth = 375;
    mocks.cardano.isEnabled = false;
    mocks.cardano.stakeKey = undefined;
    mocks.voter = undefined;
  });

  it("collapses the nav into a round menu button", () => {
    renderAt("/");
    expect(screen.getByTestId("open-drawer-button")).toHaveStyle({
      borderRadius: "50%",
      height: "36px",
    });
    expect(screen.queryByTestId("drep-directory-link")).toBeNull();
  });

  it("lists the nav items in the drawer, without FAQ", () => {
    renderAt("/dreps");
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    expect(screen.getByTestId("drep-directory-link")).toHaveStyle({
      color: "#BF4B20",
    });
    expect(screen.getByTestId("guides-link")).toBeInTheDocument();
    expect(screen.getByTestId("governance-actions")).toBeInTheDocument();
    expect(screen.queryByTestId("faqs-link")).toBeNull();
    expect(screen.queryByTestId("governance-actions-link")).toBeNull();
  });

  it("keeps a phone's bar to search, bell and menu; the wallet is in the drawer", () => {
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    renderAt("/dashboard");
    expect(screen.queryByTestId("wallet-menu-button")).toBeNull();
    expect(screen.getByTestId("notifications-bell")).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    expect(screen.getByTestId("disconnect-button")).toBeInTheDocument();
  });

  it("keeps the wallet pill in a phone's bar inside a wallet flow", () => {
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    render(
      <MemoryRouter initialEntries={["/my/drep/register"]}>
        <TopNav onNavigateAway={vi.fn()} />
      </MemoryRouter>,
    );
    expect(screen.getByTestId("wallet-menu-button")).toBeInTheDocument();
  });

  it("shows the wallet pill in the bar on a tablet", () => {
    window.innerWidth = 600;
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    renderAt("/dashboard");
    expect(screen.getByTestId("wallet-menu-button")).toBeInTheDocument();
  });
});

describe("TopNav in a wallet flow", () => {
  const LocationProbe = () => <span data-testid="location">{useLocation().pathname}</span>;
  const renderInFlow = (onNavigateAway: () => void) =>
    render(
      <MemoryRouter initialEntries={["/actions/new"]}>
        <TopNav onNavigateAway={onNavigateAway} />
        <LocationProbe />
      </MemoryRouter>,
    );

  beforeEach(() => {
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    mocks.voter = undefined;
  });

  it("asks before the logo leaves the flow", () => {
    window.innerWidth = 1440;
    const onNavigateAway = vi.fn();
    renderInFlow(onNavigateAway);
    fireEvent.click(screen.getByTestId("logo-button"));
    expect(onNavigateAway).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId("location")).toHaveTextContent("/actions/new");
  });

  it("asks before a nav link leaves the flow", () => {
    window.innerWidth = 1440;
    const onNavigateAway = vi.fn();
    renderInFlow(onNavigateAway);
    fireEvent.click(screen.getByTestId("drep-directory-link"));
    expect(onNavigateAway).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId("location")).toHaveTextContent("/actions/new");
  });

  it("asks before a drawer link leaves the flow", () => {
    window.innerWidth = 375;
    const onNavigateAway = vi.fn();
    renderInFlow(onNavigateAway);
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    fireEvent.click(screen.getByTestId("drep-directory-link"));
    expect(onNavigateAway).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId("location")).toHaveTextContent("/actions/new");
  });
});
