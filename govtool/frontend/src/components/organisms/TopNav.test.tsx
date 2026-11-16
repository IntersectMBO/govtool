import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
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

  it("shows the logo, the four nav items and Connect Wallet", () => {
    renderAt("/");
    expect(screen.getByTestId("logo-button")).toBeInTheDocument();
    expect(screen.getByTestId("connect-wallet-button")).toBeInTheDocument();
    ["Home", "DRep Directory", "Governance Actions", "Guides"].forEach(
      (label) => expect(screen.getByText(label)).toBeInTheDocument(),
    );
    expect(screen.queryByTestId("faqs-link")).toBeNull();
  });

  it("opens the wallet chooser from Connect Wallet", () => {
    renderAt("/");
    fireEvent.click(screen.getByTestId("connect-wallet-button"));
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "chooseWallet" });
  });

  it("highlights Governance Actions in orange on its pages", () => {
    renderAt("/governance_actions");
    expect(screen.getByTestId("governance-actions")).toHaveStyle({
      color: "#E9682F",
    });
    expect(screen.getByTestId("drep-directory-link")).toHaveStyle({
      color: "#667085",
    });
  });

  it("highlights Governance Actions on forum pages too", () => {
    renderAt("/proposal_discussion");
    expect(screen.getByTestId("governance-actions")).toHaveStyle({
      color: "#E9682F",
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

    it("keeps the wallet connected when the logo is clicked", () => {
      mocks.cardano.disconnectWallet.mockClear();
      renderAt("/connected/drep_directory");
      const logo = screen.getByTestId("logo-button");
      expect(logo).toHaveAttribute("href", "/dashboard");
      fireEvent.click(logo);
      expect(mocks.cardano.disconnectWallet).not.toHaveBeenCalled();
    });

    it("shows a DRep's voting power without opening anything", () => {
      mocks.voter = { isRegisteredAsDRep: true };
      renderAt("/dashboard");
      expect(screen.getByTestId("voting-power-chips")).toBeInTheDocument();
    });

    it("hides voting power for a wallet that is not a voter", () => {
      renderAt("/dashboard");
      expect(screen.queryByTestId("voting-power-chips")).toBeNull();
    });

    it("uses the connected nav ids", () => {
      renderAt("/dashboard");
      expect(screen.getByTestId("governance-actions-link")).toHaveAttribute(
        "href",
        "/connected/governance_actions",
      );
      expect(screen.getByTestId("dashboard-link")).toHaveAttribute(
        "href",
        "/dashboard",
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
    renderAt("/drep_directory");
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    expect(screen.getByTestId("drep-directory-link")).toHaveStyle({
      color: "#E9682F",
    });
    expect(screen.getByTestId("guides-link")).toBeInTheDocument();
    expect(screen.getByTestId("governance-actions")).toBeInTheDocument();
    expect(screen.queryByTestId("faqs-link")).toBeNull();
    expect(screen.queryByTestId("governance-actions-link")).toBeNull();
  });

  it("shows the wallet card in the drawer when connected", () => {
    mocks.cardano.isEnabled = true;
    mocks.cardano.stakeKey = "stake1u9abcdefghijklmnopqrstuvwxyz";
    renderAt("/dashboard");
    expect(screen.getByTestId("wallet-menu-button")).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    expect(screen.getByTestId("disconnect-button")).toBeInTheDocument();
  });
});

describe("TopNav in a wallet flow", () => {
  const LocationProbe = () => <span data-testid="location">{useLocation().pathname}</span>;
  const renderInFlow = (onNavigateAway: () => void) =>
    render(
      <MemoryRouter initialEntries={["/create_governance_action"]}>
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
    expect(screen.getByTestId("location")).toHaveTextContent("/create_governance_action");
  });

  it("asks before a nav link leaves the flow", () => {
    window.innerWidth = 1440;
    const onNavigateAway = vi.fn();
    renderInFlow(onNavigateAway);
    fireEvent.click(screen.getByTestId("drep-directory-link"));
    expect(onNavigateAway).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId("location")).toHaveTextContent("/create_governance_action");
  });

  it("asks before a drawer link leaves the flow", () => {
    window.innerWidth = 375;
    const onNavigateAway = vi.fn();
    renderInFlow(onNavigateAway);
    fireEvent.click(screen.getByTestId("open-drawer-button"));
    fireEvent.click(screen.getByTestId("drep-directory-link"));
    expect(onNavigateAway).toHaveBeenCalledTimes(1);
    expect(screen.getByTestId("location")).toHaveTextContent("/create_governance_action");
  });
});
