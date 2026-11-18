import { beforeEach, describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { GovernanceProfilePage } from "./GovernanceProfile";

const STAKE = `e1${"ab".repeat(28)}`;
const mocks = vi.hoisted(() => ({
  voter: undefined as Record<string, unknown> | undefined,
  navigate: vi.fn(),
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));
vi.mock("@/context/wallet", () => ({ useCardano: () => ({ stakeKey: STAKE }) }));
vi.mock("@/context/snackbar", () => ({ useSnackbar: () => ({ addSuccessAlert: vi.fn() }) }));
vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({ useGetVoterInfo: () => ({ voter: mocks.voter }) }));
vi.mock("@/hooks/queries/useStakeKeyVotingPower", () => ({
  useStakeKeyVotingPowers: () => ({
    powers: { [STAKE]: 125_430_000_000 },
    isLoading: false,
    isError: false,
  }),
}));
vi.mock("@/utils/checkIsWalletConnected", () => ({ checkIsWalletConnected: () => true }));

const renderPage = () => render(<MemoryRouter><GovernanceProfilePage /></MemoryRouter>);

describe("GovernanceProfilePage", () => {
  beforeEach(() => {
    window.localStorage.setItem("wallet_data_name", JSON.stringify("lace"));
  });

  it("shows the wallet, stake key and voting power, and offers to become a DRep", () => {
    mocks.voter = {
      isRegisteredAsDRep: false,
      isRegisteredAsSoleVoter: false,
      wasRegisteredAsDRep: false,
    };
    renderPage();
    expect(screen.getByRole("heading", { name: "Your governance profile" })).toBeInTheDocument();
    expect(screen.getByTestId("profile-voting-power-value")).toHaveTextContent("₳ 125,430");
    expect(screen.getByTestId("profile-stake-address")).toHaveTextContent(/^stake1u/);
    expect(screen.getByText("DRep status — Not registered")).toBeInTheDocument();
    expect(screen.getByTestId("profile-become-drep")).toHaveAttribute("href", "/register_drep");
  });

  it("sends a registered DRep to its dashboard", () => {
    mocks.voter = { isRegisteredAsDRep: true };
    renderPage();
    expect(screen.getByText("DRep status — Registered")).toBeInTheDocument();
    expect(screen.getByTestId("profile-open-dashboard")).toHaveAttribute("href", "/dashboard");
    expect(screen.queryByTestId("profile-become-drep")).toBeNull();
  });
});
