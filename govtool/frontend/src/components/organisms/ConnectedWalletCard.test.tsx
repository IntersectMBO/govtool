import { fireEvent, render, screen } from "@testing-library/react";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";

import { compactAda, ConnectedWalletCard } from "./ConnectedWalletCard";

const mocks = vi.hoisted(() => ({
  balance: undefined as number | undefined,
  votingPower: undefined as number | undefined,
  confirmDisconnect: vi.fn(),
  switchWallet: vi.fn(),
}));

vi.mock("@/context/wallet", () => ({
  useCardano: () => ({ stakeKey: `e1${"ab".repeat(28)}` }),
}));
vi.mock("@/context/snackbar", () => ({
  useSnackbar: () => ({ addSuccessAlert: vi.fn() }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderVotingPowerQuery", () => ({
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: mocks.votingPower }),
}));
vi.mock("@/hooks/useConnectedWallet", () => ({
  useWalletBalance: () => ({ balance: mocks.balance }),
  useDisconnectWallet: () => ({
    confirmDisconnect: mocks.confirmDisconnect,
    switchWallet: mocks.switchWallet,
  }),
}));
vi.mock("@/utils/connectedWallet", () => ({
  connectedWallet: () => ({ name: "Lace", icon: undefined }),
}));

beforeEach(() => {
  mocks.balance = undefined;
  mocks.votingPower = undefined;
  mocks.confirmDisconnect.mockReset();
  mocks.switchWallet.mockReset();
});

describe("compactAda", () => {
  it("shortens lovelace to ada with a unit", () => {
    expect(compactAda(80_040_000_000_000)).toBe("₳ 80.04M");
    expect(compactAda(1_500_000)).toBe("₳ 1.5");
    expect(compactAda(undefined)).toBeUndefined();
    expect(compactAda(null)).toBeUndefined();
  });
});

describe("ConnectedWalletCard", () => {
  it("shows the wallet, its stake address, balance and voting power", () => {
    mocks.balance = 80_040_000_000_000;
    mocks.votingPower = 80_000_000_000_000;
    render(<ConnectedWalletCard onViewProfile={() => {}} />);
    expect(screen.getByText("Lace Wallet")).toBeInTheDocument();
    expect(screen.getAllByText(/^stake1u/).length).toBeGreaterThan(0);
    expect(screen.getByTestId("wallet-card-balance")).toHaveTextContent("80.04M");
    expect(screen.getByTestId("wallet-card-voting-power")).toHaveTextContent("80M");
  });

  it("shows a dash for a figure it could not read", () => {
    render(<ConnectedWalletCard onViewProfile={() => {}} />);
    expect(screen.getByTestId("wallet-card-balance")).toHaveTextContent("—");
  });

  it("switches wallet, opens the profile and disconnects", () => {
    const onViewProfile = vi.fn();
    render(<ConnectedWalletCard onViewProfile={onViewProfile} />);
    fireEvent.click(screen.getByRole("button", { name: "Switch wallet" }));
    fireEvent.click(screen.getByTestId("view-profile-button"));
    fireEvent.click(screen.getByTestId("disconnect-button"));
    expect(mocks.switchWallet).toHaveBeenCalled();
    expect(onViewProfile).toHaveBeenCalled();
    expect(mocks.confirmDisconnect).toHaveBeenCalled();
  });
});
