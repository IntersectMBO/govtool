import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { act, fireEvent, render, screen, waitFor, within } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { ChooseStakeKeyPanel } from "../ChooseStakeKeyPanel";
import { ChooseWalletModal } from "../Modal/ChooseWalletModal";
import { WalletOptionButton } from "../../molecules/WalletOption";
import { DisconnectWalletModal } from "./DisconnectWalletModal";
import { WalletConnectionModal } from "./WalletConnectionModal";

const STAKE = `e1${"ab".repeat(28)}`;
const STAKE2 = `e1${"cd".repeat(28)}`;

const mocks = vi.hoisted(() => ({
  modalState: null as unknown,
  openModal: vi.fn(),
  closeModal: vi.fn(),
  navigate: vi.fn(),
  cardano: {} as Record<string, unknown>,
  voter: undefined as Record<string, unknown> | undefined,
  delegation: undefined as unknown,
  powers: {} as Record<string, number | undefined>,
  errors: {} as Record<string, boolean>,
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));
vi.mock("@/context/modal", () => ({
  useModal: () => ({
    state: mocks.modalState,
    openModal: mocks.openModal,
    closeModal: mocks.closeModal,
  }),
}));
vi.mock("@/context/wallet", () => ({ useCardano: () => mocks.cardano }));
vi.mock("@/context/appContext", () => ({ useAppContext: () => ({ network: "mainnet" }) }));
vi.mock("@/context/snackbar", () => ({ useSnackbar: () => ({ addSuccessAlert: vi.fn() }) }));
vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({ useGetVoterInfo: () => ({ voter: mocks.voter }) }));
vi.mock("@/hooks/queries/useGetAdaHolderCurrentDelegationQuery", () => ({
  useGetAdaHolderCurrentDelegationQuery: () => ({ currentDelegation: mocks.delegation }),
}));
vi.mock("@/hooks/queries/useStakeKeyVotingPower", () => ({
  useStakeKeyVotingPowers: () => ({
    powers: mocks.powers,
    errors: mocks.errors,
    isLoading: false,
    isError: Object.values(mocks.errors).some(Boolean),
  }),
}));

const inRouter = (node: React.ReactNode) => render(<MemoryRouter>{node}</MemoryRouter>);

afterEach(() => {
  vi.useRealTimers();
});

beforeEach(() => {
  mocks.modalState = null;
  mocks.openModal.mockReset();
  mocks.closeModal.mockReset();
  mocks.navigate.mockReset();
  mocks.cardano = { isEnabled: true, stakeKey: STAKE, dRepID: "ab".repeat(28), isEnableLoading: null };
  mocks.errors = {};
  mocks.voter = { isRegisteredAsDRep: false, isRegisteredAsSoleVoter: false };
  mocks.delegation = null;
  mocks.powers = { [STAKE]: 125_430_000_000 };
  window.localStorage.setItem("wallet_data_name", JSON.stringify("lace"));
  (window as unknown as { cardano?: unknown }).cardano = undefined;
});

describe("WalletConnectionModal", () => {
  it("checks the wallet, then sums it up and opens the profile menu or dashboard", () => {
    mocks.modalState = { view: "checking" };
    mocks.voter = undefined;
    const { rerender } = inRouter(<WalletConnectionModal />);
    expect(screen.getByText("Checking your wallet")).toBeInTheDocument();
    expect(screen.getByText("Governance status checked")).toBeInTheDocument();

    mocks.voter = { isRegisteredAsDRep: true };
    rerender(<MemoryRouter><WalletConnectionModal /></MemoryRouter>);
    expect(screen.getByTestId("wallet-connected-summary")).toHaveTextContent("Wallet connected");
    expect(screen.getByTestId("wallet-summary-votingPower")).toHaveTextContent("₳ 125,430");
    expect(screen.getByTestId("wallet-summary-network")).toHaveTextContent("Cardano Mainnet");
    expect(screen.getByTestId("wallet-summary-governance")).toHaveTextContent("Registered DRep");
    fireEvent.click(screen.getByTestId("view-your-profile-button"));
    expect(mocks.closeModal).toHaveBeenCalled();
    // No profile menu is on screen here, so it goes to the dashboard.
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard");
    expect(screen.getByTestId("confirm-modal-button")).toBeInTheDocument();
  });

  it("does not wait on a voter record GovTool cannot read", () => {
    mocks.modalState = { view: "checking" };
    mocks.voter = undefined;
    mocks.cardano = { ...mocks.cardano, dRepID: "" };
    inRouter(<WalletConnectionModal />);
    expect(screen.getByTestId("wallet-summary-governance")).toHaveTextContent("–");
  });

  it("stops checking after a while and shows what it has", () => {
    vi.useFakeTimers();
    mocks.modalState = { view: "checking" };
    mocks.voter = undefined;
    inRouter(<WalletConnectionModal />);
    expect(screen.getByText("Checking your wallet")).toBeInTheDocument();
    act(() => { vi.advanceTimersByTime(15_000); });
    expect(screen.getByTestId("wallet-summary-governance")).toHaveTextContent("–");
    vi.useRealTimers();
  });

  it("asks to switch network, keeping the wallet's message", () => {
    mocks.modalState = {
      view: "error",
      walletName: "Lace",
      error: "Error: You are trying to connect with a wallet connected to testnet. Please adjust your wallet settings to connect to mainnet or select a different wallet.",
    };
    inRouter(<WalletConnectionModal />);
    const modal = screen.getByTestId("wallet-connection-error-modal");
    expect(modal).toHaveTextContent("Switch to Cardano Mainnet");
    expect(modal).toHaveTextContent(/You are trying to connect/);
    fireEvent.click(screen.getByTestId("try-again-connect-button"));
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "chooseWallet" });
  });

  it("explains a declined request, an unsupported wallet and any other failure", () => {
    mocks.modalState = { view: "error", walletName: "Lace", error: "user declined" };
    const { rerender, unmount } = inRouter(<WalletConnectionModal />);
    expect(screen.getByText("Connection request declined")).toBeInTheDocument();
    mocks.modalState = { view: "error", walletName: "Flint", error: "Your wallet does not support CIP-30 extensions." };
    rerender(<MemoryRouter><WalletConnectionModal /></MemoryRouter>);
    expect(screen.getByText("Wallet is not supported")).toBeInTheDocument();
    unmount();
    mocks.modalState = { view: "error", walletName: "Lace", error: "No addresses found" };
    inRouter(<WalletConnectionModal />);
    expect(screen.getByText("Couldn't connect your wallet")).toBeInTheDocument();
    expect(screen.getByText("No addresses found")).toBeInTheDocument();
  });
});

describe("DisconnectWalletModal", () => {
  it("disconnects only on confirmation", async () => {
    const onConfirm = vi.fn();
    mocks.modalState = { onConfirm };
    inRouter(<DisconnectWalletModal />);
    expect(screen.getByRole("dialog", { name: "Disconnect Lace from GovTool?" })).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("cancel-disconnect-button"));
    expect(onConfirm).not.toHaveBeenCalled();
    fireEvent.click(screen.getByTestId("confirm-disconnect-button"));
    await waitFor(() => expect(onConfirm).toHaveBeenCalled());
  });
});

describe("ChooseWalletModal", () => {
  const lace = { icon: "data:image/svg+xml;base64,PHN2Zy8+", name: "lace", supportedExtensions: [{ cip: 95 }] };

  it("says when no Cardano wallet is installed", () => {
    inRouter(<ChooseWalletModal />);
    expect(screen.getByTestId("no-wallet-detected")).toHaveTextContent("Lace");
  });

  it("lists supported wallets, and waits on the one being approved", () => {
    (window as unknown as { cardano?: unknown }).cardano = { lace, other: { icon: "x", name: "Old", supportedExtensions: [{ cip: 30 }] } };
    const { rerender } = inRouter(<ChooseWalletModal />);
    expect(screen.getByTestId("lace-wallet-button")).toBeInTheDocument();
    expect(screen.queryByTestId("other-wallet-button")).toBeNull();
    mocks.cardano = { ...mocks.cardano, isEnableLoading: "lace" };
    rerender(<MemoryRouter><ChooseWalletModal /></MemoryRouter>);
    expect(screen.getByText("Approve in lace")).toBeInTheDocument();
    expect(screen.getByTestId("waiting-for-approval-button")).toBeDisabled();
  });
});

describe("WalletOptionButton", () => {
  const renderButton = () =>
    inRouter(<WalletOptionButton cip95Available dataTestId="lace-wallet-button" icon="x" label="Lace" name="lace" />);

  it("opens the failure state when the wallet refuses", async () => {
    mocks.cardano = { ...mocks.cardano, enable: vi.fn().mockRejectedValue({ status: "ERROR", error: "user declined" }) };
    renderButton();
    fireEvent.click(screen.getByTestId("lace-wallet-button"));
    await waitFor(() =>
      expect(mocks.openModal).toHaveBeenCalledWith({
        type: "walletConnection",
        state: { view: "error", error: "user declined", walletName: "Lace" },
      }));
  });

  it("checks the wallet after connecting, or asks for a stake key", async () => {
    mocks.cardano = { ...mocks.cardano, enable: vi.fn().mockResolvedValue({ status: "ok", stakeKey: true }) };
    const { unmount } = renderButton();
    fireEvent.click(screen.getByTestId("lace-wallet-button"));
    await waitFor(() => expect(mocks.openModal).toHaveBeenCalledWith({ type: "walletConnection", state: { view: "checking" } }));
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard", expect.anything());
    unmount();

    mocks.cardano = { ...mocks.cardano, enable: vi.fn().mockResolvedValue({ status: "ok", stakeKey: false }) };
    renderButton();
    fireEvent.click(screen.getByTestId("lace-wallet-button"));
    await waitFor(() => expect(mocks.navigate).toHaveBeenCalledWith("/stake_keys"));
    expect(mocks.closeModal).toHaveBeenCalled();
  });
});

describe("ChooseStakeKeyPanel", () => {
  it("moves between keys with the arrow keys, keeping Copy outside each radio", () => {
    mocks.cardano = {
      ...mocks.cardano,
      stakeKeys: [STAKE, STAKE2],
      setStakeKey: vi.fn(),
      disconnectWallet: vi.fn(),
    };
    inRouter(<ChooseStakeKeyPanel />);
    const radios = screen.getAllByRole("radio");
    expect(within(radios[0]).queryByRole("button")).toBeNull();
    fireEvent.keyDown(radios[0], { key: "ArrowDown" });
    expect(radios[1]).toHaveAttribute("aria-checked", "true");
    expect(screen.getByTestId("select-button")).toBeEnabled();
  });

  it("shows a dash when a key's voting power cannot be read", () => {
    mocks.cardano = {
      ...mocks.cardano,
      stakeKeys: [STAKE],
      setStakeKey: vi.fn(),
      disconnectWallet: vi.fn(),
    };
    mocks.powers = {};
    mocks.errors = { [STAKE]: true };
    inRouter(<ChooseStakeKeyPanel />);
    expect(screen.getByTestId(`${STAKE}-voting-power`)).toHaveTextContent("–");
  });

  it("shows each key's voting power and continues with the chosen one", () => {
    const setStakeKey = vi.fn();
    mocks.cardano = {
      ...mocks.cardano,
      stakeKeys: [STAKE, STAKE2],
      setStakeKey,
      disconnectWallet: vi.fn(),
    };
    mocks.powers = { [STAKE]: 125_430_000_000, [STAKE2]: 0 };
    inRouter(<ChooseStakeKeyPanel />);
    expect(screen.getByTestId(`${STAKE}-voting-power`)).toHaveTextContent("₳ 125,430");
    expect(screen.getByTestId("select-button")).toBeDisabled();
    fireEvent.click(screen.getByTestId(`${STAKE2}-radio`).querySelector("div") as Element);
    fireEvent.click(screen.getByTestId("select-button"));
    expect(setStakeKey).toHaveBeenCalledWith(STAKE2);
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard");
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "walletConnection", state: { view: "checking" } });
  });
});
