import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { RetireAsDrep } from "./RetireAsDrep";

const mocks = vi.hoisted(() => ({
  pending: {} as Record<string, unknown>,
  voter: undefined as Record<string, unknown> | undefined,
  submit: vi.fn(),
  navigate: vi.fn(),
}));

vi.mock("react-router", async (importOriginal) => ({
  ...(await importOriginal<typeof import("react-router")>()),
  useNavigate: () => mocks.navigate,
}));
vi.mock("@sentry/react", () => ({ captureException: vi.fn() }));
vi.mock("@/context/wallet", () => ({
  useCardano: () => ({
    isPendingTransaction: () => false,
    buildDRepRetirementCert: vi.fn(async () => "cert"),
    buildSignSubmitConwayCertTx: mocks.submit,
    pendingTransaction: mocks.pending,
  }),
}));
vi.mock("@/context/appContext", () => ({ useAppContext: () => ({ cExplorerBaseUrl: "https://cexplorer.io" }) }));
vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({ useGetVoterInfo: () => ({ voter: mocks.voter }) }));
vi.mock("@/hooks/useWalletErrorModal", () => ({ useWalletErrorModal: () => vi.fn() }));
vi.mock("@/utils/checkIsWalletConnected", () => ({ checkIsWalletConnected: () => true }));
vi.mock("@/components/molecules/CenteredBoxPageWrapper", () => ({
  CenteredBoxPageWrapper: ({ children }: { children: React.ReactNode }) => <div>{children}</div>,
}));
vi.mock("@/components/organisms/WrongRouteInfo", () => ({
  WrongRouteInfo: ({ title }: { title: string }) => <div data-testid="wrong-route">{title}</div>,
}));

const dRepVoter = { isRegisteredAsDRep: true, deposit: 500_000_000, givenName: "MrDRep" };

describe("RetireAsDrep", () => {
  beforeEach(() => {
    mocks.pending = {};
    mocks.voter = dRepVoter;
    mocks.navigate.mockReset();
    mocks.submit.mockReset().mockResolvedValue("tx1");
  });

  it("keeps the progress dialog while the voter reloads, then goes to the dashboard", async () => {
    const { rerender } = render(<RetireAsDrep />);
    fireEvent.click(screen.getByTestId("retirement-ack-voting"));
    fireEvent.click(screen.getByTestId("retirement-ack-deposit"));
    fireEvent.click(screen.getByTestId("continue-retirement-button"));
    await screen.findByTestId("retirement-transaction-submitted-modal");

    // The pending hash is in the voter query key, so the voter reloads.
    mocks.pending = { retireAsDrep: { transactionHash: "tx1" } };
    mocks.voter = undefined;
    rerender(<RetireAsDrep />);
    expect(screen.getByTestId("retirement-transaction-submitted-modal")).toHaveTextContent("MrDRep");

    // Confirmation writes the retired voter before the pending state clears.
    mocks.voter = { ...dRepVoter, isRegisteredAsDRep: false };
    rerender(<RetireAsDrep />);
    expect(screen.queryByTestId("wrong-route")).not.toBeInTheDocument();
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard");
  });

  it("shows a retirement still pending after a reload", () => {
    mocks.pending = { retireAsDrep: { transactionHash: "tx9" } };
    render(<RetireAsDrep />);
    expect(screen.getByTestId("retirement-transaction-submitted-modal")).toBeInTheDocument();
  });

  it("says a wallet that is not a DRep cannot retire", () => {
    mocks.voter = { ...dRepVoter, isRegisteredAsDRep: false };
    render(<RetireAsDrep />);
    expect(screen.getByTestId("wrong-route")).toBeInTheDocument();
  });
});
