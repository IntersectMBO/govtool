import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { RetireAsDirectVoter } from "./RetireAsDirectVoter";

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
vi.mock("@/context/wallet", () => ({
  useCardano: () => ({
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

const directVoter = { isRegisteredAsSoleVoter: true, deposit: 2_000_000 };

describe("RetireAsDirectVoter", () => {
  beforeEach(() => {
    mocks.pending = {};
    mocks.voter = directVoter;
    mocks.navigate.mockReset();
    mocks.submit.mockReset().mockResolvedValue("tx1");
  });

  it("explains retirement, then keeps the progress through the voter reload and lands on the dashboard", async () => {
    const { rerender } = render(<RetireAsDirectVoter />);
    expect(screen.getByRole("heading", { name: "Retire as Direct Voter?" })).toBeInTheDocument();
    expect(screen.getByText(/Your 2 ADA deposit will be refunded/)).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("continue-retirement-button"));
    expect(await screen.findByTestId("retirement-transaction-submitted-modal")).toHaveTextContent(
      "Direct Voter retirement in progress",
    );

    mocks.pending = { retireAsDirectVoter: { transactionHash: "tx1" } };
    mocks.voter = undefined;
    rerender(<RetireAsDirectVoter />);
    expect(screen.getByTestId("retirement-transaction-submitted-modal")).toBeInTheDocument();

    mocks.voter = { ...directVoter, isRegisteredAsSoleVoter: false };
    rerender(<RetireAsDirectVoter />);
    expect(screen.queryByTestId("wrong-route")).toBeNull();
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard", {
      replace: true,
      state: { retired: "directVoter" },
    });
  });

  it("goes to the dashboard without the success banner when the retirement expires", async () => {
    const { rerender } = render(<RetireAsDirectVoter />);
    fireEvent.click(screen.getByTestId("continue-retirement-button"));
    await screen.findByTestId("retirement-transaction-submitted-modal");
    mocks.pending = { retireAsDirectVoter: { transactionHash: "tx1" } };
    rerender(<RetireAsDirectVoter />);
    mocks.pending = {};
    rerender(<RetireAsDirectVoter />);
    expect(mocks.navigate).toHaveBeenCalledWith("/dashboard", { replace: true });
    expect(mocks.navigate).not.toHaveBeenCalledWith("/dashboard", expect.objectContaining({ state: expect.anything() }));
  });

  it("says a wallet that is not a direct voter cannot retire as one", () => {
    mocks.voter = { isRegisteredAsSoleVoter: false };
    render(<RetireAsDirectVoter />);
    expect(screen.getByTestId("wrong-route")).toBeInTheDocument();
  });
});
