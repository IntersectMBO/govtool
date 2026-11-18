import { beforeEach, describe, expect, it, vi } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";

import "@/i18n";

import { ReRegisterIntro } from "./ReRegisterIntro";
import { WhatRetirementMeans } from "./WhatRetirementMeans";

const mocks = vi.hoisted(() => ({
  pending: {} as Record<string, unknown>,
  submit: vi.fn(),
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
vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({
  useGetVoterInfo: () => ({ voter: { deposit: 500_000_000, givenName: "MrDRep" } }),
}));
vi.mock("@/hooks/useWalletErrorModal", () => ({ useWalletErrorModal: () => vi.fn() }));

describe("WhatRetirementMeans", () => {
  beforeEach(() => {
    mocks.pending = {};
    mocks.submit.mockReset().mockResolvedValue("txhash");
  });

  it("asks for both acknowledgements before retiring", () => {
    render(<WhatRetirementMeans onClickCancel={vi.fn()} />);
    const go = screen.getByTestId("continue-retirement-button");
    expect(go).toBeDisabled();
    fireEvent.click(screen.getByTestId("retirement-ack-voting"));
    expect(go).toBeDisabled();
    fireEvent.click(screen.getByTestId("retirement-ack-deposit"));
    expect(go).toBeEnabled();
  });

  it("shows the retirement in progress, and goes to the dashboard once it confirms", async () => {
    const back = vi.fn();
    const { rerender } = render(<WhatRetirementMeans onClickCancel={back} />);
    fireEvent.click(screen.getByTestId("retirement-ack-voting"));
    fireEvent.click(screen.getByTestId("retirement-ack-deposit"));
    fireEvent.click(screen.getByTestId("continue-retirement-button"));
    const dialog = await screen.findByTestId("retirement-transaction-submitted-modal");
    expect(dialog).toHaveTextContent("MrDRep");
    expect(dialog).toHaveTextContent("₳500");
    expect(back).not.toHaveBeenCalled();

    mocks.pending = { retireAsDrep: { transactionHash: "txhash" } };
    rerender(<WhatRetirementMeans onClickCancel={back} />);
    expect(back).not.toHaveBeenCalled();
    mocks.pending = {};
    rerender(<WhatRetirementMeans onClickCancel={back} />);
    expect(back).toHaveBeenCalled();
  });

  it("lets the user leave the in-progress dialog for the dashboard", async () => {
    const back = vi.fn();
    render(<WhatRetirementMeans onClickCancel={back} />);
    fireEvent.click(screen.getByTestId("retirement-ack-voting"));
    fireEvent.click(screen.getByTestId("retirement-ack-deposit"));
    fireEvent.click(screen.getByTestId("continue-retirement-button"));
    fireEvent.click(await screen.findByTestId("confirm-modal-button"));
    expect(back).toHaveBeenCalled();
  });
});

describe("ReRegisterIntro", () => {
  it("continues to the profile form or goes back", () => {
    const onContinue = vi.fn();
    const onCancel = vi.fn();
    render(<ReRegisterIntro onCancel={onCancel} onContinue={onContinue} />);
    expect(screen.getByRole("heading", { name: "Register as a DRep again" })).toBeInTheDocument();
    fireEvent.click(screen.getByTestId("continue-register-again-button"));
    expect(onContinue).toHaveBeenCalled();
    fireEvent.click(screen.getByTestId("cancel-register-again-button"));
    expect(onCancel).toHaveBeenCalled();
  });
});
