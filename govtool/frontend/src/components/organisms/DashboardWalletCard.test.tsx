import { afterEach, describe, expect, it, vi } from "vitest";
import { render, screen } from "@testing-library/react";
import { MemoryRouter } from "react-router";

import "@/i18n";

import { DashboardWalletCard } from "./DashboardWalletCard";

vi.mock("@context", () => ({ useSnackbar: () => ({ addSuccessAlert: vi.fn() }) }));

const ICON = "data:image/svg+xml;base64,PHN2Zy8+";

describe("DashboardWalletCard", () => {
  afterEach(() => {
    window.localStorage.clear();
    (window as unknown as { cardano?: unknown }).cardano = undefined;
  });

  it("shows the connected wallet's icon and name, and the stake address in bech32", () => {
    window.localStorage.setItem("wallet_data_name", JSON.stringify("lace"));
    (window as unknown as { cardano?: unknown }).cardano = { lace: { name: "lace", icon: ICON } };
    render(
      <MemoryRouter>
        <DashboardWalletCard currentDelegation={null} isDelegationKnown stakeKey={`e1${"ab".repeat(28)}`} votingPower={0} />
      </MemoryRouter>,
    );
    expect(screen.getByTestId("dashboard-wallet-icon")).toHaveAttribute("src", ICON);
    expect(screen.getByText("Lace Wallet")).toBeInTheDocument();
    expect(screen.getByTestId("dashboard-wallet-stake-key")).toHaveTextContent(/^stake1u/);
  });
});
