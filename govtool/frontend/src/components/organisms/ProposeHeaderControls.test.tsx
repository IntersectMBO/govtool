import { fireEvent, render, screen } from "@testing-library/react";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";

import {
  type ForumProposeState,
  useProposeHeaderControls,
} from "./ProposeHeaderControls";

const mocks = vi.hoisted(() => ({ isEnabled: false, openModal: vi.fn() }));

vi.mock("@/context/wallet", () => ({
  useCardano: () => ({ isEnabled: mocks.isEnabled }),
}));
vi.mock("@/context/modal", () => ({
  useModal: () => ({ openModal: mocks.openModal }),
}));

const forum = (status: ForumProposeState["status"]): ForumProposeState => ({
  status,
  verify: vi.fn(),
  createUsername: vi.fn(),
  propose: vi.fn(),
});

const Harness = ({ state }: { state: ForumProposeState | null }) => {
  const { action, notice } = useProposeHeaderControls(state);
  return (
    <>
      {action}
      {notice}
    </>
  );
};

const button = () => screen.getByTestId("propose-a-governance-action-button");

beforeEach(() => {
  mocks.isEnabled = false;
  mocks.openModal.mockReset();
});

describe("useProposeHeaderControls", () => {
  it("asks a visitor without a wallet to connect one", () => {
    render(<Harness state={null} />);
    expect(button()).toBeEnabled();
    expect(screen.getByRole("status")).toHaveTextContent(
      "To submit a proposal, you need to connect a Cardano Wallet.",
    );
    fireEvent.click(screen.getByTestId("connect-wallet-link"));
    fireEvent.click(button());
    expect(mocks.openModal).toHaveBeenCalledTimes(2);
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "chooseWallet" });
  });

  it("waits quietly while the forum has not reported yet", () => {
    mocks.isEnabled = true;
    render(<Harness state={null} />);
    expect(button()).toBeDisabled();
    expect(screen.queryByRole("status")).toBeNull();
  });

  it("asks a connected wallet to sign in to the forum", () => {
    mocks.isEnabled = true;
    const state = forum("unverified");
    render(<Harness state={state} />);
    expect(button()).toBeDisabled();
    expect(screen.getByRole("status")).toHaveTextContent(
      "To submit a proposal, you need to verify yourself by signing a transaction.",
    );
    fireEvent.click(screen.getByTestId("verify-user-link"));
    expect(state.verify).toHaveBeenCalled();
  });

  it("asks a signed-in user without a display name to create one", () => {
    mocks.isEnabled = true;
    const state = forum("noUsername");
    render(<Harness state={state} />);
    expect(button()).toBeDisabled();
    fireEvent.click(screen.getByTestId("create-govtool-display-name-link"));
    expect(state.createUsername).toHaveBeenCalled();
  });

  it("proposes once ready, with no notice", () => {
    mocks.isEnabled = true;
    const state = forum("ready");
    render(<Harness state={state} />);
    expect(screen.queryByRole("status")).toBeNull();
    fireEvent.click(button());
    expect(state.propose).toHaveBeenCalled();
  });
});
