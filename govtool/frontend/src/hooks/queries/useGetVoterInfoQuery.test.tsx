import { ReactNode } from "react";
import { renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { useGetVoterInfo } from "./useGetVoterInfoQuery";

type Wallet = {
  dRepID: string;
  stakeKey?: string;
  pendingTransaction: Record<string, unknown>;
};

const mocks = vi.hoisted(() => ({
  wallet: { dRepID: "", pendingTransaction: {} } as Wallet,
  getVoterInfo: vi.fn(async (dRepID: string) => ({ dRepID })),
}));
vi.mock("@context", () => ({ useCardano: () => mocks.wallet }));
vi.mock("@services", () => ({ getVoterInfo: mocks.getVoterInfo }));

const render = () => {
  // The app's defaults: a remounted query does not refetch on its own.
  const client = new QueryClient({
    defaultOptions: {
      queries: { refetchOnWindowFocus: false, refetchOnMount: false },
    },
  });
  const wrapper = ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={client}>{children}</QueryClientProvider>
  );
  return renderHook(() => useGetVoterInfo(), { wrapper });
};

beforeEach(() => {
  vi.clearAllMocks();
  window.localStorage.clear();
  mocks.wallet = { dRepID: "", pendingTransaction: {} };
});

afterEach(() => {
  vi.useRealTimers();
});

describe("useGetVoterInfo", () => {
  it("returns the new wallet's voter after switching wallets", async () => {
    mocks.wallet = { ...mocks.wallet, dRepID: "aa" };
    const hook = render();
    await waitFor(() => expect(hook.result.current.voter).toEqual({ dRepID: "aa" }));

    mocks.wallet = { ...mocks.wallet, dRepID: "bb" };
    hook.rerender();
    await waitFor(() => expect(hook.result.current.voter).toEqual({ dRepID: "bb" }));
    expect(mocks.getVoterInfo).toHaveBeenCalledWith("bb");
  });

  it("returns no voter once the wallet disconnects", async () => {
    mocks.wallet = { ...mocks.wallet, dRepID: "aa" };
    const hook = render();
    await waitFor(() => expect(hook.result.current.voter).toEqual({ dRepID: "aa" }));

    mocks.wallet = { ...mocks.wallet, dRepID: "" };
    hook.rerender();
    expect(hook.result.current.voter).toBeUndefined();
    expect(mocks.getVoterInfo).toHaveBeenCalledTimes(1);
  });

  it("does not fetch without a DRep ID", () => {
    const hook = render();
    expect(hook.result.current.voter).toBeUndefined();
    expect(mocks.getVoterInfo).not.toHaveBeenCalled();
  });

  describe("polling", () => {
    const calls = () => mocks.getVoterInfo.mock.calls.length;
    const wallet = { dRepID: "aa", stakeKey: "stake_test1" };

    it("does not poll a wallet with no registration under way", async () => {
      vi.useFakeTimers();
      mocks.wallet = { ...wallet, pendingTransaction: {} };
      render();
      await vi.waitFor(() => expect(calls()).toBe(1));

      await vi.advanceTimersByTimeAsync(5 * 60_000);
      expect(calls()).toBe(1);
    });

    it("polls while a registration is pending", async () => {
      vi.useFakeTimers();
      mocks.wallet = {
        ...wallet,
        pendingTransaction: { registerAsDrep: { transactionHash: "tx1" } },
      };
      render();
      await vi.waitFor(() => expect(calls()).toBe(1));

      await vi.advanceTimersByTimeAsync(20_000);
      expect(calls()).toBe(2);
    });

    it("polls for 30 minutes after a registration expired, then stops", async () => {
      vi.useFakeTimers();
      window.localStorage.setItem(
        "voter_transaction_expired_stake_test1",
        JSON.stringify(Date.now()),
      );
      mocks.wallet = { ...wallet, pendingTransaction: {} };
      render();
      await vi.waitFor(() => expect(calls()).toBe(1));

      await vi.advanceTimersByTimeAsync(20_000);
      expect(calls()).toBe(2);

      await vi.advanceTimersByTimeAsync(30 * 60_000);
      const afterWindow = calls();
      await vi.advanceTimersByTimeAsync(5 * 60_000);
      expect(calls()).toBe(afterWindow);
    });
  });
});
