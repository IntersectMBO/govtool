import { ReactNode } from "react";
import { renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { useGetVoterInfo } from "./useGetVoterInfoQuery";

const mocks = vi.hoisted(() => ({
  wallet: { dRepID: "", pendingTransaction: {} },
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
  mocks.wallet = { dRepID: "", pendingTransaction: {} };
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
});
