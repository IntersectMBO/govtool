import { act, renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { ReactNode } from "react";
import { MemoryRouter } from "react-router";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import {
  useDisconnectWallet,
  useOpenChooserAfterSwitch,
  useWalletBalance,
} from "./useConnectedWallet";

const mocks = vi.hoisted(() => ({
  getBalance: vi.fn(),
  disconnectWallet: vi.fn(),
  openModal: vi.fn(),
}));

vi.mock("@/context/wallet", () => ({
  useCardano: () => ({
    walletApi: { getBalance: mocks.getBalance },
    stakeKey: "e1ab",
    disconnectWallet: mocks.disconnectWallet,
  }),
}));
vi.mock("@/context/modal", () => ({
  useModal: () => ({ openModal: mocks.openModal }),
}));

// As main.tsx: a remount never refetches unless a query asks to.
const client = () =>
  new QueryClient({ defaultOptions: { queries: { refetchOnMount: false, retry: false } } });

const wrap = (queryClient: QueryClient) => ({ children }: { children: ReactNode }) => (
  <QueryClientProvider client={queryClient}>
    <MemoryRouter>{children}</MemoryRouter>
  </QueryClientProvider>
);

const reload = vi.fn();

beforeEach(() => {
  mocks.getBalance.mockReset().mockResolvedValue("1a3cea7b80");
  mocks.disconnectWallet.mockReset().mockResolvedValue(undefined);
  mocks.openModal.mockReset();
  reload.mockReset();
  vi.stubGlobal("location", { ...window.location, pathname: "/dashboard", reload });
  sessionStorage.clear();
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("useWalletBalance", () => {
  it("reads the balance again when the popup opens after it went stale", async () => {
    const queryClient = client();
    const first = renderHook(() => useWalletBalance(), { wrapper: wrap(queryClient) });
    await waitFor(() => expect(first.result.current.balance).toBe(1022000000));
    first.unmount();

    const later = Date.now() + 60_000;
    vi.spyOn(Date, "now").mockReturnValue(later);
    renderHook(() => useWalletBalance(), { wrapper: wrap(queryClient) });
    await waitFor(() => expect(mocks.getBalance).toHaveBeenCalledTimes(2));
  });
});

describe("switching wallets", () => {
  it("reloads like a disconnect, so no data of the old wallet survives", async () => {
    const { result } = renderHook(() => useDisconnectWallet(), { wrapper: wrap(client()) });
    await act(async () => {
      await result.current.switchWallet();
    });
    expect(mocks.disconnectWallet).toHaveBeenCalled();
    expect(reload).toHaveBeenCalled();
    expect(mocks.openModal).not.toHaveBeenCalled();
  });

  it("opens the wallet chooser once after that reload", async () => {
    const { result } = renderHook(() => useDisconnectWallet(), { wrapper: wrap(client()) });
    await act(async () => {
      await result.current.switchWallet();
    });
    renderHook(() => useOpenChooserAfterSwitch(), { wrapper: wrap(client()) });
    expect(mocks.openModal).toHaveBeenCalledWith({ type: "chooseWallet" });
    renderHook(() => useOpenChooserAfterSwitch(), { wrapper: wrap(client()) });
    expect(mocks.openModal).toHaveBeenCalledTimes(1);
  });
});
