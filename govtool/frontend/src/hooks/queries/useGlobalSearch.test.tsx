import { ReactNode } from "react";
import { beforeEach, describe, expect, it, vi } from "vitest";
import { renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { useGlobalSearch } from "./useGlobalSearch";

const mocks = vi.hoisted(() => ({
  searchGovernanceActions: vi.fn(),
  searchDReps: vi.fn(),
  searchForumProposals: vi.fn(),
  getGovernanceActionHistory: vi.fn(),
  lookupStakeAddress: vi.fn(),
  getHandleDetails: vi.fn(),
}));

vi.mock("@services", () => ({
  searchGovernanceActions: mocks.searchGovernanceActions,
  searchDReps: mocks.searchDReps,
  searchForumProposals: mocks.searchForumProposals,
  getGovernanceActionHistory: mocks.getGovernanceActionHistory,
  lookupStakeAddress: mocks.lookupStakeAddress,
}));

vi.mock("@/services/AdaHandle", () => ({
  adaHandleService: { getHandleDetails: mocks.getHandleDetails },
}));

const STAKE = "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw";

const wrapper = ({ children }: { children: ReactNode }) => (
  <QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}>
    {children}
  </QueryClientProvider>
);

const page = (count: number, total: number) => ({
  elements: Array.from({ length: count }, (_, n) => ({ id: n })),
  total,
  page: 0,
  pageSize: 5,
});

describe("useGlobalSearch", () => {
  beforeEach(() => {
    Object.values(mocks).forEach((mock) => mock.mockReset());
    mocks.searchGovernanceActions.mockResolvedValue(page(5, 12));
    mocks.searchDReps.mockResolvedValue(page(2, 2));
    mocks.searchForumProposals.mockResolvedValue({ elements: [{ id: 1 }], total: 1 });
    mocks.getGovernanceActionHistory.mockResolvedValue(
      Array.from({ length: 5 }, (_, n) => ({ id: `${n}` })),
    );
    mocks.getHandleDetails.mockResolvedValue(null);
    mocks.lookupStakeAddress.mockResolvedValue({ stakeAddress: STAKE, isRegistered: true });
  });

  it("searches nothing for an empty or one-letter query", () => {
    renderHook(() => useGlobalSearch("t", { isForumEnabled: true }), { wrapper });
    expect(mocks.searchGovernanceActions).not.toHaveBeenCalled();
    expect(mocks.getHandleDetails).not.toHaveBeenCalled();
  });

  it("searches every text category and the one-word handle for a keyword", async () => {
    const { result } = renderHook(
      () => useGlobalSearch("treasury", { isForumEnabled: true }),
      { wrapper },
    );
    await waitFor(() => expect(result.current.actions.isLoading).toBe(false));
    await waitFor(() => expect(result.current.outcomes.isLoading).toBe(false));
    expect(mocks.searchGovernanceActions).toHaveBeenCalledWith("treasury", 0, 5);
    expect(mocks.searchDReps).toHaveBeenCalledWith("treasury", 0, 5);
    expect(mocks.searchForumProposals).toHaveBeenCalledWith("treasury", 1, 5);
    expect(mocks.getGovernanceActionHistory).toHaveBeenCalledWith(
      "treasury",
      [],
      "newestFirst",
      1,
      5,
    );
    expect(mocks.getHandleDetails).toHaveBeenCalledWith("treasury");
    expect(mocks.lookupStakeAddress).not.toHaveBeenCalled();
    expect(result.current.actions).toMatchObject({ total: 12, hasMore: true });
    expect(result.current.dreps).toMatchObject({ total: 2, hasMore: false });
    // The history endpoint gives no total; a full page means there may be more.
    expect(result.current.outcomes).toMatchObject({ total: undefined, hasMore: true });
    expect(result.current.addresses.isEnabled).toBe(false);
  });

  it("leaves the forum out while it is off", async () => {
    const { result } = renderHook(
      () => useGlobalSearch("treasury", { isForumEnabled: false }),
      { wrapper },
    );
    await waitFor(() => expect(result.current.actions.isLoading).toBe(false));
    expect(mocks.searchForumProposals).not.toHaveBeenCalled();
    expect(result.current.proposals.isEnabled).toBe(false);
  });

  it("looks up only the stake address for a pasted address", async () => {
    const { result } = renderHook(() => useGlobalSearch(STAKE, { isForumEnabled: true }), {
      wrapper,
    });
    await waitFor(() => expect(result.current.addresses.items).toHaveLength(1));
    expect(mocks.lookupStakeAddress).toHaveBeenCalledWith(STAKE);
    expect(mocks.searchGovernanceActions).not.toHaveBeenCalled();
    expect(mocks.getHandleDetails).not.toHaveBeenCalled();
  });

  it("loads the next page of a category", async () => {
    const { result } = renderHook(
      () => useGlobalSearch("treasury", { isForumEnabled: true }),
      { wrapper },
    );
    await waitFor(() => expect(result.current.actions.items).toHaveLength(5));
    result.current.actions.loadMore();
    await waitFor(() => expect(result.current.actions.items).toHaveLength(10));
    expect(mocks.searchGovernanceActions).toHaveBeenLastCalledWith("treasury", 1, 5);
  });

  it("reports a failed category without failing the others", async () => {
    mocks.searchDReps.mockRejectedValue(new Error("down"));
    const { result } = renderHook(
      () => useGlobalSearch("treasury", { isForumEnabled: true }),
      { wrapper },
    );
    // One retry, a second apart, before the error shows.
    await waitFor(() => expect(result.current.dreps.isError).toBe(true), { timeout: 3000 });
    await waitFor(() => expect(result.current.actions.items).toHaveLength(5));
    expect(result.current.actions.isError).toBe(false);
  });
});
