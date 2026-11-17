import { ReactNode } from "react";
import { act, renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { DRepStatus } from "@/models";

import { useNotifications } from "./useNotifications";

const STAKE = "e1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

const mocks = vi.hoisted(() => ({
  config: undefined as unknown,
  page: { items: [], total: 0, unread: 0, counts: {} } as Record<string, unknown>,
  getNotificationsPage: vi.fn(),
  markNotificationsRead: vi.fn(),
  dRepStatus: "Retired",
}));

vi.mock("@context", () => ({
  useCardano: () => ({ isEnabled: true, stakeKey: STAKE, dRepID: "drep1me", walletApi: undefined }),
}));

vi.mock("@/services/requests/notifications", () => ({
  getNotificationsConfig: async () => mocks.config,
  getNotificationsPage: mocks.getNotificationsPage,
  markNotificationsRead: mocks.markNotificationsRead,
  markAllNotificationsRead: vi.fn(),
  getNotificationPreferences: vi.fn(),
  saveNotificationPreferences: vi.fn(),
  requestNotificationsNonce: vi.fn(),
  signInToNotifications: vi.fn(),
}));

vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({
  useGetVoterInfo: () => ({ voter: { isRegisteredAsDRep: false, isRegisteredAsSoleVoter: false } }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderCurrentDelegationQuery", () => ({
  useGetAdaHolderCurrentDelegationQuery: () => ({
    currentDelegation: { dRepHash: "h", dRepView: "drep1hazel", isDRepScriptBased: false, txHash: null },
  }),
}));
vi.mock("@/hooks/queries/useGetDrepDetailsQuery", () => ({
  useGetDRepDetailsQuery: (id?: string) => ({
    dRep: id ? { view: "drep1hazel", status: mocks.dRepStatus, givenName: "Hazel Pool" } : undefined,
  }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderVotingPowerQuery", () => ({
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: 5 }),
}));
vi.mock("@/hooks/queries/useGetDRepVotesByIdQuery", () => ({
  useGetDRepVotesByIdQuery: () => ({ dRepVotes: [] }),
}));
vi.mock("@/hooks/queries/useGetLiveProposalsQuery", () => ({
  useGetLiveProposalsQuery: () => ({ liveProposals: [] }),
}));
vi.mock("@/hooks/queries/useGetDecidedActionsQuery", () => ({
  useGetDecidedActionsQuery: () => ({ decidedActions: [] }),
}));

const wrapper = ({ children }: { children: ReactNode }) => (
  <QueryClientProvider client={new QueryClient({ defaultOptions: { queries: { retry: false } } })}>
    {children}
  </QueryClientProvider>
);

describe("useNotifications", () => {
  beforeEach(() => {
    localStorage.clear();
    sessionStorage.clear();
    mocks.config = undefined;
    mocks.dRepStatus = DRepStatus.Retired;
    mocks.getNotificationsPage.mockReset();
    mocks.markNotificationsRead.mockReset();
  });

  it("works out notifications in the browser while no service runs", async () => {
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.mode).toBe("local"));
    await waitFor(() => expect(result.current.items).toHaveLength(1));
    expect(result.current.items[0]).toMatchObject({ kind: "drepChange", read: false });
    expect(result.current.unread).toBe(1);
    expect(result.current.capabilities.email).toBe(false);
  });

  it("keeps read state in this browser", async () => {
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.items).toHaveLength(1));
    await act(() => result.current.markRead([result.current.items[0].id], true));
    await waitFor(() => expect(result.current.unread).toBe(0));
    const second = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(second.result.current.items).toHaveLength(1));
    expect(second.result.current.items[0].read).toBe(true);
  });

  it("leaves out an event switched off in the preferences", async () => {
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.items).toHaveLength(1));
    await act(() =>
      result.current.savePreferences({
        ...result.current.preferences,
        events: { ...result.current.preferences.events, drepChange: "off" },
      }),
    );
    await waitFor(() => expect(result.current.items).toHaveLength(0));
  });

  it("asks for a wallet sign-in once a service runs, without showing local data", async () => {
    mocks.config = {
      version: 1,
      channels: { inApp: true, email: true },
      events: [],
      frequencies: [],
    };
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.mode).toBe("remote"));
    expect(result.current.needsSignIn).toBe(true);
    expect(result.current.items).toEqual([]);
    expect(result.current.capabilities.email).toBe(true);
    expect(mocks.getNotificationsPage).not.toHaveBeenCalled();
  });

  it("reads the service's feed with a stored token and marks items read there", async () => {
    mocks.config = {
      version: 1,
      channels: { inApp: true, email: false },
      events: [],
      frequencies: [],
    };
    sessionStorage.setItem(
      `govtool_notifications_token_${STAKE}`,
      JSON.stringify({ token: "tok", expiresAt: "2999-01-01T00:00:00Z" }),
    );
    mocks.getNotificationsPage.mockResolvedValue({
      items: [{
        id: "n1",
        kind: "system",
        createdAt: "2026-10-01T00:00:00Z",
        read: false,
        data: { title: "Hi", body: "b", url: null },
      }],
      total: 1,
      unread: 1,
      counts: { all: 1, unread: 1, governanceActions: 0, delegation: 0, drep: 0, myActivity: 0 },
    });
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.items).toHaveLength(1));
    expect(result.current.needsSignIn).toBe(false);
    expect(mocks.getNotificationsPage).toHaveBeenCalledWith("tok", {
      page: 1,
      pageSize: 7,
      filter: "all",
      sort: "newest",
    });
    await act(() => result.current.markRead(["n1"], true));
    expect(mocks.markNotificationsRead).toHaveBeenCalledWith("tok", ["n1"], true);
  });
});
