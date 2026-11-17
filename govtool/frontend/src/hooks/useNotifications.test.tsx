import { ReactNode } from "react";
import { act, renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { beforeEach, describe, expect, it, vi } from "vitest";

import { DRepStatus } from "@/models";

import { useNotifications } from "./useNotifications";

const STAKE = "e1aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

const mocks = vi.hoisted(() => ({
  cardano: {} as Record<string, unknown>,
  signData: vi.fn(),
  requestNotificationsNonce: vi.fn(),
  signInToNotifications: vi.fn(),
  config: undefined as unknown,
  page: { items: [], total: 0, unread: 0, counts: {} } as Record<string, unknown>,
  getNotificationsPage: vi.fn(),
  markNotificationsRead: vi.fn(),
  dRepStatus: "Retired",
}));

vi.mock("@context", () => ({
  useCardano: () => mocks.cardano,
}));

vi.mock("@/services/requests/notifications", () => ({
  getNotificationsConfig: async () => mocks.config,
  getNotificationsPage: mocks.getNotificationsPage,
  markNotificationsRead: mocks.markNotificationsRead,
  markAllNotificationsRead: vi.fn(),
  getNotificationPreferences: vi.fn(),
  saveNotificationPreferences: vi.fn(),
  requestNotificationsNonce: mocks.requestNotificationsNonce,
  signInToNotifications: mocks.signInToNotifications,
  getClosingProposals: async () => ({ elements: [], total: 0 }),
}));

vi.mock("@/hooks/queries/useGetVoterInfoQuery", () => ({
  useGetVoterInfo: () => ({ voter: { isRegisteredAsDRep: false, isRegisteredAsSoleVoter: false } }),
}));
vi.mock("@/hooks/queries/useGetAdaHolderCurrentDelegationQuery", () => ({
  useGetAdaHolderCurrentDelegationQuery: () => ({
    currentDelegation: { dRepHash: "h", dRepView: "drep1hazel", isDRepScriptBased: false, txHash: "tx-hazel" },
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

const client = () => new QueryClient({ defaultOptions: { queries: { retry: false } } });
const wrapper = ({ children }: { children: ReactNode }) => (
  <QueryClientProvider client={client()}>{children}</QueryClientProvider>
);
const REMOTE = { version: 1, channels: { inApp: true, email: false }, events: [], frequencies: [] };
const tokenStorageKey = `govtool_notifications_token_${STAKE}`;
const storeKey = `govtool_notifications_${STAKE}`;

describe("useNotifications", () => {
  beforeEach(() => {
    localStorage.clear();
    sessionStorage.clear();
    mocks.config = undefined;
    mocks.cardano = {
      isEnabled: true,
      stakeKey: STAKE,
      dRepID: "drep1me",
      walletApi: { signData: mocks.signData },
      pendingTransaction: {},
    };
    mocks.signData.mockReset();
    mocks.requestNotificationsNonce.mockReset();
    mocks.signInToNotifications.mockReset();
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

  it("records no delegation change while a delegation transaction is pending", async () => {
    localStorage.setItem(
      storeKey,
      JSON.stringify({ since: "2026-01-01T00:00:00Z", readIds: [], firstSeen: {}, lastDelegation: "drep1previous", history: [] }),
    );
    mocks.cardano = { ...mocks.cardano, pendingTransaction: { delegate: { transactionHash: "tx" } } };
    const { result, rerender } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.items).toHaveLength(1));
    expect(result.current.items.map((item) => item.kind)).toEqual(["drepChange"]);
    mocks.cardano = { ...mocks.cardano, pendingTransaction: {} };
    rerender();
    await waitFor(() => expect(result.current.items).toHaveLength(2));
    expect(result.current.items.map((item) => item.id)).toContain("delegation-tx-hazel");
  });

  it("signs in once for the bell and the centre, with the wallet's signature", async () => {
    mocks.config = REMOTE;
    mocks.requestNotificationsNonce.mockResolvedValue({ nonce: "hi", expiresAt: "2999-01-01T00:00:00Z" });
    mocks.signData.mockResolvedValue({ signature: "sig", key: "key" });
    mocks.signInToNotifications.mockResolvedValue({ token: "tok", expiresAt: "2999-01-01T00:00:00Z" });
    mocks.getNotificationsPage.mockResolvedValue({ items: [], total: 0, unread: 0, counts: {} });
    const shared = client();
    const sharedWrapper = ({ children }: { children: ReactNode }) => (
      <QueryClientProvider client={shared}>{children}</QueryClientProvider>
    );
    const bell = renderHook(() => useNotifications({ pageSize: 4 }), { wrapper: sharedWrapper });
    const center = renderHook(() => useNotifications(), { wrapper: sharedWrapper });
    await waitFor(() => expect(center.result.current.needsSignIn).toBe(true));
    await act(() => center.result.current.signIn());
    expect(mocks.signData).toHaveBeenCalledWith(STAKE, "6869");
    expect(mocks.signInToNotifications).toHaveBeenCalledWith(
      expect.objectContaining({ nonce: "hi", signature: "sig", key: "key" }),
    );
    await waitFor(() => expect(bell.result.current.needsSignIn).toBe(false));
    expect(sessionStorage.getItem(tokenStorageKey)).toContain("tok");
  });

  it("asks to sign in again when the service rejects the token", async () => {
    mocks.config = REMOTE;
    sessionStorage.setItem(tokenStorageKey, JSON.stringify({ token: "old", expiresAt: "2999-01-01T00:00:00Z" }));
    mocks.getNotificationsPage.mockRejectedValue(Object.assign(new Error("401"), { response: { status: 401 } }));
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.needsSignIn).toBe(true));
    expect(result.current.isError).toBe(false);
  });

  it("does not save preferences in this browser while a service runs but no one is signed in", async () => {
    mocks.config = REMOTE;
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.needsSignIn).toBe(true));
    await expect(result.current.savePreferences(result.current.preferences)).rejects.toThrow();
    expect(localStorage.getItem(`govtool_notification_prefs_${STAKE}`)).toBeNull();
  });

  it("leaves out malformed or repeated items from the service", async () => {
    mocks.config = REMOTE;
    sessionStorage.setItem(tokenStorageKey, JSON.stringify({ token: "tok", expiresAt: "2999-01-01T00:00:00Z" }));
    const good = { id: "a", kind: "system", createdAt: "2026-10-01T00:00:00Z", read: false, data: { title: "T", body: "B", url: null } };
    mocks.getNotificationsPage.mockResolvedValue({
      items: [good, good, { id: "b", kind: "delegationChange", createdAt: "x", read: false, data: {} }, { id: "c", kind: "poll" }],
      total: 4,
      unread: 1,
      counts: {},
    });
    const { result } = renderHook(() => useNotifications(), { wrapper });
    await waitFor(() => expect(result.current.items.map((item) => item.id)).toEqual(["a"]));
  });
});
