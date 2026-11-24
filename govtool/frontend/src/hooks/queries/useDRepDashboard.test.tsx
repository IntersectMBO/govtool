import { ReactNode } from "react";
import { describe, expect, it, vi } from "vitest";
import { renderHook, waitFor } from "@testing-library/react";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts/queryKeys";

const { getVotes } = vi.hoisted(() => ({ getVotes: vi.fn() }));

vi.mock("@/services/requests", () => ({ getDRepVotesById: getVotes }));
vi.mock("@/context/wallet", () => ({ useCardano: () => ({ pendingTransaction: {} }) }));
vi.mock("@/context/appContext", () => ({ useAppContext: () => ({}) }));
vi.mock("@/context/featureFlag", () => ({ useFeatureFlag: () => ({}) }));

// eslint-disable-next-line import/first
import { useDRepDashboardVotes } from "./useDRepDashboard";

// main.tsx turns refetchOnMount off for every query.
const wrapperWith = (client: QueryClient) =>
  ({ children }: { children: ReactNode }) => (
    <QueryClientProvider client={client}>{children}</QueryClientProvider>
  );

const appClient = () =>
  new QueryClient({ defaultOptions: { queries: { refetchOnMount: false, retry: false } } });

describe("useDRepDashboardVotes", () => {
  it("reads the votes again when the dashboard mounts over a cached answer", async () => {
    const client = appClient();
    client.setQueryData([QUERY_KEYS.drepDashboardVotesKey, undefined, "id"], []);
    getVotes.mockResolvedValue([{ vote: null, proposal: {} }]);
    const { result } = renderHook(() => useDRepDashboardVotes("id"), { wrapper: wrapperWith(client) });
    await waitFor(() => expect(result.current.votes).toHaveLength(1));
  });

  it("treats a body that is not a list as no votes", async () => {
    getVotes.mockResolvedValue(null);
    const { result } = renderHook(() => useDRepDashboardVotes("id2"), { wrapper: wrapperWith(appClient()) });
    await waitFor(() => expect(result.current.isLoading).toBe(false));
    expect(result.current.votes).toEqual([]);
  });
});
