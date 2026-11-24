import { describe, expect, it, vi, beforeEach } from "vitest";
import { render } from "@testing-library/react";
import { MemoryRouter } from "react-router";
import { useEffect } from "react";

import { DRepDirectoryContent } from "./DRepDirectoryContent";

const mocks = vi.hoisted(() => ({
  mounts: 0,
  currentDelegation: { dRepView: null, dRepHash: null } as unknown,
}));

vi.mock("@organisms", () => ({
  AutomatedVotingOptions: () => null,
  DRepBanner: () => null,
  DRepCard: () => null,
  DelegationReviewDialog: () => {
    useEffect(() => {
      mocks.mounts += 1;
    }, []);
    return null;
  },
}));

vi.mock("@molecules", () => ({
  EmptyStateDrepDirectory: () => null,
  PageTitle: () => null,
  SearchField: () => null,
  SearchNotReady: () => null,
  SortSelect: () => null,
}));

vi.mock("@/components/molecules/PaginationFooter", () => ({
  PaginationFooter: () => null,
}));

vi.mock("@atoms", () => ({
  Button: () => null,
  Typography: ({ children }: { children: React.ReactNode }) => <span>{children}</span>,
}));

vi.mock("@consts", () => ({
  DREP_DIRECTORY_FILTERS: [],
  PATHS: { registerAsdRep: "/my/drep/register", dRepDirectoryDRep: "/dreps/:dRepId" },
}));

vi.mock("@utils", () => ({
  isSameDRep: () => false,
  uniqBy: <T, >(items: T[]) => items,
  parseBoolean: () => false,
  correctDRepDirectoryFormat: () => "0",
}));

vi.mock("@context", () => ({
  useCardano: () => ({ dRepID: "", pendingTransaction: {}, stakeKey: "stake_test1" }),
  useDataActionsBar: () => ({
    searchText: "",
    debouncedSearchText: "",
    setSearchText: vi.fn(),
    lastPath: "/dreps",
    chosenFilters: [],
    chosenSorting: "VotingPower",
    setChosenFilters: vi.fn(),
    setChosenSorting: vi.fn(),
  }),
  useFeatureFlag: () => ({
    dRepDirectorySort: {
      isAvailable: true,
      options: [],
      isSelectionStale: () => false,
      fallbackSelection: () => "VotingPower",
    },
  }),
  useModal: () => ({ openModal: vi.fn() }),
  usePagination: () => ({ page: 1, pageSize: 12, setPage: vi.fn(), setPageSize: vi.fn() }),
}));

vi.mock("@hooks", () => ({
  useDelegateTodRep: () => ({ delegate: vi.fn(), isDelegating: null }),
  useGetAdaHolderVotingPowerQuery: () => ({ votingPower: 1 }),
  useGetAdaHolderCurrentDelegationQuery: () => ({
    currentDelegation: mocks.currentDelegation,
  }),
  useGetDRepDetailsQuery: () => ({ dRep: undefined }),
  useGetDRepListPaginatedQuery: () => ({
    dRepData: [],
    isFetching: false,
    isPreviousData: false,
    total: 0,
    baselineTotalForStatus: 0,
    isSearchNotReady: false,
  }),
}));

const renderPage = () => (
  <MemoryRouter>
    <DRepDirectoryContent isConnected />
  </MemoryRouter>
);

describe("DRepDirectoryContent", () => {
  beforeEach(() => {
    mocks.mounts = 0;
    mocks.currentDelegation = { dRepView: null, dRepHash: null };
  });

  it("keeps the delegation dialog mounted while the page shows its loader", () => {
    const { rerender } = render(renderPage());
    expect(mocks.mounts).toBe(1);

    // After signing, the delegation query is re-keyed by the transaction and
    // reads undefined until it loads, which shows the loader.
    mocks.currentDelegation = undefined;
    rerender(renderPage());
    mocks.currentDelegation = { dRepView: null, dRepHash: null };
    rerender(renderPage());

    expect(mocks.mounts).toBe(1);
  });
});
