import {
  InfiniteData,
  useInfiniteQuery,
  useQuery,
  UseInfiniteQueryResult,
} from "@tanstack/react-query";

import {
  getGovernanceActionHistory,
  lookupStakeAddress,
  searchDReps,
  searchForumProposals,
  searchGovernanceActions,
} from "@services";
import { QUERY_KEYS } from "@/consts/queryKeys";
import type {
  DRepData,
  ForumProposal,
  GovernanceActionRecord,
  ProposalData,
  StakeAddressLookup,
} from "@/models";
import type { HandleObject } from "@/models/adaHandle";
import { adaHandleService } from "@/services/AdaHandle";
import { classifyQuery } from "@/utils/globalSearch";
import { isSearchNotReady } from "@/utils/searchNotReady";

export const SEARCH_PAGE_SIZE = 5;
const MIN_LENGTH = 2;

export type SearchCategory =
  | "actions"
  | "dreps"
  | "proposals"
  | "outcomes"
  | "addresses"
  | "handles";

export type CategoryResults<T> = {
  items: T[];
  /** Undefined when the source gives no total. */
  total: number | undefined;
  hasMore: boolean;
  /** False when this query does not search the category. */
  isEnabled: boolean;
  isLoading: boolean;
  isError: boolean;
  isSearchNotReady: boolean;
  isFetchingMore: boolean;
  loadMore: () => void;
  retry: () => void;
};

// A search that is not ready yet will not be ready a second later; anything
// else gets one more try before the dialog shows the error.
const retry = (count: number, error: unknown) => !isSearchNotReady(error) && count < 1;

const fromInfinite = <P, T>(
  query: UseInfiniteQueryResult<InfiniteData<P>, unknown>,
  isEnabled: boolean,
  items: (page: P) => T[],
  total: (pages: P[]) => number | undefined,
): CategoryResults<T> => {
  const pages = query.data?.pages ?? [];
  return {
    items: pages.flatMap(items),
    total: pages.length ? total(pages) : undefined,
    hasMore: !!query.hasNextPage,
    isEnabled,
    isLoading: isEnabled && query.isLoading,
    isError: isEnabled && query.isError && !query.data,
    isSearchNotReady: isEnabled && isSearchNotReady(query.error),
    isFetchingMore: query.isFetchingNextPage,
    loadMore: () => {
      if (query.hasNextPage && !query.isFetchingNextPage) query.fetchNextPage();
    },
    retry: () => {
      query.refetch();
    },
  };
};

type Paged<T> = { elements: T[]; total: number };

/** Every category of the header's global search, for a debounced query. */
export const useGlobalSearch = (
  query: string,
  { isForumEnabled }: { isForumEnabled: boolean },
) => {
  const classified = classifyQuery(query);
  const { text } = classified;
  const isTextSearch = text.length >= MIN_LENGTH && classified.kind !== "address";
  const key = (category: SearchCategory, ...rest: unknown[]) => [
    QUERY_KEYS.useGlobalSearchKey,
    category,
    ...rest,
  ];
  const nextZeroBased = (last: Paged<unknown>, all: Paged<unknown>[]) =>
    (all.length * SEARCH_PAGE_SIZE < last.total ? all.length : undefined);

  const actionsQuery = useInfiniteQuery({
    queryKey: key("actions", text),
    queryFn: ({ pageParam }) => searchGovernanceActions(text, pageParam, SEARCH_PAGE_SIZE),
    initialPageParam: 0,
    getNextPageParam: nextZeroBased,
    enabled: isTextSearch,
    retry,
  });

  const dRepsQuery = useInfiniteQuery({
    queryKey: key("dreps", text),
    queryFn: ({ pageParam }) => searchDReps(text, pageParam, SEARCH_PAGE_SIZE),
    initialPageParam: 0,
    getNextPageParam: nextZeroBased,
    enabled: isTextSearch,
    retry,
  });

  const isForumSearch = isTextSearch && isForumEnabled;
  const proposalsQuery = useInfiniteQuery({
    queryKey: key("proposals", text),
    queryFn: ({ pageParam }) => searchForumProposals(text, pageParam, SEARCH_PAGE_SIZE),
    initialPageParam: 1,
    getNextPageParam: (last, all) =>
      (all.length * SEARCH_PAGE_SIZE < last.total ? all.length + 1 : undefined),
    enabled: isForumSearch,
    retry,
  });

  const outcomesQuery = useInfiniteQuery({
    queryKey: key("outcomes", text),
    queryFn: ({ pageParam }) =>
      getGovernanceActionHistory(text, [], "newestFirst", pageParam, SEARCH_PAGE_SIZE),
    initialPageParam: 1,
    getNextPageParam: (last, all) =>
      (last.length === SEARCH_PAGE_SIZE ? all.length + 1 : undefined),
    enabled: isTextSearch,
    retry,
  });

  const { stakeAddress, handle } = classified;
  const isAddressSearch = classified.kind === "address";
  const addressQuery = useQuery({
    queryKey: key("addresses", stakeAddress),
    queryFn: () => lookupStakeAddress(stakeAddress as string),
    enabled: isAddressSearch && !!stakeAddress,
    retry,
  });

  const isHandleSearch = !!handle && text.length >= MIN_LENGTH;
  const handleQuery = useQuery({
    queryKey: key("handles", handle),
    queryFn: () => adaHandleService.getHandleDetails(handle as string),
    enabled: isHandleSearch,
    retry,
  });

  const single = <T, >(
    result: { data?: T | null; isLoading: boolean; isError: boolean; refetch: () => unknown },
    isEnabled: boolean,
    isQueried: boolean,
  ): CategoryResults<T> => ({
    items: isQueried && result.data ? [result.data] : [],
    total: isQueried && !result.isLoading && !result.isError ? (result.data ? 1 : 0) : undefined,
    hasMore: false,
    isEnabled,
    isLoading: isQueried && result.isLoading,
    isError: isQueried && result.isError,
    isSearchNotReady: false,
    isFetchingMore: false,
    loadMore: () => {},
    retry: () => {
      result.refetch();
    },
  });

  return {
    classified,
    isSearching: isTextSearch || isAddressSearch,
    actions: fromInfinite<Paged<ProposalData>, ProposalData>(
      actionsQuery,
      isTextSearch,
      (p) => p.elements,
      (pages) => pages[0].total,
    ),
    dreps: fromInfinite<Paged<DRepData>, DRepData>(
      dRepsQuery,
      isTextSearch,
      (p) => p.elements,
      (pages) => pages[0].total,
    ),
    proposals: fromInfinite<Paged<ForumProposal>, ForumProposal>(
      proposalsQuery,
      isForumSearch,
      (p) => p.elements,
      (pages) => pages[0].total,
    ),
    outcomes: fromInfinite<GovernanceActionRecord[], GovernanceActionRecord>(
      outcomesQuery,
      isTextSearch,
      (p) => p,
      () => undefined,
    ),
    // An address with no stake part is searched but can match nothing.
    addresses: single<StakeAddressLookup>(addressQuery, isAddressSearch, !!stakeAddress),
    handles: single<HandleObject>(handleQuery, isHandleSearch, isHandleSearch),
  };
};
