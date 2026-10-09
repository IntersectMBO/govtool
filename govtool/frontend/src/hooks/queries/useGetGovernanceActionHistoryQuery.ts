import { useInfiniteQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getGovernanceActionHistory } from "@services";
import { isSearchNotReady, toGovernanceActionId } from "@utils";

export const useGetGovernanceActionHistoryQuery = (
  search: string,
  filters: string[],
  sort: string,
  limit: number,
) => {
  const searchPhrase = toGovernanceActionId(search);

  const {
    data,
    isLoading,
    error,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  } = useInfiniteQuery({
    queryKey: [
      QUERY_KEYS.useGetGovernanceActionHistoryKey,
      searchPhrase,
      filters,
      sort,
      limit,
    ],
    queryFn: ({ pageParam }) =>
      getGovernanceActionHistory(
        searchPhrase,
        filters,
        sort,
        pageParam,
        limit,
      ),
    initialPageParam: 1,
    getNextPageParam: (lastPage, allPages) =>
      (lastPage.length === limit ? allPages.length + 1 : undefined),
  });

  return {
    govActions: data,
    isGovActionsLoading: isLoading,
    govActionsError: error,
    isSearchNotReady: isSearchNotReady(error),
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  };
};
