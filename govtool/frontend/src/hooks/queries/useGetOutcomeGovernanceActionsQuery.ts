import { useInfiniteQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getOutcomeGovernanceActions } from "@services";
import { toOutcomeGovActionId } from "@utils";

export const useGetOutcomeGovernanceActionsQuery = (
  search: string,
  filters: string[],
  sort: string,
  limit: number,
) => {
  const searchPhrase = toOutcomeGovActionId(search);

  const {
    data,
    isLoading,
    error,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  } = useInfiniteQuery({
    queryKey: [
      QUERY_KEYS.useGetOutcomeGovernanceActionsKey,
      searchPhrase,
      filters,
      sort,
      limit,
    ],
    queryFn: ({ pageParam }) =>
      getOutcomeGovernanceActions(
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
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  };
};
