import { useInfiniteQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals } from "@services";

export const useGetProposalsInfiniteQuery = (
  filters: string[],
  sorting: string,
  pageSize: number = 10
) => {
  const { voteTransaction, isEnabled } = useCardano();

  const fetchProposals = async ({ pageParam = 0 }) => {
    return await getProposals(filters, sorting, pageParam, pageSize);
  };

  const {
    data,
    isLoading,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
  } = useInfiniteQuery(
    [
      QUERY_KEYS.useGetProposalsInfiniteKey,
      filters,
      sorting,
      voteTransaction.proposalId,
      isEnabled,
    ],
    fetchProposals,
    {
      getNextPageParam: (lastPage) => {
        if (lastPage.elements.length === 0) {
          return undefined;
        }
        return lastPage.page + 1;
      },
      refetchInterval: 20000,
    }
  );

  const proposals = data?.pages.flatMap(
    (page) => page.elements
  ) as ActionType[];

  return {
    proposals,
    isLoading,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
  };
};
