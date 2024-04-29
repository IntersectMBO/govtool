import { UseInfiniteQueryOptions, useInfiniteQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListArguments, getDRepList } from "@services";
import { InfinityDRepData } from "@/models";

export const useGetDRepListInfiniteQuery = (
  {
    filters = [],
    pageSize = 10,
    searchPhrase,
    sorting,
    status,
  }: GetDRepListArguments,
  options?: UseInfiniteQueryOptions<InfinityDRepData>,
) => {
  const { pendingTransaction } = useCardano();

  const {
    data,
    isLoading,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
    isPreviousData,
  } = useInfiniteQuery(
    [
      QUERY_KEYS.useGetDRepListInfiniteKey,
      (
        pendingTransaction.registerAsSoleVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsSoleVoter ||
        pendingTransaction.retireAsDrep
      )?.transactionHash,
      filters,
      searchPhrase,
      sorting,
      status,
    ],
    async ({ pageParam = 0 }) =>
      getDRepList({
        page: pageParam,
        pageSize,
        filters,
        searchPhrase,
        sorting,
        status,
      }),
    {
      getNextPageParam: (lastPage) => {
        if (lastPage.elements.length === 0) {
          return undefined;
        }

        return lastPage.page + 1;
      },
      enabled: options?.enabled,
      keepPreviousData: options?.keepPreviousData,
    },
  );

  return {
    dRepListFetchNextPage: fetchNextPage,
    dRepListHasNextPage: hasNextPage,
    isDRepListFetching: isFetching,
    isDRepListFetchingNextPage: isFetchingNextPage,
    isDRepListLoading: isLoading,
    dRepData: data?.pages.flatMap((page) => page.elements),
    isPreviousData,
  };
};
