import { keepPreviousData, useInfiniteQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListArguments, getDRepList } from "@services";

export const useGetDRepListInfiniteQuery = (
  {
    filters = [],
    pageSize = 10,
    searchPhrase,
    sorting,
    status,
  }: GetDRepListArguments,
  options?: { enabled?: boolean; keepPreviousData?: boolean },
) => {
  const { pendingTransaction } = useCardano();

  const {
    data,
    isLoading,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
    isPlaceholderData,
  } = useInfiniteQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepListInfiniteKey,
      (
        pendingTransaction.registerAsDirectVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsDirectVoter ||
        pendingTransaction.retireAsDrep
      )?.transactionHash ?? "noPendingTransaction",
      filters.length ? filters : "",
      searchPhrase ?? "",
      sorting ?? "",
      status?.length ? status : "",
    ],
    queryFn: async ({ pageParam }) =>
      getDRepList({
        page: pageParam,
        pageSize,
        filters,
        searchPhrase,
        sorting,
        status,
      }),
    initialPageParam: 0,
    getNextPageParam: (lastPage) => {
      if (lastPage.elements.length === 0) return undefined;
      return lastPage.page + 1;
    },
    enabled: options?.enabled,
    placeholderData: options?.keepPreviousData ? keepPreviousData : undefined,
  });

  return {
    dRepListFetchNextPage: fetchNextPage,
    dRepListHasNextPage: hasNextPage,
    isDRepListFetching: isFetching,
    isDRepListFetchingNextPage: isFetchingNextPage,
    isDRepListLoading: isLoading,
    dRepData: data?.pages.flatMap((page) => page.elements),
    isPreviousData: isPlaceholderData,
  };
};
