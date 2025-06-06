import { UseInfiniteQueryOptions, useInfiniteQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListArguments, getDRepList, getDRepSyncAiList } from "@services";
import { DRepData, Infinite } from "@/models";

export const useGetDRepListInfiniteQuery = (
  {
    filters = [],
    pageSize = 10,
    searchPhrase,
    sorting,
    status,
  }: GetDRepListArguments,
  options?: UseInfiniteQueryOptions<Infinite<DRepData>>,
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

export const useGetDRepSyncAiListInfiniteQuery = (
  { pageSize = 10, searchPhrase }: GetDRepListArguments,
  options?: UseInfiniteQueryOptions<Infinite<DRepData>>,
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
      QUERY_KEYS.useGetDRepSyncAiListInfiniteKey,
      (
        pendingTransaction.registerAsDirectVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsDirectVoter ||
        pendingTransaction.retireAsDrep
      )?.transactionHash ?? "noPendingTransaction",
      searchPhrase ?? "",
    ],
    async ({ pageParam = 0 }) =>
      getDRepSyncAiList({
        page: pageParam,
        pageSize,
        searchPhrase,
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
