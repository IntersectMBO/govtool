import {
  UseInfiniteQueryOptions,
  useInfiniteQuery,
  useQuery,
} from "react-query";
import { useRef, useMemo } from "react";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListArguments, getDRepList } from "@services";
import { DRepData, Infinite } from "@/models";

const makeStatusKey = (status?: string[] | undefined) =>
    (status && status.length ? [...status].sort().join("|") : "__EMPTY__");

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
  const totalsByStatusRef = useRef<Record<string, number>>({});
  const statusKey = useMemo(() => makeStatusKey(status), [status]);

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
        if (lastPage.elements.length === 0) return undefined;
        return lastPage.page + 1;
      },
      enabled: options?.enabled,
      keepPreviousData: options?.keepPreviousData,
      onSuccess: (pagesData) => {
        if (!searchPhrase) {
          const firstPage = pagesData.pages?.[0];
          if (firstPage && typeof firstPage.total === "number") {
            totalsByStatusRef.current[statusKey] = firstPage.total;
          }
        }
        options?.onSuccess?.(pagesData);
      },
    },
  );

  useQuery(
    [QUERY_KEYS.useGetDRepListInfiniteKey, "baseline", statusKey],
    async () => {
      const resp = await getDRepList({
        page: 0,
        pageSize: 1,
        filters,
        searchPhrase: "",
        sorting,
        status,
      });
      return resp;
    },
    {
      enabled:
        options?.enabled &&
        searchPhrase !== "" &&
        totalsByStatusRef.current[statusKey] === undefined,
      onSuccess: (resp) => {
        if (typeof resp.total === "number") {
          totalsByStatusRef.current[statusKey] = resp.total;
        }
      },
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
    dRepListTotal: data?.pages[0].total,
    dRepTotalsByStatus: totalsByStatusRef.current,
    dRepBaselineTotalForStatus: totalsByStatusRef.current[statusKey],
  };
};
