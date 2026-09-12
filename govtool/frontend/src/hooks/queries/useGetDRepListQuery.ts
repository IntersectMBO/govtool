import { useMemo } from "react";
import { keepPreviousData, useQuery, useQueryClient } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListArguments, getDRepList } from "@services";
import { DRepData, Infinite } from "@/models";

const makeStatusKey = (status?: string[] | undefined) =>
    (status && status.length ? [...status].sort().join("|") : "__EMPTY__");

type PaginatedResult = {
  dRepData: DRepData[] | undefined;
  isLoading: boolean;
  isFetching: boolean;
  isPreviousData: boolean;
  total: number | undefined;
  baselineTotalForStatus: number | undefined;
};

type Args = GetDRepListArguments & {
  page: number;
  pageSize?: number;
};

export function useGetDRepListPaginatedQuery(
  { page, pageSize = 10, filters = [], searchPhrase, sorting, status, sortingSeed }: Args,
  options?: { enabled?: boolean },
): PaginatedResult {
  const { pendingTransaction } = useCardano();
  const queryClient = useQueryClient();

  const statusKey = useMemo(() => makeStatusKey(status), [status]);

  const listKey = [
    QUERY_KEYS.useGetDRepListInfiniteKey,
    (
      pendingTransaction.registerAsDirectVoter ||
      pendingTransaction.registerAsDrep ||
      pendingTransaction.retireAsDirectVoter ||
      pendingTransaction.retireAsDrep
    )?.transactionHash ?? "noPendingTransaction",
    "paged",
    page,
    pageSize,
    filters.length ? filters : "",
    searchPhrase ?? "",
    sorting ?? "",
    status?.length ? status : "",
    sortingSeed ?? ""
  ];

  const baselineKey = useMemo(
    () => [QUERY_KEYS.useGetDRepListInfiniteKey, "baseline", statusKey],
    [statusKey],
  );

  const { data, isLoading, isFetching, isPlaceholderData } = useQuery({
    queryKey: listKey,
    queryFn: async () => {
      const response = await getDRepList({
        page,
        pageSize,
        filters,
        searchPhrase,
        sorting,
        status,
        sortingSeed,
      });
      if (!searchPhrase && typeof response.total === "number") {
        queryClient.setQueryData(baselineKey, response);
      }
      return response;
    },
    placeholderData: keepPreviousData,
    enabled: options?.enabled,
  });

  const { data: baselineResp } = useQuery({
    queryKey: baselineKey,
    queryFn: async () =>
      getDRepList({
        page: 0,
        pageSize: 1,
        filters,
        searchPhrase: "",
        sorting,
        status,
        sortingSeed
      }),
    initialData: () =>
      queryClient.getQueryData<Infinite<DRepData>>(baselineKey),
    enabled:
      options?.enabled &&
      !queryClient.getQueryData(baselineKey) &&
      searchPhrase !== "",
    staleTime: Infinity,
  });

  return {
    dRepData: data?.elements,
    isLoading,
    isFetching,
    isPreviousData: isPlaceholderData,
    total: data?.total,
    baselineTotalForStatus: baselineResp?.total,
  };
}
