import { useMemo } from "react";
import { useQuery, UseQueryOptions, useQueryClient } from "react-query";

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
  options?: UseQueryOptions<Infinite<DRepData>>,
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

  const { data, isLoading, isFetching, isPreviousData } = useQuery(
    listKey,
    () =>
      getDRepList({
        page,
        pageSize,
        filters,
        searchPhrase,
        sorting,
        status,
        sortingSeed,
      }),
    {
      keepPreviousData: true,
      enabled: options?.enabled,
      onSuccess: (resp) => {
        if (!searchPhrase && typeof resp?.total === "number") {
          queryClient.setQueryData(baselineKey, resp);
        }
        options?.onSuccess?.(resp);
      },
    },
  );

  const { data: baselineResp } = useQuery(
    baselineKey,
    async () =>
      getDRepList({
        page: 0,
        pageSize: 1,
        filters,
        searchPhrase: "",
        sorting,
        status,
        sortingSeed
      }),
    {
      initialData: () =>
        queryClient.getQueryData<Infinite<DRepData>>(baselineKey),
      enabled:
        options?.enabled &&
        !queryClient.getQueryData(baselineKey) &&
        searchPhrase !== "",
      staleTime: Infinity,
    },
  );

  return {
    dRepData: data?.elements,
    isLoading,
    isFetching,
    isPreviousData,
    total: data?.total,
    baselineTotalForStatus: baselineResp?.total,
  };
}
