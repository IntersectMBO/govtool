import { useMemo, useRef } from "react";
import { useQuery, UseQueryOptions } from "react-query";

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
  { page, pageSize = 10, filters = [], searchPhrase, sorting, status }: Args,
  options?: UseQueryOptions<Infinite<DRepData>>,
): PaginatedResult {
  const { pendingTransaction } = useCardano();
  const totalsByStatusRef = useRef<Record<string, number>>({});
  const statusKey = useMemo(() => makeStatusKey(status), [status]);

  const queryKey = [
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
  ];

  const { data, isLoading, isFetching, isPreviousData } = useQuery(
    queryKey,
    async () =>
      getDRepList({
        page,
        pageSize,
        filters,
        searchPhrase,
        sorting,
        status,
      }),
    {
      keepPreviousData: true,
      enabled: options?.enabled,
      onSuccess: (resp) => {
        if (!searchPhrase && typeof resp?.total === "number") {
          totalsByStatusRef.current[statusKey] = resp.total;
        }
        options?.onSuccess?.(resp);
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
    dRepData: data?.elements,
    isLoading,
    isFetching,
    isPreviousData,
    total: data?.total,
    baselineTotalForStatus: totalsByStatusRef.current[statusKey],
  };
}
