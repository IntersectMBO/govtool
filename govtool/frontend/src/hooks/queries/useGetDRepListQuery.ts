import { UseQueryOptions, useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { GetDRepListParams, getDRepList } from "@services";
import { DRepData } from "@/models";

export const useGetDRepListQuery = (
  params?: GetDRepListParams,
  options?: UseQueryOptions<DRepData[]>
) => {
  const { search, sort, status } = params || {};
  const { pendingTransaction } = useCardano();

  const { data, isLoading, isPreviousData } = useQuery<DRepData[]>({
    queryKey: [
      QUERY_KEYS.useGetDRepListKey,
      (pendingTransaction.registerAsSoleVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsSoleVoter ||
        pendingTransaction.retireAsDrep)?.transactionHash,
      search,
      sort,
      status,
    ],
    queryFn: () => getDRepList({
      ...(search && { search }),
      ...(sort && { sort }),
      ...(status && { status }),
    }),
    ...options
  });

  return { data, isLoading, isPreviousData };
};
