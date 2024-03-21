import { UseQueryOptions, useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepList } from "@services";
import { DRepData } from "@/models";

export const useGetDRepListQuery = (dRepView?: string, options?: UseQueryOptions<DRepData[]>) => {
  const { pendingTransaction } = useCardano();

  const { data, isLoading } = useQuery<DRepData[]>({
    queryKey: [
      QUERY_KEYS.useGetDRepListKey,
      (pendingTransaction.registerAsSoleVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsSoleVoter ||
        pendingTransaction.retireAsDrep)?.transactionHash,
      dRepView
    ],
    queryFn: () => getDRepList(dRepView),
    ...options
  });

  return { data, isLoading };
};
