import { useQuery } from "react-query";

import { getAdaHolderCurrentDelegation } from "@services";
import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";

export const useGetAdaHolderCurrentDelegationQuery = (stakeKey?: string) => {
  const { pendingTransaction } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.getAdaHolderCurrentDelegationKey,
      pendingTransaction.delegate,
    ],
    queryFn: () => getAdaHolderCurrentDelegation({ stakeKey }),
    enabled: !!stakeKey,
  });

  return { currentDelegation: data, isCurrentDelegationLoading: isLoading };
};
