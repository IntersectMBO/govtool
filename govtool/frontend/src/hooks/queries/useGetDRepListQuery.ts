import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepList } from "@services";

export const useGetDRepListQuery = () => {
  const { pendingTransaction } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepListKey,
      pendingTransaction.registerAsSoleVoter ||
        pendingTransaction.registerAsDrep ||
        pendingTransaction.retireAsSoleVoter ||
        pendingTransaction.retireAsDrep,
    ],
    queryFn: getDRepList,
  });

  return { data, isLoading };
};
