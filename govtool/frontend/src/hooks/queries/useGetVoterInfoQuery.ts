import { UseQueryOptions, useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getVoterInfo } from "@services";

export const useGetVoterInfo = (options?: UseQueryOptions) => {
  const { dRepID, pendingTransaction } = useCardano();
  const { data } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepInfoKey,
      (
        pendingTransaction?.registerAsDrep ||
        pendingTransaction?.registerAsDirectVoter ||
        pendingTransaction?.retireAsDrep ||
        pendingTransaction?.retireAsDirectVoter
      )?.transactionHash,
    ],
    enabled: !!dRepID && options?.enabled,
    queryFn: () => getVoterInfo(dRepID),
  });

  return { voter: data };
};
