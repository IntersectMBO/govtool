import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getVoterInfo } from "@services";

export const useGetVoterInfo = (options?: { enabled?: boolean }) => {
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
      // Last, so a pending transaction's key [key, hash] still prefixes it.
      dRepID,
    ],
    enabled: !!dRepID && (options?.enabled ?? true),
    queryFn: () => getVoterInfo(dRepID),
  });

  return { voter: data };
};
