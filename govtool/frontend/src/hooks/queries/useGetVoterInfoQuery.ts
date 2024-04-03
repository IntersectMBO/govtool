import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getVoterInfo } from "@services";

export const useGetVoterInfo = () => {
  const { dRepID, pendingTransaction } = useCardano();

  const { data } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepInfoKey,
      (
        pendingTransaction?.registerAsDrep ||
        pendingTransaction?.registerAsSoleVoter ||
        pendingTransaction?.retireAsDrep ||
        pendingTransaction?.retireAsSoleVoter
      )?.transactionHash,
    ],
    enabled: !!dRepID,
    queryFn: () => getVoterInfo(dRepID),
  });

  return { voter: data };
};
