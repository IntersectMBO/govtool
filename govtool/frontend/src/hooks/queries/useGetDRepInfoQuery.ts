import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepInfo } from "@services";

export const useGetDRepInfo = () => {
  const { registerTransaction, dRepID } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepInfoKey,
      registerTransaction?.transactionHash,
    ],
    enabled: !!dRepID,
    queryFn: async () => await getDRepInfo(dRepID),
  });

  return { data, isLoading };
};
