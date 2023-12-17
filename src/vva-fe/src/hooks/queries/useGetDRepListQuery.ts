import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepList } from "@services";

export const useGetDRepListQuery = () => {
  const { registerTransaction } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepListKey,
      registerTransaction?.transactionHash,
    ],
    queryFn: getDRepList,
  });

  return { data, isLoading };
};
