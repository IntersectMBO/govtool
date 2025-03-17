import { useQuery } from "react-query";

import { getNetworkTotalStake } from "@services";
import { QUERY_KEYS } from "@consts";

export const useGetNetworkTotalStake = () => {
  const { data: networkTotalStake, refetch: fetchNetworkTotalStake } = useQuery(
    {
      queryKey: QUERY_KEYS.useGetNetworkTotalStakeKey,
      queryFn: () => getNetworkTotalStake(),
      enabled: false,
    },
  );

  return { networkTotalStake, fetchNetworkTotalStake };
};
