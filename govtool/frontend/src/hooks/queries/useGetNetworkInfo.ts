import { useQuery } from "react-query";

import { getNetworkInfo } from "@services";
import { QUERY_KEYS } from "@consts";

export const useGetNetworkInfo = () => {
  const { data: networkInfo, refetch: fetchNetworkInfo } = useQuery({
    queryKey: QUERY_KEYS.useGetNetworkInfoKey,
    queryFn: () => getNetworkInfo(),
    enabled: false,
  });

  return { networkInfo, fetchNetworkInfo };
};
