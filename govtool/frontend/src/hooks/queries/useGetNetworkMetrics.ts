import { useQuery } from "react-query";

import { getNetworkMetrics } from "@services";
import { QUERY_KEYS } from "@consts";

export const useGetNetworkMetrics = () => {
  const { data } = useQuery({
    queryKey: QUERY_KEYS.useGetNetworkMetricsKey,
    queryFn: () => getNetworkMetrics(),
  });

  return { networkMetrics: data };
};
