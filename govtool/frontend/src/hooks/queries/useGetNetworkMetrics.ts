import { useQuery } from "react-query";

import { getNetworkMetrics } from "@services";
import { QUERY_KEYS } from "@consts";

export const useGetNetworkMetrics = () => {
  const { data: networkMetrics, refetch: fetchNetworkMetrics } = useQuery({
    queryKey: QUERY_KEYS.useGetNetworkMetricsKey,
    queryFn: () => getNetworkMetrics(),
    enabled: false,
  });

  return { networkMetrics, fetchNetworkMetrics };
};
