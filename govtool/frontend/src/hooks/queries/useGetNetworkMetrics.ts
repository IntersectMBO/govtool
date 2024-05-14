import { useQuery } from "react-query";

import { getNetworkMetrics } from "@services";
import { QUERY_KEYS } from "@consts";
import { useCardano } from "@/context";

export const useGetNetworkMetrics = () => {
  const { isEnabled } = useCardano();

  const { data } = useQuery({
    queryKey: QUERY_KEYS.useGetNetworkMetricsKey,
    queryFn: () => getNetworkMetrics(),
    enabled: isEnabled,
  });

  return { networkMetrics: data };
};
