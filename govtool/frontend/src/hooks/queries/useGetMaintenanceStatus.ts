import { useQuery } from "@tanstack/react-query";

import { getMaintenanceStatus } from "@services";
import { QUERY_KEYS } from "@consts";

/** How often an open page checks whether maintenance started or ended. */
const POLL_INTERVAL_MS = 60_000;

/**
 * Polled, so a page that is already open shows the banner when maintenance
 * starts and drops it when it ends, without a reload.
 */
export const useGetMaintenanceStatus = () => {
  const { data } = useQuery({
    queryKey: [QUERY_KEYS.useGetMaintenanceStatusKey],
    queryFn: getMaintenanceStatus,
    refetchInterval: POLL_INTERVAL_MS,
    refetchOnWindowFocus: true,
    retry: false,
  });

  return { maintenanceStatus: data ?? null };
};
