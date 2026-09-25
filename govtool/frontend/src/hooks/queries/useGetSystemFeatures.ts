import { useQuery } from "@tanstack/react-query";

import { getSystemFeatures } from "@services";
import { QUERY_KEYS } from "@consts";
import type { FeatureSet } from "@/models/featureSet";

/**
 * Fetched once during app bootstrap (see `AppContextProvider`), like
 * `useGetNetworkInfo` — hence `enabled: false` plus an explicit refetch.
 *
 * `isError` is part of the return on purpose: a failed capability fetch must be
 * distinguishable from "provider refuses this", or a transient network error
 * silently renders a feature-less app.
 */
export const useGetSystemFeatures = () => {
  const {
    data: systemFeatures,
    isError: isSystemFeaturesError,
    refetch: fetchSystemFeatures,
  } = useQuery<FeatureSet>({
    queryKey: [QUERY_KEYS.useGetSystemFeaturesKey],
    queryFn: () => getSystemFeatures(),
    enabled: false,
    retry: false,
  });

  return { systemFeatures, isSystemFeaturesError, fetchSystemFeatures };
};
