import { useCallback } from "react";
import { useQuery, useQueryClient } from "@tanstack/react-query";

import { getEpochParams } from "@services";
import { QUERY_KEYS } from "@consts";

const queryKey = [QUERY_KEYS.useGetEpochParamsKey];

export const useGetEpochParams = () => {
  const queryClient = useQueryClient();
  const { data: epochParams, refetch: fetchEpochParams } = useQuery({
    queryKey,
    queryFn: () => getEpochParams(),
    enabled: false,
  });

  /**
   * Resolves the params from the query cache, joining an in-flight fetch or
   * starting one. Callers that need the params now (building a transaction)
   * use this instead of the rendered value, which is empty until the
   * bootstrap fetch lands.
   */
  const ensureEpochParams = useCallback(
    () => queryClient.ensureQueryData({ queryKey, queryFn: getEpochParams }),
    [queryClient],
  );

  return { epochParams, fetchEpochParams, ensureEpochParams };
};
