import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import type { OutcomeGovernanceAction } from "@models";
import { getOutcomeEpochParams } from "@services";

/** Protocol parameters for the action's tally epoch, independently of voting support. */
export const useGetOutcomeEpochParams = (action?: OutcomeGovernanceAction) => {
  const epoch =
    action?.status.ratified_epoch ??
    action?.status.expired_epoch ??
    action?.status.dropped_epoch ??
    undefined;
  const query = useQuery({
    queryKey: [QUERY_KEYS.useGetOutcomeEpochParamsKey, epoch],
    queryFn: () => getOutcomeEpochParams(epoch),
    enabled: !!action,
  });
  return {
    epochParams: query.data ?? null,
    isLoading: query.isLoading,
    error: query.error,
  };
};
