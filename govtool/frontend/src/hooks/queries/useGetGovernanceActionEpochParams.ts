import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import type { GovernanceActionRecord } from "@models";
import { getGovernanceActionEpochParams } from "@services";

/** Protocol parameters for the action's tally epoch, independently of voting support. */
export const useGetGovernanceActionEpochParams = (action?: GovernanceActionRecord) => {
  const epoch =
    action?.status.ratified_epoch ??
    action?.status.expired_epoch ??
    action?.status.dropped_epoch ??
    undefined;
  const query = useQuery({
    queryKey: [QUERY_KEYS.useGetGovernanceActionEpochParamsKey, epoch],
    queryFn: () => getGovernanceActionEpochParams(epoch),
    enabled: !!action,
  });
  return {
    epochParams: query.data ?? null,
    isLoading: query.isLoading,
    error: query.error,
  };
};
