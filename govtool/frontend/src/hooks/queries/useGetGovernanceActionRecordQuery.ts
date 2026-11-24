import { useQuery } from "@tanstack/react-query";
import { isAxiosError } from "axios";

import { QUERY_KEYS } from "@consts";
import { getGovernanceActionRecord } from "@services";
import { toGovernanceActionId } from "@utils";

/**
 * `id` is a CIP-105 `txHash#index` or a CIP-129 `gov_action1…` id. A list that
 * stays open passes `refetchInterval` so its tallies keep up with the list.
 */
export const useGetGovernanceActionRecordQuery = (
  id: string,
  { refetchInterval }: { refetchInterval?: number } = {},
) => {
  const actionId = toGovernanceActionId(id);

  const { data, isLoading, error } = useQuery({
    queryKey: [QUERY_KEYS.useGetGovernanceActionRecordKey, actionId],
    queryFn: () => getGovernanceActionRecord(actionId),
    enabled: !!actionId,
    refetchInterval,
    // A malformed or unknown id is answered 4xx and stays so; retrying only
    // keeps the spinner up before "not found". Other failures retry as usual.
    retry: (failureCount, failure) =>
      !(
        isAxiosError(failure) &&
        failure.response &&
        failure.response.status >= 400 &&
        failure.response.status < 500
      ) && failureCount < 3,
  });

  return {
    governanceAction: data,
    isGovernanceActionLoading: isLoading,
    governanceActionError: error,
  };
};
