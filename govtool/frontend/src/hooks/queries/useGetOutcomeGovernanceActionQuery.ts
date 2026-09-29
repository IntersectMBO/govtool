import { useQuery } from "@tanstack/react-query";
import { isAxiosError } from "axios";

import { QUERY_KEYS } from "@consts";
import { getOutcomeGovernanceAction } from "@services";
import { toOutcomeGovActionId } from "@utils";

/** `id` is a CIP-105 `txHash#index` or a CIP-129 `gov_action1…` id. */
export const useGetOutcomeGovernanceActionQuery = (id: string) => {
  const actionId = toOutcomeGovActionId(id);

  const { data, isLoading, error } = useQuery({
    queryKey: [QUERY_KEYS.useGetOutcomeGovernanceActionKey, actionId],
    queryFn: () => getOutcomeGovernanceAction(actionId),
    enabled: !!actionId,
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
