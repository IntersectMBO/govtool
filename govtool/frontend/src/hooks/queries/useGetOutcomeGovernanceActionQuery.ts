import { useQuery } from "@tanstack/react-query";

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
  });

  return {
    governanceAction: data,
    isGovernanceActionLoading: isLoading,
    governanceActionError: error,
  };
};
