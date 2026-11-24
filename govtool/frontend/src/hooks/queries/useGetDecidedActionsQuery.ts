import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getGovernanceActionHistory } from "@services";

const DECIDED_STATUSES = ["ratified", "enacted", "expired"];

/** How many decided actions the shared query loads. */
export const DECIDED_ACTIONS_LIMIT = 20;

/** The most recently concluded governance actions: ratified, enacted or expired. */
export const useGetDecidedActionsQuery = (limit = DECIDED_ACTIONS_LIMIT) => {
  const { data, isLoading } = useQuery({
    queryKey: [QUERY_KEYS.useGetDecidedActionsKey, limit],
    queryFn: () =>
      getGovernanceActionHistory("", DECIDED_STATUSES, "newestFirst", 1, limit),
  });

  return {
    decidedActions: data ?? [],
    areDecidedActionsLoading: isLoading,
  };
};
