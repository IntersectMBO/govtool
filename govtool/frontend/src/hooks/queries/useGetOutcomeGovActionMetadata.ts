import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { OutcomeGovernanceAction } from "@models";
import { getOutcomeGovActionMetadata } from "@services";

/**
 * Validates an action's anchor through the outcomes API when the list or
 * detail row came without a resolved title or abstract.
 */
export const useGetOutcomeGovActionMetadata = (
  action?: Pick<OutcomeGovernanceAction, "url" | "data_hash"> &
    Partial<Pick<OutcomeGovernanceAction, "title" | "abstract">>,
) => {
  const shouldFetch =
    !!action?.url && (action.title === null || action.abstract === null);

  const { data, isLoading, isError } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetOutcomeGovActionMetadataKey,
      action?.url,
      action?.data_hash,
    ],
    queryFn: () =>
      getOutcomeGovActionMetadata(action?.url ?? "", action?.data_hash ?? ""),
    enabled: shouldFetch,
    retry: false,
  });

  return {
    metadata: shouldFetch ? data : undefined,
    metadataValid:
      !shouldFetch ||
      (!isError && (data === undefined || !!data.metadataValid)),
    isMetadataLoading: shouldFetch && isLoading,
  };
};
