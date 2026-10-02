import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { GovernanceActionRecord } from "@models";
import { getGovernanceActionMetadata } from "@services";

/**
 * Validates an action's anchor through the governanceActionHistory API when the list or
 * detail row came without a resolved title or abstract.
 */
export const useGetGovernanceActionMetadata = (
  action?: Pick<GovernanceActionRecord, "url" | "data_hash"> &
    Partial<Pick<GovernanceActionRecord, "title" | "abstract">>,
) => {
  const shouldFetch =
    !!action?.url && (action.title === null || action.abstract === null);

  const { data, isLoading, isError } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetGovernanceActionMetadataKey,
      action?.url,
      action?.data_hash,
    ],
    queryFn: () =>
      getGovernanceActionMetadata(action?.url ?? "", action?.data_hash ?? ""),
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
