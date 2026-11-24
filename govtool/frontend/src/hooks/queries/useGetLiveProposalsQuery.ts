import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getProposals } from "@services";

/** The governance actions open for voting, newest first, with their total. */
export const useGetLiveProposalsQuery = (pageSize = 50) => {
  const { data, dataUpdatedAt, isLoading, isError } = useQuery({
    queryKey: [QUERY_KEYS.useGetLiveProposalsKey, pageSize],
    queryFn: () => getProposals({ sorting: "NewestCreated", pageSize }),
  });

  return {
    liveProposals: data?.elements ?? [],
    liveProposalsTotal: data?.total ?? 0,
    areLiveProposalsLoading: isLoading,
    /** The list could not be read; its total of 0 then means nothing. */
    isLiveProposalsError: isError,
    /** When the list last arrived, in milliseconds; 0 before it has. */
    liveProposalsUpdatedAt: dataUpdatedAt,
  };
};
