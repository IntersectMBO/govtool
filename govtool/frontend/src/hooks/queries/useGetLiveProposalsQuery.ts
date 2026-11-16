import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getProposals } from "@services";

/** The governance actions open for voting, newest first, with their total. */
export const useGetLiveProposalsQuery = (pageSize = 50) => {
  const { data, dataUpdatedAt, isLoading } = useQuery({
    queryKey: [QUERY_KEYS.useGetLiveProposalsKey, pageSize],
    queryFn: () => getProposals({ sorting: "NewestCreated", pageSize }),
  });

  return {
    liveProposals: data?.elements ?? [],
    liveProposalsTotal: data?.total ?? 0,
    areLiveProposalsLoading: isLoading,
    /** When the list last arrived, in milliseconds; 0 before it has. */
    liveProposalsUpdatedAt: dataUpdatedAt,
  };
};
