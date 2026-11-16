import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getDRepVotesById } from "@services";

/** The votes a DRep cast, for its profile; `dRepId` is its CIP-105 view. */
export const useGetDRepVotesByIdQuery = (dRepId?: string) => {
  const { data, isLoading, isError } = useQuery({
    queryKey: [QUERY_KEYS.useGetDRepVotesByIdKey, dRepId],
    queryFn: () => getDRepVotesById(dRepId ?? ""),
    enabled: !!dRepId,
  });

  return {
    dRepVotes: data,
    areDRepVotesLoading: isLoading,
    isDRepVotesError: isError,
  };
};
