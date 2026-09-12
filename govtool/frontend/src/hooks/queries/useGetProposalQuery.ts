import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposal } from "@services";

export const useGetProposalQuery = (proposalId: string, enabled?: boolean) => {
  const { dRepID } = useCardano();

  const { data, isLoading, refetch, isRefetching, error } = useQuery({
    queryKey: [QUERY_KEYS.useGetProposalKey, dRepID, proposalId],
    queryFn: () => getProposal(proposalId, dRepID),
    staleTime: Infinity,
    enabled,
  });

  return {
    data,
    isLoading,
    refetch,
    isFetching: isRefetching,
    error,
  };
};
