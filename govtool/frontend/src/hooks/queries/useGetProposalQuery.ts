import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposal } from "@services";

export const useGetProposalQuery = (proposalId: string, enabled?: boolean) => {
  const { dRepID } = useCardano();

  const request = useQuery(
    [QUERY_KEYS.useGetProposalKey, dRepID],
    async () => await getProposal(proposalId, dRepID),
    {
      staleTime: Infinity,
      enabled,
    },
  );

  const data = request.data as {
    proposal: ActionType;
    vote: {
      proposalId: string;
      drepId: string;
      vote: string;
      url: string;
      metadataHash: string;
    };
  };

  return {
    data,
    isLoading: request.isLoading,
    refetch: request.refetch,
    isFetching: request.isRefetching,
  };
};
