import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotes } from "@services";
import { VotedProposal } from "@/models/api";

export const useGetDRepVotesQuery = (filters: string[], sorting: string) => {
  const { dRepID, voteTransaction } = useCardano();

  const { data, isLoading, refetch, isRefetching } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepVotesKey,
      voteTransaction.transactionHash,
      filters,
      sorting,
    ],
    queryFn: async () => {
      return await getDRepVotes({ dRepID, filters, sorting });
    },
    enabled: !!dRepID,
  });

  const groupedByType = data?.reduce((groups, item) => {
    const itemType = item.proposal.type;

    if (!groups[itemType]) {
      groups[itemType] = {
        title: itemType,
        actions: [],
      };
    }

    groups[itemType].actions.push(item);

    return groups;
  }, {});

  return {
    data: Object.values(groupedByType ?? []) as {
      title: string;
      actions: VotedProposal[];
    }[],
    areDRepVotesLoading: isLoading,
    refetch,
    isRefetching,
  };
};
