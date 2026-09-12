import { keepPreviousData, useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotes } from "@services";
import { VotedProposal } from "@/models";

export const useGetDRepVotesQuery = (
  type?: string[],
  sort?: string,
  search?: string
) => {
  const { dRepID, pendingTransaction } = useCardano();

  const { data, isLoading, refetch, isFetching } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepVotesKey,
      pendingTransaction.vote?.transactionHash,
      type,
      sort,
      search,
    ],
    queryFn: () =>
      getDRepVotes({
        dRepID,
        params: {
          ...(search && { search }),
          ...(sort && { sort }),
          ...(type && { type }),
        },
      }),
    enabled: !!dRepID,
    refetchOnWindowFocus: true,
    placeholderData: keepPreviousData,
    refetchInterval: 20000,
  });

  const groupedByType = data?.reduce<
    Record<string, { title: string; actions: VotedProposal[] }>
  >((groups, item) => {
    const itemType = item?.proposal.type;

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
    isFetching,
  };
};
