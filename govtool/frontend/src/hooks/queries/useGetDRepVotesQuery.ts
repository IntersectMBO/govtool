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
    queryFn: () => getDRepVotes({ dRepID, filters, sorting }),
    enabled: !!dRepID,
  });

  const groupedByType = data?.reduce((groups, item) => {
    const itemType = item.proposal.type;

    // TODO: Provide better typing for groups
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-expect-error
    if (!groups[itemType]) {
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error
      groups[itemType] = {
        title: itemType,
        actions: [],
      };
    }
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-expect-error
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
