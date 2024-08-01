import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotes } from "@services";
import { VotedProposal } from "@/models";

export const useGetDRepVotesQuery = (
  type?: string[],
  sort?: string,
  search?: string,
) => {
  const { dRepID, pendingTransaction } = useCardano();

  const { data, isLoading, refetch, isRefetching } = useQuery({
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
  });

  const groupedByType = data?.reduce((groups, item) => {
    const itemType = item?.proposal.type;

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
