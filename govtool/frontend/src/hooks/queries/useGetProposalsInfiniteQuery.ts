import { useInfiniteQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals, GetProposalsArguments } from "@services";
import { checkIsMissingGAMetadata } from "@utils";

export const useGetProposalsInfiniteQuery = ({
  filters = [],
  pageSize = 10,
  searchPhrase,
  sorting = "",
}: GetProposalsArguments) => {
  const { dRepID, isEnabled, pendingTransaction } = useCardano();

  const fetchProposals = async ({ pageParam = 0 }) => {
    const data = await getProposals({
      dRepID,
      filters,
      page: pageParam,
      pageSize,
      searchPhrase,
      sorting,
    });
    const mappedElements = await Promise.all(
      data.elements.map(async (proposal: ActionType) => {
        const isDataMissing = await checkIsMissingGAMetadata({
          hash: proposal?.metadataHash ?? "",
          url: proposal?.url ?? "",
        });
        return { ...proposal, isDataMissing };
      }),
    );

    return { ...data, elements: mappedElements };
  };

  const {
    data,
    isLoading,
    fetchNextPage,
    hasNextPage,
    isFetching,
    isFetchingNextPage,
  } = useInfiniteQuery(
    [
      QUERY_KEYS.useGetProposalsInfiniteKey,
      dRepID,
      filters,
      isEnabled,
      pendingTransaction.vote?.transactionHash,
      searchPhrase,
      sorting,
    ],
    fetchProposals,
    {
      getNextPageParam: (lastPage) => {
        if (lastPage.elements.length === 0) {
          return undefined;
        }
        return lastPage.page + 1;
      },
      refetchInterval: 20000,
    },
  );

  const proposals = data?.pages.flatMap(
    (page) => page.elements,
  ) as ActionTypeToDsiplay[];

  return {
    proposalsfetchNextPage: fetchNextPage,
    proposalsHaveNextPage: hasNextPage,
    isProposalsFetching: isFetching,
    isProposalsFetchingNextPage: isFetchingNextPage,
    isProposalsLoading: isLoading,
    proposals,
  };
};
