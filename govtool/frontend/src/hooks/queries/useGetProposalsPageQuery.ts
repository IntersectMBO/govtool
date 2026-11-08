import { keepPreviousData, useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals, GetProposalsArguments } from "@services";
import { isSearchNotReady } from "@utils";

type Args = Omit<GetProposalsArguments, "page" | "dRepID" | "enabled"> & {
  /** 1-based, as the pagination control counts. */
  page: number;
};

/** One page of live governance actions, with the total for the pagination. */
export const useGetProposalsPageQuery = ({
  filters = [],
  page,
  pageSize = 10,
  searchPhrase,
  sorting = "",
}: Args) => {
  const { dRepID, isEnabled, pendingTransaction } = useCardano();

  const { data, error, isLoading, isFetching } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetProposalsPageKey,
      dRepID,
      filters,
      isEnabled,
      pendingTransaction.vote?.transactionHash,
      searchPhrase,
      sorting,
      page,
      pageSize,
    ],
    queryFn: () =>
      getProposals({
        dRepID,
        filters,
        page: page - 1,
        pageSize,
        searchPhrase,
        sorting,
      }),
    placeholderData: keepPreviousData,
    refetchInterval: 20000,
  });

  return {
    proposals: data?.elements,
    proposalsTotal: data?.total ?? 0,
    isProposalsLoading: isLoading,
    isProposalsFetching: isFetching,
    isSearchNotReady: isSearchNotReady(error),
  };
};
