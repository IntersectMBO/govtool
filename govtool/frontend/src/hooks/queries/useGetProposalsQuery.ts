import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals, getProposalsArguments } from "@services";

export const useGetProposalsQuery = ({
  filters = [],
  sorting,
}: getProposalsArguments) => {
  const { dRepID, isEnabled, voteTransaction } = useCardano();

  const fetchProposals = async (): Promise<ActionType[]> => {
    const allProposals = await Promise.all(
      filters.map((filter) =>
        getProposals({ dRepID, filters: [filter], sorting }),
      ),
    );

    return allProposals.flatMap((proposal) => proposal.elements);
  };

  const { data, isLoading } = useQuery(
    [
      QUERY_KEYS.useGetProposalsKey,
      filters,
      sorting,
      voteTransaction.proposalId,
      isEnabled,
      dRepID,
    ],
    fetchProposals,
  );

  return {
    isProposalsLoading: isLoading,
    proposals: data,
  };
};
