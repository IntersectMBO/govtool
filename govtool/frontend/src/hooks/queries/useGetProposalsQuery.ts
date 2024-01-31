import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getProposals } from "@services";

export const useGetProposalsQuery = (
  filters: string[],
  sorting: string
): {
  proposals: ActionType[];
  isLoading: boolean;
} => {
  const { voteTransaction, isEnabled } = useCardano();

  const fetchProposals = async () => {
    const allProposals = await Promise.all(
      filters.map((filter) => getProposals([filter], sorting))
    );

    return allProposals.flatMap((proposal) => proposal.elements || []);
  };

  const request = useQuery(
    [
      QUERY_KEYS.useGetProposalsKey,
      filters,
      sorting,
      voteTransaction.proposalId,
      isEnabled,
    ],
    fetchProposals
  );

  const proposals = request.data as ActionType[];

  return {
    proposals,
    isLoading: request.isLoading,
  };
};
