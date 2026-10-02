import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getGovernanceActionProposalDiscussion } from "@services";

/** The discussion forum proposal that became the action submitted in `txHash`. */
export const useGetGovernanceActionProposalDiscussionQuery = (txHash?: string) => {
  const { data, isLoading, error } = useQuery({
    queryKey: [QUERY_KEYS.useGetGovernanceActionProposalDiscussionKey, txHash],
    queryFn: () => getGovernanceActionProposalDiscussion(txHash as string),
    enabled: !!txHash,
  });

  return {
    proposal: data,
    isProposalLoading: isLoading,
    proposalError: error,
  };
};
