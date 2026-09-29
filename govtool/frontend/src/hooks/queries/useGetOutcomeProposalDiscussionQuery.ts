import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { getOutcomeProposalDiscussion } from "@services";

/** The discussion forum proposal that became the action submitted in `txHash`. */
export const useGetOutcomeProposalDiscussionQuery = (txHash?: string) => {
  const { data, isLoading, error } = useQuery({
    queryKey: [QUERY_KEYS.useGetOutcomeProposalDiscussionKey, txHash],
    queryFn: () => getOutcomeProposalDiscussion(txHash as string),
    enabled: !!txHash,
  });

  return {
    proposal: data,
    isProposalLoading: isLoading,
    proposalError: error,
  };
};
