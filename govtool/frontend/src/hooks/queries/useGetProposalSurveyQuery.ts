import { useQuery } from "react-query";

import { QUERY_KEYS } from "@/consts";
import { getProposalSurvey } from "@/services";

export const useGetProposalSurveyQuery = (
  proposalId: string,
  enabled = true,
) => {
  const { data, isLoading, refetch, isRefetching } = useQuery(
    [QUERY_KEYS.useGetProposalSurveyKey, proposalId],
    () => getProposalSurvey(proposalId),
    {
      staleTime: 60_000,
      enabled: enabled && !!proposalId,
    },
  );

  return {
    data,
    isLoading,
    refetch,
    isFetching: isRefetching,
  };
};
