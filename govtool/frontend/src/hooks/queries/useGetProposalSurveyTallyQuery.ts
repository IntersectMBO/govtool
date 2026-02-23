import { useQuery } from "react-query";

import { QUERY_KEYS } from "@/consts";
import { getProposalSurveyTally } from "@/services";

export const useGetProposalSurveyTallyQuery = (
  proposalId: string,
  weighting: "CredentialBased" | "StakeBased" = "CredentialBased",
  enabled = true,
) => {
  const { data, isLoading, refetch, isRefetching } = useQuery(
    [QUERY_KEYS.useGetProposalSurveyTallyKey, proposalId, weighting],
    () => getProposalSurveyTally(proposalId, weighting),
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
