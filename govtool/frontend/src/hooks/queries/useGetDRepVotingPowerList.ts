import { useMemo } from "react";
import { useQuery, useQueryClient } from "react-query";
import { getDRepVotingPowerList } from "@/services";
import { QUERY_KEYS } from "@/consts";

export const useGetDRepVotingPowerList = () => {
  const queryClient = useQueryClient();

  const {
    data: dRepVotingPowerList,
    isError,
    error,
    isLoading,
  } = useQuery({
    queryKey: [QUERY_KEYS.useGetDRepVotingPowerListKey],
    queryFn: () => getDRepVotingPowerList([]),
    enabled: false,
  });

  const fetchDRepVotingPowerList = async (identifiers: string[] = []) =>
    queryClient.fetchQuery({
      queryKey: [QUERY_KEYS.useGetDRepVotingPowerListKey, ...identifiers],
      queryFn: () => getDRepVotingPowerList(identifiers),
    });

  const contextValue = useMemo(
    () => ({
      dRepVotingPowerList,
      fetchDRepVotingPowerList,
      isError,
      error,
      isLoading,
    }),
    [dRepVotingPowerList, fetchDRepVotingPowerList],
  );

  return contextValue;
};
