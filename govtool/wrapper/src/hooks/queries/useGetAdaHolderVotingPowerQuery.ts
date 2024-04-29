import { useQuery } from "react-query";

import { getAdaHolderVotingPower } from "@services";
import { QUERY_KEYS } from "@consts";

const REFRESH_TIME = 20 * 1000;

export const useGetAdaHolderVotingPowerQuery = (stakeKey?: string) => {
  const { data, isLoading } = useQuery({
    queryKey: QUERY_KEYS.getAdaHolderVotingPowerKey,
    queryFn: () => getAdaHolderVotingPower({ stakeKey }),
    enabled: !!stakeKey,
    refetchInterval: REFRESH_TIME,
  });

  return { votingPower: data, powerIsLoading: isLoading };
};
