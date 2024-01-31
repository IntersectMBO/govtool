import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotingPower } from "@services";

export const useGetDRepVotingPowerQuery = () => {
  const { dRepID: dRepId } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: QUERY_KEYS.useGetDRepVotingPowerKey,
    queryFn: async () => {
      return await getDRepVotingPower({ dRepId });
    },
    enabled: !!dRepId,
  });

  return { data, isLoading };
};
