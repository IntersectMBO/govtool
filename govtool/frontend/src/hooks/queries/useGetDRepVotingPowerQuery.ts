import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotingPower } from "@services";

export const useGetDRepVotingPowerQuery = () => {
  const { dRepID, dRep } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [QUERY_KEYS.useGetDRepVotingPowerKey, dRepID, dRep?.isRegistered],
    queryFn: async () => {
      return await getDRepVotingPower({ dRepID });
    },
    enabled: !!dRepID && dRep?.isRegistered,
  });

  return { dRepVotingPower: data, isDRepVotingPowerLoading: isLoading };
};
