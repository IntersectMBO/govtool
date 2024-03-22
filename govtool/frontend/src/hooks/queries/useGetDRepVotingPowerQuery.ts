import { useQuery } from "react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getDRepVotingPower } from "@services";
import { useGetVoterInfo } from ".";

export const useGetDRepVotingPowerQuery = () => {
  const { dRepID } = useCardano();
  const { voter } = useGetVoterInfo();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepVotingPowerKey,
      dRepID,
      voter?.isRegisteredAsDRep,
      voter?.isRegisteredAsSoleVoter,
    ],
    queryFn: () => getDRepVotingPower({ dRepID }),
    enabled:
      !!dRepID &&
      (!!voter?.isRegisteredAsDRep || !!voter?.isRegisteredAsSoleVoter),
  });

  return { dRepVotingPower: data, isDRepVotingPowerLoading: isLoading };
};
