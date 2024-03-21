import { useQuery } from 'react-query';

import { QUERY_KEYS } from '@consts';
import { useCardano } from '@context';
import { getDRepVotingPower } from '@services';

export const useGetDRepVotingPowerQuery = () => {
  const { dRepID, voter } = useCardano();

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
