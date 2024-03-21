import { useQuery } from 'react-query';

import { QUERY_KEYS } from '@consts';
import { useCardano } from '@context';
import { getVoterInfo } from '@services';

export const useGetVoterInfo = () => {
  const { dRepID, registerTransaction, soleVoterTransaction } = useCardano();

  const { data, isLoading } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepInfoKey,
      registerTransaction?.transactionHash,
      soleVoterTransaction?.transactionHash,
    ],
    enabled: !!dRepID,
    queryFn: () => getVoterInfo(dRepID),
  });

  return { data, isLoading };
};
