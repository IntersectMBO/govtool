import { useQueries } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts/queryKeys";
import { getStakeKeyVotingPower } from "@/services/requests/walletConnect";

/** Voting power per stake key: lovelace, or undefined while loading or failed. */
export const useStakeKeyVotingPowers = (stakeKeys: string[]) => {
  const results = useQueries({
    queries: stakeKeys.map((key) => ({
      queryKey: [QUERY_KEYS.stakeKeyVotingPowerKey, key],
      queryFn: () => getStakeKeyVotingPower(key),
      enabled: !!key,
      staleTime: 60_000,
    })),
  });
  return {
    powers: Object.fromEntries(
      stakeKeys.map((key, i) => [key, results[i]?.data]),
    ) as Record<string, number | undefined>,
    errors: Object.fromEntries(
      stakeKeys.map((key, i) => [key, !!results[i]?.isError]),
    ) as Record<string, boolean>,
    isLoading: results.some((result) => result.isLoading),
    isError: results.some((result) => result.isError),
  };
};
