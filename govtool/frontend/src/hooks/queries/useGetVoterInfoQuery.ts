import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import { useCardano } from "@context";
import { getVoterInfo } from "@services";
import { VOTER_TRANSACTION_EXPIRED_KEY, getItemFromLocalStorage } from "@utils";

const VOTER_INFO_REFRESH_MS = 20_000;
/** How long the voter info keeps reading after a registration expired. */
const EXPIRED_TRANSACTION_WATCH_MS = 30 * 60 * 1000;

/** Whether a registration or retirement of this wallet expired recently. */
const recentlyExpired = (stakeKey: string | undefined) => {
  const expiredAt = getItemFromLocalStorage(
    `${VOTER_TRANSACTION_EXPIRED_KEY}_${stakeKey}`,
  );
  return (
    typeof expiredAt === "number" &&
    Date.now() - expiredAt < EXPIRED_TRANSACTION_WATCH_MS
  );
};

export const useGetVoterInfo = (options?: { enabled?: boolean }) => {
  const { dRepID, pendingTransaction, stakeKey } = useCardano();
  const voterTransaction =
    pendingTransaction?.registerAsDrep ||
    pendingTransaction?.registerAsDirectVoter ||
    pendingTransaction?.retireAsDrep ||
    pendingTransaction?.retireAsDirectVoter;
  const { data } = useQuery({
    queryKey: [
      QUERY_KEYS.useGetDRepInfoKey,
      voterTransaction?.transactionHash,
      // Last, so a pending transaction's key [key, hash] still prefixes it.
      dRepID,
    ],
    enabled: !!dRepID && (options?.enabled ?? true),
    queryFn: () => getVoterInfo(dRepID),
    // Read again while a registration or retirement is pending, and for a
    // while after the frontend stopped waiting for one: a registration that
    // lands after its 3-minute expiry would otherwise show only on reload.
    // No other wallet polls.
    refetchInterval: () => {
      if (voterTransaction || recentlyExpired(stakeKey)) {
        return VOTER_INFO_REFRESH_MS;
      }
      return false;
    },
  });

  return { voter: data };
};
