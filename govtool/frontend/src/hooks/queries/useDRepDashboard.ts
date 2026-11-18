import { keepPreviousData, useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@/consts/queryKeys";
import { useAppContext } from "@/context/appContext";
import { useCardano } from "@/context/wallet";
import { useFeatureFlag } from "@/context/featureFlag";
import type { FeatureId } from "@/models/featureSet";
import {
  getAllGovernanceActions,
  getDelegationHistory,
  getDRepDelegators,
  getDRepDiscussions,
  getDRepHistory,
  getDRepVotesById,
  getDRepVotingPowerHistory,
} from "@/services/requests";

const FIVE_MINUTES = 5 * 60 * 1000;

/**
 * True only when the backend's feature set says it serves the feature. Unlike
 * useFeatureFlag's gates this fails closed: the dashboard calls these routes
 * only once a backend has declared them, so an older backend gets no 404s.
 */
export const useIsFeatureServed = (feature: FeatureId) => {
  const { capabilitiesStatus, featureSet } = useAppContext();
  return (
    capabilitiesStatus === "ready" &&
    !!featureSet &&
    !featureSet.unavailable?.[feature]
  );
};

/** Every governance action record, for outcomes and participation. */
export const useDRepDashboardActions = () => {
  const { data, isLoading, isError, refetch, dataUpdatedAt } = useQuery({
    queryKey: [QUERY_KEYS.drepDashboardActionsKey],
    queryFn: getAllGovernanceActions,
    staleTime: FIVE_MINUTES,
  });
  return { actions: data, isLoading, isError, refetch, dataUpdatedAt };
};

/** The connected DRep's votes, refreshed after its own vote lands. */
export const useDRepDashboardVotes = (dRepID?: string) => {
  const { pendingTransaction } = useCardano();
  const { data, isLoading, isError, refetch, dataUpdatedAt } = useQuery({
    queryKey: [
      QUERY_KEYS.drepDashboardVotesKey,
      pendingTransaction.vote?.transactionHash,
      dRepID,
    ],
    queryFn: () => getDRepVotesById(dRepID ?? ""),
    enabled: !!dRepID,
  });
  return { votes: data, isLoading, isError, refetch, dataUpdatedAt };
};

export const useDRepDelegators = (drepId: string | undefined, page: number, pageSize: number) => {
  const isServed = useIsFeatureServed("drep.delegatorList");
  const query = useQuery({
    queryKey: [QUERY_KEYS.drepDashboardDelegatorsKey, drepId, page, pageSize],
    queryFn: () => getDRepDelegators(drepId ?? "", page, pageSize),
    enabled: isServed && !!drepId,
    placeholderData: keepPreviousData,
  });
  return { isServed, ...query };
};

export const useDRepHistory = (
  drepId: string | undefined,
  page: number,
  pageSize: number,
  sort: "desc" | "asc",
) => {
  const isServed = useIsFeatureServed("drep.registrationHistory");
  const { pendingTransaction } = useCardano();
  const query = useQuery({
    queryKey: [
      QUERY_KEYS.drepDashboardHistoryKey,
      pendingTransaction.updateMetaData?.transactionHash,
      drepId,
      page,
      pageSize,
      sort,
    ],
    queryFn: () => getDRepHistory(drepId ?? "", page, pageSize, sort),
    enabled: isServed && !!drepId,
    placeholderData: keepPreviousData,
  });
  return { isServed, ...query };
};

export const useDelegationHistory = (stakeKey?: string) => {
  const isServed = useIsFeatureServed("account.delegationHistory");
  const { pendingTransaction } = useCardano();
  const query = useQuery({
    queryKey: [
      QUERY_KEYS.drepDashboardDelegationHistoryKey,
      pendingTransaction.delegate?.transactionHash,
      stakeKey,
    ],
    queryFn: () => getDelegationHistory(stakeKey ?? ""),
    enabled: isServed && !!stakeKey,
  });
  return { isServed, ...query };
};

export const useDRepVotingPowerHistory = (drepId?: string) => {
  const isServed = useIsFeatureServed("drep.votingPowerHistory");
  const query = useQuery({
    queryKey: [QUERY_KEYS.drepDashboardVotingPowerKey, drepId],
    queryFn: () => getDRepVotingPowerHistory(drepId ?? ""),
    enabled: isServed && !!drepId,
    staleTime: FIVE_MINUTES,
  });
  return { isServed, ...query };
};

/** Forum threads the DRep took part in; off while the forum is off. */
export const useDRepDiscussions = (dRepHash?: string) => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const query = useQuery({
    queryKey: [QUERY_KEYS.drepDashboardDiscussionsKey, dRepHash],
    queryFn: () => getDRepDiscussions(dRepHash ?? ""),
    enabled: isProposalDiscussionForumEnabled && !!dRepHash,
    staleTime: FIVE_MINUTES,
  });
  return { isForumEnabled: isProposalDiscussionForumEnabled, ...query };
};
