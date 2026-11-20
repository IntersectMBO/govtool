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
import { participationRate, pickDelegatorCount } from "@/utils/drepDashboard";

import { useGetDRepVotesByIdQuery } from "./useGetDRepVotesByIdQuery";

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
    // main.tsx turns this off app-wide; outcomes move while the user is away.
    refetchOnMount: true,
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
    queryFn: async () => {
      const votes = await getDRepVotesById(dRepID ?? "");
      return Array.isArray(votes) ? votes : [];
    },
    enabled: !!dRepID,
    // A vote cast elsewhere confirms while the dashboard is unmounted, and
    // main.tsx turns this off app-wide.
    refetchOnMount: true,
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

/**
 * A directory card's participation and delegator count. Participation comes
 * from the DRep's votes (the profile's query, so opening the profile reuses
 * it) and every action; the count from the delegators route, read only once
 * the backend serves it.
 */
export const useDRepCardStats = ({
  drepId,
  view,
  latestRegistrationDate,
  delegatorCount,
}: {
  drepId: string;
  view: string;
  latestRegistrationDate?: string;
  /** The /drep/list row's count, where the backend sends one. */
  delegatorCount?: number | null;
}) => {
  const { epochParams } = useAppContext();
  const votes = useGetDRepVotesByIdQuery(view);
  const actions = useDRepDashboardActions();
  // The route is asked only when the row carries no count.
  const route = useDRepDelegators(typeof delegatorCount === "number" ? undefined : drepId, 1, 1);
  const delegators = pickDelegatorCount(delegatorCount, {
    isServed: route.isServed,
    total: route.data?.total,
    isLoading: route.isLoading,
  });
  const currentEpoch = typeof epochParams?.epoch_no === "number" ? epochParams.epoch_no : undefined;
  const participation =
    votes.dRepVotes && actions.actions && currentEpoch !== undefined
      ? participationRate(
          Array.isArray(votes.dRepVotes) ? votes.dRepVotes : [],
          actions.actions,
          latestRegistrationDate,
          currentEpoch,
        )
      : null;
  return {
    /** A whole percentage, or null when it cannot be worked out. */
    participation,
    isParticipationLoading: votes.areDRepVotesLoading || actions.isLoading,
    /** Undefined while there is none to show (see pickDelegatorCount). */
    delegators: delegators.count,
    areDelegatorsServed: delegators.show,
    areDelegatorsLoading: delegators.isLoading,
  };
};
