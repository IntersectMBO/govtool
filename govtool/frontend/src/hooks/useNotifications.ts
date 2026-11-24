import { useCallback, useEffect, useMemo, useState } from "react";
import {
  useInfiniteQuery,
  useIsFetching,
  useQueries,
  useQuery,
  useQueryClient,
} from "@tanstack/react-query";
import { Address } from "@emurgo/cardano-serialization-lib-asmjs";

import { useCardano } from "@context";
import { QUERY_KEYS } from "@/consts/queryKeys";
import { useGetAdaHolderCurrentDelegationQuery } from "@/hooks/queries/useGetAdaHolderCurrentDelegationQuery";
import { useGetAdaHolderVotingPowerQuery } from "@/hooks/queries/useGetAdaHolderVotingPowerQuery";
import { useGetDecidedActionsQuery } from "@/hooks/queries/useGetDecidedActionsQuery";
import { useGetDRepDetailsQuery } from "@/hooks/queries/useGetDrepDetailsQuery";
import { useGetDRepVotesByIdQuery } from "@/hooks/queries/useGetDRepVotesByIdQuery";
import { useGetLiveProposalsQuery } from "@/hooks/queries/useGetLiveProposalsQuery";
import { useGetVoterInfo } from "@/hooks/queries/useGetVoterInfoQuery";
import type {
  GovernanceActionRecord,
  GovToolNotification,
  NotificationEvent,
  NotificationFilter,
  NotificationPreferences,
  ProposalData,
} from "@/models";
import { getGovernanceActionRecord } from "@/services/requests/governanceActions";
import {
  getClosingProposals,
  getNotificationPreferences,
  getNotificationsConfig,
  getNotificationsPage,
  markAllNotificationsRead,
  markNotificationsRead,
  requestNotificationsNonce,
  saveNotificationPreferences,
  signInToNotifications,
} from "@/services/requests/notifications";
import { AutomatedVotingOptionCurrentDelegation } from "@/types/automatedVotingOptions";
import {
  DEFAULT_PREFERENCES,
  NOTIFICATION_EVENTS,
  NOTIFICATION_FILTERS,
  countByFilter,
  deriveNotifications,
  isValidNotification,
  loadPreferences,
  loadStore,
  matchesFilter,
  savePreferences as saveLocalPreferences,
  saveStore,
  withRead,
  type NotificationStore,
} from "@/utils/notifications";

export const NOTIFICATION_PAGE_SIZE = 7;

const DAY = 24 * 60 * 60 * 1000;
// Votes recent enough that their action may have been decided in the past
// 30 days (an action is live for at most about 30 days), and a request cap.
const VOTE_LOOKBACK = 75 * DAY;
const MAX_VOTE_RECORDS = 40;

type Session = { token: string; expiresAt: string };

const tokenStorageKey = (stakeKey: string) => `govtool_notifications_token_${stakeKey}`;

const readSession = (stakeKey: string): string | null => {
  try {
    const stored = JSON.parse(sessionStorage.getItem(tokenStorageKey(stakeKey)) ?? "null");
    return stored?.token && new Date(stored.expiresAt) > new Date() ? stored.token : null;
  } catch {
    return null;
  }
};

const writeSession = (stakeKey: string, session: Session | null) => {
  try {
    if (session) sessionStorage.setItem(tokenStorageKey(stakeKey), JSON.stringify(session));
    else sessionStorage.removeItem(tokenStorageKey(stakeKey));
  } catch {
    // The session then lasts only as long as this page.
  }
};

const toBech32 = (hex: string) => {
  try {
    return Address.from_hex(hex).to_bech32();
  } catch {
    return hex;
  }
};

const toHex = (text: string) =>
  Array.from(new TextEncoder().encode(text))
    .map((byte) => byte.toString(16).padStart(2, "0"))
    .join("");

const isUnauthorized = (error: unknown) =>
  (error as { response?: { status?: number } })?.response?.status === 401;

const ZERO_COUNTS = Object.fromEntries(NOTIFICATION_FILTERS.map((f) => [f, 0])) as Record<
  NotificationFilter,
  number
>;

const actionKey = (txHash: string, index: number) => `${txHash}#${index}`;

type Options = {
  filter?: NotificationFilter;
  sort?: "newest" | "oldest";
  pageSize?: number;
};

/**
 * The wallet's notifications, from the notifications service when one runs
 * (docs/api/notifications.md) or worked out in this browser when none does.
 * Both modes give the same shape, so the bell and the Notification Center
 * never need to know which one they read. Shared state (the session, the local
 * store, preferences) lives in the query cache, so every instance agrees.
 */
export const useNotifications = ({
  filter = "all",
  sort = "newest",
  pageSize = NOTIFICATION_PAGE_SIZE,
}: Options = {}) => {
  const { isEnabled, stakeKey, dRepID, walletApi, pendingTransaction } = useCardano();
  const queryClient = useQueryClient();
  const isConnected = Boolean(isEnabled && stakeKey);
  const stake = stakeKey ?? "";

  const configQuery = useQuery({
    queryKey: [QUERY_KEYS.notificationsConfigKey],
    queryFn: getNotificationsConfig,
    enabled: isConnected,
    retry: false,
    staleTime: Infinity,
  });
  const config = configQuery.data;
  let mode: "off" | "loading" | "local" | "remote" = "off";
  if (isConnected) mode = configQuery.isLoading ? "loading" : (config && "remote") || "local";
  const isLocal = mode === "local";
  const isRemote = mode === "remote";

  // The session, shared by every instance through the cache.
  const sessionKey = [QUERY_KEYS.notificationsTokenKey, stake];
  const sessionQuery = useQuery({
    queryKey: sessionKey,
    queryFn: () => readSession(stake),
    enabled: isRemote,
    staleTime: Infinity,
  });
  const token = isRemote ? sessionQuery.data ?? null : null;
  const hasToken = !!token;

  const resetRemote = () => {
    queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.notificationsPageKey] });
    queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.notificationsPreferencesKey] });
  };
  const setSession = (session: Session | null) => {
    writeSession(stake, session);
    queryClient.setQueryData(sessionKey, session?.token ?? null);
    resetRemote();
  };

  // What local mode derives from.
  const { voter } = useGetVoterInfo({ enabled: isLocal });
  const isVoter = !!(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter);
  const { currentDelegation, isCurrentDelegationLoading } = useGetAdaHolderCurrentDelegationQuery(
    isLocal ? stakeKey : undefined,
  );
  // A refetch can briefly show the previous delegation (the query key has no
  // stake key, and a delegation in flight changes it): no snapshot meanwhile.
  const isDelegationFetching =
    useIsFetching({ queryKey: [QUERY_KEYS.getAdaHolderCurrentDelegationKey] }) > 0;
  const isDelegationSettled = !isDelegationFetching && !pendingTransaction?.delegate;
  const view = currentDelegation?.dRepView ?? undefined;
  const delegatedView =
    view &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_abstain &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence
      ? view
      : undefined;
  const { dRep } = useGetDRepDetailsQuery(isLocal ? delegatedView : undefined);
  const { votingPower } = useGetAdaHolderVotingPowerQuery(isLocal ? stakeKey : undefined);
  // Wait for voter info, so the votes read are the right ones from the start.
  const { dRepVotes } = useGetDRepVotesByIdQuery(
    isLocal && voter ? (isVoter ? dRepID : delegatedView) : undefined,
  );
  const { liveProposals } = useGetLiveProposalsQuery();
  const { decidedActions } = useGetDecidedActionsQuery();
  const closingQuery = useQuery({
    queryKey: [QUERY_KEYS.notificationsClosingKey],
    queryFn: getClosingProposals,
    enabled: isLocal && isVoter,
  });

  // The outcome of each recent vote whose action is no longer live, read per
  // action rather than from the capped list of the newest decisions.
  const liveKeys = new Set(liveProposals.map((p) => actionKey(p.txHash, p.index)));
  const votedRecordIds = (dRepVotes ?? [])
    .filter(({ vote, proposal }) => {
      const at = vote?.date ? new Date(vote.date).getTime() : NaN;
      const isRecent = !(Date.now() - at > VOTE_LOOKBACK);
      return isRecent && !liveKeys.has(actionKey(proposal.txHash, proposal.index));
    })
    .slice(0, MAX_VOTE_RECORDS)
    .map(({ proposal }) => actionKey(proposal.txHash, proposal.index));
  const votedRecords = useQueries({
    queries: (isLocal ? votedRecordIds : []).map((id) => ({
      queryKey: [QUERY_KEYS.useGetGovernanceActionRecordKey, id],
      queryFn: () => getGovernanceActionRecord(id),
      staleTime: 5 * 60 * 1000,
    })),
  });
  const votedRecordData = votedRecords
    .map((query) => query.data)
    .filter((record): record is GovernanceActionRecord => !!record);
  const votedRecordSignature = votedRecordData.map((record) => record.id).join();

  const storeKey = [QUERY_KEYS.notificationsLocalStoreKey, stake];
  const storeQuery = useQuery({
    queryKey: storeKey,
    queryFn: () => loadStore(stake, new Date()),
    enabled: isLocal,
    staleTime: Infinity,
  });

  const preferencesKey = [QUERY_KEYS.notificationsPreferencesKey, mode, stake, hasToken];
  const preferencesQuery = useQuery({
    queryKey: preferencesKey,
    queryFn: () => (token ? getNotificationPreferences(token) : loadPreferences(stake)),
    enabled: isLocal || hasToken,
    staleTime: Infinity,
  });
  const preferences = preferencesQuery.data ?? DEFAULT_PREFERENCES;

  const live = useMemo(() => {
    const byKey = new Map<string, ProposalData>();
    [...liveProposals, ...(closingQuery.data?.elements ?? [])].forEach((proposal) =>
      byKey.set(actionKey(proposal.txHash, proposal.index), proposal));
    return [...byKey.values()];
  }, [liveProposals, closingQuery.data]);

  const decided = useMemo(() => {
    const byKey = new Map<string, GovernanceActionRecord>();
    [...decidedActions, ...votedRecordData].forEach((record) =>
      byKey.set(actionKey(record.tx_hash, record.index), record));
    return [...byKey.values()];
  }, [decidedActions, votedRecordSignature]);

  const derived = useMemo(() => {
    if (!isLocal || !storeQuery.data) return undefined;
    return deriveNotifications(
      {
        delegation: isDelegationSettled ? currentDelegation : undefined,
        delegatedDRep: dRep && {
          view: dRep.view,
          status: dRep.status,
          givenName: dRep.givenName || null,
        },
        votes: dRepVotes,
        voteRelation: isVoter ? "voted" : "dRepVoted",
        isVoter,
        live,
        decided,
        votingPower,
      },
      storeQuery.data,
      preferences,
      new Date(),
    );
  }, [
    isLocal,
    storeQuery.data,
    isDelegationSettled,
    currentDelegation,
    dRep,
    dRepVotes,
    isVoter,
    live,
    decided,
    votingPower,
    preferences,
  ]);

  // Keep what the derivation saw (first-seen times, the delegation).
  useEffect(() => {
    if (!derived || !storeQuery.data) return;
    if (JSON.stringify(derived.store) === JSON.stringify(storeQuery.data)) return;
    saveStore(stake, derived.store);
    queryClient.setQueryData(storeKey, derived.store);
  }, [derived]);

  const [pages, setPages] = useState(1);
  useEffect(() => setPages(1), [filter, sort]);

  const pageQuery = useInfiniteQuery({
    queryKey: [QUERY_KEYS.notificationsPageKey, stake, hasToken, filter, sort, pageSize],
    queryFn: ({ pageParam }) =>
      getNotificationsPage(token as string, { page: pageParam, pageSize, filter, sort }),
    initialPageParam: 1,
    getNextPageParam: (last, all) =>
      (all.length * pageSize < last.total ? all.length + 1 : undefined),
    enabled: hasToken,
    retry: false,
  });

  // An expired session asks for a new signature.
  useEffect(() => {
    if (isUnauthorized(pageQuery.error) || isUnauthorized(preferencesQuery.error)) {
      setSession(null);
    }
  }, [pageQuery.error, preferencesQuery.error]);

  const localItems = derived?.items ?? [];
  const localFiltered = localItems.filter((item) => matchesFilter(item, filter));
  if (sort === "oldest") localFiltered.reverse();
  const firstPage = pageQuery.data?.pages[0];
  const counts = isLocal ? countByFilter(localItems) : { ...ZERO_COUNTS, ...firstPage?.counts };

  /** A remote write; an expired session asks to sign in again. */
  const remote = async (write: () => Promise<unknown>) => {
    try {
      await write();
    } catch (error) {
      if (isUnauthorized(error)) setSession(null);
      throw error;
    } finally {
      queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.notificationsPageKey] });
    }
  };

  const markRead = useCallback(
    async (ids: string[], read: boolean) => {
      if (isRemote) {
        if (!token) return;
        await remote(() => markNotificationsRead(token, ids, read)).catch(() => undefined);
        return;
      }
      const store = queryClient.getQueryData<NotificationStore>(storeKey);
      if (!store) return;
      const next = withRead(store, ids, read);
      saveStore(stake, next);
      queryClient.setQueryData(storeKey, next);
    },
    [isRemote, token, stake],
  );

  const markAllRead = useCallback(async () => {
    if (isRemote) {
      if (!token) return;
      await remote(() => markAllNotificationsRead(token)).catch(() => undefined);
      return;
    }
    await markRead(
      localItems.map((item) => item.id),
      true,
    );
  }, [isRemote, token, localItems, markRead]);

  /** Throws when it cannot save; never saves locally while a service runs. */
  const savePreferences = useCallback(
    async (next: NotificationPreferences) => {
      if (isRemote) {
        if (!token) throw new Error("Sign in to the notifications service first");
        let saved = next;
        await remote(async () => {
          saved = await saveNotificationPreferences(token, next);
        });
        queryClient.setQueryData(preferencesKey, saved);
        return;
      }
      saveLocalPreferences(stake, next);
      queryClient.setQueryData(preferencesKey, next);
    },
    [isRemote, token, stake, mode],
  );

  const signIn = useCallback(async () => {
    if (!walletApi || !stake) throw new Error("No wallet connected");
    const stakeAddress = toBech32(stake);
    const { nonce } = await requestNotificationsNonce(stakeAddress);
    const { signature, key } = await walletApi.signData(stake, toHex(nonce));
    const session = await signInToNotifications({ stakeAddress, nonce, signature, key });
    setSession(session);
  }, [walletApi, stake]);

  const remoteItems = useMemo(() => {
    const seen = new Set<string>();
    return (pageQuery.data?.pages ?? [])
      .flatMap((page) => page.items)
      .filter((item) => {
        if (!isValidNotification(item) || seen.has(item.id)) return false;
        seen.add(item.id);
        return true;
      });
  }, [pageQuery.data]);

  return {
    mode,
    needsSignIn: isRemote && sessionQuery.isFetched && !token,
    signIn,
    items: (isLocal
      ? localFiltered.slice(0, pages * pageSize)
      : remoteItems) as GovToolNotification[],
    total: isLocal ? localFiltered.length : firstPage?.total ?? 0,
    counts,
    unread: isLocal ? counts.unread : firstPage?.unread ?? 0,
    hasMore: isLocal ? localFiltered.length > pages * pageSize : !!pageQuery.hasNextPage,
    loadMore: () => {
      if (isLocal) setPages((current) => current + 1);
      else if (pageQuery.hasNextPage) pageQuery.fetchNextPage();
    },
    isLoading:
      mode === "loading" ||
      (isLocal && (!derived || !!isCurrentDelegationLoading)) ||
      (isRemote && !sessionQuery.isFetched) ||
      (hasToken && pageQuery.isLoading),
    isError: hasToken && pageQuery.isError && !isUnauthorized(pageQuery.error),
    retry: () => {
      pageQuery.refetch();
    },
    markRead,
    markAllRead,
    preferences,
    savePreferences,
    capabilities: {
      email: isRemote && !!config?.channels.email,
      emailSetupUrl: isRemote ? config?.emailSetupUrl ?? null : null,
      crossDevice: isRemote,
      events: (config?.events.filter((kind) => kind !== "system") ??
        NOTIFICATION_EVENTS) as NotificationEvent[],
    },
  };
};
