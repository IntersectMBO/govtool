import { useCallback, useEffect, useMemo, useState } from "react";
import { useInfiniteQuery, useQuery, useQueryClient } from "@tanstack/react-query";
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
  GovToolNotification,
  NotificationEvent,
  NotificationFilter,
  NotificationKind,
  NotificationPreferences,
} from "@/models";
import {
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
  loadPreferences,
  loadStore,
  matchesFilter,
  savePreferences as saveLocalPreferences,
  saveStore,
  withRead,
  type NotificationStore,
} from "@/utils/notifications";

export const NOTIFICATION_PAGE_SIZE = 7;

const KNOWN_KINDS = new Set<NotificationKind>([
  "drepChange",
  "delegationChange",
  "actionActivity",
  "relevantAction",
  "digest",
  "system",
]);

const tokenKey = (stakeKey: string) => `govtool_notifications_token_${stakeKey}`;

const readToken = (stakeKey: string) => {
  try {
    const stored = JSON.parse(sessionStorage.getItem(tokenKey(stakeKey)) ?? "null");
    return stored?.token && new Date(stored.expiresAt) > new Date()
      ? (stored.token as string)
      : undefined;
  } catch {
    return undefined;
  }
};

const writeToken = (stakeKey: string, value: { token: string; expiresAt: string } | null) => {
  try {
    if (value) sessionStorage.setItem(tokenKey(stakeKey), JSON.stringify(value));
    else sessionStorage.removeItem(tokenKey(stakeKey));
  } catch {
    // The token then lasts only as long as this page.
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

type Options = {
  filter?: NotificationFilter;
  sort?: "newest" | "oldest";
  pageSize?: number;
};

/**
 * The wallet's notifications, from the notifications service when one runs
 * (docs/api/notifications.md) or worked out in this browser when none does.
 * Both modes give the same shape, so the bell and the Notification Center
 * never need to know which one they read.
 */
export const useNotifications = ({
  filter = "all",
  sort = "newest",
  pageSize = NOTIFICATION_PAGE_SIZE,
}: Options = {}) => {
  const { isEnabled, stakeKey, dRepID, walletApi } = useCardano();
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

  const [token, setToken] = useState(() => readToken(stake));
  useEffect(() => setToken(readToken(stake)), [stake]);
  const hasToken = isRemote && !!token;

  // What local mode derives from.
  const { voter } = useGetVoterInfo({ enabled: isLocal });
  const isVoter = !!(voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter);
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(
    isLocal ? stakeKey : undefined,
  );
  const view = currentDelegation?.dRepView ?? undefined;
  const delegatedView =
    view &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_abstain &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence
      ? view
      : undefined;
  const { dRep } = useGetDRepDetailsQuery(isLocal ? delegatedView : undefined);
  const { votingPower } = useGetAdaHolderVotingPowerQuery(isLocal ? stakeKey : undefined);
  const { dRepVotes } = useGetDRepVotesByIdQuery(
    isLocal ? (isVoter ? dRepID : delegatedView) : undefined,
  );
  const { liveProposals } = useGetLiveProposalsQuery();
  const { decidedActions } = useGetDecidedActionsQuery();

  const storeKey = [QUERY_KEYS.notificationsLocalStoreKey, stake];
  const storeQuery = useQuery({
    queryKey: storeKey,
    queryFn: () => loadStore(stake, new Date()),
    enabled: isLocal,
    staleTime: Infinity,
  });

  const preferencesKey = [QUERY_KEYS.notificationsPreferencesKey, mode, stake, token];
  const preferencesQuery = useQuery({
    queryKey: preferencesKey,
    queryFn: () =>
      (isRemote && token ? getNotificationPreferences(token) : loadPreferences(stake)),
    enabled: isLocal || hasToken,
    staleTime: Infinity,
  });
  const preferences = preferencesQuery.data ?? DEFAULT_PREFERENCES;

  const derived = useMemo(() => {
    if (!isLocal || !storeQuery.data) return undefined;
    return deriveNotifications(
      {
        delegation: currentDelegation,
        delegatedDRep: dRep && {
          view: dRep.view,
          status: dRep.status,
          givenName: dRep.givenName || null,
        },
        votes: dRepVotes,
        voteRelation: isVoter ? "voted" : "dRepVoted",
        isVoter,
        live: liveProposals,
        decided: decidedActions,
        votingPower,
      },
      storeQuery.data,
      preferences,
      new Date(),
    );
  }, [
    isLocal,
    storeQuery.data,
    currentDelegation,
    dRep,
    dRepVotes,
    isVoter,
    liveProposals,
    decidedActions,
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
    queryKey: [QUERY_KEYS.notificationsPageKey, stake, token, filter, sort, pageSize],
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
      writeToken(stake, null);
      setToken(undefined);
    }
  }, [pageQuery.error, preferencesQuery.error]);

  const localItems = derived?.items ?? [];
  const localFiltered = localItems.filter((item) => matchesFilter(item, filter));
  if (sort === "oldest") localFiltered.reverse();
  const firstPage = pageQuery.data?.pages[0];
  const counts = isLocal ? countByFilter(localItems) : firstPage?.counts ?? ZERO_COUNTS;

  const refreshRemote = () =>
    queryClient.invalidateQueries({ queryKey: [QUERY_KEYS.notificationsPageKey] });

  const markRead = useCallback(
    async (ids: string[], read: boolean) => {
      if (isRemote && token) {
        await markNotificationsRead(token, ids, read);
        await refreshRemote();
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
    if (isRemote && token) {
      await markAllNotificationsRead(token);
      await refreshRemote();
      return;
    }
    await markRead(
      localItems.map((item) => item.id),
      true,
    );
  }, [isRemote, token, localItems, markRead]);

  const savePreferences = useCallback(
    async (next: NotificationPreferences) => {
      if (isRemote && token) {
        const saved = await saveNotificationPreferences(token, next);
        queryClient.setQueryData(preferencesKey, saved);
        await refreshRemote();
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
    writeToken(stake, session);
    setToken(session.token);
  }, [walletApi, stake]);

  const remoteItems = (pageQuery.data?.pages ?? [])
    .flatMap((page) => page.items)
    .filter((item) => KNOWN_KINDS.has(item.kind));

  return {
    mode,
    needsSignIn: isRemote && !token,
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
      mode === "loading" || (isLocal && !derived) || (hasToken && pageQuery.isLoading),
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
      crossDevice: isRemote,
      events: (config?.events.filter((kind) => kind !== "system") ??
        NOTIFICATION_EVENTS) as NotificationEvent[],
    },
  };
};
