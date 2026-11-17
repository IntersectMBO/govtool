import type {
  Infinite,
  NotificationFilter,
  NotificationPage,
  NotificationPreferences,
  NotificationsConfig,
  ProposalData,
} from "@/models";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// The notifications service of docs/api/notifications.md. None runs yet: the
// config probe answers 404 and the frontend stays in local mode.

/** This frontend speaks version 1; a later service lists 1 in `compatible`. */
const speaksVersionOne = (data: Partial<NotificationsConfig>) =>
  data.version === 1 || (Array.isArray(data.compatible) && data.compatible.includes(1));

/**
 * The service's config, or undefined when no notifications service runs or its
 * answer is not one this frontend can use (local mode then).
 */
export const getNotificationsConfig = async (): Promise<NotificationsConfig | undefined> => {
  try {
    const { data, status } = await GovernanceActionsAPI.get<NotificationsConfig>(
      "/notifications/config",
      { validateStatus: () => true },
    );
    const isUsable =
      status === 200 &&
      typeof data === "object" &&
      data !== null &&
      speaksVersionOne(data) &&
      typeof data.channels === "object" &&
      data.channels !== null &&
      Array.isArray(data.events);
    return isUsable ? data : undefined;
  } catch {
    return undefined;
  }
};

/**
 * Live actions closing soonest, for deadline reminders in local mode: the
 * newest-first list can leave them out when many actions are open.
 */
export const getClosingProposals = async () => {
  const { data } = await GovernanceActionsAPI.get<Infinite<ProposalData>>("/proposal/list", {
    params: { page: 0, pageSize: 50, sort: "SoonestToExpire" },
  });
  return data;
};

const auth = (token: string) => ({ headers: { Authorization: `Bearer ${token}` } });

export const requestNotificationsNonce = async (stakeAddress: string) => {
  const { data } = await GovernanceActionsAPI.post<{ nonce: string; expiresAt: string }>(
    "/notifications/auth/nonce",
    { stakeAddress },
  );
  return data;
};

export const signInToNotifications = async (body: {
  stakeAddress: string;
  nonce: string;
  signature: string;
  key: string;
}) => {
  const { data } = await GovernanceActionsAPI.post<{ token: string; expiresAt: string }>(
    "/notifications/auth",
    body,
  );
  return data;
};

export const getNotificationsPage = async (
  token: string,
  params: {
    page: number;
    pageSize: number;
    filter: NotificationFilter;
    sort: "newest" | "oldest";
  },
) => {
  const { data } = await GovernanceActionsAPI.get<NotificationPage>("/notifications", {
    ...auth(token),
    params,
  });
  return data;
};

export const markNotificationsRead = async (token: string, ids: string[], read: boolean) => {
  await GovernanceActionsAPI.post("/notifications/read", { ids, read }, auth(token));
};

export const markAllNotificationsRead = async (token: string) => {
  await GovernanceActionsAPI.post("/notifications/read-all", {}, auth(token));
};

export const getNotificationPreferences = async (token: string) => {
  const { data } = await GovernanceActionsAPI.get<NotificationPreferences>(
    "/notifications/preferences",
    auth(token),
  );
  return data;
};

export const saveNotificationPreferences = async (
  token: string,
  preferences: NotificationPreferences,
) => {
  const { data } = await GovernanceActionsAPI.put<NotificationPreferences>(
    "/notifications/preferences",
    preferences,
    auth(token),
  );
  return data;
};
