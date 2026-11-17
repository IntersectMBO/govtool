import type {
  NotificationFilter,
  NotificationPage,
  NotificationPreferences,
  NotificationsConfig,
} from "@/models";

import { GovernanceActionsAPI } from "../GovernanceActionsAPI";

// The notifications service of docs/api/notifications.md. None runs yet: the
// config probe answers 404 and the frontend stays in local mode.

/** The service's config, or undefined when no notifications service runs. */
export const getNotificationsConfig = async (): Promise<NotificationsConfig | undefined> => {
  try {
    const { data, status } = await GovernanceActionsAPI.get<NotificationsConfig>(
      "/notifications/config",
      { validateStatus: () => true },
    );
    return status === 200 && data?.version === 1 ? data : undefined;
  } catch {
    return undefined;
  }
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
