// Notifications, in the shape of docs/api/notifications.md. Local mode builds
// the same objects in the browser, so one renderer serves both modes.

export type NotificationKind =
  | "drepChange"
  | "delegationChange"
  | "actionActivity"
  | "relevantAction"
  | "digest"
  | "system";

export type DRepChangeData = {
  dRepId: string;
  dRepName: string | null;
  change: "retired" | "inactive" | "retiring" | "metadataUpdated";
  epoch: number | null;
  /** Lovelace; the service sends a decimal string, local mode a number. */
  votingPower: number | string | null;
};

export type DelegationTarget = {
  type: "drep" | "abstain" | "noConfidence";
  dRepId: string | null;
  dRepName: string | null;
};

export type DelegationChangeData = {
  change: "delegated" | "removed";
  target: DelegationTarget;
  votingPower: number | string | null;
};

export type ActionActivityData = {
  actionId: string;
  actionType: string;
  title: string | null;
  outcome: "ratified" | "enacted" | "expired";
  vote: "yes" | "no" | "abstain" | null;
  relation: "voted" | "dRepVoted";
};

export type RelevantActionData = {
  actionId: string;
  actionType: string;
  title: string | null;
  expiryDate: string | null;
  reason: "deadline" | "matched";
};

export type DigestData = {
  from: string;
  to: string;
  total: number;
  relevant: number | null;
  url: string | null;
};

export type SystemData = { title: string; body: string; url: string | null };

type Base<K extends NotificationKind, D> = {
  id: string;
  kind: K;
  createdAt: string;
  read: boolean;
  data: D;
};

export type GovToolNotification =
  | Base<"drepChange", DRepChangeData>
  | Base<"delegationChange", DelegationChangeData>
  | Base<"actionActivity", ActionActivityData>
  | Base<"relevantAction", RelevantActionData>
  | Base<"digest", DigestData>
  | Base<"system", SystemData>;

export type NotificationFilter =
  | "all"
  | "unread"
  | "governanceActions"
  | "delegation"
  | "drep"
  | "myActivity";

export type NotificationEvent = Exclude<NotificationKind, "system">;

export type NotificationFrequency = "immediate" | "daily" | "weekly" | "off";

export type NotificationPreferences = {
  channels: {
    inApp: boolean;
    email: {
      enabled: boolean;
      address: string | null;
      status?: "unverified" | "pending" | "verified";
    };
  };
  events: Record<NotificationEvent, NotificationFrequency>;
};

/** GET /notifications/config: present only once a notifications service runs. */
export type NotificationsConfig = {
  version: number;
  /** Earlier versions this service still serves, so older frontends keep working. */
  compatible?: number[];
  /** Where a user adds or confirms an email address, when email is offered. */
  emailSetupUrl?: string | null;
  channels: { inApp: boolean; email: boolean };
  events: NotificationKind[];
  frequencies: NotificationFrequency[];
};

export type NotificationPage = {
  items: GovToolNotification[];
  total: number;
  unread: number;
  counts: Record<NotificationFilter, number>;
};
