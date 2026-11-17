import {
  DRepStatus,
  type CurrentDelegation,
  type DelegationTarget,
  type GovernanceActionRecord,
  type GovToolNotification,
  type NotificationEvent,
  type NotificationFilter,
  type NotificationPreferences,
  type ProposalData,
  type VotedProposal,
} from "@/models";
import { AutomatedVotingOptionCurrentDelegation } from "@/types/automatedVotingOptions";

import { decidedStatus } from "./governanceActionFormat";

// Notifications worked out in the browser ("local mode", see
// docs/api/notifications.md) from routes GovTool already reads, plus a small
// per-browser store for read state and for changes only a snapshot can show.

const DAY = 24 * 60 * 60 * 1000;
const ACTIVITY_WINDOW = 30 * DAY;
const DEADLINE_WINDOW = 7 * DAY;
const READ_LIMIT = 500;
const HISTORY_LIMIT = 20;

export const NOTIFICATION_FILTERS: NotificationFilter[] = [
  "all",
  "unread",
  "governanceActions",
  "delegation",
  "drep",
  "myActivity",
];

export const NOTIFICATION_EVENTS: NotificationEvent[] = [
  "drepChange",
  "delegationChange",
  "actionActivity",
  "relevantAction",
  "digest",
];

/** The frequency an event takes when it is switched on. */
export const ON_FREQUENCY: Record<NotificationEvent, "immediate" | "weekly"> = {
  drepChange: "immediate",
  delegationChange: "immediate",
  actionActivity: "immediate",
  relevantAction: "immediate",
  digest: "weekly",
};

export const DEFAULT_PREFERENCES: NotificationPreferences = {
  channels: { inApp: true, email: { enabled: false, address: null } },
  events: { ...ON_FREQUENCY },
};

/** Which filter chips an item counts under; overlapping, as the design shows. */
export const matchesFilter = (item: GovToolNotification, filter: NotificationFilter) => {
  switch (filter) {
    case "all":
      return true;
    case "unread":
      return !item.read;
    case "governanceActions":
      return ["actionActivity", "relevantAction", "digest"].includes(item.kind);
    case "delegation":
      return item.kind === "delegationChange";
    case "drep":
      return item.kind === "drepChange";
    case "myActivity":
      return item.kind === "actionActivity" || item.kind === "delegationChange";
    default:
      return false;
  }
};

export const countByFilter = (items: GovToolNotification[]) =>
  Object.fromEntries(
    NOTIFICATION_FILTERS.map((filter) => [
      filter,
      items.filter((item) => matchesFilter(item, filter)).length,
    ]),
  ) as Record<NotificationFilter, number>;

// ---------------------------------------------------------------- the store

type StoredDelegationChange = Extract<GovToolNotification, { kind: "delegationChange" }>;

export type NotificationStore = {
  /** When this browser first looked for this wallet: digests start here. */
  since: string;
  readIds: string[];
  /** When a condition without a chain time was first seen, by item id. */
  firstSeen: Record<string, string>;
  /** The delegation seen last time; undefined before the first look. */
  lastDelegation?: string;
  /** Changes a snapshot found, kept so they stay in the feed. */
  history: StoredDelegationChange[];
};

export const emptyStore = (since: string): NotificationStore => ({
  since,
  readIds: [],
  firstSeen: {},
  history: [],
});

const storeKey = (stakeKey: string) => `govtool_notifications_${stakeKey}`;
const preferencesKey = (stakeKey: string) => `govtool_notification_prefs_${stakeKey}`;

const read = <T>(key: string): T | undefined => {
  try {
    const raw = localStorage.getItem(key);
    return raw ? (JSON.parse(raw) as T) : undefined;
  } catch {
    return undefined;
  }
};

const write = (key: string, value: unknown) => {
  try {
    localStorage.setItem(key, JSON.stringify(value));
  } catch {
    // Storage full or blocked: notifications still show, unsaved.
  }
};

export const loadStore = (stakeKey: string, now: Date): NotificationStore => {
  const stored = read<Partial<NotificationStore>>(storeKey(stakeKey));
  const fresh = emptyStore(now.toISOString());
  if (!stored || typeof stored !== "object") return fresh;
  return {
    since: typeof stored.since === "string" ? stored.since : fresh.since,
    readIds: Array.isArray(stored.readIds) ? stored.readIds : [],
    firstSeen: stored.firstSeen && typeof stored.firstSeen === "object" ? stored.firstSeen : {},
    lastDelegation: typeof stored.lastDelegation === "string" ? stored.lastDelegation : undefined,
    history: Array.isArray(stored.history) ? stored.history : [],
  };
};

export const saveStore = (stakeKey: string, store: NotificationStore) =>
  write(storeKey(stakeKey), {
    ...store,
    readIds: store.readIds.slice(-READ_LIMIT),
    history: store.history.slice(0, HISTORY_LIMIT),
  });

export const loadPreferences = (stakeKey: string): NotificationPreferences => {
  const stored = read<NotificationPreferences>(preferencesKey(stakeKey));
  if (!stored?.channels || !stored.events) return DEFAULT_PREFERENCES;
  return {
    channels: { ...DEFAULT_PREFERENCES.channels, inApp: stored.channels.inApp !== false },
    events: { ...DEFAULT_PREFERENCES.events, ...stored.events },
  };
};

export const savePreferences = (stakeKey: string, preferences: NotificationPreferences) =>
  write(preferencesKey(stakeKey), preferences);

/** The store with these ids marked read (or unread). */
export const withRead = (store: NotificationStore, ids: string[], isRead: boolean) => ({
  ...store,
  readIds: isRead
    ? [...new Set([...store.readIds, ...ids])]
    : store.readIds.filter((id) => !ids.includes(id)),
});

// ---------------------------------------------------------------- deriving

export type DeriveInput = {
  /** Undefined while unknown: nothing that depends on it is derived. */
  delegation: CurrentDelegation | undefined;
  /** The DRep the wallet delegates to, when it delegates to one. */
  delegatedDRep?: { view: string; status: DRepStatus; givenName: string | null };
  /** The wallet's own votes (a voter) or its DRep's votes (an ADA holder). */
  votes: VotedProposal[] | undefined;
  voteRelation: "voted" | "dRepVoted";
  isVoter: boolean;
  live: ProposalData[];
  decided: GovernanceActionRecord[];
  votingPower?: number;
};

const delegationKey = (delegation: CurrentDelegation) => delegation?.dRepView ?? "none";

const targetOf = (
  view: string,
  dRep: DeriveInput["delegatedDRep"],
): DelegationTarget => {
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_abstain)
    return { type: "abstain", dRepId: null, dRepName: null };
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence)
    return { type: "noConfidence", dRepId: null, dRepName: null };
  return {
    type: "drep",
    dRepId: view,
    dRepName: dRep?.view === view ? dRep.givenName : null,
  };
};

const actionId = (txHash: string, index: number) => `${txHash}#${index}`;

/** Monday 00:00 UTC of the time's week, as yyyy-MM-dd. */
const weekStart = (time: string) => {
  const date = new Date(time);
  const day = (date.getUTCDay() + 6) % 7;
  date.setUTCHours(0, 0, 0, 0);
  date.setUTCDate(date.getUTCDate() - day);
  return date.toISOString().slice(0, 10);
};

/**
 * Every notification the browser can work out, newest first, and the store
 * updated with what it saw (first-seen times, the delegation, new changes).
 */
export const deriveNotifications = (
  input: DeriveInput,
  previous: NotificationStore,
  preferences: NotificationPreferences,
  now: Date,
): { items: GovToolNotification[]; store: NotificationStore } => {
  const at = now.getTime();
  const store: NotificationStore = {
    ...previous,
    firstSeen: { ...previous.firstSeen },
    history: [...previous.history],
  };
  const seen = (id: string) => {
    if (!store.firstSeen[id]) store.firstSeen[id] = now.toISOString();
    return store.firstSeen[id];
  };
  const items: GovToolNotification[] = [];
  const votingPower = input.votingPower ?? null;

  // A delegation change, found against last time's snapshot.
  if (input.delegation !== undefined) {
    const key = delegationKey(input.delegation);
    // One change per delegation transaction: a stale read of the old
    // delegation in between cannot add a second card for the same one.
    const id = `delegation-${input.delegation?.txHash ?? key}`;
    const isKnown = store.history.some((change) => change.id === id);
    if (previous.lastDelegation !== undefined && previous.lastDelegation !== key && !isKnown) {
      store.history.unshift({
        id,
        kind: "delegationChange",
        createdAt: now.toISOString(),
        read: false,
        data:
          key === "none"
            ? {
              change: "removed",
              target: targetOf(previous.lastDelegation, undefined),
              votingPower,
            }
            : { change: "delegated", target: targetOf(key, input.delegatedDRep), votingPower },
      });
    }
    store.lastDelegation = key;
  }
  // A change recorded before the DRep's details loaded gets its name later.
  const named = input.delegatedDRep;
  if (named?.givenName) {
    store.history = store.history.map((change) =>
      (change.data.target.dRepId === named.view && !change.data.target.dRepName
        ? {
          ...change,
          data: { ...change.data, target: { ...change.data.target, dRepName: named.givenName } },
        }
        : change));
  }
  items.push(...store.history);

  // The delegated DRep retired or went inactive.
  const dRep = input.delegatedDRep;
  // `delegatedDRep` is only ever the DRep the wallet delegates to.
  if (dRep && (dRep.status === DRepStatus.Retired || dRep.status === DRepStatus.Inactive)) {
    const id = `drep-${dRep.view}-${dRep.status}`;
    items.push({
      id,
      kind: "drepChange",
      createdAt: seen(id),
      read: false,
      data: {
        dRepId: dRep.view,
        dRepName: dRep.givenName,
        change: dRep.status === DRepStatus.Retired ? "retired" : "inactive",
        epoch: null,
        votingPower,
      },
    });
  }

  // Outcomes of actions voted on, from the decided list.
  const votes = input.votes ?? [];
  const decidedById = new Map(
    input.decided.map((record) => [actionId(record.tx_hash, record.index), record]),
  );
  votes.forEach(({ vote, proposal }) => {
    const id = actionId(proposal.txHash, proposal.index);
    const record = decidedById.get(id);
    const outcome = record && decidedStatus(record);
    if (!record || !outcome?.time) return;
    const time = new Date(outcome.time).getTime();
    if (!(at - time <= ACTIVITY_WINDOW && time <= at)) return;
    items.push({
      id: `activity-${id}-${outcome.key}`,
      kind: "actionActivity",
      createdAt: outcome.time,
      read: false,
      data: {
        actionId: id,
        actionType: record.type,
        title: record.title ?? proposal.title ?? null,
        outcome: outcome.key,
        vote: vote?.vote ?? null,
        relation: input.voteRelation,
      },
    });
  });

  // A voter's unvoted actions closing within a week.
  if (input.isVoter && input.votes) {
    const voted = new Set(votes.map(({ proposal }) => actionId(proposal.txHash, proposal.index)));
    input.live.forEach((proposal) => {
      const id = actionId(proposal.txHash, proposal.index);
      if (!proposal.expiryDate || voted.has(id)) return;
      const expiry = new Date(proposal.expiryDate).getTime();
      if (!(expiry > at && expiry - at <= DEADLINE_WINDOW)) return;
      const opens = Math.max(new Date(proposal.createdDate).getTime(), expiry - DEADLINE_WINDOW);
      items.push({
        id: `deadline-${id}`,
        kind: "relevantAction",
        createdAt: new Date(opens).toISOString(),
        read: false,
        data: {
          actionId: id,
          actionType: proposal.type,
          title: proposal.title ?? null,
          expiryDate: proposal.expiryDate,
          reason: "deadline",
        },
      });
    });
  }

  // One digest per week of actions submitted since the first look.
  const since = new Date(store.since).getTime();
  const submitted = [
    ...input.live.map((proposal) => ({
      id: actionId(proposal.txHash, proposal.index),
      time: proposal.createdDate,
    })),
    ...input.decided.map((record) => ({
      id: actionId(record.tx_hash, record.index),
      time: record.time,
    })),
  ].filter(({ time }) => {
    const when = time ? new Date(time).getTime() : NaN;
    return when > since && when <= at;
  });
  const weeks = new Map<string, { ids: Set<string>; latest: string }>();
  submitted.forEach(({ id, time }) => {
    const week = weekStart(time);
    const entry = weeks.get(week) ?? { ids: new Set<string>(), latest: time };
    entry.ids.add(id);
    if (time > entry.latest) entry.latest = time;
    weeks.set(week, entry);
  });
  weeks.forEach(({ ids, latest }, week) => {
    const end = new Date(new Date(`${week}T00:00:00Z`).getTime() + 7 * DAY - 1).toISOString();
    items.push({
      id: `digest-${week}`,
      kind: "digest",
      createdAt: latest,
      read: false,
      data: { from: `${week}T00:00:00.000Z`, to: end, total: ids.size, relevant: null, url: null },
    });
  });

  const readIds = new Set(store.readIds);
  const visible = preferences.channels.inApp
    ? items
      .filter((item) => item.kind === "system" || preferences.events[item.kind] !== "off")
      .map(
        (item) =>
          ({
            ...item,
            // Outcomes from before this browser first looked arrive as read,
            // so a first visit is not a wall of unread history.
            read:
              readIds.has(item.id) ||
              (item.kind === "actionActivity" && item.createdAt < store.since),
          }) as GovToolNotification,
      )
      .sort((a, b) => b.createdAt.localeCompare(a.createdAt))
    : [];

  return { items: visible, store };
};

// ---------------------------------------------------------------- service data

const isObject = (value: unknown): value is Record<string, unknown> =>
  typeof value === "object" && value !== null;
const isText = (value: unknown) => typeof value === "string";
const isTextOrNull = (value: unknown) => value === null || typeof value === "string";
const isAmount = (value: unknown) =>
  value === null || typeof value === "number" || (typeof value === "string" && /^\d+$/.test(value));

/**
 * Whether an item from the notifications service has everything the screen
 * reads. Anything else (an unknown kind, a missing field) is left out rather
 * than allowed to break the header.
 */
export const isValidNotification = (value: unknown): value is GovToolNotification => {
  if (!isObject(value) || !isText(value.id) || !isText(value.createdAt)) return false;
  if (typeof value.read !== "boolean" || !isObject(value.data)) return false;
  const { data } = value;
  switch (value.kind) {
    case "drepChange":
      return isText(data.dRepId) && isTextOrNull(data.dRepName) && isText(data.change) &&
        isAmount(data.votingPower);
    case "delegationChange":
      return isText(data.change) && isObject(data.target) && isText(data.target.type) &&
        isTextOrNull(data.target.dRepId ?? null) && isAmount(data.votingPower);
    case "actionActivity":
      return isText(data.actionId) && isText(data.actionType) && isText(data.outcome) &&
        isText(data.relation);
    case "relevantAction":
      return isText(data.actionId) && isText(data.actionType) && isText(data.reason);
    case "digest":
      return isText(data.from) && isText(data.to) && typeof data.total === "number";
    case "system":
      return isText(data.title) && isText(data.body);
    default:
      return false;
  }
};
