import { bech32 } from "bech32";
import type {
  GovernanceActionRecord,
  ProposalData,
  ProposalVote,
} from "@/models";

import { GovernanceActionType } from "@/types/governanceAction";

// Figures for a DRep's own dashboard, from its votes and every governance
// action record. All dates are compared in UTC.

const DAY = 24 * 60 * 60 * 1000;

export type DashboardVote = {
  vote: Pick<ProposalVote, "vote" | "epochNo" | "date"> | null;
  proposal: Pick<ProposalData, "txHash" | "index" | "type" | "createdDate">;
};

export type DashboardAction = Pick<
  GovernanceActionRecord,
  "tx_hash" | "index" | "type" | "epoch_no" | "time" | "status" | "status_times"
>;

export type ActionOutcome =
  | "active"
  | "ratified"
  | "enacted"
  | "expired"
  | "dropped"
  | "unknown";

export const actionKey = (txHash: string, index: number | string) =>
  `${txHash}#${index}`;

const voteKey = ({ proposal }: DashboardVote) =>
  actionKey(proposal.txHash, proposal.index);

const recordKey = (record: DashboardAction) =>
  actionKey(record.tx_hash, record.index);

const share = (part: number, whole: number) =>
  (whole ? Math.round((part / whole) * 100) : 0);

/** Totals and rounded shares of a DRep's yes, no and abstain votes. */
export const voteSummary = (votes: DashboardVote[]) => {
  const cast = votes.filter((row) => row.vote);
  const count = (choice: string) =>
    cast.filter((row) => row.vote?.vote === choice).length;
  const yes = count("yes");
  const no = count("no");
  const abstain = count("abstain");
  const total = cast.length;
  return {
    total,
    yes,
    no,
    abstain,
    yesPct: share(yes, total),
    noPct: share(no, total),
    abstainPct: share(abstain, total),
  };
};

/** Where an action stands, from its record; `unknown` without one. */
export const actionOutcome = (record?: Pick<DashboardAction, "status">): ActionOutcome => {
  if (!record) return "unknown";
  const { status } = record;
  if (status.enacted_epoch !== null) return "enacted";
  if (status.ratified_epoch !== null) return "ratified";
  if (status.dropped_epoch !== null) return "dropped";
  if (status.expired_epoch !== null) return "expired";
  return "active";
};

/**
 * The epochs an action could be voted on: from its submission epoch to the
 * epoch before it was ratified, enacted, dropped or expired, or to the current
 * epoch while it is open.
 */
const votingWindow = (record: DashboardAction, currentEpoch: number) => {
  const ends = [
    record.status.ratified_epoch,
    record.status.enacted_epoch,
    record.status.dropped_epoch,
    record.status.expired_epoch,
  ].filter((epoch): epoch is number => typeof epoch === "number");
  return {
    from: record.epoch_no,
    to: ends.length ? Math.min(...ends) - 1 : currentEpoch,
  };
};

const votableSince = (
  actions: DashboardAction[],
  fromEpoch: number | undefined,
  currentEpoch: number,
) =>
  actions.filter((record) => {
    const { from, to } = votingWindow(record, currentEpoch);
    return to >= (fromEpoch ?? -Infinity) && from <= currentEpoch && from <= to;
  });

/**
 * The epoch a DRep registered in: the latest epoch known (from dated anchors
 * such as action submissions) to have started by the registration date.
 */
export const registrationEpoch = (
  registeredAt: string | undefined,
  anchors: { time: string; epoch: number }[],
) => {
  if (!registeredAt) return undefined;
  const at = new Date(registeredAt).getTime();
  if (Number.isNaN(at)) return undefined;
  const dated = anchors
    .filter(({ time }) => !!time)
    .map(({ time, epoch }) => ({ at: new Date(time).getTime(), epoch }))
    .filter((anchor) => !Number.isNaN(anchor.at));
  const before = dated
    .filter((anchor) => anchor.at <= at)
    .sort((a, b) => b.at - a.at || b.epoch - a.epoch)[0];
  if (!before) return undefined;
  const after = dated
    .filter((anchor) => anchor.at > at)
    .sort((a, b) => a.at - b.at || a.epoch - b.epoch)[0];
  if (!after || after.epoch <= before.epoch) return before.epoch;
  // Epochs are evenly spaced in time, so place the date between the two.
  const elapsed = (at - before.at) / (after.at - before.at);
  return before.epoch + Math.floor(elapsed * (after.epoch - before.epoch));
};

/** Of the actions open since registration, how many the DRep voted on. */
export const participation = (
  votes: DashboardVote[],
  actions: DashboardAction[],
  fromEpoch: number | undefined,
  currentEpoch: number,
) => {
  const voted = new Set(votes.filter((row) => row.vote).map(voteKey));
  const votable = votableSince(actions, fromEpoch, currentEpoch);
  const count = votable.filter((record) => voted.has(recordKey(record))).length;
  return {
    voted: count,
    votable: votable.length,
    rate: votable.length ? Math.floor((count / votable.length) * 100) : null,
  };
};

/** Dated epochs, to place a registration date: now, submissions and votes. */
export const epochAnchors = (
  actions: DashboardAction[],
  votes: DashboardVote[],
  currentEpoch: number | undefined,
) => [
  ...(currentEpoch !== undefined ? [{ time: new Date().toISOString(), epoch: currentEpoch }] : []),
  ...actions.map((record) => ({ time: record.time, epoch: record.epoch_no })),
  ...votes.flatMap(({ vote }) => (vote ? [{ time: vote.date, epoch: vote.epochNo }] : [])),
];

/**
 * A directory card's participation: the dashboard's rate, counted from the
 * epoch the DRep registered in, or over every action when that is unknown.
 */
export const participationRate = (
  votes: DashboardVote[],
  actions: DashboardAction[],
  registeredAt: string | undefined,
  currentEpoch: number,
) => {
  const fromEpoch = registrationEpoch(registeredAt, epochAnchors(actions, votes, currentEpoch));
  return participation(votes, actions, fromEpoch, currentEpoch).rate;
};

/**
 * A directory card's delegator count: the /drep/list row's when the backend
 * sends one (db-sync and Koios do), else the delegators route's total once it
 * is served, else none to show.
 */
export const pickDelegatorCount = (
  rowCount: number | null | undefined,
  route: { isServed: boolean; total?: number; isLoading: boolean },
) => {
  if (typeof rowCount === "number") return { show: true, count: rowCount, isLoading: false };
  if (route.isServed) return { show: true, count: route.total, isLoading: route.isLoading };
  return { show: false, count: undefined, isLoading: false };
};

/** Mean days from an action's submission to the DRep's vote, one decimal. */
export const averageResponseDays = (votes: DashboardVote[]) => {
  const gaps = votes
    .map(({ vote, proposal }) =>
      (vote?.date && proposal.createdDate
        ? new Date(vote.date).getTime() - new Date(proposal.createdDate).getTime()
        : Number.NaN))
    .filter((gap) => Number.isFinite(gap) && gap >= 0);
  if (!gaps.length) return null;
  const mean = gaps.reduce((sum, gap) => sum + gap, 0) / gaps.length / DAY;
  return Math.round(mean * 10) / 10;
};

const epochsVoted = (votes: DashboardVote[]) =>
  new Set(
    votes
      .map(({ vote }) => vote?.epochNo)
      .filter((epoch): epoch is number => typeof epoch === "number"),
  );

/**
 * Consecutive epochs with at least one vote, ending this epoch, or the last
 * one while this epoch has no vote yet.
 */
export const votingStreak = (votes: DashboardVote[], currentEpoch: number) => {
  const epochs = epochsVoted(votes);
  let epoch = epochs.has(currentEpoch) ? currentEpoch : currentEpoch - 1;
  let streak = 0;
  while (epochs.has(epoch)) {
    streak += 1;
    epoch -= 1;
  }
  return streak;
};

/** One row per epoch since registration, newest first, at most `limit`. */
export const epochActivity = (
  votes: DashboardVote[],
  actions: DashboardAction[],
  currentEpoch: number,
  fromEpoch: number | undefined,
  limit: number,
) => {
  const last = Math.max(fromEpoch ?? -Infinity, currentEpoch - limit + 1);
  const windows = actions.map((record) => votingWindow(record, currentEpoch));
  return Array.from(
    { length: Math.max(0, currentEpoch - last + 1) },
    (_, offset) => {
      const epoch = currentEpoch - offset;
      const available = windows.filter(({ from, to }) => from <= epoch && epoch <= to).length;
      const cast = new Set(
        votes.filter((row) => row.vote?.epochNo === epoch).map(voteKey),
      ).size;
      return {
        epoch,
        available,
        votes: cast,
        participation: available
          ? Math.min(100, Math.floor((cast / available) * 100))
          : null,
        isCurrent: epoch === currentEpoch,
      };
    },
  );
};

export const CATEGORY_TYPES = {
  treasury: [GovernanceActionType.TreasuryWithdrawals],
  protocol: [
    GovernanceActionType.ParameterChange,
    GovernanceActionType.HardForkInitiation,
  ],
  info: [GovernanceActionType.InfoAction],
  committee: [
    GovernanceActionType.NewCommittee,
    GovernanceActionType.NoConfidence,
    GovernanceActionType.NewConstitution,
  ],
} as const;

export type ActionCategory = keyof typeof CATEGORY_TYPES;

export const categoryOf = (type: string): ActionCategory | undefined =>
  (Object.keys(CATEGORY_TYPES) as ActionCategory[]).find((key) =>
    (CATEGORY_TYPES[key] as readonly string[]).includes(type));

/** Votable and voted actions since registration, per design category. */
export const categoryPattern = (
  votes: DashboardVote[],
  actions: DashboardAction[],
  fromEpoch: number | undefined,
  currentEpoch: number,
) => {
  const voted = new Set(votes.filter((row) => row.vote).map(voteKey));
  const votable = votableSince(actions, fromEpoch, currentEpoch);
  return (Object.keys(CATEGORY_TYPES) as ActionCategory[]).map((key) => {
    const inCategory = votable.filter((record) => categoryOf(record.type) === key);
    return {
      key,
      voted: inCategory.filter((record) => voted.has(recordKey(record))).length,
      votable: inCategory.length,
    };
  });
};

export type CalendarDay = {
  date: string;
  count: number;
  level: 0 | 1 | 2 | 3 | 4;
  future: boolean;
};

const utcDay = (time: number) => new Date(time).toISOString().slice(0, 10);

const levelOf = (count: number): CalendarDay["level"] => {
  if (count >= 5) return 4;
  if (count >= 3) return 3;
  return count as 0 | 1 | 2;
};

/** Votes per UTC day as 53 Sunday-first weeks ending with this week. */
export const voteCalendar = (votes: DashboardVote[], today: Date) => {
  const counts = new Map<string, number>();
  votes.forEach(({ vote }) => {
    if (!vote?.date) return;
    const at = new Date(vote.date).getTime();
    if (Number.isNaN(at)) return;
    const day = utcDay(at);
    counts.set(day, (counts.get(day) ?? 0) + 1);
  });
  const todayStart = Date.UTC(
    today.getUTCFullYear(),
    today.getUTCMonth(),
    today.getUTCDate(),
  );
  const firstSunday = todayStart - (today.getUTCDay() + 52 * 7) * DAY;
  return Array.from({ length: 53 }, (_, week) =>
    Array.from({ length: 7 }, (__, weekday) => {
      const time = firstSunday + (week * 7 + weekday) * DAY;
      const date = utcDay(time);
      const count = counts.get(date) ?? 0;
      return { date, count, level: levelOf(count), future: time > todayStart };
    }));
};

const csvCell = (value: string | number | null | undefined) => {
  if (value === null || value === undefined) return "";
  let text = String(value);
  // A cell a spreadsheet would read as a formula is prefixed so it stays text.
  if (/^[=+\-@\t\r]/.test(text)) text = `'${text}`;
  return /[",\r\n]/.test(text) ? `"${text.replace(/"/g, "\"\"")}"` : text;
};

/** RFC 4180 CSV, with formula-like cells defused. */
export const toCsv = (rows: (string | number | null | undefined)[][]) =>
  rows.map((row) => row.map(csvCell).join(",")).join("\r\n");

/**
 * A reward address in hex (header e0 or e1 plus a 28-byte key hash) as
 * bech32: stake1… on mainnet, stake_test1… elsewhere. Undefined otherwise.
 */
export const stakeHexToBech32 = (hex?: string) => {
  if (!hex || !/^e[01][0-9a-f]{56}$/i.test(hex)) return undefined;
  const bytes = (hex.match(/../g) ?? []).map((pair) => parseInt(pair, 16));
  const prefix = hex[1] === "1" ? "stake" : "stake_test";
  return bech32.encode(prefix, bech32.toWords(bytes));
};

/**
 * A wallet that was a DRep and is now neither a DRep nor a direct voter, with
 * no registration on its way: it gets the retired DRep dashboard.
 */
export const isRetiredDRep = (
  voter: {
    isRegisteredAsDRep: boolean;
    wasRegisteredAsDRep: boolean;
    isRegisteredAsSoleVoter: boolean;
  },
  pending: { registerAsDrep?: unknown },
) =>
  voter.wasRegisteredAsDRep &&
  !voter.isRegisteredAsDRep &&
  !voter.isRegisteredAsSoleVoter &&
  !pending.registerAsDrep;

/** Whole calendar months from one date to another, in UTC. */
export const tenureMonths = (from?: string | null, to?: string | null) => {
  if (!from || !to) return undefined;
  const start = new Date(from);
  const end = new Date(to);
  if (Number.isNaN(start.getTime()) || Number.isNaN(end.getTime()) || end < start) {
    return undefined;
  }
  return (
    (end.getUTCFullYear() - start.getUTCFullYear()) * 12 +
    end.getUTCMonth() -
    start.getUTCMonth() -
    (end.getUTCDate() < start.getUTCDate() ? 1 : 0)
  );
};
