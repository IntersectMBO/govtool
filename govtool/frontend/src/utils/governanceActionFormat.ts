import type {
  GovernanceActionRecord,
  GovernanceActionVoteAggregate,
} from "@/models";
import { voteAggregateResult } from "@/utils/voteAggregate";

const HOUR = 60 * 60 * 1000;
const DAY = 24 * HOUR;

/** "4d 1h" or "5h 20m" until the expiry date; undefined once it has passed. */
export const timeLeft = (expiry: string | undefined, now = new Date()) => {
  if (!expiry) return undefined;
  const remaining = new Date(expiry).getTime() - now.getTime();
  if (!(remaining > 0)) return undefined;
  const days = Math.floor(remaining / DAY);
  const hours = Math.floor((remaining % DAY) / HOUR);
  if (days > 0) return `${days}d ${hours}h`;
  const minutes = Math.floor((remaining % HOUR) / 60000);
  return `${hours}h ${minutes}m`;
};

type Withdrawal = { receivingAddress: string; amount: number };

/** Lovelace a treasury withdrawal moves; undefined for any other action. */
export const treasuryTotal = (details: unknown) => {
  if (!Array.isArray(details)) return undefined;
  const amounts = (details as Withdrawal[])
    .map((item) => item?.amount)
    .filter((amount): amount is number => typeof amount === "number");
  return amounts.length
    ? amounts.reduce((sum, amount) => sum + amount, 0)
    : undefined;
};

/**
 * One role's tally in percent of its non-abstaining total: yes, everything
 * that is not yes (no plus not voted, as ratification counts it), explicit no
 * votes alone, and the threshold.
 */
export const voteBar = (aggregate: GovernanceActionVoteAggregate) => {
  const result = voteAggregateResult(aggregate);
  if (!result || result.yesPercentage === undefined) return undefined;
  const { numerator, denominator } = aggregate.threshold;
  const ratification = Number(result.ratification);
  return {
    yesPercent: result.yesPercentage,
    noPercent: result.noPercentage ?? 0,
    votedNoPercent:
      ratification > 0
        ? Math.round((Number(aggregate.no) / ratification) * 10000) / 100
        : 0,
    thresholdPercent: Math.round((numerator / denominator) * 1000) / 10,
    passing: result.passing,
  };
};

export type DecidedStatus = {
  key: "enacted" | "ratified" | "expired";
  time: string | null;
  ratified: boolean;
};

/** How a concluded action ended; undefined while it is still live. */
export const decidedStatus = ({
  status,
  status_times: times,
}: Pick<GovernanceActionRecord, "status" | "status_times">):
  | DecidedStatus
  | undefined => {
  if (status.enacted_epoch !== null)
    return { key: "enacted", time: times.enacted_time, ratified: true };
  if (status.ratified_epoch !== null)
    return { key: "ratified", time: times.ratified_time, ratified: true };
  if (status.expired_epoch !== null || status.dropped_epoch !== null)
    return {
      key: "expired",
      time: times.expired_time ?? times.dropped_time,
      ratified: false,
    };
  return undefined;
};
