import { SECURITY_RELEVANT_PARAMS_MAP } from "@/consts";
import type { EpochParams } from "@/models/api";
import type {
  GovernanceActionMetadata,
  GovernanceActionRecord,
  GovernanceActionReference,
  GovernanceActionVoteAggregate,
} from "@/models/governanceActions";
import { MetadataValidationStatus } from "@/models/metadataValidation";

import { voteAggregateResult } from "./voteAggregate";

export type VoterRole = GovernanceActionVoteAggregate["role"];

type ContentSource = Pick<
  GovernanceActionRecord,
  "json_metadata" | "title" | "abstract" | "motivation" | "rationale"
>;

/**
 * What an outcome page may show from the action's anchored document. A
 * document that failed its check still comes back with its body (the hash is
 * checked after parsing), so none of it is shown then: a swapped anchor would
 * otherwise put the swapped text and links under the warning. When the check
 * itself failed the page says the document was not checked.
 */
export const outcomeContent = (
  action: ContentSource,
  metadata: Pick<GovernanceActionMetadata, "metadataStatus" | "metadataValid"> & {
    data?: Partial<GovernanceActionMetadata["data"]>;
  } | undefined,
  metadataValid: boolean,
) => {
  const failed = metadata?.metadataStatus;
  const trusted = metadataValid && !failed;
  const document = trusted ? metadata?.data : undefined;
  const own = (value?: string, fromDocument?: string) =>
    (failed ? undefined : value || fromDocument);
  const references: GovernanceActionReference[] = trusted
    ? action.json_metadata?.body?.references || document?.references || []
    : [];

  return {
    title: own(action.title, document?.title),
    abstract: own(action.abstract, document?.abstract),
    motivation: own(action.motivation, document?.motivation),
    rationale: own(action.rationale, document?.rationale),
    references,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    authors: (action.json_metadata ? action.json_metadata.authors : document?.authors) as any,
    metadataStatus: failed ?? (metadataValid ? undefined : MetadataValidationStatus.INTERNAL_ERROR),
  };
};

export type TreasuryWithdrawal = { receivingAddress: string; amount: number };

/** A treasury withdrawal's payouts; empty for every other type. */
export const treasuryWithdrawals = (
  action: Pick<GovernanceActionRecord, "type" | "description">,
): TreasuryWithdrawal[] => {
  if (action.type !== "TreasuryWithdrawals" || !Array.isArray(action.description)) return [];
  return (action.description as unknown as Partial<TreasuryWithdrawal>[]).filter(
    (withdrawal): withdrawal is TreasuryWithdrawal =>
      typeof withdrawal?.receivingAddress === "string" && withdrawal.receivingAddress !== "",
  );
};

/**
 * The three voting groups of an action: whether each votes on its type, and
 * its tally when the backend has one.
 */
export const voterGroups = (action: GovernanceActionRecord) => {
  const { type } = action;
  const securityChange = Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
    (key) => action.proposal_params?.[key as keyof EpochParams] != null,
  );
  const spoApplicable =
    !!action.vote_aggregates?.some((aggregate) => aggregate.role === "spo") ||
    ["NoConfidence", "NewCommittee", "HardForkInitiation", "InfoAction"].includes(type) ||
    (type === "ParameterChange" && securityChange);
  const applicable: Record<VoterRole, boolean> = {
    drep: true,
    spo: spoApplicable,
    cc: !["NoConfidence", "NewCommittee"].includes(type),
  };
  return (["drep", "spo", "cc"] as const).map((role) => ({
    role,
    applicable: applicable[role],
    aggregate: action.vote_aggregates?.find((aggregate) => aggregate.role === role),
  }));
};

/**
 * The yes a role needs, in its tally's own unit: lovelace or members rounded
 * up (the ledger passes yes / counted >= threshold, so 3 of 5 misses two
 * thirds and 4 is the figure), or a percent tally's fraction as it is.
 */
const thresholdValue = (aggregate: GovernanceActionVoteAggregate, ratification: string) => {
  const { numerator, denominator } = aggregate.threshold;
  if (aggregate.representation === "percent") {
    return String((Number(ratification) * numerator) / denominator);
  }
  const den = BigInt(denominator);
  return ((BigInt(ratification) * BigInt(numerator) + den - 1n) / den).toString();
};

/**
 * One role's tally for display: yes against the rest of the non-abstaining
 * stake (no and not voted), the threshold, and the stake it takes to pass.
 * Undefined when the tally does not add up.
 */
export const roleSummary = (aggregate: GovernanceActionVoteAggregate) => {
  const result = voteAggregateResult(aggregate);
  if (!result || result.yesPercentage === undefined) return undefined;
  const { numerator, denominator } = aggregate.threshold;
  const ratification = Number(result.ratification);
  const yes = Number(aggregate.yes);
  return {
    yes,
    rest: ratification - yes,
    yesPercent: result.yesPercentage,
    restPercent: Math.round((100 - result.yesPercentage) * 100) / 100,
    thresholdPercent: Math.round((numerator / denominator) * 1000) / 10,
    ratification,
    thresholdValue: thresholdValue(aggregate, result.ratification),
    passing: result.passing,
  };
};

export type OutcomeKey = "enacted" | "ratified" | "expired" | "dropped" | "live";

/** How the action ended, or "live" while it is still open. */
export const outcomeKey = ({ status }: Pick<GovernanceActionRecord, "status">): OutcomeKey => {
  if (status.enacted_epoch !== null) return "enacted";
  if (status.ratified_epoch !== null) return "ratified";
  if (status.expired_epoch !== null) return "expired";
  if (status.dropped_epoch !== null) return "dropped";
  return "live";
};

export type LifecycleEvent = {
  key: "submitted" | "ratified" | "enacted" | "expired" | "dropped";
  time: string | null;
  epoch: number;
};

/** Submission, then each status the action reached, oldest first. */
export const lifecycleEvents = (action: GovernanceActionRecord): LifecycleEvent[] => {
  const { status, status_times: times } = action;
  const events: LifecycleEvent[] = [
    { key: "submitted", time: action.time, epoch: action.epoch_no },
  ];
  (["ratified", "enacted", "expired", "dropped"] as const).forEach((key) => {
    const epoch = status[`${key}_epoch`];
    if (epoch !== null) events.push({ key, time: times[`${key}_time`], epoch });
  });
  return events.sort((a, b) => a.epoch - b.epoch);
};
