import type {
  Committee,
  EpochStamp,
  GovAction,
} from '@govtool/data-providers/chain-data';

import { toLegacyDescription } from 'src/common/legacy-description';
import type { EpochSchedule } from 'src/common/legacy-network';
import { compareFiguresDescending, voteFigure } from 'src/common/vote-figures';
import { toLegacyParamProposal } from 'src/epoch/epoch.service';
import type {
  GovernanceActionDetailRow,
  GovernanceActionListRow,
  GovernanceActionSort,
  GovernanceActionStatus,
  GovernanceActionStatusTimes,
} from './governance-actions.type';
import { governanceActionStatusFilters } from './governance-actions.type';

/**
 * `GovAction` → the governance action wire rows. Pure, so the shapes are pinned by
 * unit tests without a provider.
 *
 * The rows were db-sync `SELECT`s in the service the governance action UI was written
 * against; every field is rebuilt here from the contract entity instead.
 */

/** The contract's type names back to db-sync's, as every client expects. */
const GOVERNANCE_ACTION_TYPE: Record<GovAction['type'], string> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  UpdateCommittee: 'NewCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

/** Document text resolved from the action's anchor, when there is one. */
export type GovernanceActionText = {
  document: Record<string, unknown> | null;
  title: string | null;
  abstract: string | null;
  motivation: string | null;
  rationale: string | null;
};

export const NO_TEXT: GovernanceActionText = {
  document: null,
  title: null,
  abstract: null,
  motivation: null,
  rationale: null,
};

/** Epoch-number → epoch-start instant, as `Date#toISOString` renders it. */
export function epochStartIso(
  schedule: EpochSchedule | null,
  epoch: number,
): string | null {
  if (schedule === null || !Number.isSafeInteger(epoch) || epoch < 0) {
    return null;
  }
  const date = new Date(
    schedule.systemStartMs + epoch * schedule.epochLengthMs,
  );
  return Number.isNaN(date.getTime()) ? null : date.toISOString();
}

/** A stamp's time, normalised to `toISOString`, else its epoch's start. */
function stampTime(
  stamp: EpochStamp | null,
  schedule: EpochSchedule | null,
): string | null {
  if (stamp === null) return null;
  if (stamp.time !== undefined) {
    const ms = Date.parse(stamp.time);
    if (!Number.isNaN(ms)) return new Date(ms).toISOString();
  }
  return epochStartIso(schedule, stamp.epoch);
}

const epochOf = (stamp: EpochStamp | null) => stamp?.epoch ?? null;

export function toGovernanceActionStatus(
  action: GovAction,
): GovernanceActionStatus {
  const { ratifiedAt, enactedAt, droppedAt, expiredAt } = action.lifecycle;
  return {
    ratified_epoch: epochOf(ratifiedAt),
    enacted_epoch: epochOf(enactedAt),
    dropped_epoch: epochOf(droppedAt),
    expired_epoch: epochOf(expiredAt),
  };
}

function toGovernanceActionStatusTimes(
  action: GovAction,
  schedule: EpochSchedule | null,
): GovernanceActionStatusTimes {
  const { ratifiedAt, enactedAt, droppedAt, expiredAt } = action.lifecycle;
  return {
    ratified_time: stampTime(ratifiedAt, schedule),
    enacted_time: stampTime(enactedAt, schedule),
    dropped_time: stampTime(droppedAt, schedule),
    expired_time: stampTime(expiredAt, schedule),
  };
}

/** One tally figure, or `null` when it is not known (see `voteFigure`). */
const tally = voteFigure;

function common(
  action: GovAction,
  schedule: EpochSchedule | null,
  committee: Committee | null,
) {
  const { submitted, expires } = action.lifecycle;
  return {
    id: action.id,
    tx_hash: action.txHash,
    index: action.index,
    type: GOVERNANCE_ACTION_TYPE[action.type],
    description: toLegacyDescription(action, committee),
    expiry_date: stampTime(expires, schedule),
    expiration: expires?.epoch ?? null,
    time: stampTime(submitted, schedule),
    epoch_no: submitted.epoch,
    url: action.anchor?.url ?? null,
    data_hash: action.anchor?.dataHash ?? null,
    proposal_params:
      action.body.type === 'ParameterChange'
        ? toLegacyParamProposal(action.body.changes)
        : null,
  };
}

export function toGovernanceActionListRow(
  action: GovAction,
  schedule: EpochSchedule | null,
  committee: Committee | null,
  text: GovernanceActionText = NO_TEXT,
): GovernanceActionListRow {
  const c = common(action, schedule, committee);
  return {
    id: c.id,
    tx_hash: c.tx_hash,
    index: c.index,
    type: c.type,
    yes_votes: tally(action, 'drep', 'yes'),
    no_votes: tally(action, 'drep', 'no'),
    abstain_votes: tally(action, 'drep', 'abstain'),
    description: c.description,
    expiry_date: c.expiry_date,
    expiration: c.expiration,
    time: c.time,
    epoch_no: c.epoch_no,
    url: c.url,
    data_hash: c.data_hash,
    proposal_params: c.proposal_params,
    title: text.title,
    abstract: text.abstract,
    status: toGovernanceActionStatus(action),
    status_times: toGovernanceActionStatusTimes(action, schedule),
  };
}

/**
 * The epoch an ended action's figures are read at, as the UI picks it:
 * ratified, else expired, else dropped, else the current epoch.
 */
export function relevantEpoch(action: GovAction, currentEpoch: number): number {
  const { ratifiedAt, expiredAt, droppedAt } = action.lifecycle;
  return (
    ratifiedAt?.epoch ?? expiredAt?.epoch ?? droppedAt?.epoch ?? currentEpoch
  );
}

export function toGovernanceActionDetailRow(
  action: GovAction,
  schedule: EpochSchedule | null,
  committee: Committee | null,
  currentEpoch: number,
  text: GovernanceActionText = NO_TEXT,
): GovernanceActionDetailRow {
  const c = common(action, schedule, committee);
  return {
    ...c,
    json_metadata: text.document,
    title: text.title,
    abstract: text.abstract,
    motivation: text.motivation,
    rationale: text.rationale,
    yes_votes: tally(action, 'drep', 'yes'),
    no_votes: tally(action, 'drep', 'no'),
    abstain_votes: tally(action, 'drep', 'abstain'),
    pool_yes_votes: tally(action, 'spo', 'yes'),
    pool_no_votes: tally(action, 'spo', 'no'),
    pool_abstain_votes: tally(action, 'spo', 'abstain'),
    cc_yes_votes: tally(action, 'cc', 'yes'),
    cc_no_votes: tally(action, 'cc', 'no'),
    cc_abstain_votes: tally(action, 'cc', 'abstain'),
    // Copy only contract fields: provider extensions must not escape on the wire.
    vote_aggregates: (action.voteAggregates ?? []).map((a) => ({
      role: a.role,
      representation: a.representation,
      yes: a.yes,
      no: a.no,
      abstain: a.abstain,
      notVoted: a.notVoted,
      totalEligible: a.totalEligible,
      threshold: {
        numerator: a.threshold.numerator,
        denominator: a.threshold.denominator,
      },
      ...(a.passing === undefined ? {} : { passing: a.passing }),
    })),
    prev_gov_action_index:
      action.previousAction === null
        ? null
        : String(action.previousAction.index),
    prev_gov_action_tx_hash: action.previousAction?.txHash ?? null,
    used_epoch_no: relevantEpoch(action, currentEpoch),
    status: toGovernanceActionStatus(action),
    status_times: toGovernanceActionStatusTimes(action, schedule),
  };
}

/**
 * The list filter, exactly as the service the UI was written against applied
 * it. No filter at all lists ENDED actions only; type words and status words
 * combine as "any listed type" and "any listed status"; a type-only filter
 * also excludes live actions unless `live` is listed.
 */
export function matchesGovernanceActionFilters(
  action: GovAction,
  filters: readonly string[],
): boolean {
  const status = toGovernanceActionStatus(action);
  const isLive =
    status.ratified_epoch === null &&
    status.enacted_epoch === null &&
    status.dropped_epoch === null &&
    status.expired_epoch === null;
  if (filters.length === 0) return !isLive;

  const statusWords: readonly string[] = governanceActionStatusFilters;
  const types = filters.filter((f) => !statusWords.includes(f));
  const statuses = filters.filter((f) => statusWords.includes(f));
  const typeMatches =
    types.length === 0 || types.includes(GOVERNANCE_ACTION_TYPE[action.type]);
  if (!typeMatches) return false;

  const wantsLive = statuses.includes('live');
  if (statuses.length === 0) return wantsLive || !isLive;
  return (
    (statuses.includes('expired') && status.expired_epoch !== null) ||
    (statuses.includes('ratified') && status.ratified_epoch !== null) ||
    (statuses.includes('enacted') && status.enacted_epoch !== null) ||
    (wantsLive && isLive)
  );
}

/** Case-insensitive substring over the ids and the document's title and abstract. */
export function matchesGovernanceActionSearch(
  action: GovAction,
  text: GovernanceActionText,
  search: string,
): boolean {
  if (search === '') return true;
  const needle = search.toLowerCase();
  return [
    `${action.txHash}#${action.index}`,
    action.id,
    text.title,
    text.abstract,
  ].some((value) => value !== null && value.toLowerCase().includes(needle));
}

const submittedMs = (action: GovAction) => {
  const ms =
    action.lifecycle.submitted.time === undefined
      ? Number.NaN
      : Date.parse(action.lifecycle.submitted.time);
  return Number.isNaN(ms) ? 0 : ms;
};

/**
 * Newest first by submission epoch (the default), oldest first, or by DRep
 * yes stake; ties broken by submission time, newest first, then by id so the
 * order is total and pages never overlap.
 */
export function compareGovernanceActions(sort: GovernanceActionSort) {
  return (a: GovAction, b: GovAction): number => {
    const ea = a.lifecycle.submitted.epoch;
    const eb = b.lifecycle.submitted.epoch;
    let primary = 0;
    if (sort === 'oldestFirst') primary = ea - eb;
    else if (sort === 'newestFirst') primary = eb - ea;
    else
      primary = compareFiguresDescending(
        tally(a, 'drep', 'yes'),
        tally(b, 'drep', 'yes'),
      );
    if (primary !== 0) return primary;
    const byTime = submittedMs(b) - submittedMs(a);
    if (byTime !== 0) return byTime;
    return a.id < b.id ? 1 : a.id > b.id ? -1 : 0;
  };
}
