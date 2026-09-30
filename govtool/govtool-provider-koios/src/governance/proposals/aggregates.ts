/**
 * Vote aggregates from Koios' `/proposal_voting_summary`, in the db-sync
 * provider's semantics (see its proposals/aggregates.ts):
 *
 *   drep, spo  `stake` (lovelace).   cc  `count` (members).
 *   yes + no + abstain + notVoted = totalEligible, and the ledger's ratio is
 *   yes / (totalEligible - abstain), compared with the threshold.
 *
 * Koios computes every figure at the action's epoch of interest — ratified,
 * expired or dropped in, else the current epoch — which is the db-sync
 * provider's tally epoch, so the denominators are the totals as they stood for
 * that action (SPEC.md §5.2). Koios' fields are re-cut as follows:
 *
 * DRep. Koios' `drep_yes_vote_power + drep_no_vote_power` is the stake that
 *   is not abstaining (all DRep stake less inactive DReps, always-abstain and
 *   explicit abstentions), with always-no-confidence inside it. So
 *     yes      = explicit yes (+ always-no-confidence on a NoConfidence)
 *     no       = explicit no  (+ always-no-confidence otherwise)
 *     abstain  = explicit abstain
 *     notVoted = the rest of the non-abstaining stake (the ledger counts it No)
 *
 * SPO. Koios' yes + no is the pool distribution less explicit and passive
 *   (reward account -> always-abstain) abstentions; on a HardForkInitiation
 *   passive delegations do not count and only explicit abstentions come off.
 *     HardForkInitiation  yes, no, abstain explicit; notVoted = the rest
 *     bootstrap (< 10)    a pool that did not vote abstains; notVoted = 0
 *     otherwise           passive always-no-confidence is Yes on NoConfidence
 *                         and No elsewhere; passive always-abstain abstains
 *
 * CC. `/proposal_voting_summary` counts committee votes but never the number
 *   of members eligible AS OF the tally epoch, and Koios has no history of
 *   hot-key authorisations to rebuild it. The count aggregate is therefore
 *   served only where the tally epoch is the current epoch (a live action),
 *   from `/committee_info` (members in force, authorised, unexpired, not
 *   resigned) and `/vote_list` (their latest votes). For a concluded action
 *   the cc row is left out rather than given a guessed denominator.
 */
import type { Ratio, VoteAggregate, VoterRole } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../../context';
import { internal, staleData } from '../../errors';
import { inList } from '../../http';
import { encodeCommitteeHotId } from '../../ids';
import { toLovelace } from '../../numbers';
import { maxRatio, requireRatio } from '../../ratio';
import type { CommitteeInfoRow, EpochParamsRow, ProposalRow, VotingSummaryRow } from '../../rows';
import { readCommitteeInfo } from '../committee';
import { paramGroups, toGovActionType } from './body';
import { tallyEpoch } from './rows';
import { readVotesOn } from './voters';

export interface TallyTarget {
  row: ProposalRow;
  /** Ledger keys of a ParameterChange. */
  paramKeys?: string[];
}

const NEVER: Ratio = { numerator: 1, denominator: 1 };
const ZERO: Ratio = { numerator: 0, denominator: 1 };

const big = (v: string | number | null | undefined): bigint => (v === null || v === undefined ? 0n : BigInt(toLovelace(v)));

export interface Figures {
  yes: bigint;
  no: bigint;
  abstain: bigint;
  notVoted: bigint;
}

const total = (f: Figures) => f.yes + f.no + f.abstain + f.notVoted;

export function drepFigures(type: string, s: VotingSummaryRow): Figures {
  const yesCast = big(s.drep_active_yes_vote_power);
  const noCast = big(s.drep_active_no_vote_power);
  const abstain = big(s.drep_active_abstain_vote_power);
  const noConfidence = big(s.drep_always_no_confidence_vote_power);
  const nonAbstaining = big(s.drep_yes_vote_power) + big(s.drep_no_vote_power);
  const isNoConfidence = type === 'NoConfidence';
  const yes = yesCast + (isNoConfidence ? noConfidence : 0n);
  const no = noCast + (isNoConfidence ? 0n : noConfidence);
  if (yes !== big(s.drep_yes_vote_power)) throw internal('Koios DRep yes figures disagree with each other');
  return { yes, no, abstain, notVoted: nonAbstaining - yes - no };
}

export function spoFigures(type: string, s: VotingSummaryRow, protocolMajor: number): Figures {
  const yesCast = big(s.pool_active_yes_vote_power);
  const noCast = big(s.pool_active_no_vote_power);
  const abstainCast = big(s.pool_active_abstain_vote_power);
  const passiveAbstain = big(s.pool_passive_always_abstain_vote_power);
  const passiveNoConfidence = big(s.pool_passive_always_no_confidence_vote_power);
  const koiosYesNo = big(s.pool_yes_vote_power) + big(s.pool_no_vote_power);
  if (type === 'HardForkInitiation') {
    const all = koiosYesNo + abstainCast;
    return { yes: yesCast, no: noCast, abstain: abstainCast, notVoted: all - yesCast - noCast - abstainCast };
  }
  // Koios zeroes every pool figure on the types it thinks pools do not vote on;
  // passive abstentions alone cannot be the whole pool distribution.
  if (koiosYesNo + abstainCast === 0n) throw internal('Koios did not tally stake pool votes for an action pools vote on');
  const all = koiosYesNo + abstainCast + passiveAbstain;
  if (protocolMajor < 10) return { yes: yesCast, no: noCast, abstain: all - yesCast - noCast, notVoted: 0n };
  const isNoConfidence = type === 'NoConfidence';
  const yes = yesCast + (isNoConfidence ? passiveNoConfidence : 0n);
  const no = noCast + (isNoConfidence ? 0n : passiveNoConfidence);
  const abstain = abstainCast + passiveAbstain;
  if (yes !== big(s.pool_yes_vote_power)) throw internal('Koios pool yes figures disagree with each other');
  return { yes, no, abstain, notVoted: all - yes - no - abstain };
}

type ParamsRow = Pick<EpochParamsRow, 'epoch_no' | 'protocol_major'> & Record<string, number | null>;

const param = (row: ParamsRow, column: string): Ratio => requireRatio(row[column], `epoch ${row.epoch_no} ${column}`);

/**
 * The threshold each role is judged by, or undefined where the role does not
 * vote on this type — the db-sync provider's table. InfoAction can never be
 * ratified and is given 1/1, the unreachable bound.
 */
export function thresholds(
  target: TallyTarget,
  params: ParamsRow,
  committee: { exists: boolean; quorum?: Ratio },
): Partial<Record<VoterRole, Ratio>> {
  const type = toGovActionType(target.row.proposal_type);
  const bootstrap = (params.protocol_major ?? 0) < 10;
  const drep = (column: string) => (bootstrap ? ZERO : param(params, column));
  const cc = committee.quorum;
  switch (type) {
    case 'InfoAction':
      return { drep: NEVER, spo: NEVER, cc: NEVER };
    case 'NoConfidence':
      return { drep: drep('dvt_motion_no_confidence'), spo: param(params, 'pvt_motion_no_confidence') };
    case 'UpdateCommittee':
      return committee.exists
        ? { drep: drep('dvt_committee_normal'), spo: param(params, 'pvt_committee_normal') }
        : { drep: drep('dvt_committee_no_confidence'), spo: param(params, 'pvt_committee_no_confidence') };
    case 'NewConstitution':
      return { drep: drep('dvt_update_to_constitution'), ...(cc ? { cc } : {}) };
    case 'HardForkInitiation':
      return { drep: drep('dvt_hard_fork_initiation'), spo: param(params, 'pvt_hard_fork_initiation'), ...(cc ? { cc } : {}) };
    case 'TreasuryWithdrawals':
      return { drep: drep('dvt_treasury_withdrawal'), ...(cc ? { cc } : {}) };
    case 'ParameterChange': {
      const groups = paramGroups(target.paramKeys ?? []);
      const column: Record<string, string> = {
        ppNetworkGroup: 'dvt_p_p_network_group',
        ppEconomicGroup: 'dvt_p_p_economic_group',
        ppTechnicalGroup: 'dvt_p_p_technical_group',
        ppGovGroup: 'dvt_p_p_gov_group',
      };
      const drepThreshold = bootstrap
        ? ZERO
        : groups.drep.map((g) => param(params, column[g]!)).reduce<Ratio | undefined>((a, b) => (a ? maxRatio(a, b) : b), undefined);
      return {
        ...(drepThreshold ? { drep: drepThreshold } : {}),
        ...(groups.security ? { spo: param(params, 'pvtpp_security_group') } : {}),
        ...(cc ? { cc } : {}),
      };
    }
  }
}

function aggregate(role: VoterRole, figures: Figures, threshold: Ratio): VoteAggregate {
  for (const [k, v] of Object.entries(figures)) {
    if (v < 0n) throw internal(`Koios ${role} figures do not add up (negative ${k})`);
  }
  return {
    role,
    representation: role === 'cc' ? 'count' : 'stake',
    yes: figures.yes.toString(),
    no: figures.no.toString(),
    abstain: figures.abstain.toString(),
    notVoted: figures.notVoted.toString(),
    totalEligible: total(figures).toString(),
    threshold,
  };
}

/** The committee's eligible members now, keyed by hot credential, and its quorum. */
export function eligibleCommittee(info: CommitteeInfoRow | undefined, epoch: number): { hot: Set<string>; quorum: Ratio } | undefined {
  if (!info || !info.members || info.quorum_numerator === null || info.quorum_denominator === null) return undefined;
  const hot = new Set<string>();
  for (const m of info.members) {
    if (m.status !== 'authorized' || !m.cc_hot_hex || m.cc_hot_has_script === null) continue;
    if (m.expiration_epoch === null || m.expiration_epoch < epoch) continue;
    hot.add(encodeCommitteeHotId(m.cc_hot_hex.toLowerCase(), m.cc_hot_has_script));
  }
  return {
    hot,
    quorum: requireRatio({ numerator: info.quorum_numerator, denominator: info.quorum_denominator }, 'committee quorum'),
  };
}

/** Aggregates for a batch of actions, keyed by CIP-129 id. */
export async function loadAggregates(ctx: Ctx, targets: readonly TallyTarget[], currentEpoch: number): Promise<Map<string, VoteAggregate[]>> {
  const out = new Map<string, VoteAggregate[]>();
  if (targets.length === 0) return out;

  const summaries = await Promise.all(
    targets.map(async (t) => {
      const { rows } = await ctx.http.get<VotingSummaryRow>('proposal_voting_summary', { _proposal_id: t.row.proposal_id });
      const s = rows[0];
      if (!s) throw staleData('Koios has no vote summary for this action yet', { id: t.row.proposal_id });
      if (s.epoch_no !== tallyEpoch(t.row, currentEpoch)) {
        throw staleData('Koios tallied this action at a different epoch than its lifecycle names', { id: t.row.proposal_id });
      }
      return s;
    }),
  );

  const epochs = [...new Set(summaries.map((s) => s.epoch_no))];
  const live = targets.filter((t) => tallyEpoch(t.row, currentEpoch) === currentEpoch);
  const needsLineage = targets.some((t) => t.row.proposal_type === 'NewCommittee');
  const [params, lineage, info, ccVotes] = await Promise.all([
    ctx.http.get<ParamsRow>(
      'epoch_params',
      { epoch_no: inList(epochs.map(String)) },
      {
        select:
          'epoch_no,protocol_major,dvt_motion_no_confidence,dvt_committee_normal,dvt_committee_no_confidence,' +
          'dvt_update_to_constitution,dvt_hard_fork_initiation,dvt_p_p_network_group,dvt_p_p_economic_group,' +
          'dvt_p_p_technical_group,dvt_p_p_gov_group,dvt_treasury_withdrawal,pvt_motion_no_confidence,' +
          'pvt_committee_normal,pvt_committee_no_confidence,pvt_hard_fork_initiation,pvtpp_security_group',
      },
    ),
    needsLineage
      ? ctx.http.getAll<{ proposal_type: string; enacted_epoch: number }>(
          'proposal_list',
          { proposal_type: 'in.(NewCommittee,NoConfidence)', enacted_epoch: 'not.is.null' },
          { select: 'proposal_type,enacted_epoch,block_time' },
        )
      : Promise.resolve([]),
    live.length > 0 ? readCommitteeInfo(ctx) : Promise.resolve(undefined),
    live.length > 0 ? readVotesOn(ctx, live.map((t) => t.row.proposal_id), { voter_role: 'eq.ConstitutionalCommittee' }) : Promise.resolve([]),
  ]);
  const paramsByEpoch = new Map(params.rows.map((p) => [p.epoch_no, p]));
  const committeeNow = eligibleCommittee(info, currentEpoch);

  /** Whether a committee is in force at `epoch`: none after an enacted NoConfidence. */
  const committeeExists = (epoch: number) => {
    const head = lineage.filter((l) => l.enacted_epoch <= epoch).sort((a, b) => b.enacted_epoch - a.enacted_epoch)[0];
    return head === undefined || head.proposal_type === 'NewCommittee';
  };

  targets.forEach((target, i) => {
    const s = summaries[i]!;
    const p = paramsByEpoch.get(s.epoch_no);
    if (!p) throw staleData('Koios has no protocol parameters for the tally epoch yet', { epoch: s.epoch_no });
    const isLive = s.epoch_no === currentEpoch;
    const cc = isLive ? committeeNow : undefined;
    const t = thresholds(target, p, {
      exists: isLive ? committeeNow !== undefined : committeeExists(s.epoch_no),
      ...(cc ? { quorum: cc.quorum } : {}),
    });
    const list: VoteAggregate[] = [];
    if (t.drep) list.push(aggregate('drep', drepFigures(target.row.proposal_type, s), t.drep));
    if (t.spo) list.push(aggregate('spo', spoFigures(target.row.proposal_type, s, p.protocol_major ?? 0), t.spo));
    if (t.cc && cc) {
      const votes = ccVotes.filter((v) => v.proposal_id === target.row.proposal_id && cc.hot.has(v.voter_id));
      const count = (choice: string) => BigInt(votes.filter((v) => v.vote === choice).length);
      const yes = count('Yes');
      const no = count('No');
      const abstain = count('Abstain');
      list.push(aggregate('cc', { yes, no, abstain, notVoted: BigInt(cc.hot.size) - yes - no - abstain }, t.cc));
    }
    out.set(target.row.proposal_id, list);
  });
  return out;
}
