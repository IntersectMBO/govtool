/**
 * Vote aggregates, computed the way the Conway ledger ratifies — for LIVE
 * actions only.
 *
 * Blockfrost serves stake as it stands NOW: the DRep directory's `amount`,
 * `/pools/extended`'s `active_stake`, the committee in force. For a live
 * action the tally epoch IS the current epoch, so those are the totals as they
 * stood for that action (SPEC.md §5.2). For a concluded action they are not,
 * and there is no per-epoch history to read, so a concluded action carries no
 * `voteAggregates` at all rather than today's figures under an old vote.
 *
 * Representations, as the db-sync provider:
 *   drep, spo  `stake` (lovelace) — what the ledger decides by.
 *   cc         `count` (members) — the committee is one member, one vote.
 *
 * In every aggregate yes + no + abstain + notVoted = totalEligible, and the
 * ledger's ratio is yes / (totalEligible - abstain), compared with threshold.
 *
 * DRep:  eligible = stake of active DReps (neither retired nor expired) plus
 *        the always-no-confidence stake. always-abstain stake is outside the
 *        denominator, as in the ledger. always-no-confidence is a Yes on a
 *        NoConfidence action and a No on everything else. An active DRep that
 *        did not vote is `notVoted` (the ledger counts it as No). A vote by a
 *        DRep that is not active carries no stake.
 * SPO:   eligible = the epoch's active stake, the pool distribution total;
 *        each pool's weight is its `/pools/extended` `active_stake`.
 *        A pool that did not vote follows the ledger default where the data
 *        allows: on HardForkInitiation it is `notVoted` (No); during
 *        bootstrap (protocol major 9) it abstains.
 *
 *        TWO DEVIATIONS from the ledger (and from the db-sync provider), both
 *        measured on mainnet in epoch 657:
 *
 *        1. Snapshot. Blockfrost's `active_stake` is the epoch's leader-
 *           schedule snapshot. The ledger weighs SPO votes with the snapshot
 *           taken at the epoch's start (db-sync `pool_stat.voting_power`), one
 *           snapshot newer, which Blockfrost does not serve. Per pool the two
 *           differ by a fraction of a percent (one pool: 57.19M vs 57.37M ada).
 *        2. Silent pools. After bootstrap a silent pool's default is decided by
 *           its REWARD ACCOUNT's DRep delegation: always-abstain → abstain,
 *           always-no-confidence → Yes on NoConfidence and No elsewhere.
 *           Reading that is `/pools/{id}` plus `/accounts/{reward}` for every
 *           silent pool — about 6,000 requests on mainnet — so every silent
 *           pool is `notVoted` here. This is NOT small: 623 pools holding
 *           12.1B ada of 21.4B active stake had always-abstain reward
 *           accounts, so for every type SPOs vote on except HardForkInitiation
 *           the SPO `abstain` is understated and `notVoted` overstated by about
 *           that much, and the ratio yes / (totalEligible - abstain) reads far
 *           lower than the ledger's.
 * CC:    eligible = members of the current committee who are unexpired, have an
 *        authorised hot key and have not resigned. None when dissolved.
 */
import type { GovActionType, Ratio, VoteAggregate, VoterRole } from '@govtool/data-providers/chain-data';

import { loadClock } from '../../chain';
import type { Session } from '../../context';
import { internal } from '../../errors';
import { big, toLovelace } from '../../numbers';
import { loadCurrentParams, type BfParams } from '../../network';
import { maxRatio, requireRatio } from '../../ratio';
import { credentialKey, loadCommittee, type CommitteeState } from '../committee';
import { loadDirectory, type Directory } from '../dreps/directory';
import { paramGroups } from './body';
import type { Vote } from './votes';
import { encodeDRepId, encodePoolId } from '../../ids';

const NEVER: Ratio = { numerator: 1, denominator: 1 };
const ZERO: Ratio = { numerator: 0, denominator: 1 };

export interface Figures {
  yes: bigint;
  no: bigint;
  abstain: bigint;
  notVoted: bigint;
}

const total = (f: Figures) => f.yes + f.no + f.abstain + f.notVoted;

interface BfPoolExtended {
  pool_id: string;
  active_stake: string;
}

/** Everything a tally needs beyond the votes, loaded once per call. */
export interface Eligibility {
  epoch: number;
  params: BfParams;
  directory: Directory;
  /** pool1 id → active stake. */
  pools: Map<string, bigint>;
  poolTotal: bigint;
  committee: CommitteeState;
}

export const loadEligibility = (s: Session): Promise<Eligibility> =>
  s.once('eligibility', async () => {
    const [clock, params, directory, poolRows, committee] = await Promise.all([
      loadClock(s),
      loadCurrentParams(s),
      loadDirectory(s),
      s.http.getAll<BfPoolExtended>('/pools/extended'),
      loadCommittee(s),
    ]);
    if (clock.latest.active_stake === null) throw internal('Blockfrost has no active stake for the current epoch');
    const pools = new Map(poolRows.map((p) => [p.pool_id, BigInt(toLovelace(p.active_stake, 'pool active_stake'))]));
    return {
      epoch: clock.epoch,
      params,
      directory,
      pools,
      poolTotal: BigInt(toLovelace(clock.latest.active_stake, 'epoch active_stake')),
      committee,
    };
  });

export function drepFigures(type: GovActionType, votes: readonly Vote[], e: Eligibility): Figures {
  const active = new Map<string, bigint>();
  let activeTotal = 0n;
  for (const d of e.directory.dreps) {
    if (d.status !== 'active') continue;
    const amount = big(d.amount);
    active.set(d.id, amount);
    activeTotal += amount;
  }
  const cast = { yes: 0n, no: 0n, abstain: 0n };
  for (const v of votes) {
    if (v.voter.kind !== 'drep') continue;
    const amount = active.get(encodeDRepId(v.voter.hash, v.voter.isScript));
    if (amount !== undefined) cast[v.choice] += amount;
  }
  const noConfidence = big(e.directory.alwaysNoConfidence);
  const isNoConfidence = type === 'NoConfidence';
  return {
    yes: cast.yes + (isNoConfidence ? noConfidence : 0n),
    no: cast.no + (isNoConfidence ? 0n : noConfidence),
    abstain: cast.abstain,
    notVoted: activeTotal - cast.yes - cast.no - cast.abstain,
  };
}

export function spoFigures(type: GovActionType, votes: readonly Vote[], e: Eligibility): Figures {
  const cast = { yes: 0n, no: 0n, abstain: 0n };
  for (const v of votes) {
    if (v.voter.kind !== 'spo') continue;
    // A pool outside the current distribution has no stake to vote with.
    cast[v.choice] += e.pools.get(encodePoolId(v.voter.hash)) ?? 0n;
  }
  const silent = e.poolTotal - cast.yes - cast.no - cast.abstain;
  const bootstrap = Number(e.params.protocol_major_ver) < 10;
  if (type !== 'HardForkInitiation' && bootstrap) return { ...cast, abstain: cast.abstain + silent, notVoted: 0n };
  return { ...cast, notVoted: silent };
}

export function ccFigures(votes: readonly Vote[], e: Eligibility): Figures {
  const eligible = new Set<string>();
  for (const [hotKey, member] of e.committee.byHot) {
    if (member.hasResigned || member.termExpiryEpoch === null || member.termExpiryEpoch < e.epoch) continue;
    eligible.add(`cc:${hotKey}`);
  }
  const cast = { yes: 0n, no: 0n, abstain: 0n };
  for (const v of votes) if (v.voter.kind === 'cc' && eligible.has(`cc:${credentialKey(v.voter)}`)) cast[v.choice] += 1n;
  return { ...cast, notVoted: BigInt(eligible.size) - cast.yes - cast.no - cast.abstain };
}

const param = (p: BfParams, key: keyof BfParams): Ratio => requireRatio(p[key], `current ${String(key)}`);

/**
 * The threshold each role is judged by, or undefined where the role does not
 * vote on this type — the db-sync provider's table, over the current
 * parameters. InfoAction has no ledger threshold (it can never be ratified)
 * and is given 1/1, the unreachable bound.
 */
export function thresholds(
  type: GovActionType,
  paramKeys: readonly string[] | undefined,
  p: BfParams,
  committee: { exists: boolean; quorum?: Ratio },
): Partial<Record<VoterRole, Ratio>> {
  const bootstrap = Number(p.protocol_major_ver) < 10;
  const drep = (key: keyof BfParams) => (bootstrap ? ZERO : param(p, key));
  const cc = committee.quorum;
  switch (type) {
    case 'InfoAction':
      return { drep: NEVER, spo: NEVER, cc: NEVER };
    case 'NoConfidence':
      return { drep: drep('dvt_motion_no_confidence'), spo: param(p, 'pvt_motion_no_confidence') };
    case 'UpdateCommittee':
      return committee.exists
        ? { drep: drep('dvt_committee_normal'), spo: param(p, 'pvt_committee_normal') }
        : { drep: drep('dvt_committee_no_confidence'), spo: param(p, 'pvt_committee_no_confidence') };
    case 'NewConstitution':
      return { drep: drep('dvt_update_to_constitution'), ...(cc ? { cc } : {}) };
    case 'HardForkInitiation':
      return { drep: drep('dvt_hard_fork_initiation'), spo: param(p, 'pvt_hard_fork_initiation'), ...(cc ? { cc } : {}) };
    case 'TreasuryWithdrawals':
      return { drep: drep('dvt_treasury_withdrawal'), ...(cc ? { cc } : {}) };
    case 'ParameterChange': {
      const groups = paramGroups(paramKeys ?? []);
      const key: Record<string, keyof BfParams> = {
        ppNetworkGroup: 'dvt_p_p_network_group',
        ppEconomicGroup: 'dvt_p_p_economic_group',
        ppTechnicalGroup: 'dvt_p_p_technical_group',
        ppGovGroup: 'dvt_p_p_gov_group',
      };
      const drepThreshold = bootstrap
        ? ZERO
        : groups.drep.map((g) => param(p, key[g]!)).reduce<Ratio | undefined>((a, b) => (a ? maxRatio(a, b) : b), undefined);
      return {
        ...(drepThreshold ? { drep: drepThreshold } : {}),
        ...(groups.security ? { spo: param(p, 'pvtpp_security_group') } : {}),
        ...(cc ? { cc } : {}),
      };
    }
  }
}

function aggregate(role: VoterRole, figures: Figures, threshold: Ratio): VoteAggregate {
  if (figures.notVoted < 0n) throw internal(`Negative not-voted ${role} figure`);
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

/** Aggregates for one LIVE action. Pure; unit-tested. */
export function assembleAggregates(
  type: GovActionType,
  paramKeys: readonly string[] | undefined,
  votes: readonly Vote[],
  e: Eligibility,
): VoteAggregate[] {
  const exists = !e.committee.committee.isDissolved;
  const t = thresholds(type, paramKeys, e.params, exists ? { exists, quorum: e.committee.committee.quorum } : { exists });
  const out: VoteAggregate[] = [];
  if (t.drep) out.push(aggregate('drep', drepFigures(type, votes, e), t.drep));
  if (t.spo) out.push(aggregate('spo', spoFigures(type, votes, e), t.spo));
  // With no committee in force there is no quorum and nobody eligible; the
  // committee row is left out rather than given an invented threshold.
  if (t.cc && exists) out.push(aggregate('cc', ccFigures(votes, e), t.cc));
  return out;
}
