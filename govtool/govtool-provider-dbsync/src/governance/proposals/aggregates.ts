/**
 * Vote aggregates, computed the way the Conway ledger ratifies.
 *
 * Every figure is taken at the action's TALLY EPOCH: the epoch it was
 * ratified, expired or dropped in, or the current epoch while it is live. The
 * denominators are therefore the totals as they stood for that action
 * (SPEC.md §5.2), and a concluded action's aggregate does not drift.
 *
 * The ledger decides at an epoch boundary from the state the new epoch starts
 * with, and db-sync's ratified_epoch and expired_epoch name that epoch. So a
 * concluded action's tally reads certificates (hot-key authorisations,
 * resignations, DRep retirements, vote delegations) up to the end of the epoch
 * before its tally epoch; one made during the tally epoch played no part. A
 * live action's tally reads the chain as it stands. See BOUND.
 *
 * Representations:
 *   drep, spo  `stake` (lovelace) — what the ledger decides by.
 *   cc         `count` (members) — the committee is one member, one vote.
 *
 * In every aggregate yes + no + abstain + notVoted = totalEligible, and the
 * ledger's ratio is yes / (totalEligible - abstain), compared with threshold.
 *
 * DRep:  eligible = stake of registered DReps active at the tally epoch plus
 *        the always-no-confidence stake. always-abstain stake is outside the
 *        denominator, as in the ledger. always-no-confidence is a Yes on a
 *        NoConfidence action and a No on everything else. An active DRep that
 *        did not vote is `notVoted` (the ledger counts it as No). On a live
 *        action a DRep that voted, or sent a registration or update
 *        certificate, since the epoch's snapshot is active whatever the
 *        snapshot says: that renewed it for the ledger's next one (RENEWED),
 *        and the DRep directory already calls it active. One that retired
 *        since, and has not registered again, is out of the votes and the
 *        total alike, as the ledger's next count will have it (RETIRED).
 * SPO:   eligible = the pool distribution. A pool that did not vote follows
 *        the ledger default: on HardForkInitiation it is `notVoted` (No);
 *        during bootstrap (protocol major 9) it abstains; otherwise its reward
 *        account's DRep delegation decides — always-abstain abstains,
 *        always-no-confidence is Yes on NoConfidence and No elsewhere, and
 *        anything else is `notVoted` (No).
 * CC:    the ledger's own count: members of the committee in force at the
 *        tally epoch who are unexpired, have an authorised hot key and have
 *        not resigned, each with that hot key's vote. A concluded action reads
 *        them as its tally epoch started (BOUND), so a member who resigns,
 *        rotates its hot key or is replaced after the decision keeps its vote
 *        in that tally, and one who left before it does not count.
 */
import { ChainDataError, type Ratio, type VoteAggregate, type VoterRole } from '@govtool/data-providers/chain-data';

import type { Db } from '../../db';
import { internal } from '../../errors';
import { toGovActionType, paramGroups } from './body';
import { maxRatio, requireRatio } from './ratio';

export interface TallyTarget {
  /** gov_action_proposal.id */
  id: string;
  dbType: string;
  /** The tally epoch. */
  epoch: number;
  /** Not yet ratified, expired or dropped: tallied on the chain as it stands. */
  live: boolean;
  /** Ledger keys of a ParameterChange. */
  paramKeys?: string[];
}

const NEVER: Ratio = { numerator: 1, denominator: 1 };
const ZERO: Ratio = { numerator: 0, denominator: 1 };

const big = (v: unknown): bigint => (v === null || v === undefined ? 0n : BigInt(String(v).replace(/\.0+$/, '')));

const stale = (what: string, epoch: number) =>
  new ChainDataError('STALE_DATA', `db-sync has no ${what} for epoch ${epoch} yet`, {
    retryable: true,
    details: { epoch },
  });

/* ------------------------------------------------------------------------- */
/* SQL                                                                        */
/* ------------------------------------------------------------------------- */

/** $1 = proposal ids, $2 = their tally epochs, $3 = whether each is live, index-aligned. */
const TARGETS = `p AS (SELECT * FROM unnest($1::bigint[], $2::int[], $3::boolean[]) AS p(id, e, live)),
  eps AS (SELECT DISTINCT e FROM p),
  cuts AS (SELECT DISTINCT e, live FROM p)`;

/**
 * Where each tally stops reading certificates, per tally epoch and liveness
 * (`cuts`), as transaction ids (tx ids grow with blocks).
 *
 *   snap_tx  the last transaction of the epoch before the tally epoch: the
 *            ledger state the tally epoch started with.
 *   max_tx   snap_tx for a concluded action, which that state decided. The
 *            last transaction so far for a live one.
 *
 * The last block of an epoch is read through the epoch index, the OFFSET 0
 * fence stopping the planner from walking the block table back from the tip;
 * the tx is then one backward step of the tx block index. No join to `tx` is
 * needed afterwards, which on mainnet would be a scan of every transaction.
 */
export const BOUND = `bound AS MATERIALIZED (
    SELECT lb.e, lb.live,
           (SELECT t.id FROM tx t WHERE t.block_id <= lb.snap_block ORDER BY t.block_id DESC, t.id DESC LIMIT 1) AS snap_tx,
           (SELECT t.id FROM tx t WHERE t.block_id <= CASE WHEN lb.live THEN lb.last_block ELSE lb.snap_block END
             ORDER BY t.block_id DESC, t.id DESC LIMIT 1) AS max_tx
      FROM (SELECT cuts.e, cuts.live,
                   (SELECT max(x.id) FROM (SELECT b.id FROM block b WHERE b.epoch_no = cuts.e - 1 OFFSET 0) x) AS snap_block,
                   (SELECT max(x.id) FROM (SELECT b.id FROM block b WHERE b.epoch_no = cuts.e OFFSET 0) x) AS last_block
              FROM cuts) lb)`;

/**
 * The same tally-epoch validity rule must govern displayed totals and ranking.
 * `target` names the row carrying the tally's `e` and `live`.
 */
export const validDRepVoteSql = (vote: string, target: string) => `NOT EXISTS (
       SELECT 1 FROM drep_registration retired
       JOIN bound ON bound.e = ${target}.e AND bound.live = ${target}.live
        WHERE retired.drep_hash_id = ${vote}.voter AND retired.deposit < 0
          AND retired.tx_id > ${vote}.tx_id AND retired.tx_id <= bound.max_tx)`;

/**
 * DReps that renewed their activity after a live tally's snapshot: a vote, or
 * a registration or update certificate, since the epoch began. The snapshot's
 * `active_until` predates it, but the ledger's next snapshot will not, so the
 * DRep counts as active, as the directory has it. A concluded tally keeps the
 * snapshot's answer, the one the ledger decided by. voting_procedure has no
 * index on tx_id; the scan is one per call, and only with a live target.
 */
export const RENEWED = `renewed AS (
    SELECT DISTINCT bound.e, a.hash_id
      FROM bound
      CROSS JOIN LATERAL (
        SELECT vp.drep_voter AS hash_id FROM voting_procedure vp
         WHERE vp.drep_voter IS NOT NULL AND vp.tx_id > bound.snap_tx AND vp.tx_id <= bound.max_tx
        UNION
        SELECT r.drep_hash_id FROM drep_registration r
         WHERE (r.deposit IS NULL OR r.deposit > 0) AND r.tx_id > bound.snap_tx AND r.tx_id <= bound.max_tx) a
     WHERE bound.live)`;

/**
 * DReps that retired after a live tally's snapshot and have not registered
 * again since. The snapshot still lists their stake, but the ledger's next
 * count leaves an unregistered DRep out, its votes and its stake alike. A
 * concluded tally keeps the snapshot it was decided from. drep_registration
 * holds a row per certificate, and its (drep_hash_id, deposit, tx_id) index
 * covers both reads.
 */
export const RETIRED = `retired AS (
    SELECT DISTINCT bound.e, r.drep_hash_id AS hash_id
      FROM bound
      JOIN drep_registration r ON r.deposit < 0 AND r.tx_id > bound.snap_tx AND r.tx_id <= bound.max_tx
     WHERE bound.live
       AND NOT EXISTS (
        SELECT 1 FROM drep_registration again
         WHERE again.drep_hash_id = r.drep_hash_id AND again.deposit > 0
           AND again.tx_id > r.tx_id AND again.tx_id <= bound.max_tx))`;

/**
 * Whether a DRep in the tally's distribution `dd` counts as active (see
 * RENEWED and RETIRED); `target` carries `live`. The INs are uncorrelated, so
 * `renewed` and `retired` are hashed once rather than scanned per row.
 */
export const activeDRepSql = (dd: string, target: string) => `((${dd}.active_until >= ${dd}.epoch_no
          OR (${target}.live AND (${dd}.hash_id, ${dd}.epoch_no) IN (SELECT rn.hash_id, rn.e FROM renewed rn)))
         AND NOT (${target}.live AND (${dd}.hash_id, ${dd}.epoch_no) IN (SELECT rt.hash_id, rt.e FROM retired rt)))`;

/**
 * Each voter's latest vote on each action that its tally counts: the latest
 * cast up to the tally's cutoff (BOUND). A concluded action takes the last
 * vote cast before its tally epoch began, the state the ledger decided from;
 * one cast during that epoch came after the decision. A live action takes the
 * latest so far. `target` names the rows carrying each action's `id`, `e` and
 * `live`; `filter` narrows voting_procedure by action id for its index. The
 * displayed totals and the ranking both read votes through this.
 */
export const tallyVotesSql = (column: string, target: string, filter: string) => `
  SELECT DISTINCT ON (vp.gov_action_proposal_id, vp.${column})
         vp.gov_action_proposal_id AS pid, vp.${column} AS voter, vp.vote::text AS vote, vp.tx_id
    FROM voting_procedure vp
    JOIN ${target} ON ${target}.id = vp.gov_action_proposal_id
    JOIN bound ON bound.e = ${target}.e AND bound.live = ${target}.live
   WHERE vp.invalid IS NULL AND vp.${column} IS NOT NULL AND ${filter} AND vp.tx_id <= bound.max_tx
   ORDER BY vp.gov_action_proposal_id, vp.${column}, vp.tx_id DESC, vp.id DESC`;

const latestVotes = (column: string) => tallyVotesSql(column, 'p', 'vp.gov_action_proposal_id = ANY($1::bigint[])');

/**
 * DRep totals. `drep_distr` has no epoch-leading index in db-sync 13.x, so the
 * per-epoch totals read the whole (hash_id, epoch_no) index once per call:
 * proportional to the table (35 ms for 1.4M rows on preview), not a lookup.
 */
export const DREP_SQL = `
WITH ${TARGETS},
  ${BOUND},
  ${RENEWED},
  ${RETIRED},
  v AS (${latestVotes('drep_voter')}),
  cast_ AS (
    SELECT p.id,
           sum(dd.amount) FILTER (WHERE v.vote = 'Yes') AS yes,
           sum(dd.amount) FILTER (WHERE v.vote = 'No') AS no,
           sum(dd.amount) FILTER (WHERE v.vote = 'Abstain') AS abstain
      FROM p
      JOIN v ON v.pid = p.id
      JOIN drep_distr dd ON dd.hash_id = v.voter AND dd.epoch_no = p.e
     WHERE ${activeDRepSql('dd', 'p')}
       AND ${validDRepVoteSql('v', 'p')}
     GROUP BY p.id),
  tot AS (
    SELECT cuts.e, cuts.live,
           sum(dd.amount) FILTER (WHERE ${activeDRepSql('dd', 'cuts')}) AS active,
           sum(dd.amount) FILTER (WHERE dh.view = 'drep_always_no_confidence') AS no_confidence
      FROM cuts
      JOIN drep_distr dd ON dd.epoch_no = cuts.e
      JOIN drep_hash dh ON dh.id = dd.hash_id
     GROUP BY cuts.e, cuts.live)
SELECT p.id::text AS id, p.e, tot.e IS NOT NULL AS has_distr, tot.active::text AS active,
       tot.no_confidence::text AS no_confidence,
       cast_.yes::text AS yes, cast_.no::text AS no, cast_.abstain::text AS abstain
  FROM p
  LEFT JOIN cast_ ON cast_.id = p.id
  LEFT JOIN tot ON tot.e = p.e AND tot.live = p.live`;

/**
 * SPO totals. `pool_stat` has no epoch index and `delegation_vote` no address
 * index in db-sync 13.x: both are sequential scans, once per call (about 45 ms
 * together on preview's 500k and 90k rows).
 */
export const SPO_SQL = `
WITH ${TARGETS},
  ${BOUND},
  ps AS MATERIALIZED (
    SELECT ps.epoch_no AS e, ps.pool_hash_id, COALESCE(ps.voting_power, 0) AS power
      FROM pool_stat ps
     WHERE ps.epoch_no IN (SELECT e FROM eps)),
  reward AS MATERIALIZED (
    SELECT DISTINCT ON (ps.e, ps.pool_hash_id) ps.e, ps.pool_hash_id, pu.reward_addr_id
      FROM ps
      JOIN pool_update pu ON pu.hash_id = ps.pool_hash_id AND pu.active_epoch_no <= ps.e
     ORDER BY ps.e, ps.pool_hash_id, pu.active_epoch_no DESC, pu.registered_tx_id DESC, pu.id DESC),
  rdv AS MATERIALIZED (
    SELECT dv.addr_id, dv.tx_id, dv.id, dh.view
      FROM delegation_vote dv
      JOIN drep_hash dh ON dh.id = dv.drep_hash_id
     WHERE dv.addr_id IN (SELECT reward_addr_id FROM reward)),
  deleg AS (
    SELECT DISTINCT ON (bound.e, bound.live, r.reward_addr_id) bound.e, bound.live, r.reward_addr_id, rdv.view
      FROM reward r
      JOIN bound ON bound.e = r.e
      JOIN rdv ON rdv.addr_id = r.reward_addr_id AND rdv.tx_id <= bound.max_tx
     ORDER BY bound.e, bound.live, r.reward_addr_id, rdv.tx_id DESC, rdv.id DESC),
  v AS (${latestVotes('pool_voter')})
SELECT p.id::text AS id, p.e,
       count(ps.pool_hash_id) AS pools,
       sum(ps.power)::text AS total,
       (sum(ps.power) FILTER (WHERE v.vote = 'Yes'))::text AS yes,
       (sum(ps.power) FILTER (WHERE v.vote = 'No'))::text AS no,
       (sum(ps.power) FILTER (WHERE v.vote = 'Abstain'))::text AS abstain,
       (sum(ps.power) FILTER (WHERE v.vote IS NULL))::text AS silent,
       (sum(ps.power) FILTER (WHERE v.vote IS NULL AND d.view = 'drep_always_no_confidence'))::text AS silent_no_confidence,
       (sum(ps.power) FILTER (WHERE v.vote IS NULL AND d.view = 'drep_always_abstain'))::text AS silent_abstain
  FROM p
  LEFT JOIN ps ON ps.e = p.e
  LEFT JOIN v ON v.pid = p.id AND v.voter = ps.pool_hash_id
  LEFT JOIN reward r ON r.e = ps.e AND r.pool_hash_id = ps.pool_hash_id
  LEFT JOIN deleg d ON d.e = r.e AND d.live = p.live AND d.reward_addr_id = r.reward_addr_id
 GROUP BY p.id, p.e`;

/**
 * Committee counts. The committee in force at an epoch is the one produced by
 * the latest ENACTED committee-lineage action: none after a NoConfidence, the
 * genesis committee (db-sync `committee` row with no proposal) before any.
 * It is read at each tally epoch.
 *
 * A member's hot key at the cutoff (BOUND) is its newest authorisation up to
 * it, with no resignation since, and the member counts by that key's vote.
 * committee_registration and committee_de_registration have no index but the
 * primary key; they hold a row per authorisation, so the lateral reads stay
 * small.
 */
export const CC_SQL = `
WITH ${TARGETS},
  ${BOUND},
  head AS (
    SELECT eps.e, h.id AS head_id, h.type::text AS head_type
      FROM eps
      LEFT JOIN LATERAL (
        SELECT g.id, g.type FROM gov_action_proposal g
         WHERE g.type IN ('NewCommittee', 'NoConfidence') AND g.enacted_epoch <= eps.e
         ORDER BY g.enacted_epoch DESC, g.id DESC
         LIMIT 1) h ON true),
  comm AS (
    SELECT head.e, c.id AS committee_id, c.quorum_numerator, c.quorum_denominator
      FROM head
      JOIN committee c ON (head.head_id IS NULL AND c.gov_action_proposal_id IS NULL)
                       OR (head.head_type = 'NewCommittee' AND c.gov_action_proposal_id = head.head_id)),
  seated AS (
    SELECT bound.e, bound.live, cm.committee_hash_id AS cold_id, reg.hot_key_id AS hot_id
      FROM bound
      JOIN comm ON comm.e = bound.e
      JOIN committee_member cm ON cm.committee_id = comm.committee_id AND cm.expiration_epoch >= bound.e
      JOIN LATERAL (
        SELECT cr.hot_key_id, cr.tx_id FROM committee_registration cr
         WHERE cr.cold_key_id = cm.committee_hash_id AND cr.tx_id <= bound.max_tx
         ORDER BY cr.tx_id DESC, cr.id DESC
         LIMIT 1) reg ON true
     WHERE NOT EXISTS (
        SELECT 1 FROM committee_de_registration d
         WHERE d.cold_key_id = cm.committee_hash_id AND d.tx_id > reg.tx_id AND d.tx_id <= bound.max_tx)),
  v AS (${latestVotes('committee_voter')})
SELECT p.id::text AS id, p.e, comm.committee_id IS NOT NULL AS has_committee,
       comm.quorum_numerator::text AS quorum_numerator, comm.quorum_denominator::text AS quorum_denominator,
       count(s.cold_id) AS eligible,
       count(s.cold_id) FILTER (WHERE v.vote = 'Yes') AS yes,
       count(s.cold_id) FILTER (WHERE v.vote = 'No') AS no,
       count(s.cold_id) FILTER (WHERE v.vote = 'Abstain') AS abstain
  FROM p
  LEFT JOIN comm ON comm.e = p.e
  LEFT JOIN seated s ON s.e = p.e AND s.live = p.live
  LEFT JOIN v ON v.pid = p.id AND v.voter = s.hot_id
 GROUP BY p.id, p.e, comm.committee_id, comm.quorum_numerator, comm.quorum_denominator`;

/** Thresholds and protocol version at each tally epoch. */
export const PARAMS_SQL = `
SELECT epoch_no AS e, protocol_major, committee_min_size,
       dvt_motion_no_confidence, dvt_committee_normal, dvt_committee_no_confidence, dvt_update_to_constitution,
       dvt_hard_fork_initiation, dvt_p_p_network_group, dvt_p_p_economic_group, dvt_p_p_technical_group,
       dvt_p_p_gov_group, dvt_treasury_withdrawal,
       pvt_motion_no_confidence, pvt_committee_normal, pvt_committee_no_confidence, pvt_hard_fork_initiation,
       pvtpp_security_group
  FROM epoch_param
 WHERE epoch_no = ANY($1::int[])`;

/* ------------------------------------------------------------------------- */
/* Rows                                                                       */
/* ------------------------------------------------------------------------- */

export interface DRepRow {
  id: string;
  e: number;
  has_distr: boolean;
  active: string | null;
  no_confidence: string | null;
  yes: string | null;
  no: string | null;
  abstain: string | null;
}

export interface SpoRow {
  id: string;
  e: number;
  pools: string | number;
  total: string | null;
  yes: string | null;
  no: string | null;
  abstain: string | null;
  silent: string | null;
  silent_no_confidence: string | null;
  silent_abstain: string | null;
}

export interface CcRow {
  id: string;
  e: number;
  has_committee: boolean;
  quorum_numerator: string | null;
  quorum_denominator: string | null;
  eligible: string | number;
  yes: string | number;
  no: string | number;
  abstain: string | number;
}

export type ParamsRow = { e: number; protocol_major: number } & Record<string, number | string | null>;

/* ------------------------------------------------------------------------- */
/* Pure computation                                                           */
/* ------------------------------------------------------------------------- */

export interface Figures {
  yes: bigint;
  no: bigint;
  abstain: bigint;
  notVoted: bigint;
}

const total = (f: Figures) => f.yes + f.no + f.abstain + f.notVoted;

export function drepFigures(dbType: string, row: DRepRow): Figures {
  if (!row.has_distr) throw stale('DRep stake distribution', row.e);
  const yesCast = big(row.yes);
  const noCast = big(row.no);
  const abstainCast = big(row.abstain);
  const noConfidence = big(row.no_confidence);
  const isNoConfidence = dbType === 'NoConfidence';
  return {
    yes: yesCast + (isNoConfidence ? noConfidence : 0n),
    no: noCast + (isNoConfidence ? 0n : noConfidence),
    abstain: abstainCast,
    notVoted: big(row.active) - yesCast - noCast - abstainCast,
  };
}

export function spoFigures(dbType: string, row: SpoRow, protocolMajor: number): Figures {
  if (Number(row.pools) === 0) throw stale('pool stake distribution', row.e);
  const yes = big(row.yes);
  const no = big(row.no);
  const abstain = big(row.abstain);
  const silent = big(row.silent);
  if (dbType === 'HardForkInitiation') return { yes, no, abstain, notVoted: silent };
  if (protocolMajor < 10) return { yes, no, abstain: abstain + silent, notVoted: 0n };
  const silentNoConfidence = big(row.silent_no_confidence);
  const silentAbstain = big(row.silent_abstain);
  const isNoConfidence = dbType === 'NoConfidence';
  return {
    yes: yes + (isNoConfidence ? silentNoConfidence : 0n),
    no: no + (isNoConfidence ? 0n : silentNoConfidence),
    abstain: abstain + silentAbstain,
    notVoted: silent - silentNoConfidence - silentAbstain,
  };
}

export function ccFigures(row: CcRow): Figures {
  const yes = BigInt(row.yes);
  const no = BigInt(row.no);
  const abstain = BigInt(row.abstain);
  return { yes, no, abstain, notVoted: BigInt(row.eligible) - yes - no - abstain };
}

const param = (row: ParamsRow, column: string): Ratio => requireRatio(row[column], `epoch ${row.e} ${column}`);

/**
 * The threshold each role is judged by, or undefined where the role does not
 * vote on this type. InfoAction has no ledger threshold at all — it can never
 * be ratified — and is given 1/1, the unreachable bound, with passing unset.
 */
export function thresholds(
  target: TallyTarget,
  params: ParamsRow,
  committee: { exists: boolean; quorum?: Ratio },
): Partial<Record<VoterRole, Ratio>> {
  const type = toGovActionType(target.dbType);
  const bootstrap = params.protocol_major < 10;
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

/** Assemble aggregates from the four result sets. Pure; unit-tested. */
export function assembleAggregates(
  targets: readonly TallyTarget[],
  rows: { drep: DRepRow[]; spo: SpoRow[]; cc: CcRow[]; params: ParamsRow[] },
): Map<string, VoteAggregate[]> {
  const drep = new Map(rows.drep.map((r) => [String(r.id), r]));
  const spo = new Map(rows.spo.map((r) => [String(r.id), r]));
  const cc = new Map(rows.cc.map((r) => [String(r.id), r]));
  const params = new Map(rows.params.map((r) => [Number(r.e), r]));
  const out = new Map<string, VoteAggregate[]>();
  for (const target of targets) {
    const p = params.get(target.epoch);
    if (!p) throw stale('protocol parameters', target.epoch);
    const c = cc.get(target.id);
    const committee = c?.has_committee
      ? {
          exists: true,
          quorum: requireRatio({ numerator: c.quorum_numerator, denominator: c.quorum_denominator }, 'committee quorum'),
        }
      : { exists: false };
    const t = thresholds(target, p, committee);
    const list: VoteAggregate[] = [];
    if (t.drep) {
      const r = drep.get(target.id);
      if (!r) throw stale('DRep stake distribution', target.epoch);
      list.push(aggregate('drep', drepFigures(target.dbType, r), t.drep));
    }
    if (t.spo) {
      const r = spo.get(target.id);
      if (!r) throw stale('pool stake distribution', target.epoch);
      list.push(aggregate('spo', spoFigures(target.dbType, r, p.protocol_major), t.spo));
    }
    // With no committee in force (after an enacted NoConfidence) there is no
    // quorum and nobody eligible; the committee row is left out rather than
    // given an invented threshold.
    if (t.cc && c?.has_committee) {
      const result = aggregate('cc', ccFigures(c), t.cc);
      if (target.dbType !== 'InfoAction' && p.protocol_major >= 10) {
        const minSize = p.committee_min_size;
        if (minSize == null) throw stale('committee minimum size', target.epoch);
        // db-sync stores this as an int8, which pg returns as a string.
        if (!/^\d+$/.test(String(minSize))) throw internal('Invalid committee minimum size');
        // A quorum ratio cannot approve an undersized committee, even at a
        // zero threshold. Keep its breakdown, but veto the inferred decision.
        if (BigInt(c.eligible) < BigInt(minSize)) result.passing = false;
      }
      list.push(result);
    }
    out.set(target.id, list);
  }
  return out;
}

/** Fetch and assemble aggregates for a set of actions: four queries, in parallel. */
export async function loadAggregates(db: Db, targets: readonly TallyTarget[]): Promise<Map<string, VoteAggregate[]>> {
  if (targets.length === 0) return new Map();
  const ids = targets.map((t) => t.id);
  const epochs = targets.map((t) => t.epoch);
  const live = targets.map((t) => t.live);
  const distinct = [...new Set(epochs)];
  const [drep, spo, cc, params] = await Promise.all([
    db.query<DRepRow>(DREP_SQL, [ids, epochs, live]),
    db.query<SpoRow>(SPO_SQL, [ids, epochs, live]),
    db.query<CcRow>(CC_SQL, [ids, epochs, live]),
    db.query<ParamsRow>(PARAMS_SQL, [distinct]),
  ]);
  return assembleAggregates(targets, { drep, spo, cc, params });
}
