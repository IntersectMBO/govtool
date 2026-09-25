/**
 * `ProposalsApi` over db-sync 13.x: `gov_action_proposal`, `voting_procedure`,
 * the stake distributions (`drep_distr`, `pool_stat`) and the committee
 * tables. See ./proposals/aggregates.ts for how votes are tallied.
 */
import type {
  EpochStamp,
  GovAction,
  GovActionLineage,
  GovActionRef,
  GovActionSort,
  GovActionStatus,
  GovActionType,
  OptionalArgument,
  PageRequest,
  ProposalFilter,
  ProposalListQuery,
  ProposalsApi,
  VoteAggregateRepresentation,
  VoteRecord,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { invalidInput, notFound } from '../errors';
import { decodeGovActionId, encodeGovActionId, isHex } from '../ids';
import { toInt } from '../numbers';
import { toPage, toWindow } from '../paging';
import { loadAggregates, type TallyTarget } from './proposals/aggregates';
import { LINEAGE_DB_TYPES, TYPE_TO_DB } from './proposals/body';
import {
  deriveStatus,
  PROPOSAL_COLUMNS,
  PROPOSAL_JOINS,
  STATUS_SQL,
  TALLY_EPOCH_SQL,
  toGovAction,
  toLifecycle,
  type ProposalRow,
} from './proposals/rows';
import {
  latestVotesSql,
  parseVoterId,
  toVoteRecord,
  VOTE_COLUMNS,
  VOTE_JOINS,
  voterPredicate,
  type VoterKey,
  type VoteRow,
} from './proposals/voters';

/* ------------------------------------------------------------------------- */
/* Declaration — system.ts builds the capability document from these          */
/* ------------------------------------------------------------------------- */

/** What `list` honours. `newest` and `oldest` are required. */
export const PROPOSAL_SORTS: GovActionSort[] = [
  'newest',
  'oldest',
  'soonestToExpire',
  'mostYesVotes',
  'highestParticipation',
];
export const PROPOSAL_FILTERS: ProposalFilter[] = ['type', 'status'];
/** `stake` for DReps and pools, `count` for the committee. */
export const VOTE_AGGREGATE: VoteAggregateRepresentation[] = ['stake', 'count'];
export const PROPOSAL_OPTIONAL_ARGUMENTS: OptionalArgument[] = ['proposals.voterContextOnList'];

/* ------------------------------------------------------------------------- */
/* Whitelists: every sort and filter value maps to fixed SQL                   */
/* ------------------------------------------------------------------------- */

const ORDER_SQL: Record<GovActionSort, string> = {
  newest: 'g.id DESC',
  oldest: 'g.id ASC',
  soonestToExpire: 'g.expiration ASC NULLS LAST, g.id ASC',
  mostYesVotes: 'r.yes_key DESC, g.id DESC',
  highestParticipation: 'r.participation_key DESC NULLS LAST, g.id DESC',
};

const own = <T extends object>(table: T, key: unknown): key is keyof T =>
  typeof key === 'string' && Object.prototype.hasOwnProperty.call(table, key);

/**
 * Per-action DRep ranking keys over the filtered set `f`, for the two
 * vote-derived sorts. Yes stake reads `drep_distr` by (hash, epoch), which is
 * indexed; participation also needs each tally epoch's active total, which is
 * a sequential scan of `drep_distr`.
 */
function rankingCtes(sort: 'mostYesVotes' | 'highestParticipation'): string {
  const participation = sort === 'highestParticipation';
  return `,
  dv AS (
    SELECT DISTINCT ON (vp.gov_action_proposal_id, vp.drep_voter)
           vp.gov_action_proposal_id AS pid, vp.drep_voter AS voter, vp.vote::text AS vote
      FROM voting_procedure vp
     WHERE vp.invalid IS NULL AND vp.drep_voter IS NOT NULL AND vp.gov_action_proposal_id IN (SELECT id FROM f)
     ORDER BY vp.gov_action_proposal_id, vp.drep_voter, vp.tx_id DESC, vp.id DESC),
  dc AS (
    SELECT f.id, sum(dd.amount) FILTER (WHERE dv.vote = 'Yes') AS yes, sum(dd.amount) AS voted
      FROM f
      JOIN dv ON dv.pid = f.id
      JOIN drep_distr dd ON dd.hash_id = dv.voter AND dd.epoch_no = f.e AND dd.active_until >= f.e
     GROUP BY f.id),
  nc AS (
    SELECT dd.epoch_no AS e, sum(dd.amount) AS amount
      FROM drep_distr dd
     WHERE dd.hash_id IN (SELECT id FROM drep_hash WHERE view = 'drep_always_no_confidence')
       AND dd.epoch_no IN (SELECT e FROM f)
     GROUP BY dd.epoch_no)${
       participation
         ? `,
  act AS (
    SELECT dd.epoch_no AS e, sum(dd.amount) AS amount
      FROM drep_distr dd
     WHERE dd.active_until >= dd.epoch_no AND dd.epoch_no IN (SELECT DISTINCT e FROM f)
     GROUP BY dd.epoch_no)`
         : ''
     },
  r AS (
    SELECT f.id,
           COALESCE(dc.yes, 0) + CASE WHEN f.db_type = 'NoConfidence' THEN COALESCE(nc.amount, 0) ELSE 0 END AS yes_key,
           ${
             participation
               ? 'COALESCE(dc.voted, 0)::numeric / NULLIF(COALESCE(act.amount, 0) + COALESCE(nc.amount, 0), 0)'
               : 'NULL::numeric'
           } AS participation_key
      FROM f
      LEFT JOIN dc ON dc.id = f.id
      LEFT JOIN nc ON nc.e = f.e${participation ? '\n      LEFT JOIN act ON act.e = f.e' : ''})`;
}

/** A search term this provider can match: the action's CIP-129 id, or its transaction hash. */
function parseSearch(term: string): { txHash: string; index?: number } | undefined {
  const t = term.trim();
  if (t.startsWith('gov_action1')) {
    try {
      return decodeGovActionId(t);
    } catch {
      return undefined;
    }
  }
  if (isHex(t, 32)) return { txHash: t.toLowerCase() };
  return undefined;
}

function validateList(q: ProposalListQuery) {
  if (q === null || typeof q !== 'object') throw invalidInput('query must be an object');
  const window = toWindow(q);
  const sort = q.sort ?? 'newest';
  if (!own(ORDER_SQL, sort)) throw invalidInput('Unknown sort', { sort });
  const types = q.type ?? [];
  if (!Array.isArray(types) || !types.every((t) => own(TYPE_TO_DB, t))) {
    throw invalidInput('type must be a list of governance action types', { type: q.type });
  }
  const statuses = q.status ?? [];
  if (!Array.isArray(statuses) || !statuses.every((s) => own(STATUS_SQL, s))) {
    throw invalidInput('status must be a list of governance action statuses', { status: q.status });
  }
  if (q.voted !== undefined && typeof q.voted !== 'boolean') throw invalidInput('voted must be a boolean');
  if (q.voted !== undefined && q.voterId === undefined) throw invalidInput('voted needs voterId');
  const voter = q.voterId === undefined ? undefined : parseVoterId(q.voterId);
  if (q.search !== undefined && typeof q.search !== 'string') throw invalidInput('search must be a string');
  return { window, sort, types: types as GovActionType[], statuses: statuses as GovActionStatus[], voter };
}

export function createProposalsApi(ctx: Ctx): ProposalsApi {
  const { db } = ctx;

  /** Attach aggregates, computed at each action's tally epoch. */
  async function withAggregates(rows: ProposalRow[]): Promise<GovAction[]> {
    const mapped = rows.map((row) => ({ row, ...toGovAction(row, ctx.network) }));
    const targets: TallyTarget[] = mapped.map(({ row, paramKeys }) => ({
      id: row.id,
      dbType: row.db_type,
      epoch: Number(row.tally_epoch),
      ...(paramKeys ? { paramKeys } : {}),
    }));
    const aggregates = await loadAggregates(db, targets);
    return mapped.map(({ row, action }) => ({ ...action, voteAggregates: aggregates.get(row.id) ?? [] }));
  }

  /** The voter's latest vote on each of the given actions. */
  async function votesBy(voter: VoterKey, proposalIds: string[]): Promise<Map<string, VoteRecord>> {
    if (proposalIds.length === 0) return new Map();
    const params: unknown[] = [proposalIds];
    const predicate = voterPredicate(voter, params);
    const rows = await db.query<VoteRow>(
      `WITH v AS (${latestVotesSql(`vp.gov_action_proposal_id = ANY($1::bigint[]) AND (${predicate})`)})
       SELECT ${VOTE_COLUMNS} FROM v ${VOTE_JOINS}
        ORDER BY v.tx_id DESC, v.id DESC`,
      params,
    );
    const out = new Map<string, VoteRecord>();
    // A cold credential can match several hot keys; the latest vote wins.
    for (const row of rows) if (!out.has(String(row.proposal_id))) out.set(String(row.proposal_id), toVoteRecord(row));
    return out;
  }

  /** Resolve a CIP-129 id to its row; NOT_FOUND when well-formed but unknown. */
  async function findProposal(id: string): Promise<ProposalRow> {
    if (typeof id !== 'string') throw invalidInput('id must be a CIP-129 governance action id');
    const { txHash, index } = decodeGovActionId(id);
    const [row] = await db.query<ProposalRow>(
      `WITH cur AS (SELECT max(no) AS no FROM epoch)
       SELECT ${PROPOSAL_COLUMNS}
         FROM tx t
         JOIN gov_action_proposal g ON g.tx_id = t.id
        CROSS JOIN cur
        ${PROPOSAL_JOINS}
        WHERE t.hash = decode($1, 'hex') AND g.index = $2`,
      [txHash, index],
    );
    if (!row) throw notFound('Governance action not found', { id });
    return row;
  }

  return {
    async list(q) {
      const { window, sort, types, statuses, voter } = validateList(q);
      const params: unknown[] = [];
      const bind = (value: unknown) => {
        params.push(value);
        return `$${params.length}`;
      };
      const conditions: string[] = [];
      if (types.length) conditions.push(`g.type::text = ANY(${bind(types.map((t) => TYPE_TO_DB[t]))}::text[])`);
      if (statuses.length) conditions.push(`(${statuses.map((s) => `(${STATUS_SQL[s]})`).join(' OR ')})`);
      if (q.search !== undefined && q.search.trim() !== '') {
        const match = parseSearch(q.search);
        // A term that is neither an action id nor a transaction hash matches nothing.
        if (!match) return ctx.paged({ elements: [], total: 0 });
        conditions.push(`t.hash = decode(${bind(match.txHash)}, 'hex')`);
        if (match.index !== undefined) conditions.push(`g.index = ${bind(match.index)}`);
      }
      if (voter && q.voted !== undefined) {
        const predicate = voterPredicate(voter, params);
        conditions.push(
          `${q.voted ? '' : 'NOT '}EXISTS (SELECT 1 FROM voting_procedure vp
             WHERE vp.gov_action_proposal_id = g.id AND vp.invalid IS NULL AND (${predicate}))`,
        );
      }
      const where = conditions.length ? `WHERE ${conditions.join(' AND ')}` : '';
      const filtered = `
        WITH cur AS (SELECT max(no) AS no FROM epoch),
        f AS (
          SELECT g.id, g.type::text AS db_type, ${TALLY_EPOCH_SQL} AS e
            FROM gov_action_proposal g
            JOIN tx t ON t.id = g.tx_id
           CROSS JOIN cur
           ${where})`;
      const ranked = sort === 'mostYesVotes' || sort === 'highestParticipation';
      const limit = bind(window.limit);
      const offset = bind(window.offset);
      const rows = await db.query<ProposalRow>(
        `${filtered}${ranked ? rankingCtes(sort) : ''}
         SELECT ${PROPOSAL_COLUMNS}, count(*) OVER () AS total_count
           FROM f
           JOIN gov_action_proposal g ON g.id = f.id
           JOIN tx t ON t.id = g.tx_id
          CROSS JOIN cur
          ${ranked ? 'JOIN r ON r.id = f.id' : ''}
          ${PROPOSAL_JOINS}
          ORDER BY ${ORDER_SQL[sort]}
          LIMIT ${limit} OFFSET ${offset}`,
        params,
      );
      let total = rows[0] ? Number(rows[0].total_count) : undefined;
      if (total === undefined) {
        const [count] = await db.query<{ n: string }>(
          `${filtered} SELECT count(*) AS n FROM f`,
          params.slice(0, params.length - 2),
        );
        total = Number(count?.n ?? 0);
      }
      const actions = await withAggregates(rows);
      const mine = voter ? await votesBy(voter, rows.map((r) => r.id)) : undefined;
      const elements = mine
        ? actions.map((action, i) => ({ ...action, myVote: mine.get(rows[i]!.id) ?? null }))
        : actions;
      return ctx.paged({ elements, total });
    },

    async get(id, q) {
      const voter = q?.voterId === undefined ? undefined : parseVoterId(q.voterId);
      const row = await findProposal(id);
      const [action] = await withAggregates([row]);
      if (!voter) return ctx.envelope(action!);
      const mine = await votesBy(voter, [row.id]);
      return ctx.envelope({ ...action!, myVote: mine.get(row.id) ?? null });
    },

    async getEnacted(lineage: GovActionLineage) {
      if (!own(LINEAGE_DB_TYPES, lineage)) throw invalidInput('Unknown governance action lineage', { lineage });
      const [row] = await db.query<{ tx_hash: string; index: string | number }>(
        `SELECT encode(t.hash, 'hex') AS tx_hash, g.index
           FROM gov_action_proposal g
           JOIN tx t ON t.id = g.tx_id
          WHERE g.type::text = ANY($1::text[]) AND g.enacted_epoch IS NOT NULL
          ORDER BY g.enacted_epoch DESC, g.id DESC
          LIMIT 1`,
        [LINEAGE_DB_TYPES[lineage]],
      );
      const ref: GovActionRef | null = row
        ? { id: encodeGovActionId(row.tx_hash, toInt(row.index)), txHash: row.tx_hash, index: toInt(row.index) }
        : null;
      return ctx.envelope(ref);
    },

    async listVotes(id: string, q: PageRequest) {
      const window = toWindow(q);
      const proposal = await findProposal(id);
      const rows = await db.query<VoteRow & { total_count: string }>(
        `WITH v AS (${latestVotesSql('vp.gov_action_proposal_id = $1')})
         SELECT ${VOTE_COLUMNS}, count(*) OVER () AS total_count
           FROM v ${VOTE_JOINS}
          ORDER BY v.tx_id DESC, v.id DESC
          LIMIT $2 OFFSET $3`,
        [proposal.id, window.limit, window.offset],
      );
      let totalWhenEmpty: number | undefined;
      if (rows.length === 0) {
        const [count] = await db.query<{ n: string }>(
          `SELECT count(*) AS n FROM (${latestVotesSql('vp.gov_action_proposal_id = $1')}) v`,
          [proposal.id],
        );
        totalWhenEmpty = Number(count?.n ?? 0);
      }
      return ctx.paged(toPage(rows, toVoteRecord, totalWhenEmpty));
    },

    async listActivity(id: string, q: PageRequest) {
      const window = toWindow(q);
      const row = await findProposal(id);
      const lifecycle = toLifecycle(row);
      const events: { status: GovActionStatus; at: EpochStamp }[] = [{ status: 'live', at: lifecycle.submitted }];
      if (lifecycle.ratifiedAt) events.push({ status: 'ratified', at: lifecycle.ratifiedAt });
      if (lifecycle.enactedAt) events.push({ status: 'enacted', at: lifecycle.enactedAt });
      if (lifecycle.expiredAt && deriveStatus(row) === 'expired') events.push({ status: 'expired', at: lifecycle.expiredAt });
      if (lifecycle.droppedAt) events.push({ status: 'dropped', at: lifecycle.droppedAt });
      return ctx.paged({
        elements: events.slice(window.offset, window.offset + window.limit),
        total: events.length,
      });
    },
  };
}
