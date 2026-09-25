/**
 * `ProposalsApi` over Koios: `/proposal_list` (filtered, ordered and paged by
 * PostgREST), `/proposal_voting_summary` per action for the aggregates, and
 * `/vote_list` for individual votes. See ./proposals/aggregates.ts.
 *
 * Cost: a page of N actions is 1 + N requests plus a few shared ones —
 * `/proposal_voting_summary` takes one action at a time.
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
import { invalidInput, notFound, unsupported } from '../errors';
import { decodeGovActionId, encodeGovActionId, isHex } from '../ids';
import { slicePage, toWindow } from '../paging';
import type { ProposalRow, VoteListRow } from '../rows';
import { coldByHot, enactedLineage, readCommitteeInfo } from './committee';
import { loadAggregates } from './proposals/aggregates';
import { LINEAGE_DB_TYPES, TYPE_TO_DB } from './proposals/body';
import { deriveStatus, PROPOSAL_SELECT, refOf, STATUS_FILTER, toGovAction, toLifecycle } from './proposals/rows';
import { parseVoterId, readVotes, readVotesOn, toVoteRecord, type VoterKey } from './proposals/voters';

/* ------------------------------------------------------------------------- */
/* Declaration — system.ts builds the capability document from these          */
/* ------------------------------------------------------------------------- */

/**
 * `mostYesVotes` and `highestParticipation` are not offered: ranking by vote
 * weight needs every action's `/proposal_voting_summary`, one request each.
 */
export const PROPOSAL_SORTS: GovActionSort[] = ['newest', 'oldest', 'soonestToExpire'];
export const PROPOSAL_FILTERS: ProposalFilter[] = ['type', 'status'];
/** `stake` for DReps and pools, `count` for the committee. */
export const VOTE_AGGREGATE: VoteAggregateRepresentation[] = ['stake', 'count'];
export const PROPOSAL_OPTIONAL_ARGUMENTS: OptionalArgument[] = ['proposals.voterContextOnList'];

const ALL_SORTS: readonly GovActionSort[] = ['newest', 'oldest', 'soonestToExpire', 'mostYesVotes', 'highestParticipation'];

/** PostgREST orderings; every one is total (tx hash and index break ties). */
const ORDER: Record<string, string> = {
  newest: 'block_time.desc,proposal_tx_hash.desc,proposal_index.desc',
  oldest: 'block_time.asc,proposal_tx_hash.asc,proposal_index.asc',
  soonestToExpire: 'expiration.asc.nullslast,block_time.asc,proposal_tx_hash.asc,proposal_index.asc',
};

const own = <T extends object>(table: T, key: unknown): key is keyof T =>
  typeof key === 'string' && Object.prototype.hasOwnProperty.call(table, key);

/** A search term this provider can match: the action's CIP-129 id, or its transaction hash. */
function parseSearch(term: string): Record<string, string> | undefined {
  const t = term.trim();
  if (t.startsWith('gov_action1')) {
    try {
      const { txHash, index } = decodeGovActionId(t);
      return { proposal_id: `eq.${encodeGovActionId(txHash, index)}` };
    } catch {
      return undefined;
    }
  }
  if (isHex(t, 32)) return { proposal_tx_hash: `eq.${t.toLowerCase()}` };
  return undefined;
}

function validateList(q: ProposalListQuery) {
  if (q === null || typeof q !== 'object') throw invalidInput('query must be an object');
  const window = toWindow(q);
  const sort = q.sort ?? 'newest';
  if (!(ALL_SORTS as readonly unknown[]).includes(sort)) throw invalidInput('Unknown sort', { sort });
  if (!own(ORDER, sort)) throw unsupported(`proposal sort '${String(sort)}'`);
  const types = q.type ?? [];
  if (!Array.isArray(types) || !types.every((t) => own(TYPE_TO_DB, t))) {
    throw invalidInput('type must be a list of governance action types', { type: q.type });
  }
  const statuses = q.status ?? [];
  if (!Array.isArray(statuses) || !statuses.every((s) => own(STATUS_FILTER, s))) {
    throw invalidInput('status must be a list of governance action statuses', { status: q.status });
  }
  if (q.voted !== undefined && typeof q.voted !== 'boolean') throw invalidInput('voted must be a boolean');
  if (q.voted !== undefined && q.voterId === undefined) throw invalidInput('voted needs voterId');
  const voter = q.voterId === undefined ? undefined : parseVoterId(q.voterId);
  if (q.search !== undefined && typeof q.search !== 'string') throw invalidInput('search must be a string');
  return { window, sort, types: types as GovActionType[], statuses: statuses as GovActionStatus[], voter };
}

export function createProposalsApi(ctx: Ctx): ProposalsApi {
  /** Map rows and attach aggregates, computed at each action's tally epoch. */
  async function withAggregates(rows: ProposalRow[]): Promise<GovAction[]> {
    if (rows.length === 0) return [];
    const tip = await ctx.chain.tip();
    const mapped = rows.map((row) => ({ row, ...toGovAction(row, ctx.network) }));
    const aggregates = await loadAggregates(
      ctx,
      mapped.map(({ row, paramKeys }) => ({ row, ...(paramKeys ? { paramKeys } : {}) })),
      tip.epoch_no,
    );
    return mapped.map(({ row, action }) => ({ ...action, voteAggregates: aggregates.get(row.proposal_id) ?? [] }));
  }

  /** The voter's latest vote on each of the given actions. */
  async function votesBy(voter: VoterKey, proposalIds: string[]): Promise<Map<string, VoteRecord>> {
    const rows = await readVotesOn(ctx, proposalIds, { voter_id: `eq.${voter.id}`, voter_role: `eq.${voter.role}` });
    const coldOf = voter.role === 'ConstitutionalCommittee' ? coldByHot(await readCommitteeInfo(ctx)) : new Map<string, string>();
    return new Map(rows.map((row) => [row.proposal_id, toVoteRecord(row, coldOf)]));
  }

  async function findProposal(id: string): Promise<ProposalRow> {
    if (typeof id !== 'string') throw invalidInput('id must be a CIP-129 governance action id');
    const { txHash, index } = decodeGovActionId(id.trim());
    const canonical = encodeGovActionId(txHash, index);
    const { rows } = await ctx.http.get<ProposalRow>('proposal_list', { proposal_id: `eq.${canonical}` }, { select: PROPOSAL_SELECT });
    const row = rows[0];
    if (!row) throw notFound('Governance action not found', { id: canonical });
    return row;
  }

  return {
    async list(q) {
      const { window, sort, types, statuses, voter } = validateList(q);
      const query: Record<string, string> = {};
      if (types.length) query['proposal_type'] = `in.(${[...new Set(types.map((t) => TYPE_TO_DB[t]))].join(',')})`;
      if (statuses.length) query['or'] = `(${[...new Set(statuses)].map((s) => STATUS_FILTER[s]).join(',')})`;
      if (q.search !== undefined && q.search.trim() !== '') {
        const match = parseSearch(q.search);
        // A term that is neither an action id nor a transaction hash matches nothing.
        if (!match) return ctx.paged({ elements: [], total: 0 });
        Object.assign(query, match);
      }
      const order = ORDER[sort]!;

      let rows: ProposalRow[];
      let total: number;
      if (voter && q.voted !== undefined) {
        // Voted / not-voted is a set difference against the voter's votes; the
        // action list is small (hundreds), so it is filtered here, whole.
        const [all, votes] = await Promise.all([
          ctx.http.getAll<ProposalRow>('proposal_list', query, { select: PROPOSAL_SELECT, order }),
          readVotes(ctx, { voter_id: `eq.${voter.id}`, voter_role: `eq.${voter.role}` }),
        ]);
        const voted = new Set(votes.map((v) => v.proposal_id));
        const kept = all.filter((row) => voted.has(row.proposal_id) === q.voted);
        const page = slicePage(kept, window);
        rows = page.elements;
        total = page.total!;
      } else {
        ({ rows, total } = await ctx.http.getWindow<ProposalRow>('proposal_list', query, window, { select: PROPOSAL_SELECT, order }));
      }

      const actions = await withAggregates(rows);
      const mine = voter ? await votesBy(voter, rows.map((r) => r.proposal_id)) : undefined;
      const elements = mine ? actions.map((action) => ({ ...action, myVote: mine.get(action.id) ?? null })) : actions;
      return ctx.paged({ elements, total });
    },

    async get(id, q) {
      const voter = q?.voterId === undefined ? undefined : parseVoterId(q.voterId);
      const row = await findProposal(id);
      const [action] = await withAggregates([row]);
      if (!voter) return ctx.envelope(action!);
      const mine = await votesBy(voter, [row.proposal_id]);
      return ctx.envelope({ ...action!, myVote: mine.get(row.proposal_id) ?? null });
    },

    async getEnacted(lineage: GovActionLineage) {
      if (!own(LINEAGE_DB_TYPES, lineage)) throw invalidInput('Unknown governance action lineage', { lineage });
      const chain = await enactedLineage(ctx, lineage, LINEAGE_DB_TYPES[lineage]);
      const head = chain[chain.length - 1];
      const ref: GovActionRef | null = head ? refOf(head.proposal_id, head.proposal_tx_hash, head.proposal_index) : null;
      return ctx.envelope(ref);
    },

    async listVotes(id: string, q: PageRequest) {
      const window = toWindow(q);
      const proposal = await findProposal(id);
      const votes: VoteListRow[] = await readVotes(ctx, { proposal_id: `eq.${proposal.proposal_id}` });
      const coldOf = votes.some((v) => v.voter_role === 'ConstitutionalCommittee')
        ? coldByHot(await readCommitteeInfo(ctx))
        : new Map<string, string>();
      const page = slicePage(votes, window);
      return ctx.paged({ elements: page.elements.map((v) => toVoteRecord(v, coldOf)), total: page.total! });
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
      return ctx.paged(slicePage(events, window));
    },
  };
}
