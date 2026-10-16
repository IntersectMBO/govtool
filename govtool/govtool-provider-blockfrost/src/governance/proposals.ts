/**
 * `ProposalsApi` over Blockfrost (SPEC.md §5.2). See ./proposals/records.ts
 * for what one proposal costs and ./proposals/aggregates.ts for how votes are
 * tallied.
 *
 * `/governance/proposals` lists ids and types only, in chain order, with no
 * filters and no total. So a listing reads the whole index (two requests on
 * mainnet), filters by type there, and reads records only where it must:
 *
 *   type filter, newest, oldest  records for the requested page only
 *   status filter, soonestToExpire
 *                                every record of the type-filtered set (the
 *                                status and expiry are on the record), then
 *                                the page
 *
 * Either way the filtered set is known in full, so `total` is exact and no
 * page is short except the last.
 *
 * Declined:
 *   mostYesVotes, highestParticipation   aggregates exist only for live
 *                                        actions (see aggregates.ts), so a
 *                                        ranking over the whole set cannot be
 *                                        computed.
 *   proposals.voterContextOnList         annotating a page by a committee
 *                                        voter needs every action's vote list;
 *                                        rejected rather than half-served.
 */
import type {
  EpochStamp,
  GovAction,
  GovActionLineage,
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

import type { Ctx, Session } from '../context';
import { invalidInput, unsupported } from '../errors';
import { decodeGovActionId, isHex } from '../ids';
import { toWindow } from '../paging';
import { assembleAggregates, loadEligibility } from './proposals/aggregates';
import { LINEAGE_TYPES } from './proposals/body';
import { enactedHead } from './proposals/enacted';
import {
  deriveStatus,
  hydrate,
  loadIndex,
  requireRecord,
  toLifecycle,
  type Hydrated,
  type IndexEntry,
  type ProposalRecord,
} from './proposals/records';
import { loadProposalVotes, parseVoterId, toVoteRecord, type Vote } from './proposals/votes';
import { loadCommittee } from './committee';
import { txStamp } from '../chain';

/* ------------------------------------------------------------------------- */
/* Declaration — system.ts builds the capability document from these          */
/* ------------------------------------------------------------------------- */

export const PROPOSAL_SORTS: GovActionSort[] = ['newest', 'oldest', 'soonestToExpire'];
export const PROPOSAL_FILTERS: ProposalFilter[] = ['type', 'status'];
/** `stake` for DReps and pools, `count` for the committee — for live actions. */
export const VOTE_AGGREGATE: VoteAggregateRepresentation[] = ['stake', 'count'];
export const PROPOSAL_OPTIONAL_ARGUMENTS: OptionalArgument[] = [];

const ALL_SORTS: readonly GovActionSort[] = ['newest', 'oldest', 'soonestToExpire', 'mostYesVotes', 'highestParticipation'];
const TYPES: readonly GovActionType[] = [
  'ParameterChange',
  'HardForkInitiation',
  'TreasuryWithdrawals',
  'NoConfidence',
  'UpdateCommittee',
  'NewConstitution',
  'InfoAction',
];
const STATUSES: readonly GovActionStatus[] = ['live', 'ratified', 'enacted', 'expired', 'dropped'];

function enumList<V extends string>(name: string, value: unknown, allowed: readonly V[]): V[] {
  if (value === undefined || value === null) return [];
  if (!Array.isArray(value) || !value.every((v) => (allowed as readonly unknown[]).includes(v))) {
    throw invalidInput(`${name} must be a list of ${allowed.join(', ')}`, { [name]: value });
  }
  return [...new Set(value as V[])];
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
  if (!(ALL_SORTS as readonly unknown[]).includes(sort)) throw invalidInput('Unknown sort', { sort });
  if (!PROPOSAL_SORTS.includes(sort)) throw unsupported(`proposal sort '${sort}'`);
  const types = enumList('type', q.type, TYPES);
  const statuses = enumList('status', q.status, STATUSES);
  if (q.voted !== undefined && typeof q.voted !== 'boolean') throw invalidInput('voted must be a boolean');
  if (q.voted !== undefined && q.voterId === undefined) throw invalidInput('voted needs voterId');
  // Declined (see header): refused, never accepted and ignored.
  if (q.voterId !== undefined) throw unsupported('voter context on a proposal listing (proposals.voterContextOnList)');
  if (q.search !== undefined && typeof q.search !== 'string') throw invalidInput('search must be a string');
  return { window, sort, types, statuses };
}

export function createProposalsApi(ctx: Ctx): ProposalsApi {
  /** The full GovAction; aggregates attached when the action is live. */
  async function withAggregates(s: Session, h: Hydrated): Promise<GovAction> {
    if (deriveStatus(h.record) !== 'live') return h.action;
    const [eligibility, votes] = await Promise.all([
      loadEligibility(s),
      loadProposalVotes(s, h.record.txHash, h.record.index),
    ]);
    return { ...h.action, voteAggregates: assembleAggregates(h.record.type, h.paramKeys, votes, eligibility) };
  }

  async function findEntry(s: Session, id: unknown): Promise<IndexEntry & { record: ProposalRecord }> {
    if (typeof id !== 'string') throw invalidInput('id must be a CIP-129 governance action id');
    const { txHash, index } = decodeGovActionId(id);
    const record = await requireRecord(s, { id, txHash, index });
    return { id: record.id, txHash, index, type: record.type, seq: -1, record };
  }

  /** The caller's vote on one action, or null. See parseVoterId for the committee caveat. */
  async function myVote(s: Session, record: ProposalRecord, voterId: string): Promise<VoteRecord | null> {
    const { keys } = await parseVoterId(s, voterId);
    const votes = await loadProposalVotes(s, record.txHash, record.index);
    const mine = votes.find((v) => keys.has(v.voter.key));
    if (mine) return toVoteRecord(s, mine, record);
    if (voterId.startsWith('cc_cold1')) {
      // A committee vote under a hot key the current committee does not hold
      // may be this member's under a rotated key; Blockfrost cannot say whose.
      const { byHot } = await loadCommittee(s);
      const unattributed = votes.some((v) => v.voter.kind === 'cc' && !byHot.has(v.voter.key.slice(3)));
      if (unattributed) throw unsupported('attributing a committee vote cast under a hot key the current committee no longer holds');
    }
    return null;
  }

  return {
    async list(q) {
      const { window, sort, types, statuses } = validateList(q);
      const s = ctx.session();
      let entries = await loadIndex(s);
      if (types.length) entries = entries.filter((e) => types.includes(e.type));
      const term = q.search?.trim();
      if (term) {
        const match = parseSearch(term);
        // A term that is neither an action id nor a transaction hash matches nothing.
        if (!match) return ctx.paged({ elements: [], total: 0 });
        entries = entries.filter((e) => e.txHash === match.txHash && (match.index === undefined || e.index === match.index));
      }

      let ordered: IndexEntry[];
      if (statuses.length || sort === 'soonestToExpire') {
        const records = new Map(
          (await Promise.all(entries.map((e) => requireRecord(s, e)))).map((r) => [r.id, r] as const),
        );
        let kept = entries;
        if (statuses.length) kept = kept.filter((e) => statuses.includes(deriveStatus(records.get(e.id)!)));
        ordered =
          sort === 'soonestToExpire'
            ? [...kept].sort((a, b) => {
                const x = records.get(a.id)!.expiration ?? Infinity;
                const y = records.get(b.id)!.expiration ?? Infinity;
                return x - y || a.seq - b.seq;
              })
            : sort === 'newest'
              ? [...kept].reverse()
              : kept;
      } else {
        ordered = sort === 'newest' ? [...entries].reverse() : entries;
      }

      const page = ordered.slice(window.offset, window.offset + window.limit);
      const elements = await Promise.all(
        page.map(async (e) => withAggregates(s, await hydrate(s, await requireRecord(s, e), ctx.network))),
      );
      return ctx.paged({ elements, total: ordered.length });
    },

    async get(id, q) {
      const s = ctx.session();
      const { record } = await findEntry(s, id);
      const action = await withAggregates(s, await hydrate(s, record, ctx.network));
      if (q?.voterId === undefined) return ctx.envelope(action);
      return ctx.envelope({ ...action, myVote: await myVote(s, record, q.voterId) });
    },

    async getEnacted(lineage: GovActionLineage) {
      if (typeof lineage !== 'string' || !Object.prototype.hasOwnProperty.call(LINEAGE_TYPES, lineage)) {
        throw invalidInput('Unknown governance action lineage', { lineage });
      }
      return ctx.envelope(await enactedHead(ctx.session(), lineage, ctx.network));
    },

    /** Newest first, as the db-sync provider. Anchors come from each vote transaction's CBOR. */
    async listVotes(id: string, q: PageRequest) {
      const window = toWindow(q);
      const s = ctx.session();
      const { record } = await findEntry(s, id);
      const votes: Vote[] = [...(await loadProposalVotes(s, record.txHash, record.index))].reverse();
      const page = votes.slice(window.offset, window.offset + window.limit);
      const elements = await Promise.all(page.map((v) => toVoteRecord(s, v, record)));
      return ctx.paged({ elements, total: votes.length });
    },

    async listActivity(id: string, q: PageRequest) {
      const window = toWindow(q);
      const s = ctx.session();
      const { record } = await findEntry(s, id);
      const lifecycle = toLifecycle(record, await txStamp(s, record.txHash));
      const events: { status: GovActionStatus; at: EpochStamp }[] = [{ status: 'live', at: lifecycle.submitted }];
      if (lifecycle.ratifiedAt) events.push({ status: 'ratified', at: lifecycle.ratifiedAt });
      if (lifecycle.enactedAt) events.push({ status: 'enacted', at: lifecycle.enactedAt });
      if (lifecycle.expiredAt && deriveStatus(record) === 'expired') events.push({ status: 'expired', at: lifecycle.expiredAt });
      if (lifecycle.droppedAt) events.push({ status: 'dropped', at: lifecycle.droppedAt });
      return ctx.paged({ elements: events.slice(window.offset, window.offset + window.limit), total: events.length });
    },
  };
}

