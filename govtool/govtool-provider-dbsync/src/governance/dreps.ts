/**
 * `governance.dreps` over db-sync (SPEC.md §5.3).
 *
 * Omitted, on purpose:
 *   listDelegators   each row needs the delegator's ACTIVE voting power, the
 *                    stake the ledger counted in the DRep distribution. db-sync
 *                    records only the per-DRep total (drep_distr); per-account
 *                    figures exist only in epoch_stake, which is the POOL
 *                    snapshot, taken at a different boundary and missing every
 *                    account that delegates to a DRep but not to a pool. A row
 *                    without that figure is not conformant (D32), so the
 *                    method is absent rather than half-filled.
 *   liveVotingPower  no live per-DRep figure in db-sync.
 */
import type {
  DRep,
  DRepFilter,
  DRepKind,
  DRepListQuery,
  DRepSort,
  DRepStatus,
  DRepsApi,
  SearchMode,
  VoteSort,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, invalidInput, notFound, unsupported } from '../errors';
import { decodeDRepId } from '../ids';
import { toInt } from '../numbers';
import { toPage, toWindow } from '../paging';
import { toDRep, toHistoryEvent, toVoteRow, type DetailRow, type HistoryRow, type ListRow, type VoteRow } from './dreps/map';
import {
  activitySql,
  countSql,
  countsSql,
  delegatorCountsSql,
  detailsSql,
  historyCountSql,
  historySql,
  listSql,
  resolveSql,
  votesCountSql,
  votesSql,
  type ListOrderKey,
} from './dreps/sql';

/** What `list` honours. The capability declaration is assembled from these. */
export const DREP_SORTS: DRepSort[] = ['votingPower', 'registrationDate', 'random'];
export const DREP_FILTERS: DRepFilter[] = ['status', 'kind'];
/** `exactId` is always supported. Name-based search needs metadata: the index provider's job. */
export const DREP_SEARCH: SearchMode[] = ['exactId'];

/** Vote-listing orders `listVotes` honours. */
export const DREP_VOTE_SORTS: VoteSort[] = ['newest', 'oldest'];

const STATUSES: readonly DRepStatus[] = ['active', 'inactive', 'retired'];
const KINDS: readonly DRepKind[] = ['drep', 'anonymous'];
const ALL_SORTS: readonly DRepSort[] = ['votingPower', 'registrationDate', 'activity', 'random'];

type Id = number | string;

interface Search {
  hash: Buffer;
  isScript: boolean;
}

/** An enum-array filter: each value checked against its whitelist. Empty = no filter. */
function enumFilter<V extends string>(name: string, value: unknown, allowed: readonly V[]): V[] | null {
  if (value === undefined || value === null) return null;
  if (!Array.isArray(value)) throw invalidInput(`${name} must be an array`, { [name]: value });
  for (const v of value) {
    if (typeof v !== 'string' || !(allowed as readonly string[]).includes(v)) {
      throw invalidInput(`Unknown ${name} value`, { [name]: v, allowed });
    }
  }
  return value.length === 0 ? null : [...new Set(value as V[])];
}

function listSort(value: unknown): ListOrderKey {
  if (value === undefined || value === null) return 'random';
  if (typeof value !== 'string' || !(ALL_SORTS as readonly string[]).includes(value)) {
    throw invalidInput('Unknown DRep sort', { sort: value, allowed: ALL_SORTS });
  }
  if (!(DREP_SORTS as readonly string[]).includes(value)) throw unsupported(`DRep sort '${value}'`);
  return value as ListOrderKey;
}

/**
 * Search is one term with no mode (D108). Only `exactId` is supported, so the
 * term matches when it decodes as a CIP-129 DRep id and never otherwise. Any
 * other input, a CIP-105 id included, is simply not a match: search is free
 * text, so it is not an input error.
 */
function toSearch(value: unknown): Search | null | 'none' {
  if (value === undefined || value === null) return null;
  if (typeof value !== 'string') throw invalidInput('search must be a string', { search: value });
  const term = value.trim();
  if (term === '') return null;
  try {
    const { hash, isScript } = decodeDRepId(term);
    return { hash: Buffer.from(hash, 'hex'), isScript };
  } catch {
    return 'none';
  }
}

function requireQuery(q: unknown): void {
  if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
}

export function createDRepsApi(ctx: Ctx): DRepsApi {
  const { db } = ctx;

  /** drep_hash id of a DRep that has registered; NOT_FOUND otherwise. Rejects CIP-105 and junk. */
  async function resolve(id: string): Promise<Id> {
    if (typeof id !== 'string') throw invalidInput('DRep id must be a string', { id });
    const { hash, isScript } = decodeDRepId(id);
    const [row] = await db.query<{ id: Id }>(resolveSql, [Buffer.from(hash, 'hex'), isScript]);
    if (!row) throw notFound('DRep not found', { id });
    return row.id;
  }

  /** Registration facts, delegator counts and activity for a page of DReps. */
  async function hydrate(rows: ListRow[]): Promise<DRep[]> {
    if (rows.length === 0) return [];
    const ids = rows.map((r) => String(r.id));
    const [details, delegators, activity] = await Promise.all([
      db.query<DetailRow>(detailsSql, [ids]),
      db.query<{ drep_hash_id: Id; delegators: Id }>(delegatorCountsSql, [ids]),
      db.query<{ drep_hash_id: Id; votable: Id; voted: Id }>(activitySql, [ids]),
    ]);
    const byDRep = new Map<string, DetailRow[]>();
    for (const d of details) {
      const key = String(d.drep_hash_id);
      byDRep.set(key, [...(byDRep.get(key) ?? []), d]);
    }
    const counts = new Map(delegators.map((d) => [String(d.drep_hash_id), toInt(d.delegators)]));
    const acts = new Map(activity.map((a) => [String(a.drep_hash_id), { voted: toInt(a.voted), votable: toInt(a.votable) }]));
    return rows.map((row) => {
      const key = String(row.id);
      const act = acts.get(key);
      return toDRep(row, byDRep.get(key) ?? [], {
        // Computed for every DRep on the page; no row means none.
        delegatorCount: counts.get(key) ?? 0,
        // Every registered DRep has a window, so a missing row means an empty listing.
        activity: act ?? { voted: 0, votable: 0 },
      });
    });
  }

  return {
    list: async (q: DRepListQuery) => {
      requireQuery(q);
      const sort = listSort(q.sort);
      const window = toWindow(q);
      if (sort === 'random' && q.page !== 1) {
        throw invalidInput('A randomly ordered DRep read is not paged: page must be 1', { page: q.page });
      }
      const status = enumFilter('status', q.status, STATUSES);
      const kind = enumFilter('kind', q.kind, KINDS);
      const search = toSearch(q.search);
      if (search === 'none') return ctx.paged({ elements: [], total: 0 });

      const filters = [status, kind, search?.hash ?? null, search?.isScript ?? null];
      const rows = await db.query<ListRow>(listSql(sort), [...filters, window.limit, window.offset]);
      let totalWhenEmpty: number | undefined;
      if (rows.length === 0) {
        totalWhenEmpty =
          window.offset === 0 ? 0 : toInt((await db.query<{ total_count: Id }>(countSql, filters))[0]?.total_count ?? 0);
      }
      const elements = await hydrate(rows);
      const total = rows[0]?.total_count !== undefined ? toInt(rows[0].total_count) : totalWhenEmpty;
      return ctx.paged({ elements, ...(total === undefined ? {} : { total }) });
    },

    get: async (id: string) => {
      if (typeof id !== 'string') throw invalidInput('DRep id must be a string', { id });
      const { hash, isScript } = decodeDRepId(id);
      const rows = await db.query<ListRow>(listSql('registrationDate'), [
        null,
        null,
        Buffer.from(hash, 'hex'),
        isScript,
        1,
        0,
      ]);
      const [drep] = await hydrate(rows);
      if (!drep) throw notFound('DRep not found', { id });
      return ctx.envelope(drep);
    },

    listVotes: async (id, q) => {
      requireQuery(q);
      const window = toWindow(q);
      const order = q.sort ?? 'newest';
      if (!(DREP_VOTE_SORTS as readonly string[]).includes(order)) {
        throw invalidInput('Unknown vote sort', { sort: order, allowed: DREP_VOTE_SORTS });
      }
      if (q.voted !== undefined && q.voted !== null && typeof q.voted !== 'boolean') {
        throw invalidInput('voted must be a boolean', { voted: q.voted });
      }
      const voted = typeof q.voted === 'boolean' ? q.voted : null;
      const drepId = String(await resolve(id));
      const rows = await db.query<VoteRow>(votesSql(order), [[drepId], voted, window.limit, window.offset]);
      let totalWhenEmpty: number | undefined;
      if (rows.length === 0) {
        totalWhenEmpty =
          window.offset === 0
            ? 0
            : toInt((await db.query<{ total_count: Id }>(votesCountSql, [[drepId], voted]))[0]?.total_count ?? 0);
      }
      return ctx.paged(toPage(rows, toVoteRow, totalWhenEmpty));
    },

    listUpdateHistory: async (id, q) => {
      requireQuery(q);
      const window = toWindow(q);
      const order = q.sort ?? 'desc';
      if (order !== 'asc' && order !== 'desc') throw invalidInput('sort must be asc or desc', { sort: order });
      const drepId = String(await resolve(id));
      const rows = await db.query<HistoryRow>(historySql(order), [drepId, window.limit, window.offset]);
      let totalWhenEmpty: number | undefined;
      if (rows.length === 0) {
        totalWhenEmpty =
          window.offset === 0 ? 0 : toInt((await db.query<{ total_count: Id }>(historyCountSql, [drepId]))[0]?.total_count ?? 0);
      }
      return ctx.paged(toPage(rows, toHistoryEvent, totalWhenEmpty));
    },

    getCounts: async () => {
      const [row] = await db.query<{ registered: Id; active: Id; inactive: Id; anonymous: Id }>(countsSql, [
        null,
        null,
        null,
        null,
      ]);
      if (!row) throw internal('DRep count query returned no row');
      return ctx.envelope({
        totalRegistered: toInt(row.registered),
        totalActive: toInt(row.active),
        totalInactive: toInt(row.inactive),
        anonymous: toInt(row.anonymous),
      });
    },
  };
}
