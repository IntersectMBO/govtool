/**
 * `governance.dreps` over Koios (SPEC.md §5.3). See ./dreps/directory.ts for
 * which endpoint answers what.
 *
 * `list` reads `/drep_list` for the directory and hydrates only the page,
 * unless a filter or sort needs a fact for every DRep: splitting active from
 * inactive, or sorting by voting power, needs `/drep_info` for the whole
 * candidate set (one POST per ~70 DReps); a `kind` filter or the
 * registration-date sort needs every certificate (`/drep_updates` in bulk).
 * Everything is then filtered, ordered and paged here, so `total` is exact.
 *
 * Omitted, on purpose:
 *   listDelegators   each row needs the delegator's ACTIVE voting power, the
 *                    stake the ledger counted. `/drep_delegators` reports each
 *                    account's live balance, not the snapshot, so a row would
 *                    not be conformant (the db-sync provider omits it too).
 *   liveVotingPower  Koios has no live per-DRep figure.
 *   activity on list rows — it needs each DRep's votes and the set of actions
 *                    votable while it was registered; it is served on `get`.
 */
import type {
  DRep,
  DRepFilter,
  DRepKind,
  DRepListQuery,
  DRepSort,
  DRepStatus,
  DRepVoteRow,
  DRepsApi,
  RegistrationEvent,
  SearchMode,
  VoteSort,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, invalidInput, notFound, unsupported } from '../errors';
import { decodeDRepId, encodeDRepId } from '../ids';
import { toLovelace } from '../numbers';
import { slicePage, toWindow } from '../paging';
import type { DRepInfoRow, DRepUpdateRow, ProposalRow, VoteListRow } from '../rows';
import {
  certFacts,
  certOrder,
  drepStatusOf,
  loadDirectory,
  loadDRepInfo,
  loadUpdates,
  toAnchor,
  toRegistrationEvent,
  type DirectoryEntry,
} from './dreps/directory';
import { toGovActionType } from './proposals/body';
import { refOf } from './proposals/rows';
import { readVotes, toChoice } from './proposals/voters';

/** What `list` honours. The capability declaration is assembled from these. */
export const DREP_SORTS: DRepSort[] = ['votingPower', 'registrationDate', 'random'];
export const DREP_FILTERS: DRepFilter[] = ['status', 'kind'];
/** `exactId` only. Name-based search needs metadata: the index provider's job. */
export const DREP_SEARCH: SearchMode[] = ['exactId'];
/** Vote-listing orders `listVotes` honours. */
export const DREP_VOTE_SORTS: VoteSort[] = ['newest', 'oldest'];

const STATUSES: readonly DRepStatus[] = ['active', 'inactive', 'retired'];
const KINDS: readonly DRepKind[] = ['drep', 'anonymous'];
const ALL_SORTS: readonly DRepSort[] = ['votingPower', 'registrationDate', 'activity', 'random'];

function enumFilter<V extends string>(name: string, value: unknown, allowed: readonly V[]): Set<V> | null {
  if (value === undefined || value === null) return null;
  if (!Array.isArray(value)) throw invalidInput(`${name} must be an array`, { [name]: value });
  for (const v of value) {
    if (typeof v !== 'string' || !(allowed as readonly string[]).includes(v)) {
      throw invalidInput(`Unknown ${name} value`, { [name]: v, allowed });
    }
  }
  return value.length === 0 ? null : new Set(value as V[]);
}

function listSort(value: unknown): DRepSort {
  if (value === undefined || value === null) return 'random';
  if (typeof value !== 'string' || !(ALL_SORTS as readonly string[]).includes(value)) {
    throw invalidInput('Unknown DRep sort', { sort: value, allowed: ALL_SORTS });
  }
  if (!(DREP_SORTS as readonly string[]).includes(value)) throw unsupported(`DRep sort '${value}'`);
  return value as DRepSort;
}

/**
 * Search is one term with no mode. Only `exactId` is supported, so the term
 * matches when it decodes as a CIP-129 DRep id and never otherwise; a CIP-105
 * id is simply not a match.
 */
function toSearch(value: unknown): string | null | 'none' {
  if (value === undefined || value === null) return null;
  if (typeof value !== 'string') throw invalidInput('search must be a string', { search: value });
  const term = value.trim();
  if (term === '') return null;
  try {
    const { hash, isScript } = decodeDRepId(term);
    return encodeDRepId(hash, isScript);
  } catch {
    return 'none';
  }
}

/** Uniform random sample without replacement (partial Fisher-Yates). */
function sample<T>(items: readonly T[], n: number): T[] {
  const copy = [...items];
  const k = Math.min(n, copy.length);
  for (let i = 0; i < k; i++) {
    const j = i + Math.floor(Math.random() * (copy.length - i));
    [copy[i], copy[j]] = [copy[j]!, copy[i]!];
  }
  return copy.slice(0, k);
}

/* ------------------------------------------------------------------------- */
/* Vote rows: voted and votable, the db-sync provider's definition             */
/* ------------------------------------------------------------------------- */

type Proposal = Pick<
  ProposalRow,
  'proposal_id' | 'proposal_tx_hash' | 'proposal_index' | 'proposal_type' | 'block_time' | 'proposed_epoch' | 'ratified_epoch' | 'dropped_epoch' | 'expired_epoch'
>;

interface VoteWindow {
  startEpoch: number;
  endEpoch: number;
  bootstrapEnd: number;
}

interface ListedVote {
  proposal: Proposal;
  vote?: VoteListRow;
}

/**
 * Every action the DRep voted on, plus every action votable while it was
 * registered: submitted by the window's end and still open at its start (the
 * last votable epoch is the one before ratification or drop, or the expiry
 * epoch). During bootstrap (protocol < 10) DReps could vote only on
 * InfoActions, so any other action counts only if it was open after it.
 */
export function voteRows(proposals: readonly Proposal[], votes: readonly VoteListRow[], w: VoteWindow): ListedVote[] {
  const byProposal = new Map(votes.map((v) => [v.proposal_id, v]));
  const rows: ListedVote[] = [];
  for (const p of proposals) {
    const vote = byProposal.get(p.proposal_id);
    const last =
      p.ratified_epoch !== null ? p.ratified_epoch - 1 : p.dropped_epoch !== null ? p.dropped_epoch - 1 : p.expired_epoch;
    const open = p.proposed_epoch <= w.endEpoch && (last === null || last >= w.startEpoch);
    const bootstrapOk =
      p.proposal_type === 'InfoAction' || (w.endEpoch >= w.bootstrapEnd && (last === null || last >= w.bootstrapEnd));
    if (vote || (open && bootstrapOk)) rows.push(vote ? { proposal: p, vote } : { proposal: p });
  }
  return rows;
}

export function createDRepsApi(ctx: Ctx): DRepsApi {
  /** The directory entry of a DRep that has registered; NOT_FOUND otherwise. Rejects CIP-105 and junk. */
  async function resolve(id: unknown): Promise<DirectoryEntry> {
    if (typeof id !== 'string') throw invalidInput('DRep id must be a string', { id });
    const { hash, isScript } = decodeDRepId(id.trim());
    const canonical = encodeDRepId(hash, isScript);
    const [entry] = await loadDirectory(ctx, { drep_id: `eq.${canonical}` });
    if (!entry) throw notFound('DRep not found', { id: canonical });
    return entry;
  }

  /** Assemble full DReps for a page. */
  async function hydrate(
    entries: readonly DirectoryEntry[],
    currentEpoch: number,
    loaded: { info?: Map<string, DRepInfoRow>; updates?: Map<string, DRepUpdateRow[]> } = {},
  ): Promise<DRep[]> {
    if (entries.length === 0) return [];
    const ids = entries.map((e) => e.id);
    const [info, updates] = await Promise.all([
      loaded.info && ids.every((id) => loaded.info!.has(id)) ? loaded.info : loadDRepInfo(ctx, { ids }).then((s) => s.dreps),
      loaded.updates ?? loadUpdates(ctx, ids),
    ]);
    return Promise.all(
      entries.map(async (entry) => {
        const row = info.get(entry.id);
        if (!row) throw internal('Koios has no drep_info for a listed DRep', { id: entry.id });
        return toDRep(entry, row, updates.get(entry.id), currentEpoch);
      }),
    );
  }

  async function toDRep(entry: DirectoryEntry, row: DRepInfoRow, certs: DRepUpdateRow[] | undefined, currentEpoch: number): Promise<DRep> {
    const facts = certFacts(entry.id, certs);
    const status = drepStatusOf(row, currentEpoch);
    if ((status === 'retired') !== (facts.retirement !== null)) {
      throw internal('Koios drep_info and drep_updates disagree on whether a DRep is retired', { id: entry.id });
    }
    const [latest, latestUpdate, retiredAt] = await Promise.all([
      toRegistrationEvent(ctx, facts.latest),
      facts.latestUpdate ? toRegistrationEvent(ctx, facts.latestUpdate) : Promise.resolve(null),
      facts.retirement ? ctx.chain.stampAt(facts.retirement.block_time) : Promise.resolve(null),
    ]);
    // `/drep_info.amount` is the current epoch's DRep distribution, 0 when the
    // DRep is not in it. A DRep registered this epoch has no snapshot yet.
    const votingPower =
      latest.at.epoch >= currentEpoch || row.amount === null
        ? null
        : { amount: toLovelace(row.amount), basis: 'active' as const, epoch: currentEpoch };
    return {
      role: 'drep',
      id: entry.id,
      isScriptBased: entry.isScript,
      // `kind` is derived from the anchor, never independently.
      kind: facts.anchor === null ? 'anonymous' : 'drep',
      anchor: facts.anchor,
      registration: { latest, latestUpdate, retiredAt: status === 'retired' ? retiredAt : null },
      status,
      ...(status !== 'retired' && row.expires_epoch_no !== null ? { expiryEpoch: row.expires_epoch_no } : {}),
      votingPower,
      ...(row.live_delegator_count === null ? {} : { delegatorCount: row.live_delegator_count }),
    };
  }

  /** The DRep's vote listing rows, newest or oldest first. */
  async function listedVotes(entry: DirectoryEntry, order: VoteSort): Promise<ListedVote[]> {
    const [updates, proposals, bootstrap, votes, tip] = await Promise.all([
      loadUpdates(ctx, [entry.id]),
      ctx.http.getAll<Proposal>('proposal_list', {}, {
        select: 'proposal_id,proposal_tx_hash,proposal_index,proposal_type,block_time,proposed_epoch,ratified_epoch,dropped_epoch,expired_epoch',
        order: 'block_time.asc,proposal_tx_hash.asc,proposal_index.asc',
      }),
      ctx.http.get<{ epoch_no: number }>('epoch_params', { protocol_major: 'gte.10' }, { select: 'epoch_no', order: 'epoch_no.asc', limit: 1 }),
      readVotes(ctx, { voter_id: `eq.${entry.id}`, voter_role: 'eq.DRep' }),
      ctx.chain.tip(),
    ]);
    const facts = certFacts(entry.id, updates.get(entry.id));
    const clock = await ctx.chain.clock();
    const epochOf = (t: number) => clock.anchorEpoch + Math.floor((t - clock.anchorStart) / clock.epochSeconds);
    const window: VoteWindow = {
      startEpoch: epochOf(facts.first.block_time),
      endEpoch: facts.retirement ? epochOf(facts.retirement.block_time) : tip.epoch_no,
      bootstrapEnd: bootstrap.rows[0]?.epoch_no ?? Number.MAX_SAFE_INTEGER,
    };
    const rows = voteRows(proposals, votes, window);
    const key = (r: ListedVote) => r.vote?.block_time ?? r.proposal.block_time;
    const tie = (a: ListedVote, b: ListedVote) =>
      a.proposal.block_time - b.proposal.block_time ||
      (a.proposal.proposal_tx_hash < b.proposal.proposal_tx_hash ? -1 : a.proposal.proposal_tx_hash > b.proposal.proposal_tx_hash ? 1 : 0) ||
      a.proposal.proposal_index - b.proposal.proposal_index;
    rows.sort((a, b) => key(a) - key(b) || tie(a, b));
    if (order === 'newest') rows.reverse();
    return rows;
  }

  function toVoteRow(r: ListedVote): DRepVoteRow {
    const ref = refOf(r.proposal.proposal_id, r.proposal.proposal_tx_hash, r.proposal.proposal_index);
    const action = { id: ref.id, type: toGovActionType(r.proposal.proposal_type) };
    if (!r.vote) return { voted: false, action };
    const at = {
      epoch: r.vote.epoch_no,
      ...(r.vote.block_height === null ? {} : { block: r.vote.block_height }),
      time: new Date(r.vote.block_time * 1000).toISOString().replace('.000Z', 'Z'),
    };
    return {
      voted: true,
      action,
      choice: toChoice(r.vote.vote),
      anchor: toAnchor(r.vote.meta_url, r.vote.meta_hash),
      txRef: { txHash: r.vote.vote_tx_hash, ...(at.block === undefined ? {} : { block: at.block }) },
      at,
    };
  }

  return {
    list: async (q: DRepListQuery) => {
      if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
      const sort = listSort(q.sort);
      const window = toWindow(q);
      if (sort === 'random' && q.page !== 1) {
        throw invalidInput('A randomly ordered DRep read is not paged: page must be 1', { page: q.page });
      }
      const status = enumFilter('status', q.status, STATUSES);
      const kind = enumFilter('kind', q.kind, KINDS);
      const search = toSearch(q.search);
      if (search === 'none') return ctx.paged({ elements: [], total: 0 });

      // Push down what /drep_list can filter: the id, and retired vs not.
      const filter: Record<string, string> = {};
      if (search) filter['drep_id'] = `eq.${search}`;
      if (status && !status.has('retired')) filter['registered'] = 'eq.true';
      else if (status && status.size === 1) filter['registered'] = 'eq.false';

      const [tip, directory] = await Promise.all([ctx.chain.tip(), loadDirectory(ctx, filter)]);
      const epoch = tip.epoch_no;
      const splitActive = status !== null && status.has('active') !== status.has('inactive');
      const needInfo = splitActive || sort === 'votingPower';
      const needCerts = (kind !== null && kind.size === 1) || sort === 'registrationDate';

      // For the retired side of a split, the registered flag decides; only registered DReps need info.
      const infoIds = directory.filter((e) => sort === 'votingPower' || e.registered).map((e) => e.id);
      const [info, updates] = await Promise.all([
        needInfo ? loadDRepInfo(ctx, { ids: infoIds }).then((s) => s.dreps) : Promise.resolve(undefined),
        needCerts ? loadUpdates(ctx, directory.length > 1 ? 'all' : directory.map((e) => e.id)) : Promise.resolve(undefined),
      ]);

      let candidates = directory;
      if (splitActive) {
        candidates = candidates.filter((e) => {
          if (!e.registered) return status!.has('retired');
          const row = info!.get(e.id);
          if (!row) throw internal('Koios has no drep_info for a listed DRep', { id: e.id });
          return status!.has(drepStatusOf(row, epoch));
        });
      }
      if (kind !== null && kind.size === 1) {
        candidates = candidates.filter((e) => kind.has(certFacts(e.id, updates!.get(e.id)).anchor === null ? 'anonymous' : 'drep'));
      }

      let ordered: DirectoryEntry[];
      if (sort === 'random') {
        ordered = sample(candidates, window.limit);
      } else if (sort === 'votingPower') {
        // Largest first; no figure last; the credential breaks ties.
        const power = (e: DirectoryEntry) => {
          const a = info!.get(e.id)?.amount;
          return a === null || a === undefined ? -1n : BigInt(toLovelace(a));
        };
        ordered = [...candidates].sort((a, b) => {
          const d = power(b) - power(a);
          return d > 0n ? 1 : d < 0n ? -1 : a.id < b.id ? -1 : a.id > b.id ? 1 : 0;
        });
      } else {
        // registrationDate: newest registration certificate first.
        const latest = new Map(candidates.map((e) => [e.id, certFacts(e.id, updates!.get(e.id)).latest]));
        ordered = [...candidates].sort((a, b) => certOrder(latest.get(b.id)!, latest.get(a.id)!) || (a.id < b.id ? -1 : 1));
      }

      const pageEntries = sort === 'random' ? ordered : ordered.slice(window.offset, window.offset + window.limit);
      const elements = await hydrate(pageEntries, epoch, {
        ...(info ? { info } : {}),
        ...(updates ? { updates } : {}),
      });
      return ctx.paged({ elements, total: candidates.length });
    },

    get: async (id: string) => {
      const entry = await resolve(id);
      const [tip, rows] = await Promise.all([ctx.chain.tip(), listedVotes(entry, 'newest')]);
      const [drep] = await hydrate([entry], tip.epoch_no);
      if (!drep) throw notFound('DRep not found', { id: entry.id });
      return ctx.envelope({ ...drep, activity: { voted: rows.filter((r) => r.vote).length, votable: rows.length } });
    },

    listVotes: async (id, q) => {
      if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
      const window = toWindow(q);
      const order = q.sort ?? 'newest';
      if (!(DREP_VOTE_SORTS as readonly string[]).includes(order)) {
        throw invalidInput('Unknown vote sort', { sort: order, allowed: DREP_VOTE_SORTS });
      }
      if (q.voted !== undefined && q.voted !== null && typeof q.voted !== 'boolean') {
        throw invalidInput('voted must be a boolean', { voted: q.voted });
      }
      const entry = await resolve(id);
      const rows = (await listedVotes(entry, order)).filter((r) => typeof q.voted !== 'boolean' || (r.vote !== undefined) === q.voted);
      const page = slicePage(rows, window);
      return ctx.paged({ elements: page.elements.map(toVoteRow), total: page.total! });
    },

    listUpdateHistory: async (id, q) => {
      if (typeof q !== 'object' || q === null) throw invalidInput('A page request { page, size } is required');
      const window = toWindow(q);
      const order = q.sort ?? 'desc';
      if (order !== 'asc' && order !== 'desc') throw invalidInput('sort must be asc or desc', { sort: order });
      const entry = await resolve(id);
      const certs = ((await loadUpdates(ctx, [entry.id])).get(entry.id) ?? []).filter((c) => c.action !== 'deregistered');
      if (order === 'desc') certs.reverse();
      const page = slicePage(certs, window);
      const elements: RegistrationEvent[] = await Promise.all(page.elements.map((c) => toRegistrationEvent(ctx, c)));
      return ctx.paged({ elements, total: page.total! });
    },

    getCounts: async () => {
      const [tip, set] = await Promise.all([ctx.chain.tip(), loadDRepInfo(ctx)]);
      let active = 0;
      let inactive = 0;
      let anonymous = 0;
      for (const row of set.dreps.values()) {
        const s = drepStatusOf(row, tip.epoch_no);
        if (s === 'active') active++;
        else if (s === 'inactive') inactive++;
        // For a registered DRep, /drep_info's anchor is its newest registration or update certificate's.
        if (s !== 'retired' && toAnchor(row.meta_url, row.meta_hash) === null) anonymous++;
      }
      return ctx.envelope({ totalRegistered: active + inactive, totalActive: active, totalInactive: inactive, anonymous });
    },
  };
}
