/**
 * `governance.dreps` over Blockfrost (SPEC.md §5.3).
 *
 * The directory is read whole (./dreps/directory.ts, 18 requests on mainnet)
 * and filtered, sorted, counted and paged in memory, so `total` is exact and
 * no page is short except the last. Each DReps on the returned page then
 * costs its registration: `/updates` plus one dated `/txs` read per
 * certificate reported (./dreps/registration.ts).
 *
 * Sorts: `votingPower` (the directory's `amount`, the active distribution)
 * and `random`. Declined: `registrationDate` — ordering by the latest
 * registration needs every DRep's certificates, about 3,400 requests on
 * mainnet for any one page; `activity` — it needs every DRep's full vote
 * listing.
 *
 * Omitted, on purpose:
 *   listDelegators   `/delegators` gives each delegator's CURRENT balance
 *                    (it equals `/accounts/{stake}.controlled_amount`), not
 *                    the stake the ledger counted at the epoch boundary. The
 *                    contract's row requires the active figure (D32), so the
 *                    method is absent rather than filled with the live one.
 *   liveVotingPower, activity, delegatorCount, expiryEpoch, retiredAt
 *                    not in Blockfrost's DRep rows; each would be extra reads
 *                    per DRep (or, for expiryEpoch, a reconstruction).
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

import { drepCertAnchorOf } from '../cbor';
import { loadTxCbor, txStamp } from '../chain';
import type { Ctx, Session } from '../context';
import { invalidInput, notFound, unsupported } from '../errors';
import { decodeDRepId, encodeDRepId } from '../ids';
import { toLovelace } from '../numbers';
import { slicePage, toWindow } from '../paging';
import { toDirectoryDRep, loadDirectory, type BfDRepRow, type DirectoryDRep } from './dreps/directory';
import { indexCertificates, loadRegistration, loadUpdates } from './dreps/registration';
import { loadDRepVotable } from './dreps/votes';
import { procedureOf, toVoter } from './proposals/votes';

/** What `list` honours. The capability declaration is assembled from these. */
export const DREP_SORTS: DRepSort[] = ['votingPower', 'random'];
export const DREP_FILTERS: DRepFilter[] = ['status', 'kind'];
/** `exactId` is always supported. Name-based search needs metadata: the index provider's job. */
export const DREP_SEARCH: SearchMode[] = ['exactId'];
/**
 * Vote-listing orders `listVotes` honours. The listing is ordered by ACTION
 * (newest = most recently submitted action first), not by when the vote was
 * cast: Blockfrost does not date a vote without one more read per vote.
 */
export const DREP_VOTE_SORTS: VoteSort[] = ['newest', 'oldest'];

const STATUSES: readonly DRepStatus[] = ['active', 'inactive', 'retired'];
const KINDS: readonly DRepKind[] = ['drep', 'anonymous'];
const ALL_SORTS: readonly DRepSort[] = ['votingPower', 'registrationDate', 'activity', 'random'];

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

function listSort(value: unknown): DRepSort {
  if (value === undefined || value === null) return 'random';
  if (typeof value !== 'string' || !(ALL_SORTS as readonly string[]).includes(value)) {
    throw invalidInput('Unknown DRep sort', { sort: value, allowed: ALL_SORTS });
  }
  if (!(DREP_SORTS as readonly string[]).includes(value)) throw unsupported(`DRep sort '${value}'`);
  return value as DRepSort;
}

/**
 * Search is one term with no mode (D108). Only `exactId` is supported, so the
 * term matches when it decodes as a CIP-129 DRep id and never otherwise. Any
 * other input, a CIP-105 id included, is simply not a match.
 */
function searchId(value: unknown): string | null | 'none' {
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

/** Uniform shuffle; the directory's default order must not become a rich list. */
function shuffle<T>(items: readonly T[]): T[] {
  const out = [...items];
  for (let i = out.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [out[i], out[j]] = [out[j]!, out[i]!];
  }
  return out;
}

const byPower = (a: DirectoryDRep, b: DirectoryDRep) => {
  const x = BigInt(a.amount);
  const y = BigInt(b.amount);
  return x === y ? (a.id < b.id ? -1 : a.id > b.id ? 1 : 0) : x > y ? -1 : 1;
};

/** One DRep read on its own: the detail row plus its anchor. NOT_FOUND unless it has registered. */
async function loadOne(s: Session, id: string): Promise<DirectoryDRep> {
  const [row, meta] = await Promise.all([
    s.http.getOrNull<BfDRepRow>(`/governance/dreps/${id}`),
    s.http.getOrNull<{ url: string; hash: string }>(`/governance/dreps/${id}/metadata`),
  ]);
  const drep = row ? toDirectoryDRep(row, meta) : undefined;
  if (!drep || drep.id !== id) throw notFound('DRep not found', { id });
  return drep;
}

function requireId(id: unknown): string {
  if (typeof id !== 'string') throw invalidInput('DRep id must be a string', { id });
  const { hash, isScript } = decodeDRepId(id.trim());
  return encodeDRepId(hash, isScript);
}

export function createDRepsApi(ctx: Ctx): DRepsApi {
  async function toDRep(s: Session, d: DirectoryDRep): Promise<DRep> {
    const registration = await loadRegistration(s, d.id);
    return {
      role: 'drep',
      id: d.id,
      isScriptBased: d.isScript,
      // `kind` is derived from the anchor, never independently.
      kind: d.anchor === null ? 'anonymous' : 'drep',
      anchor: d.anchor,
      registration,
      status: d.status,
      votingPower: { amount: d.amount, basis: 'active' },
    };
  }

  return {
    list: async (q: DRepListQuery) => {
      const window = toWindow(q);
      const sort = listSort(q.sort);
      if (sort === 'random' && q.page !== 1) {
        throw invalidInput('A randomly ordered DRep read is not paged: page must be 1', { page: q.page });
      }
      const status = enumFilter('status', q.status, STATUSES);
      const kind = enumFilter('kind', q.kind, KINDS);
      const search = searchId(q.search);
      if (search === 'none') return ctx.paged({ elements: [], total: 0 });

      const s = ctx.session();
      let set: DirectoryDRep[];
      if (search !== null) {
        try {
          set = [await loadOne(s, search)];
        } catch (error) {
          if ((error as { code?: string }).code === 'NOT_FOUND') set = [];
          else throw error;
        }
      } else {
        set = (await loadDirectory(s)).dreps;
      }
      if (status) set = set.filter((d) => status.includes(d.status));
      if (kind) set = set.filter((d) => kind.includes(d.anchor === null ? 'anonymous' : 'drep'));
      const ordered = sort === 'random' ? shuffle(set) : [...set].sort(byPower);
      const page = slicePage(ordered, window);
      return ctx.paged({ elements: await Promise.all(page.elements.map((d) => toDRep(s, d))), total: page.total! });
    },

    get: async (id: string) => {
      const s = ctx.session();
      return ctx.envelope(await toDRep(s, await loadOne(s, requireId(id))));
    },

    listVotes: async (id, q) => {
      const window = toWindow(q);
      const order = q.sort ?? 'newest';
      if (!(DREP_VOTE_SORTS as readonly string[]).includes(order)) {
        throw invalidInput('Unknown vote sort', { sort: order, allowed: DREP_VOTE_SORTS });
      }
      if (q.voted !== undefined && q.voted !== null && typeof q.voted !== 'boolean') {
        throw invalidInput('voted must be a boolean', { voted: q.voted });
      }
      const s = ctx.session();
      const drep = await loadOne(s, requireId(id));
      let rows = await loadDRepVotable(s, drep.id);
      if (typeof q.voted === 'boolean') rows = rows.filter((r) => (r.vote !== undefined) === q.voted);
      if (order === 'newest') rows = [...rows].reverse();
      const page = slicePage(rows, window);
      const voter = toVoter('drep', { hash: drep.hash, isScript: drep.isScript });
      const elements = await Promise.all(
        page.elements.map(async (r): Promise<DRepVoteRow> => {
          const action = { id: r.action.id, type: r.action.type };
          if (!r.vote) return { voted: false, action };
          const procedure = await procedureOf(s, { voter, txHash: r.vote.txHash }, r.action);
          return {
            voted: true,
            action,
            choice: r.vote.choice,
            anchor: procedure.anchor,
            txRef: { txHash: r.vote.txHash, index: r.vote.certIndex },
          };
        }),
      );
      return ctx.paged({ elements, total: page.total! });
    },

    /** Every registration and update certificate (each sets the anchor), dated, with the anchor from its transaction. */
    listUpdateHistory: async (id, q) => {
      const window = toWindow(q);
      const order = q.sort ?? 'desc';
      if (order !== 'asc' && order !== 'desc') throw invalidInput('sort must be asc or desc', { sort: order });
      const s = ctx.session();
      const drep = await loadOne(s, requireId(id));
      const updates = await loadUpdates(s, drep.id);
      indexCertificates(updates, drep.id);
      let certs = updates.filter((u) => u.action === 'registered' || u.action === 'updated');
      if (order === 'desc') certs = [...certs].reverse();
      const page = slicePage(certs, window);
      const elements = await Promise.all(
        page.elements.map(async (u): Promise<RegistrationEvent> => {
          const [at, cbor] = await Promise.all([txStamp(s, u.tx_hash), loadTxCbor(s, u.tx_hash)]);
          return {
            txRef: { txHash: u.tx_hash, index: u.cert_index, ...(at.block === undefined ? {} : { block: at.block }) },
            at,
            anchor: drepCertAnchorOf(cbor, u.cert_index),
            deposit: u.deposit === null ? null : toLovelace(u.deposit, 'DRep deposit'),
          };
        }),
      );
      return ctx.paged({ elements, total: page.total! });
    },

    getCounts: async () => {
      const { dreps } = await loadDirectory(ctx.session());
      const live = dreps.filter((d) => d.status !== 'retired');
      return ctx.envelope({
        totalRegistered: live.length,
        totalActive: live.filter((d) => d.status === 'active').length,
        totalInactive: live.filter((d) => d.status === 'inactive').length,
        anonymous: live.filter((d) => d.anchor === null).length,
      });
    },
  };
}

