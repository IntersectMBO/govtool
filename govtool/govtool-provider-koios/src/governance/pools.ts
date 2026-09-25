/**
 * `/governance/pools` over Koios: stake pools as governance voters (SPEC.md
 * §5.5).
 *
 * Ticker and name are pool METADATA, resolved by the metadata service, so they
 * are neither served nor searched here even though Koios returns them. Search
 * is `exactId` only: a `pool1…` id.
 *
 * The listing covers registered pools (including ones with a retirement still
 * pending), ordered as the db-sync provider orders them: voting power at the
 * current epoch, largest first, pools with none last, then by hash. That order
 * needs the whole distribution, so a listing reads `/pool_list` and
 * `/pool_voting_power_history` for the epoch in full (a few thousand rows,
 * 1000 per request). `get` also answers for a retired pool.
 */
import type { Anchor, PageRequest, PoolsApi, SpoVoter, VoteRecord, VotedActionSummary } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, invalidInput, notFound } from '../errors';
import { decodePoolId, encodePoolId } from '../ids';
import { stripBytea, toLovelace } from '../numbers';
import { slicePage, toWindow } from '../paging';
import type { PoolInfoRow, PoolListRow, VotingPowerHistoryRow } from '../rows';
import { toAnchor } from './dreps/directory';
import { toGovActionType } from './proposals/body';
import { refOf } from './proposals/rows';
import { readVotes, toVoteRecord } from './proposals/voters';

/**
 * One pool's vote, with the action it was cast on. `VoteRecord` is shaped for
 * a per-action listing and does not name the action; a per-pool history is
 * unreadable without it, so it travels as an extra field (SPEC.md §3.3).
 */
export interface PoolVoteRecord extends VoteRecord {
  action: VotedActionSummary;
}

const POOL_SELECT = 'pool_id_bech32,pool_id_hex,pledge,meta_url,meta_hash,pool_status,active_stake';

function canonicalPool(row: { pool_id_bech32: string; pool_id_hex?: string | null }): { id: string; hash: string } {
  const hash = row.pool_id_hex ? stripBytea(row.pool_id_hex) : decodePoolId(row.pool_id_bech32);
  const id = encodePoolId(hash);
  if (id !== row.pool_id_bech32) throw internal('Koios sent a pool id that disagrees with its hash', { pool: row.pool_id_bech32 });
  return { id, hash };
}

function toSpoVoter(row: PoolListRow | PoolInfoRow, power: string | null | undefined, epoch: number): SpoVoter {
  const { id } = canonicalPool(row);
  const anchor: Anchor | null = toAnchor(row.meta_url, row.meta_hash);
  const live = 'live_stake' in row ? row.live_stake : null;
  return {
    role: 'spo',
    id,
    poolId: id,
    isScriptBased: false,
    anchor,
    // Absent from the epoch's distribution (not delegated to, newly registered,
    // or retired) means no voting power in it.
    votingPower: power === null || power === undefined ? null : { amount: toLovelace(power), basis: 'active', epoch },
    ...(row.active_stake === null ? {} : { activeStake: toLovelace(row.active_stake) }),
    ...(live === null || live === undefined ? {} : { liveStake: toLovelace(live) }),
    ...(row.pledge === null ? {} : { pledge: toLovelace(row.pledge) }),
  };
}

/** Accept only a `pool1…` id as a search term; anything else matches nothing. */
function searchId(search: unknown): string | null | undefined {
  if (search === undefined || search === null) return null;
  if (typeof search !== 'string') return undefined;
  const term = search.trim();
  if (!term) return null;
  try {
    return encodePoolId(decodePoolId(term));
  } catch {
    return undefined;
  }
}

export function createPoolsApi(ctx: Ctx) {
  const api = {
    list: async (q: PageRequest & { search?: string }) => {
      const window = toWindow(q);
      const id = searchId(q.search);
      if (id === undefined) return ctx.paged<SpoVoter>({ elements: [], total: 0 });
      const tip = await ctx.chain.tip();
      const filter: Record<string, string> = { pool_status: 'neq.retired', ...(id ? { pool_id_bech32: `eq.${id}` } : {}) };
      const [pools, power] = await Promise.all([
        ctx.http.getAll<PoolListRow>('pool_list', filter, { select: POOL_SELECT, order: 'pool_id_hex.asc' }),
        ctx.http.getAll<VotingPowerHistoryRow>(
          'pool_voting_power_history',
          { _epoch_no: tip.epoch_no, ...(id ? { pool_id_bech32: `eq.${id}` } : {}) },
          { select: 'pool_id_bech32,amount' },
        ),
      ]);
      const byPool = new Map(power.map((p) => [p.pool_id_bech32!, p.amount]));
      const amount = (row: PoolListRow) => {
        const a = byPool.get(row.pool_id_bech32);
        return a === null || a === undefined ? -1n : BigInt(toLovelace(a));
      };
      const ordered = [...pools].sort((a, b) => {
        const d = amount(b) - amount(a);
        return d > 0n ? 1 : d < 0n ? -1 : a.pool_id_hex < b.pool_id_hex ? -1 : a.pool_id_hex > b.pool_id_hex ? 1 : 0;
      });
      const page = slicePage(ordered, window);
      return ctx.paged({
        elements: page.elements.map((row) => toSpoVoter(row, byPool.get(row.pool_id_bech32), tip.epoch_no)),
        total: page.total!,
      });
    },

    get: async (id: string) => {
      if (typeof id !== 'string') throw invalidInput('pool id must be a pool1 string');
      const canonical = encodePoolId(decodePoolId(id.trim()));
      const [tip, { rows }] = await Promise.all([
        ctx.chain.tip(),
        ctx.http.post<PoolInfoRow>('pool_info', { _pool_bech32_ids: [canonical] }),
      ]);
      const row = rows.find((r) => r.pool_id_bech32 === canonical);
      if (!row) throw notFound('Unknown pool', { id: canonical });
      // `/pool_info.voting_power` is the current epoch's SPO distribution.
      return ctx.envelope(toSpoVoter(row, row.voting_power, tip.epoch_no));
    },

    listVotes: async (id: string, q: PageRequest) => {
      const window = toWindow(q);
      const canonical = encodePoolId(decodePoolId(typeof id === 'string' ? id.trim() : ''));
      const [{ rows: known }, votes] = await Promise.all([
        ctx.http.get<{ pool_id_bech32: string }>('pool_list', { pool_id_bech32: `eq.${canonical}` }, { select: 'pool_id_bech32' }),
        readVotes(ctx, { voter_id: `eq.${canonical}`, voter_role: 'eq.SPO' }),
      ]);
      if (known.length === 0) throw notFound('Unknown pool', { id: canonical });
      const page = slicePage(votes, window);
      const elements: PoolVoteRecord[] = page.elements.map((v) => {
        const ref = refOf(v.proposal_id, v.proposal_tx_hash, v.proposal_index);
        return { ...toVoteRecord(v), action: { id: ref.id, type: toGovActionType(v.proposal_type) } };
      });
      return ctx.paged({ elements, total: page.total! });
    },
  } satisfies PoolsApi;
  return api;
}
