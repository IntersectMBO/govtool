/**
 * `/governance/pools`: stake pools as governance voters (SPEC.md §5.5).
 *
 *   list  `/pools/extended`, paged straight through (100 a page). No total:
 *         counting means reading all ~30 pages, so it is left out and the
 *         end is signalled by the short last page, as SPEC.md §3.4 allows.
 *   get   `/pools/{id}` plus `/pools/{id}/metadata` for the anchor.
 *
 * Search is `exactId` only: a `pool1…` id. Ticker and name are pool METADATA,
 * resolved by the metadata service, so they are neither served nor searched.
 *
 * Omitted: listVotes. `/pools/{id}/votes` rows name the vote transaction but
 * not the action voted on, so both the action and the de-duplication of
 * re-votes need one CBOR read per vote across the pool's whole history.
 */
import type { Anchor, PageRequest, PoolsApi, SpoVoter } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, invalidInput, notFound } from '../errors';
import { decodePoolId, encodePoolId } from '../ids';
import { toLovelace } from '../numbers';
import { readWindow, toWindow } from '../paging';

interface BfPoolExtended {
  pool_id: string;
  hex: string;
  active_stake: string;
  live_stake: string;
  declared_pledge: string;
  metadata: { url: string | null; hash: string | null } | null;
}

interface BfPool {
  pool_id: string;
  hex: string;
  active_stake: string;
  live_stake: string;
  declared_pledge: string;
}

const anchorOf = (m: { url?: string | null; hash?: string | null } | null | undefined): Anchor | null =>
  m && m.url && m.hash ? { url: m.url, dataHash: m.hash.toLowerCase() } : null;

function toSpoVoter(row: BfPool, anchor: Anchor | null): SpoVoter {
  const hash = decodePoolId(row.pool_id);
  if (row.hex && row.hex.toLowerCase() !== hash) throw internal('Blockfrost pool id disagrees with its hex', { poolId: row.pool_id });
  const poolId = encodePoolId(hash);
  const active = toLovelace(row.active_stake, 'pool active_stake');
  return {
    role: 'spo',
    id: poolId,
    poolId,
    isScriptBased: false,
    anchor,
    // The epoch's active stake is the SPO voting distribution.
    votingPower: { amount: active, basis: 'active' },
    activeStake: active,
    liveStake: toLovelace(row.live_stake, 'pool live_stake'),
    pledge: toLovelace(row.declared_pledge, 'pool declared_pledge'),
  };
}

export function createPoolsApi(ctx: Ctx): PoolsApi {
  async function getOne(hash: string): Promise<SpoVoter> {
    const id = encodePoolId(hash);
    const [row, meta] = await Promise.all([
      ctx.http.getOrNull<BfPool>(`/pools/${id}`),
      ctx.http.getOrNull<{ url?: string | null; hash?: string | null }>(`/pools/${id}/metadata`),
    ]);
    if (!row) throw notFound('Unknown pool', { id });
    return toSpoVoter(row, anchorOf(meta));
  }

  return {
    list: async (q: PageRequest & { search?: string }) => {
      const window = toWindow(q);
      const term = typeof q.search === 'string' ? q.search.trim() : '';
      if (term) {
        let hash: string;
        try {
          hash = decodePoolId(term);
        } catch {
          return ctx.paged<SpoVoter>({ elements: [], total: 0 });
        }
        try {
          const pool = await getOne(hash);
          return ctx.paged({ elements: window.offset === 0 ? [pool] : [], total: 1 });
        } catch (error) {
          if ((error as { code?: string }).code === 'NOT_FOUND') return ctx.paged<SpoVoter>({ elements: [], total: 0 });
          throw error;
        }
      }
      const page = await readWindow<BfPoolExtended>(ctx.http, '/pools/extended', window);
      const elements = page.elements.map((row) => toSpoVoter(row, anchorOf(row.metadata)));
      return ctx.paged(page.total === undefined ? { elements } : { elements, total: page.total });
    },

    get: async (id: string) => {
      if (typeof id !== 'string') throw invalidInput('pool id must be a string', { id });
      return ctx.envelope(await getOne(decodePoolId(id.trim())));
    },
  };
}
