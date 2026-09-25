import type {
  Anchor,
  GovActionType,
  PageRequest,
  PoolsApi,
  SpoVoter,
  VoteChoice,
  VoteRecord,
  VotedActionSummary,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, notFound, unsupported } from '../errors';
import { decodePoolId, encodeGovActionId, encodePoolId } from '../ids';
import { toPage, toWindow } from '../paging';
import { toInt, toIso, toLovelace } from '../numbers';
import { POOL_ID_SQL, POOL_VOTES_SQL, POOLS_SQL } from './pools/queries';

/**
 * `/governance/pools`: stake pools as governance voters (SPEC.md §5.5, D63,
 * D69, D107).
 *
 * Ticker and name are pool METADATA, anchored by the registration certificate
 * and resolved by the metadata service (D63, D94), so neither is served nor
 * searched here even though db-sync caches it in `off_chain_pool_data`.
 * Search is `exactId` only: a `pool1…` id. A term that is not one matches no
 * pool, which is the truthful answer for an id search.
 *
 * The listing covers registered pools (including ones with a retirement still
 * pending); `get` also answers for a retired pool, whose history is still
 * addressable.
 */

/**
 * One pool's vote, with the action it was cast on. `VoteRecord` is shaped for
 * a per-action listing and does not name the action; a per-pool history is
 * unreadable without it, so it travels as an extra field (SPEC.md §3.3).
 */
export interface PoolVoteRecord extends VoteRecord {
  action: VotedActionSummary;
}

interface PoolRow {
  hash: string;
  pledge: string;
  retired: boolean;
  meta_url: string | null;
  meta_hash: string | null;
  snap_epoch: number | null;
  voting_power: string | null;
  active_stake: string | null;
  active_known: boolean;
  total_count: string;
}

interface PoolVoteRow {
  vote: string;
  tx_hash: string;
  index: number;
  block_no: string | null;
  epoch_no: number;
  slot_no: string | null;
  time: Date | string;
  anchor_url: string | null;
  anchor_hash: string | null;
  action_tx_hash: string;
  action_index: number;
  action_type: string;
  total_count: string;
}

const CHOICES: Record<string, VoteChoice> = { Yes: 'yes', No: 'no', Abstain: 'abstain' };

/** db-sync `govactiontype` to the contract's names; they differ only for UpdateCommittee. */
const ACTION_TYPES: Record<string, GovActionType> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  NewCommittee: 'UpdateCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

function toSpoVoter(row: PoolRow): SpoVoter {
  if (row.snap_epoch === null) {
    // No stake snapshot at all: db-sync runs without ledger state. A null
    // voting power would read as "this pool has none", so refuse instead.
    throw unsupported('pool voting power (pool_stat is empty)');
  }
  const poolId = encodePoolId(row.hash);
  const anchor: Anchor | null =
    row.meta_url !== null && row.meta_hash !== null ? { url: row.meta_url, dataHash: row.meta_hash } : null;
  const activeStake =
    row.active_stake !== null ? toLovelace(row.active_stake) : row.active_known ? '0' : undefined;
  return {
    role: 'spo',
    id: poolId,
    poolId,
    isScriptBased: false,
    anchor,
    // Absent from the latest snapshot (not delegated to, newly registered, or
    // retired) means no voting power in it.
    votingPower:
      row.voting_power === null
        ? null
        : { amount: toLovelace(row.voting_power), basis: 'active', epoch: row.snap_epoch },
    ...(activeStake !== undefined ? { activeStake } : {}),
    pledge: toLovelace(row.pledge),
  };
}

function toVoteRecord(row: PoolVoteRow, poolId: string): PoolVoteRecord {
  const choice = CHOICES[row.vote];
  const type = ACTION_TYPES[row.action_type];
  if (!choice || !type) throw internal('Unrecognised vote or action type in db-sync', { vote: row.vote, type: row.action_type });
  const at = {
    epoch: row.epoch_no,
    ...(row.slot_no !== null ? { slot: toInt(row.slot_no) } : {}),
    ...(row.block_no !== null ? { block: toInt(row.block_no) } : {}),
    time: toIso(row.time),
  };
  return {
    voter: { role: 'spo', id: poolId, isScriptBased: false },
    choice,
    anchor: row.anchor_url !== null && row.anchor_hash !== null ? { url: row.anchor_url, dataHash: row.anchor_hash } : null,
    txRef: { txHash: row.tx_hash, index: row.index, ...(at.block !== undefined ? { block: at.block } : {}) },
    at,
    action: { id: encodeGovActionId(row.action_tx_hash, row.action_index), type },
  };
}

/** Accept only a `pool1…` id as a search term; anything else matches nothing. */
function searchHash(search: string | undefined): string | null | undefined {
  const term = search?.trim();
  if (!term) return null;
  try {
    return decodePoolId(term);
  } catch {
    return undefined;
  }
}

export function createPoolsApi(ctx: Ctx) {
  const queryPools = (hash: string | null, limit: number, offset: number, includeRetired: boolean) =>
    ctx.db.query<PoolRow>(POOLS_SQL, [hash, limit, offset, includeRetired]);

  const api = {
    list: async (q: PageRequest & { search?: string }) => {
      const { limit, offset } = toWindow(q);
      const hash = searchHash(q.search);
      if (hash === undefined) return ctx.paged<SpoVoter>({ elements: [], total: 0 });
      const rows = await queryPools(hash, limit, offset, false);
      let totalWhenEmpty = 0;
      if (rows.length === 0 && offset > 0) {
        const [first] = await queryPools(hash, 1, 0, false);
        totalWhenEmpty = first ? Number(first.total_count) : 0;
      }
      return ctx.paged(toPage(rows, toSpoVoter, totalWhenEmpty));
    },

    get: async (id: string) => {
      const hash = decodePoolId(id);
      const [row] = await queryPools(hash, 1, 0, true);
      if (!row) throw notFound('Unknown pool', { id });
      return ctx.envelope(toSpoVoter(row));
    },

    listVotes: async (id: string, q: PageRequest) => {
      const { limit, offset } = toWindow(q);
      const hash = decodePoolId(id);
      const [pool] = await ctx.db.query<{ id: string }>(POOL_ID_SQL, [hash]);
      if (!pool) throw notFound('Unknown pool', { id });
      const poolId = encodePoolId(hash);
      const rows = await ctx.db.query<PoolVoteRow>(POOL_VOTES_SQL, [pool.id, limit, offset]);
      let totalWhenEmpty = 0;
      if (rows.length === 0 && offset > 0) {
        const [first] = await ctx.db.query<PoolVoteRow>(POOL_VOTES_SQL, [pool.id, 1, 0]);
        totalWhenEmpty = first ? Number(first.total_count) : 0;
      }
      return ctx.paged(toPage(rows, (row) => toVoteRecord(row, poolId), totalWhenEmpty));
    },
  } satisfies PoolsApi;
  return api;
}
