/**
 * AccountsApi over db-sync (SPEC.md §5.4).
 *
 * An account is looked up by its reward address bytes (`stake_address.hash_raw`,
 * header byte + 28-byte credential, unique btree). db-sync holds the whole
 * chain, so an address it has never seen is a KNOWN answer, not a missing one:
 * such a credential has never been registered and delegates to nobody. That is
 * why an unknown but well-formed address gets an explicit empty answer
 * (`isRegistered: false`, `null`, an empty page) rather than NOT_FOUND — the
 * connected wallet of a brand-new user is exactly this case, and "you are not
 * delegated" is the true statement for it (SPEC.md §3.2).
 *
 * `balance` is not served: db-sync records withdrawals without saying whether
 * they drew on staking or non-staking rewards, so `rewards` and `rewardsRest`
 * cannot be split correctly, and the contract forbids serving `total` alone.
 */
import type {
  Account,
  AccountsApi,
  Delegation,
  DelegationHistoryEvent,
  DelegationTarget,
  PoolDelegation,
  TxRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { internal, invalidInput } from './errors';
import { decodeStakeAddress, encodeDRepId, encodePoolId, encodeStakeAddress, type Credential } from './ids';
import { TIP_EPOCH_CTE, toStamp, type BlockCols } from './network/chain';
import { toInt, toLovelace } from './numbers';
import { toPage, toWindow } from './paging';

type DbNumber = number | string;

/* ------------------------------------------------------------------------- */
/* Address                                                                     */
/* ------------------------------------------------------------------------- */

export interface ResolvedAddress extends Credential {
  /** The canonical (lowercase, re-encoded) address. */
  stakeAddress: string;
  /** Hex of `stake_address.hash_raw`: header byte then credential hash. */
  hashRaw: string;
}

/** Decode and validate for this provider's network; INVALID_INPUT otherwise. */
export function resolveAddress(address: unknown, network: string): ResolvedAddress {
  if (typeof address !== 'string') throw invalidInput('stakeAddress must be a string', { stakeAddress: address });
  const credential = decodeStakeAddress(address.trim(), network);
  const header = (credential.isScript ? 0xf0 : 0xe0) | (network === 'mainnet' ? 1 : 0);
  return {
    ...credential,
    stakeAddress: encodeStakeAddress(credential.hash, credential.isScript, network),
    hashRaw: header.toString(16) + credential.hash,
  };
}

const SA_CTE = `sa AS (SELECT id FROM stake_address WHERE hash_raw = decode($1, 'hex'))`;

/* ------------------------------------------------------------------------- */
/* SQL                                                                         */
/* ------------------------------------------------------------------------- */

/**
 * Registered when the newest registration certificate is newer than the newest
 * deregistration, ordered by (tx, certificate index) so a deregister and
 * re-register in one transaction, or in one epoch, resolve correctly. Both
 * tables are indexed on `addr_id`.
 */
export const ACCOUNT_SQL = `WITH ${SA_CTE}
SELECT sa.id,
       (SELECT e.kind FROM (
          SELECT 'reg' AS kind, tx_id, cert_index FROM stake_registration WHERE addr_id = sa.id
          UNION ALL
          SELECT 'dereg' AS kind, tx_id, cert_index FROM stake_deregistration WHERE addr_id = sa.id
        ) e ORDER BY e.tx_id DESC, e.cert_index DESC LIMIT 1) AS last_event
  FROM sa`;

/** A certificate for this account newer than the delegation `x` voids it. */
const DEREGISTERED_AFTER = (x: string) => `EXISTS (
         SELECT 1 FROM stake_deregistration sd
          WHERE sd.addr_id = (SELECT id FROM sa) AND (sd.tx_id, sd.cert_index) > (${x}.tx_id, ${x}.cert_index)
       )`;

/**
 * The newest vote delegation, and whether it still stands.
 *
 * It does not stand if the account deregistered afterwards, or if the DRep
 * retired afterwards: from protocol 10 the ledger clears a retiring DRep's
 * delegations, and the protocol-10 hard fork cleared delegations to DReps that
 * were unregistered at that moment. Under protocol 9 a retirement left the
 * delegation dangling, and it came back to life if the DRep re-registered
 * before the hard fork — the inner NOT EXISTS keeps exactly that case.
 *
 * PERFORMANCE: `delegation_vote` has no index on `addr_id` in db-sync 13.x, so
 * this is a sequential scan of it (~90k rows on preview, ~13 ms server-side).
 * Mainnet's is many times larger, so expect a few hundred ms there; an index on
 * `delegation_vote (addr_id)` makes it a point lookup. `drep_registration` is
 * likewise scanned by `drep_hash_id` (~23k rows on preview, ~3 ms).
 */
export const DELEGATION_SQL = `WITH ${SA_CTE},
dv AS (
  SELECT tx_id, cert_index, drep_hash_id FROM delegation_vote
   WHERE addr_id = (SELECT id FROM sa) ORDER BY tx_id DESC, cert_index DESC LIMIT 1
)
SELECT encode(h.raw, 'hex') AS drep_hash, h.view AS drep_view, h.has_script,
       encode(tx.hash, 'hex') AS tx_hash, dv.cert_index,
       b.epoch_no, b.slot_no, b.block_no, b.time,
       ${DEREGISTERED_AFTER('dv')} AS account_deregistered,
       EXISTS (
         SELECT 1 FROM drep_registration r
           JOIN tx rt ON rt.id = r.tx_id JOIN block rb ON rb.id = rt.block_id
          WHERE r.drep_hash_id = dv.drep_hash_id AND r.deposit < 0
            AND (r.tx_id, r.cert_index) > (dv.tx_id, dv.cert_index)
            AND (rb.proto_major >= 10 OR NOT EXISTS (
              SELECT 1 FROM drep_registration g
                JOIN tx gt ON gt.id = g.tx_id JOIN block gb ON gb.id = gt.block_id
               WHERE g.drep_hash_id = r.drep_hash_id AND g.deposit > 0
                 AND (g.tx_id, g.cert_index) > (r.tx_id, r.cert_index) AND gb.proto_major < 10))
       ) AS drep_retired
  FROM dv
  JOIN drep_hash h ON h.id = dv.drep_hash_id
  JOIN tx ON tx.id = dv.tx_id
  JOIN block b ON b.id = tx.block_id`;

/**
 * The newest pool delegation, and whether it still stands: not if the account
 * deregistered afterwards, and not if the pool retired after it (POOLREAP
 * removes delegations to a retiring pool). A retirement counts when its epoch
 * has been reached and it was not cancelled by a re-registration or a newer
 * retirement announced before that epoch. All tables involved are indexed on
 * the columns used (`delegation.addr_id`, `pool_retire.hash_id`,
 * `pool_update.hash_id`).
 */
export const POOL_DELEGATION_SQL = `WITH ${TIP_EPOCH_CTE}, ${SA_CTE},
d AS (
  SELECT tx_id, cert_index, pool_hash_id FROM delegation
   WHERE addr_id = (SELECT id FROM sa) ORDER BY tx_id DESC, cert_index DESC LIMIT 1
)
SELECT encode(ph.hash_raw, 'hex') AS pool_hash,
       encode(tx.hash, 'hex') AS tx_hash, d.cert_index,
       b.epoch_no, b.slot_no, b.block_no, b.time,
       ${DEREGISTERED_AFTER('d')} AS account_deregistered,
       EXISTS (
         SELECT 1 FROM pool_retire pr
          WHERE pr.hash_id = d.pool_hash_id
            AND pr.retiring_epoch <= (SELECT epoch_no FROM tip)
            AND pr.retiring_epoch > b.epoch_no
            AND NOT EXISTS (
              SELECT 1 FROM pool_update pu
                JOIN tx ut ON ut.id = pu.registered_tx_id JOIN block ub ON ub.id = ut.block_id
               WHERE pu.hash_id = pr.hash_id AND pu.registered_tx_id > pr.announced_tx_id
                 AND ub.epoch_no < pr.retiring_epoch)
            AND NOT EXISTS (
              SELECT 1 FROM pool_retire pr2
                JOIN tx rt ON rt.id = pr2.announced_tx_id JOIN block rb ON rb.id = rt.block_id
               WHERE pr2.hash_id = pr.hash_id AND pr2.announced_tx_id > pr.announced_tx_id
                 AND rb.epoch_no < pr.retiring_epoch)
       ) AS pool_retired
  FROM d
  JOIN pool_hash ph ON ph.id = d.pool_hash_id
  JOIN tx ON tx.id = d.tx_id
  JOIN block b ON b.id = tx.block_id`;

/**
 * What the ledger counts for a registered account's governance stake: its
 * unspent UTxO value, its reward account balance, and the deposits of its
 * proposals that are still held (the ledger adds those to the DRep
 * distribution of the return address).
 *
 * Unspent means neither `consumed_by_tx_id` (set when db-sync runs with
 * `tx_out.value = consumed`) nor a `tx_in` (set otherwise) marks it spent, so
 * the query is correct under either configuration. The reward balance is every
 * reward spendable by now, minus every withdrawal.
 *
 * PERFORMANCE: `tx_out.stake_address_id` and `tx_in.tx_out_id` are indexed, so
 * the UTxO sum is proportional to the account's own outputs. `reward_rest` and
 * `gov_action_proposal` have no index on the columns used and are scanned;
 * both are small (reward_rest: ~1.5k rows on preview; it holds MIR,
 * treasury and deposit refunds, so it is larger on mainnet).
 */
export const VOTING_POWER_SQL = `WITH ${TIP_EPOCH_CTE}, ${SA_CTE}
SELECT sa.id, tip.epoch_no,
       (SELECT e.kind FROM (
          SELECT 'reg' AS kind, tx_id, cert_index FROM stake_registration WHERE addr_id = sa.id
          UNION ALL
          SELECT 'dereg' AS kind, tx_id, cert_index FROM stake_deregistration WHERE addr_id = sa.id
        ) e ORDER BY e.tx_id DESC, e.cert_index DESC LIMIT 1) AS last_event,
       (SELECT COALESCE(sum(o.value), 0) FROM tx_out o
         WHERE o.stake_address_id = sa.id AND o.consumed_by_tx_id IS NULL
           AND NOT EXISTS (SELECT 1 FROM tx_in i WHERE i.tx_out_id = o.tx_id AND i.tx_out_index = o.index)) AS utxo,
       (SELECT COALESCE(sum(amount), 0) FROM reward
         WHERE addr_id = sa.id AND spendable_epoch <= tip.epoch_no) AS rewards,
       (SELECT COALESCE(sum(amount), 0) FROM reward_rest
         WHERE addr_id = sa.id AND spendable_epoch <= tip.epoch_no) AS rewards_rest,
       (SELECT COALESCE(sum(amount), 0) FROM withdrawal WHERE addr_id = sa.id) AS withdrawn,
       (SELECT COALESCE(sum(deposit), 0) FROM gov_action_proposal
         WHERE return_address = sa.id AND enacted_epoch IS NULL
           AND dropped_epoch IS NULL AND expired_epoch IS NULL) AS proposal_deposits
  FROM tip LEFT JOIN sa ON TRUE`;

/**
 * Both kinds of delegation certificate, newest first. A combined
 * stake-and-vote delegation certificate yields one row of each kind at the same
 * (tx, index); `kind` breaks the tie so paging is deterministic.
 * `$2` narrows to one kind. Same `delegation_vote` scan note as above.
 */
export const HISTORY_SQL = `WITH ${SA_CTE},
ev AS (
  SELECT 'governance' AS kind, dv.tx_id, dv.cert_index,
         encode(h.raw, 'hex') AS drep_hash, h.view AS drep_view, h.has_script, NULL::text AS pool_hash
    FROM delegation_vote dv JOIN drep_hash h ON h.id = dv.drep_hash_id
   WHERE dv.addr_id = (SELECT id FROM sa) AND ($2::text IS NULL OR $2::text = 'governance')
  UNION ALL
  SELECT 'pool' AS kind, d.tx_id, d.cert_index,
         NULL, NULL, NULL, encode(ph.hash_raw, 'hex')
    FROM delegation d JOIN pool_hash ph ON ph.id = d.pool_hash_id
   WHERE d.addr_id = (SELECT id FROM sa) AND ($2::text IS NULL OR $2::text = 'pool')
)
SELECT ev.*, encode(tx.hash, 'hex') AS tx_hash, b.epoch_no, b.slot_no, b.block_no, b.time,
       count(*) OVER () AS total_count
  FROM ev JOIN tx ON tx.id = ev.tx_id JOIN block b ON b.id = tx.block_id
 ORDER BY ev.tx_id DESC, ev.cert_index DESC, ev.kind
 LIMIT $3 OFFSET $4`;

/** The total, for a page requested past the end (which returns no rows to read it from). */
export const HISTORY_COUNT_SQL = `WITH ${SA_CTE}
SELECT (SELECT count(*) FROM delegation_vote WHERE addr_id = (SELECT id FROM sa)
          AND ($2::text IS NULL OR $2::text = 'governance'))
     + (SELECT count(*) FROM delegation WHERE addr_id = (SELECT id FROM sa)
          AND ($2::text IS NULL OR $2::text = 'pool')) AS total_count`;

/* ------------------------------------------------------------------------- */
/* Mapping                                                                     */
/* ------------------------------------------------------------------------- */

interface DRepCols {
  drep_hash: string | null;
  drep_view: string | null;
  has_script: boolean | null;
}

/** A DRep row as a delegation target. The predefined targets have no hash. */
export function toTarget(row: DRepCols): DelegationTarget {
  if (row.drep_hash) {
    const isScriptBased = row.has_script === true;
    return { kind: 'drep', drep: { role: 'drep', id: encodeDRepId(row.drep_hash, isScriptBased), isScriptBased } };
  }
  if (row.drep_view === 'drep_always_abstain') return { kind: 'predefined', target: 'alwaysAbstain' };
  if (row.drep_view === 'drep_always_no_confidence') return { kind: 'predefined', target: 'alwaysNoConfidence' };
  throw internal(`unrecognised DRep row without a credential: ${String(row.drep_view)}`);
}

interface CertCols extends BlockCols {
  tx_hash: string;
  cert_index: DbNumber;
}

const toTxRef = (row: CertCols): TxRef => ({
  txHash: row.tx_hash,
  index: toInt(row.cert_index),
  ...(row.block_no === null ? {} : { block: toInt(row.block_no) }),
});

export interface DelegationRow extends DRepCols, CertCols {
  account_deregistered: boolean;
  drep_retired: boolean;
}

export function mapDelegation(row: DelegationRow | undefined): Delegation | null {
  if (!row || row.account_deregistered || row.drep_retired) return null;
  return { target: toTarget(row), txRef: toTxRef(row), since: toStamp(row) };
}

export interface PoolDelegationRow extends CertCols {
  pool_hash: string;
  account_deregistered: boolean;
  pool_retired: boolean;
}

export function mapPoolDelegation(row: PoolDelegationRow | undefined): PoolDelegation | null {
  if (!row || row.account_deregistered || row.pool_retired) return null;
  return { poolId: encodePoolId(row.pool_hash), txRef: toTxRef(row), since: toStamp(row) };
}

export interface VotingPowerRow {
  id: DbNumber | null;
  epoch_no: number | null;
  last_event: 'reg' | 'dereg' | null;
  utxo: DbNumber | null;
  rewards: DbNumber | null;
  rewards_rest: DbNumber | null;
  withdrawn: DbNumber | null;
  proposal_deposits: DbNumber | null;
}

/**
 * `null` when the ledger counts nothing for the account: it is not registered
 * (or has never appeared on chain), so it has no reward account and no
 * delegation. A registered account's figure may legitimately be `"0"`.
 * A negative reward balance means the reward tables are incomplete (db-sync
 * run with rewards disabled or pruned); that is refused, never clamped.
 */
export function mapVotingPower(row: VotingPowerRow | undefined): VotingPower | null {
  if (!row || row.epoch_no === null) throw internal('db-sync has no blocks');
  if (row.id === null || row.last_event !== 'reg') return null;
  const big = (v: DbNumber | null) => BigInt(toLovelace(v ?? 0));
  const rewardBalance = big(row.rewards) + big(row.rewards_rest) - big(row.withdrawn);
  if (rewardBalance < 0n) {
    throw internal('reward history is incomplete in this db-sync (withdrawals exceed recorded rewards)');
  }
  const amount = big(row.utxo) + rewardBalance + big(row.proposal_deposits);
  return { amount: amount.toString(), basis: 'live', epoch: toInt(row.epoch_no) };
}

export interface HistoryRow extends DRepCols, CertCols {
  kind: 'governance' | 'pool';
  pool_hash: string | null;
  total_count: DbNumber;
}

export function mapHistoryEvent(row: HistoryRow): DelegationHistoryEvent {
  const at = toStamp(row);
  const txRef = toTxRef(row);
  if (row.kind === 'pool') {
    if (!row.pool_hash) throw internal('pool delegation row without a pool');
    return { kind: 'pool', poolId: encodePoolId(row.pool_hash), at, txRef };
  }
  return { kind: 'governance', target: toTarget(row), at, txRef };
}

/* ------------------------------------------------------------------------- */
/* API                                                                         */
/* ------------------------------------------------------------------------- */

export function createAccountsApi(ctx: Ctx): AccountsApi {
  return {
    async get(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const [row] = await ctx.db.query<{ id: DbNumber; last_event: 'reg' | 'dereg' | null }>(ACCOUNT_SQL, [address.hashRaw]);
      const account: Account = {
        stakeAddress: address.stakeAddress,
        stakeKeyHash: address.hash,
        isRegistered: row?.last_event === 'reg',
        isScriptBased: address.isScript,
      };
      return ctx.envelope(account);
    },

    async getDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const [row] = await ctx.db.query<DelegationRow>(DELEGATION_SQL, [address.hashRaw]);
      return ctx.envelope(mapDelegation(row));
    },

    async getPoolDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const [row] = await ctx.db.query<PoolDelegationRow>(POOL_DELEGATION_SQL, [address.hashRaw]);
      return ctx.envelope(mapPoolDelegation(row));
    },

    async getVotingPower(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const [row] = await ctx.db.query<VotingPowerRow>(VOTING_POWER_SQL, [address.hashRaw]);
      return ctx.envelope(mapVotingPower(row));
    },

    async listDelegationHistory(stakeAddress, q) {
      const address = resolveAddress(stakeAddress, ctx.network);
      if (!q) throw invalidInput('page and size are required');
      const { kind } = q;
      if (kind !== undefined && kind !== 'governance' && kind !== 'pool') {
        throw invalidInput("kind must be 'governance' or 'pool'", { kind });
      }
      const { limit, offset } = toWindow(q);
      const rows = await ctx.db.query<HistoryRow>(HISTORY_SQL, [address.hashRaw, kind ?? null, limit, offset]);
      let totalWhenEmpty: number | undefined;
      if (rows.length === 0) {
        totalWhenEmpty = 0;
        if (offset > 0) {
          const [count] = await ctx.db.query<{ total_count: DbNumber }>(HISTORY_COUNT_SQL, [address.hashRaw, kind ?? null]);
          totalWhenEmpty = count ? toInt(count.total_count) : 0;
        }
      }
      return ctx.paged(toPage(rows, mapHistoryEvent, totalWhenEmpty));
    },
  };
}
