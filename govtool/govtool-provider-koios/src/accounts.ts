/**
 * AccountsApi over Koios (SPEC.md §5.4): `/account_info` for state and current
 * delegations, `/account_updates` to date and reference the certificate that
 * made each delegation.
 *
 * An address Koios has never seen is a KNOWN answer: such a credential has
 * never been registered and delegates to nobody. So a well-formed unknown
 * address gets `isRegistered: false` and `null`, never NOT_FOUND (SPEC.md §3.2).
 *
 * A delegation stands only while its target does. From protocol 10 the ledger
 * clears a retiring DRep's delegations, and the protocol-10 hard fork cleared
 * delegations to DReps unregistered at that moment; under protocol 9 a
 * retirement left the delegation dangling, and it revived if the DRep
 * re-registered before the hard fork. A pool retirement ends delegations to
 * the pool. These are checked here, as the db-sync provider checks them.
 *
 * `balance` is not served: withdrawals are not split between staking and
 * non-staking rewards, so `rewards` and `rewardsRest` cannot be served
 * correctly, and the contract forbids `total` alone.
 * `listDelegationHistory` is not served: `/account_updates` dates each
 * delegation certificate but does not say what it delegated to.
 */
import type {
  Account,
  AccountsApi,
  Delegation,
  DelegationTarget,
  PoolDelegation,
  TxRef,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { internal, invalidInput } from './errors';
import { canonicalDRepId, certOrder, loadUpdates } from './governance/dreps/directory';
import { decodeDRepId, decodePoolId, decodeStakeAddress, encodePoolId, encodeStakeAddress, type Credential } from './ids';
import { toIso, toLovelace } from './numbers';
import type { AccountInfoRow, AccountUpdate, AccountUpdatesRow, PoolInfoRow } from './rows';

export interface ResolvedAddress extends Credential {
  /** The canonical (lowercase, re-encoded) address. */
  stakeAddress: string;
}

/** Decode and validate for this provider's network; INVALID_INPUT otherwise. */
export function resolveAddress(address: unknown, network: string): ResolvedAddress {
  if (typeof address !== 'string') throw invalidInput('stakeAddress must be a string', { stakeAddress: address });
  const credential = decodeStakeAddress(address.trim().toLowerCase(), network);
  return { ...credential, stakeAddress: encodeStakeAddress(credential.hash, credential.isScript, network) };
}

async function accountInfo(ctx: Ctx, address: string): Promise<AccountInfoRow | undefined> {
  const { rows } = await ctx.http.post<AccountInfoRow>('account_info', { _stake_addresses: [address] });
  return rows.find((r) => r.stake_address === address);
}

/** The newest certificate of `action` for the account, or undefined. */
async function latestUpdate(ctx: Ctx, address: string, action: AccountUpdate['action_type']): Promise<AccountUpdate | undefined> {
  const { rows } = await ctx.http.post<AccountUpdatesRow>('account_updates', { _stake_addresses: [address] });
  const updates = rows.find((r) => r.stake_address === address)?.updates ?? [];
  return updates
    .filter((u) => u.action_type === action)
    .sort((a, b) => b.absolute_slot - a.absolute_slot || (a.tx_hash < b.tx_hash ? 1 : -1))[0];
}

const txRefOf = (u: AccountUpdate): TxRef => ({ txHash: u.tx_hash });
const stampOf = (u: AccountUpdate) => ({ epoch: u.epoch_no, slot: u.absolute_slot, time: toIso(u.block_time) });

/** A Koios `delegated_drep` value as a delegation target. */
export function toTarget(value: string): DelegationTarget {
  if (value === 'drep_always_abstain') return { kind: 'predefined', target: 'alwaysAbstain' };
  if (value === 'drep_always_no_confidence') return { kind: 'predefined', target: 'alwaysNoConfidence' };
  const { hash, isScript } = decodeDRepId(value);
  return { kind: 'drep', drep: { role: 'drep', id: canonicalDRepId({ drep_id: value, hex: hash, has_script: isScript }), isScriptBased: isScript } };
}

export function createAccountsApi(ctx: Ctx): AccountsApi {
  /** Whether a delegation to `drepId` made at `at` (UNIX s) was cleared by the DRep retiring. */
  async function drepRetiredSince(drepId: string, at: number): Promise<boolean> {
    const [updates, proto10] = await Promise.all([
      loadUpdates(ctx, [drepId]),
      ctx.http.get<{ epoch_no: number; protocol_major: number }>(
        'epoch_params',
        { protocol_major: 'gte.10' },
        { select: 'epoch_no,protocol_major', order: 'epoch_no.asc', limit: 1 },
      ),
    ]);
    const certs = [...(updates.get(drepId) ?? [])].sort(certOrder);
    const clock = await ctx.chain.clock();
    const epochOf = (t: number) => clock.anchorEpoch + Math.floor((t - clock.anchorStart) / clock.epochSeconds);
    const hardFork = proto10.rows[0]?.epoch_no ?? Number.MAX_SAFE_INTEGER;
    for (const r of certs) {
      if (r.action !== 'deregistered' || r.block_time <= at) continue;
      if (epochOf(r.block_time) >= hardFork) return true;
      // A protocol-9 retirement: the delegation revives only if the DRep
      // re-registered before the hard fork.
      const revived = certs.some((g) => g.action === 'registered' && certOrder(g, r) > 0 && epochOf(g.block_time) < hardFork);
      if (!revived) return true;
    }
    return false;
  }

  return {
    async get(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const row = await accountInfo(ctx, address.stakeAddress);
      const account: Account = {
        stakeAddress: address.stakeAddress,
        stakeKeyHash: address.hash,
        isRegistered: row?.status === 'registered',
        isScriptBased: address.isScript,
      };
      return ctx.envelope(account);
    },

    async getDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const row = await accountInfo(ctx, address.stakeAddress);
      if (!row || row.status !== 'registered' || !row.delegated_drep) return ctx.envelope(null);
      const target = toTarget(row.delegated_drep);
      const cert = await latestUpdate(ctx, address.stakeAddress, 'delegation_drep');
      if (!cert) throw internal('Koios reports a DRep delegation with no delegation certificate');
      if (target.kind === 'drep' && (await drepRetiredSince(target.drep.id, cert.block_time))) return ctx.envelope(null);
      const delegation: Delegation = { target, txRef: txRefOf(cert), since: stampOf(cert) };
      return ctx.envelope(delegation);
    },

    async getPoolDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const row = await accountInfo(ctx, address.stakeAddress);
      if (!row || row.status !== 'registered' || !row.delegated_pool) return ctx.envelope(null);
      const poolId = encodePoolId(decodePoolId(row.delegated_pool));
      const [cert, tip, { rows: pools }] = await Promise.all([
        latestUpdate(ctx, address.stakeAddress, 'delegation_pool'),
        ctx.chain.tip(),
        ctx.http.post<PoolInfoRow & { retiring_epoch: number | null }>('pool_info', { _pool_bech32_ids: [poolId] }),
      ]);
      if (!cert) throw internal('Koios reports a pool delegation with no delegation certificate');
      const pool = pools[0];
      // POOLREAP removes delegations to a pool whose retirement epoch has been reached.
      if (pool && pool.pool_status === 'retired' && pool.retiring_epoch !== null && pool.retiring_epoch <= tip.epoch_no && pool.retiring_epoch > cert.epoch_no) {
        return ctx.envelope(null);
      }
      const delegation: PoolDelegation = { poolId, txRef: txRefOf(cert), since: stampOf(cert) };
      return ctx.envelope(delegation);
    },

    /**
     * What the ledger counts for a registered account's governance stake: its
     * UTxO, its reward account balance (staking rewards plus reserve and
     * treasury MIRs and proposal refunds, less withdrawals), and the deposits
     * of its proposals still held. Koios' own `total_balance` is not used: it
     * leaves the non-staking rewards out while subtracting their withdrawals,
     * and goes negative on real accounts. A negative reward balance is refused.
     */
    async getVotingPower(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const [row, tip, deposits] = await Promise.all([
        accountInfo(ctx, address.stakeAddress),
        ctx.chain.tip(),
        ctx.http.getAll<{ deposit: string | null }>(
          'proposal_list',
          {
            return_address: `eq.${address.stakeAddress}`,
            enacted_epoch: 'is.null',
            dropped_epoch: 'is.null',
            expired_epoch: 'is.null',
          },
          { select: 'deposit' },
        ),
      ]);
      if (!row || row.status !== 'registered') return ctx.envelope(null);
      const big = (v: string | null | undefined) => BigInt(toLovelace(v ?? '0'));
      const refund = row.proposal_refund ?? row['proposal-refund'] ?? '0';
      const rewardBalance = big(row.rewards) + big(row.reserves) + big(row.treasury) + big(refund) - big(row.withdrawals);
      if (rewardBalance < 0n) throw internal('Koios reward history is inconsistent (withdrawals exceed recorded rewards)');
      const held = deposits.reduce((sum, d) => sum + big(d.deposit), 0n);
      const power: VotingPower = { amount: (big(row.utxo) + rewardBalance + held).toString(), basis: 'live', epoch: tip.epoch_no };
      return ctx.envelope(power);
    },
  };
}
