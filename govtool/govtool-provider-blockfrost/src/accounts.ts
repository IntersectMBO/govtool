/**
 * AccountsApi over Blockfrost (SPEC.md §5.4): `/accounts/{stake}`.
 *
 * Blockfrost indexes the whole chain, so an address it answers 404 for has
 * never appeared on chain: a KNOWN answer — never registered, delegated to
 * nobody — not a missing one. That is the connected wallet of a brand-new
 * user, and "you are not delegated" is the true statement for it.
 *
 * `drep_id` arrives in whichever encoding Blockfrost uses on this endpoint
 * (CIP-105 on blockfrost-ryo 3.1.1, CIP-129 on hosted v6.8 at the time of
 * writing) and is decoded and re-encoded to CIP-129.
 *
 * Omitted, on purpose:
 *   getVotingPower          `controlled_amount` is the UTxO plus withdrawable
 *                           rewards. What the ledger counts also includes the
 *                           deposits of the account's live proposals, which
 *                           would take reading every proposal record per call.
 *                           A figure short by 100k ada for a proposer is
 *                           wrong, so the method is absent.
 *   balance                 the withdrawable balance is not split between
 *                           staking and non-staking rewards, and the contract
 *                           forbids serving `total` alone.
 *   listDelegationHistory   there is no governance-delegation history endpoint;
 *                           a pool-only history would half-serve the method.
 */
import type { Account, AccountsApi, Delegation, DelegationTarget, PoolDelegation } from '@govtool/data-providers/chain-data';

import { loadClock, stampOf } from './chain';
import type { Ctx, Session } from './context';
import { invalidInput } from './errors';
import { decodePoolId, decodeStakeAddress, encodeDRepId, encodePoolId, encodeStakeAddress, fromBlockfrostDRepId, type Credential } from './ids';

interface BfAccount {
  stake_address: string;
  active: boolean;
  registered?: boolean;
  drep_id: string | null;
  pool_id: string | null;
}

interface BfAccountDelegation {
  active_epoch: number;
  tx_hash: string;
  pool_id: string;
  tx_slot: number;
  block_time: number;
  block_height: number;
}

export interface ResolvedAddress extends Credential {
  /** The canonical (lowercase, re-encoded) address. */
  stakeAddress: string;
}

/** Decode and validate for this provider's network; INVALID_INPUT otherwise. */
export function resolveAddress(address: unknown, network: string): ResolvedAddress {
  if (typeof address !== 'string') throw invalidInput('stakeAddress must be a string', { stakeAddress: address });
  const credential = decodeStakeAddress(address.trim(), network);
  return { ...credential, stakeAddress: encodeStakeAddress(credential.hash, credential.isScript, network) };
}

const loadAccount = (s: Session, address: string) => s.once(`account:${address}`, () => s.http.getOrNull<BfAccount>(`/accounts/${address}`));

/** Registered now. `registered` is the certificate state; older deployments only send `active`. */
const isRegistered = (a: BfAccount | null) => a !== null && (a.registered ?? a.active) === true;

export function toTarget(drepId: string): DelegationTarget {
  if (drepId === 'drep_always_abstain') return { kind: 'predefined', target: 'alwaysAbstain' };
  if (drepId === 'drep_always_no_confidence') return { kind: 'predefined', target: 'alwaysNoConfidence' };
  const c = fromBlockfrostDRepId(drepId)!;
  return { kind: 'drep', drep: { role: 'drep', id: encodeDRepId(c.hash, c.isScript), isScriptBased: c.isScript } };
}

export function createAccountsApi(ctx: Ctx): AccountsApi {
  return {
    async get(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const account = await loadAccount(ctx.session(), address.stakeAddress);
      const out: Account = {
        stakeAddress: address.stakeAddress,
        stakeKeyHash: address.hash,
        isRegistered: isRegistered(account),
        isScriptBased: address.isScript,
      };
      return ctx.envelope(out);
    },

    /**
     * The delegation stands only while the account is registered (a
     * deregistration clears it) and while the DRep has not retired (from
     * protocol 10 a retirement clears its delegations). The retirement is read
     * off the DRep's current record, one more request. Not detectable here: a
     * DRep that retired and re-registered after the delegation, which the
     * ledger also cleared; Blockfrost does not say when the delegation was made.
     */
    async getDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const s = ctx.session();
      const account = await loadAccount(s, address.stakeAddress);
      if (!isRegistered(account) || !account!.drep_id) return ctx.envelope(null);
      const target = toTarget(account!.drep_id);
      if (target.kind === 'drep') {
        const drep = await s.http.getOrNull<{ retired: boolean }>(`/governance/dreps/${target.drep.id}`);
        if (!drep || drep.retired) return ctx.envelope(null);
      }
      // Blockfrost does not say which certificate set the vote delegation.
      const delegation: Delegation = { target, txRef: null };
      return ctx.envelope(delegation);
    },

    async getPoolDelegation(stakeAddress) {
      const address = resolveAddress(stakeAddress, ctx.network);
      const s = ctx.session();
      const account = await loadAccount(s, address.stakeAddress);
      if (!isRegistered(account) || !account!.pool_id) return ctx.envelope(null);
      const poolId = encodePoolId(decodePoolId(account!.pool_id));
      const [latest] = await s.http.get<BfAccountDelegation[]>(`/accounts/${address.stakeAddress}/delegations`, {
        count: 1,
        order: 'desc',
      });
      // The newest certificate is the one in force only if it names the same pool.
      if (!latest || latest.pool_id !== account!.pool_id) {
        return ctx.envelope<PoolDelegation>({ poolId, txRef: null, since: null });
      }
      const since = stampOf(
        { hash: latest.tx_hash, block_height: latest.block_height, block_time: latest.block_time, slot: latest.tx_slot, index: 0 },
        await loadClock(s),
      );
      return ctx.envelope<PoolDelegation>({
        poolId,
        txRef: { txHash: latest.tx_hash, block: latest.block_height },
        since,
      });
    },
  };
}
