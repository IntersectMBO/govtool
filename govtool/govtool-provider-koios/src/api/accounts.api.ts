import type {
  Account,
  AccountExpand,
  AccountsApi,
  DelegationHistoryEvent,
  Delegation,
  DelegationTarget,
  Envelope,
  PagedEnvelope,
  PageRequest,
  StakeRegistrationEvent,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../common/errors';
import { normalizeStakeAddress } from '../common/ids';
import { envelope } from '../common/meta';
import { paginateLocally } from '../common/paging';
import type { KoiosHttpClient } from '../http/client';
import {
  accountIdentity,
  mapBalance,
  mapDelegation,
  mapDelegationTarget,
  mapPoolDelegation,
  mapStakeEvent,
  mapVotingPower,
  toTxRef,
} from '../mappers/account.mapper';
import type {
  AccountInfoRow,
  AccountUpdateEntry,
  AccountUpdatesRow,
  TxInfoRow,
} from '../rows';

export class KoiosAccountsApi implements AccountsApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * One `/account_info` covers registration, both delegations and the whole
   * balance breakdown, so most `expand` values cost nothing extra. Only
   * `delegation`'s certificate reference needs `/account_updates`, and only
   * `drep` and `adaHandles` have no source at all.
   */
  async get(
    stakeAddress: string,
    q?: { expand?: AccountExpand[] },
  ): Promise<Envelope<Account>> {
    const expand = new Set<AccountExpand>(q?.expand ?? []);
    if (expand.has('drep')) {
      throw unsupported(
        'accounts.get{expand:drep}',
        'Koios cannot tell whether a stake credential is also registered as a DRep',
      );
    }
    if (expand.has('adaHandles')) {
      throw unsupported(
        'accounts.get{expand:adaHandles}',
        'resolving $handles needs an asset lookup this provider does not make',
      );
    }

    const address = normalizeStakeAddress(stakeAddress);
    const row = await this.accountInfo(address);

    const wantsDelegationTx =
      expand.has('delegation') || expand.has('poolDelegation');
    const updates = wantsDelegationTx ? await this.accountUpdates(address) : [];

    const identity = accountIdentity(address);
    const account: Account = {
      stakeAddress: row.stake_address,
      stakeKeyHash: identity.stakeKeyHash,
      isScriptBased: identity.isScriptBased,
      isRegistered: row.status === 'registered',
    };

    if (expand.has('balance')) {
      account.balance = mapBalance(row);
    }
    if (expand.has('votingPower')) {
      account.votingPower = mapVotingPower(row);
    }
    if (expand.has('delegation')) {
      const latest = latestOfType(updates, 'delegation_drep');
      const delegation = mapDelegation(
        row,
        latest === undefined ? null : toTxRef(latest),
        latest === undefined ? null : (toTxRef(latest).at ?? null),
      );
      if (delegation !== null) {
        account.delegation = delegation;
      }
    }
    if (expand.has('poolDelegation')) {
      const latest = latestOfType(updates, 'delegation_pool');
      account.poolDelegation = mapPoolDelegation(
        row,
        latest === undefined ? null : toTxRef(latest),
      );
    }
    return envelope(account);
  }

  /**
   * `/account_updates` gives the certificate stream but never what a
   * delegation certificate pointed *at*, so each event's target is resolved
   * from the transaction that carried it. `/tx_info` takes every hash in one
   * POST, so the whole history costs two requests rather than one per event.
   */
  async listDelegationHistory(
    stakeAddress: string,
    q?: PageRequest & { kind?: ('governance' | 'pool')[] },
  ): Promise<PagedEnvelope<DelegationHistoryEvent>> {
    const address = normalizeStakeAddress(stakeAddress);
    const kinds = new Set(q?.kind ?? ['governance', 'pool']);
    const updates = (await this.accountUpdates(address))
      .filter(
        (entry) =>
          (kinds.has('governance') &&
            entry.action_type === 'delegation_drep') ||
          (kinds.has('pool') && entry.action_type === 'delegation_pool'),
      )
      .sort((a, b) => a.block_time - b.block_time);

    const targets = await this.resolveDelegationTargets(
      address,
      updates.map((entry) => entry.tx_hash),
    );

    const events: DelegationHistoryEvent[] = [];
    let previousGovernance: DelegationHistoryEvent['to'] = { kind: 'none' };
    let previousPool: DelegationHistoryEvent['to'] = {
      kind: 'pool',
      poolId: null,
    };

    for (const entry of updates) {
      const isGovernance = entry.action_type === 'delegation_drep';
      const to = targets.get(`${entry.tx_hash}:${entry.action_type}`);
      if (to === undefined) {
        continue;
      }
      const from = isGovernance ? previousGovernance : previousPool;
      events.push({
        kind: isGovernance ? 'governance' : 'pool',
        at: toTxRef(entry).at!,
        txRef: toTxRef(entry),
        from,
        to,
      });
      if (isGovernance) {
        previousGovernance = to;
      } else {
        previousPool = to;
      }
    }

    events.reverse();
    return envelope(paginateLocally(events, q));
  }

  async listStakeEvents(
    stakeAddress: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<StakeRegistrationEvent>> {
    const address = normalizeStakeAddress(stakeAddress);
    // Sorted on the raw rows: the mapped event's `at` may carry an epoch and
    // no timestamp, but `block_time` is always present on the update.
    const events = (await this.accountUpdates(address))
      .sort((a, b) => b.block_time - a.block_time)
      .map(mapStakeEvent)
      .filter((event): event is StakeRegistrationEvent => event !== null);
    return envelope(paginateLocally(events, q));
  }

  /** See `mapVotingPower` for why this is recomputed rather than read off. */
  async getVotingPower(
    stakeAddress: string,
    q?: { epoch?: number },
  ): Promise<Envelope<VotingPower | null>> {
    if (q?.epoch !== undefined) {
      throw unsupported(
        'accounts.getVotingPower{epoch}',
        'Koios reports account balances at the tip only, with no per-epoch history',
      );
    }
    const row = await this.accountInfo(normalizeStakeAddress(stakeAddress));
    return envelope(mapVotingPower(row));
  }

  async getDelegation(
    stakeAddress: string,
  ): Promise<Envelope<Delegation | null>> {
    const row = await this.accountInfo(normalizeStakeAddress(stakeAddress));
    return envelope(mapDelegation(row));
  }

  /* --------------------------------------------------------------------- */

  private async accountInfo(address: string): Promise<AccountInfoRow> {
    const response = await this.http.post<AccountInfoRow>('account_info', {
      _stake_addresses: [address],
    });
    const row = response.rows[0];
    if (row === undefined) {
      throw notFound('Koios has no record of that stake address', {
        stakeAddress: address,
      });
    }
    return row;
  }

  private async accountUpdates(address: string): Promise<AccountUpdateEntry[]> {
    const response = await this.http.post<AccountUpdatesRow>(
      'account_updates',
      { _stake_addresses: [address] },
    );
    return response.rows[0]?.updates ?? [];
  }

  /**
   * Reads the delegation certificates out of the transactions that carried
   * them, keyed by `txHash:action_type`.
   *
   * A transaction can hold both a DRep and a pool delegation for the same
   * account, which is why the key carries the action type; and it can hold
   * certificates for *other* accounts too, which is why each certificate's
   * own stake address is checked before it is used.
   */
  private async resolveDelegationTargets(
    address: string,
    txHashes: string[],
  ): Promise<Map<string, DelegationHistoryEvent['to']>> {
    const targets = new Map<string, DelegationHistoryEvent['to']>();
    if (txHashes.length === 0) {
      return targets;
    }

    const response = await this.http.post<TxInfoRow>('tx_info', {
      _tx_hashes: [...new Set(txHashes)],
      _inputs: false,
      _metadata: false,
      _assets: false,
      _withdrawals: false,
      _certs: true,
      _scripts: false,
      _bytecode: false,
      _governance: false,
    });

    for (const tx of response.rows) {
      for (const certificate of tx.certificates ?? []) {
        const info = certificate.info ?? {};
        if (info['stake_address'] !== address) {
          continue;
        }
        const drepId = info['drep_id'];
        if (typeof drepId === 'string') {
          targets.set(
            `${tx.tx_hash}:delegation_drep`,
            mapDelegationTarget(drepId) as DelegationTarget,
          );
        }
        const poolId = info['pool_id_bech32'] ?? info['pool_id'];
        if (typeof poolId === 'string') {
          targets.set(`${tx.tx_hash}:delegation_pool`, {
            kind: 'pool',
            poolId,
          });
        }
      }
    }
    return targets;
  }
}

function latestOfType(
  updates: AccountUpdateEntry[],
  action: AccountUpdateEntry['action_type'],
): AccountUpdateEntry | undefined {
  return updates
    .filter((entry) => entry.action_type === action)
    .sort((a, b) => b.block_time - a.block_time)[0];
}
