import type {
  Account,
  AccountExpand,
  AccountsApi,
  Delegation,
  DelegationHistoryEvent,
  Envelope,
  PagedEnvelope,
  PageRequest,
  StakeRegistrationEvent,
  VotingPower,
} from '@govtool/data-providers/chain-data';

import { notFound, unsupported } from '../common/errors';
import { toBlockfrostStakeAddress } from '../common/ids';
import { envelope } from '../common/meta';
import { toBlockfrostPage, toContractPage } from '../common/paging';
import type { EpochTimeResolver } from '../common/epoch-time';
import type { BlockfrostClient } from '../http/client';
import type {
  BfAccount,
  BfAccountDelegation,
  BfAccountRegistration,
} from '../http/types';
import {
  mapAccount,
  mapBalance,
  mapDelegation,
} from '../mappers/account.mapper';

export class BlockfrostAccountsApi implements AccountsApi {
  constructor(
    private readonly client: BlockfrostClient,
    private readonly epochs: EpochTimeResolver,
  ) {}

  /**
   * One read covers registration, balance, the governance delegation and the
   * pool delegation — Blockfrost's account record carries all of them, so the
   * `expand` fields cost nothing extra here and are honoured unconditionally
   * where the data is present.
   *
   * `votingPower` is not among them: Blockfrost has no per-account voting
   * power, only per-DRep. `drep` (the account's own DRep record) and
   * `adaHandles` have no source either.
   */
  async get(
    stakeAddress: string,
    q?: { expand?: AccountExpand[] },
  ): Promise<Envelope<Account>> {
    for (const field of q?.expand ?? []) {
      if (
        field === 'votingPower' ||
        field === 'drep' ||
        field === 'adaHandles'
      ) {
        throw unsupported(
          `accounts.get#${field}`,
          field === 'votingPower'
            ? 'Blockfrost reports voting power per DRep, never per account'
            : 'no Blockfrost route serves this field',
        );
      }
    }

    const bech32 = toBlockfrostStakeAddress(stakeAddress);
    const record = await this.client.getOrNull<BfAccount>(
      `/accounts/${bech32}`,
    );
    if (record === null) {
      throw notFound(`Account ${stakeAddress} not found`, { stakeAddress });
    }

    const account = mapAccount(record);
    const expand = q?.expand ?? [];

    if (expand.includes('balance')) {
      account.balance = mapBalance(record);
    }
    if (expand.includes('delegation')) {
      account.delegation = mapDelegation(record) ?? {
        target: { kind: 'none' },
        txRef: null,
      };
    }
    if (expand.includes('poolDelegation')) {
      account.poolDelegation = {
        poolId: record.pool_id,
        txRef: null,
        since: null,
      };
    }

    return envelope(account);
  }

  async getDelegation(
    stakeAddress: string,
  ): Promise<Envelope<Delegation | null>> {
    const bech32 = toBlockfrostStakeAddress(stakeAddress);
    const record = await this.client.getOrNull<BfAccount>(
      `/accounts/${bech32}`,
    );
    return envelope(record === null ? null : mapDelegation(record));
  }

  getVotingPower(
    _stakeAddress: string,
    _q?: { epoch?: number },
  ): Promise<Envelope<VotingPower | null>> {
    return Promise.reject(
      unsupported(
        'accounts.getVotingPower',
        'Blockfrost reports voting power per DRep, never per stake account',
      ),
    );
  }

  /**
   * Stake-pool delegation history only. `/accounts/{a}/delegations` gives the
   * epoch each certificate took effect and the pool it chose, so `to` and
   * `at` are filled; `from` is left unset, as the contract allows, because
   * the endpoint reports each certificate's target and not the one it
   * replaced.
   *
   * Governance delegation history has no endpoint — only the *current*
   * `drep_id` on the account — so a request for it is refused rather than
   * answered with the pool history.
   */
  async listDelegationHistory(
    stakeAddress: string,
    q?: PageRequest & { kind?: ('governance' | 'pool')[] },
  ): Promise<PagedEnvelope<DelegationHistoryEvent>> {
    const kinds = q?.kind ?? ['pool'];
    if (kinds.includes('governance')) {
      throw unsupported(
        'accounts.listDelegationHistory{kind=governance}',
        'Blockfrost reports only the current drep_id on an account, with no history',
      );
    }

    const bech32 = toBlockfrostStakeAddress(stakeAddress);
    const page = toBlockfrostPage(q, { count: 25, order: 'desc' });
    const rows = await this.client.get<BfAccountDelegation[]>(
      `/accounts/${bech32}/delegations`,
      page,
    );

    const elements: DelegationHistoryEvent[] = [];
    for (const row of rows) {
      const at = await this.epochs.stamp(row.active_epoch);
      const event: DelegationHistoryEvent = {
        kind: 'pool',
        txRef: { txHash: row.tx_hash },
        to: { kind: 'pool', poolId: row.pool_id },
      };
      if (at !== null) event.at = at;
      elements.push(event);
    }

    return envelope(toContractPage(elements, page));
  }

  /**
   * `/accounts/{a}/registrations` lists the certificates but dates none of
   * them — no epoch, slot or block — so each event carries only its action
   * and transaction. The contract allows that; a consumer needing the date
   * resolves the transaction itself.
   */
  async listStakeEvents(
    stakeAddress: string,
    q?: PageRequest,
  ): Promise<PagedEnvelope<StakeRegistrationEvent>> {
    const bech32 = toBlockfrostStakeAddress(stakeAddress);
    const page = toBlockfrostPage(q, { count: 25, order: 'desc' });
    const rows = await this.client.get<BfAccountRegistration[]>(
      `/accounts/${bech32}/registrations`,
      page,
    );

    const elements = rows
      .filter(
        (
          row,
        ): row is BfAccountRegistration & {
          action: 'registered' | 'deregistered';
        } => row.action === 'registered' || row.action === 'deregistered',
      )
      .map((row) => ({
        action: row.action,
        txRef: { txHash: row.tx_hash },
      }));

    return envelope(toContractPage(elements, page));
  }
}
