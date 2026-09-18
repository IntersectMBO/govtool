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

import { internal, unsupported } from '../common/errors';
import { normalizeStakeKey } from '../common/ids';
import { envelope } from '../common/meta';
import { runSql } from '../db/run';
import type { Queryable } from '../db/queryable';
import {
  mapAccountInfoRow,
  mapDelegationRow,
  mapStakeKeyVotingPowerRow,
} from '../mappers/account.mapper';
import type {
  AccountInfoRow,
  CurrentDelegationRow,
  StakeKeyVotingPowerRow,
} from '../rows';

/** Legacy message, byte for byte. */
export const ACCOUNT_INFO_ERROR = 'Could not query the account info.';

export class DbSyncAccountsApi implements AccountsApi {
  constructor(private readonly db: Queryable) {}

  /**
   * `get-account-info.sql` returns registration state only. `votingPower` and
   * `delegation` are separate statements, so they are served only when the
   * caller opts in via `expand`; `balance`, `poolDelegation`, `drep`,
   * `adaHandles` and the stake-event fields have no statement at all.
   */
  async get(
    stakeAddress: string,
    q?: { expand?: AccountExpand[] },
  ): Promise<Envelope<Account>> {
    const hash = normalizeStakeKey(stakeAddress);
    const expand = q?.expand ?? [];

    for (const field of expand) {
      if (field !== 'votingPower' && field !== 'delegation') {
        throw unsupported(`accounts.get#${field}`);
      }
    }

    const rows = await runSql<AccountInfoRow>(this.db, 'get-account-info.sql', [
      hash,
    ]);
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      throw internal(ACCOUNT_INFO_ERROR);
    }

    const account = mapAccountInfoRow(hash, row);

    if (expand.includes('votingPower')) {
      account.votingPower = (await this.getVotingPower(stakeAddress)).data;
    }
    if (expand.includes('delegation')) {
      account.delegation = (await this.getDelegation(stakeAddress)).data ?? {
        target: { kind: 'none' },
        txRef: null,
      };
    }

    return envelope(account);
  }

  async getDelegation(
    stakeAddress: string,
  ): Promise<Envelope<Delegation | null>> {
    const hash = normalizeStakeKey(stakeAddress);
    const rows = await runSql<CurrentDelegationRow>(
      this.db,
      'get-current-delegation.sql',
      [hash],
    );
    const row = rows[0];
    if (row === undefined) {
      return envelope(null);
    }
    // The statement is `LIMIT 1`, so >1 row cannot occur; the legacy
    // "multiple delegations" branch was unreachable and is not reproduced.
    return envelope(mapDelegationRow(row));
  }

  /**
   * The legacy service swallowed every failure here and returned 0, which
   * hides an outage as "no voting power". This returns `null` for "no rows"
   * and lets a real failure propagate; a consumer that needs the legacy
   * number maps `null` to 0 itself.
   */
  async getVotingPower(
    stakeAddress: string,
    q?: { epoch?: number },
  ): Promise<Envelope<VotingPower | null>> {
    if (q?.epoch !== undefined) {
      throw unsupported('accounts.getVotingPower{epoch}');
    }
    const hash = normalizeStakeKey(stakeAddress);
    const rows = await runSql<StakeKeyVotingPowerRow>(
      this.db,
      'get-stake-key-voting-power.sql',
      [hash],
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      return envelope(null);
    }
    return envelope(mapStakeKeyVotingPowerRow(row));
  }

  listDelegationHistory(
    _stakeAddress: string,
    _q?: PageRequest & { kind?: ('governance' | 'pool')[] },
  ): Promise<PagedEnvelope<DelegationHistoryEvent>> {
    return Promise.reject(unsupported('accounts.listDelegationHistory'));
  }

  listStakeEvents(
    _stakeAddress: string,
    _q?: PageRequest,
  ): Promise<PagedEnvelope<StakeRegistrationEvent>> {
    return Promise.reject(unsupported('accounts.listStakeEvents'));
  }
}
