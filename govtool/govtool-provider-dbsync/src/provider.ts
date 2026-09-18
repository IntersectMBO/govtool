import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { PoolConfig } from 'pg';

import { DbSyncAccountsApi } from './api/accounts.api';
import { DbSyncGovernanceApi } from './api/governance';
import { DbSyncNetworkApi } from './api/network.api';
import { DbSyncSurveysApi } from './api/surveys.api';
import { DbSyncSystemApi } from './api/system.api';
import type { DbSyncSystemApiOptions } from './api/system.api';
import { DbSyncTransactionsApi } from './api/transactions.api';
import { createPgQueryable } from './db/pg-pool';
import type { ClosableQueryable, Queryable } from './db/queryable';

export type DbSyncProviderOptions = DbSyncSystemApiOptions;

/**
 * `ChainDataApiV1` over a db-sync database, using the SQL the legacy GovTool
 * backend ships, unmodified.
 *
 * It is a read model and nothing else: no caching, no request coalescing, no
 * background refresh. Those are policy, they differ per deployment, and the
 * consumer already owns them — GovTool's backend has its own cache and cache
 * warmer in front of this.
 *
 * Roughly half the contract is `CAPABILITY_UNSUPPORTED`, because the legacy
 * SQL was written for GovTool's screens rather than as a general ledger API.
 * Every gap is declared at `system.getCapabilities()`; see `./capabilities`.
 */
export class DbSyncChainDataProvider implements ChainDataApiV1 {
  readonly network: DbSyncNetworkApi;
  readonly accounts: DbSyncAccountsApi;
  readonly governance: DbSyncGovernanceApi;
  readonly transactions: DbSyncTransactionsApi;
  readonly surveys: DbSyncSurveysApi;
  readonly system: DbSyncSystemApi;

  constructor(
    private readonly db: Queryable,
    options: DbSyncProviderOptions = {},
  ) {
    this.network = new DbSyncNetworkApi(db);
    this.accounts = new DbSyncAccountsApi(db);
    this.governance = new DbSyncGovernanceApi(db);
    this.transactions = new DbSyncTransactionsApi(db);
    this.surveys = new DbSyncSurveysApi(db);
    this.system = new DbSyncSystemApi(db, options);
  }

  /** Closes the connection pool, when this provider owns one. */
  async dispose(): Promise<void> {
    const db = this.db as Partial<ClosableQueryable>;
    if (typeof db.end === 'function') {
      await db.end();
    }
  }
}

/**
 * Opens a pool with GovTool's long-standing settings and returns the provider.
 * A consumer that already owns a `pg.Pool` should construct
 * `DbSyncChainDataProvider` against its own `Queryable` instead.
 */
export function createDbSyncProvider(
  config: PoolConfig,
  options: DbSyncProviderOptions = {},
): DbSyncChainDataProvider {
  return new DbSyncChainDataProvider(createPgQueryable(config), options);
}
