/**
 * `ChainDataApiV1` over a cardano-db-sync PostgreSQL database.
 *
 *   const { chainData, close } = createDbSyncProvider({
 *     network: 'mainnet',
 *     connection: { host, port, database, user, password },
 *   });
 */
import type { ChainDataApiV1, NetworkId } from '@govtool/data-providers/chain-data';

import { createAccountsApi } from './accounts';
import { createCtx } from './context';
import { createPgDb, guardDb, type Db, type PgDbOptions } from './db';
import { createGovernanceApi } from './governance';
import { createNetworkApi } from './network';
import { createSystemApi } from './system';
import { createTransactionsApi } from './transactions';

export interface DbSyncProviderOptions {
  /** The network the database follows. Decides stake address prefixes. */
  network: NetworkId;
  /** Connection settings; the provider opens and owns a pool. */
  connection?: PgDbOptions;
  /** Or a database the caller owns, such as a test double. */
  db?: Db;
}

export interface DbSyncProvider {
  chainData: ChainDataApiV1;
  /** Closes the pool the provider opened. A no-op for a caller-owned `db`. */
  close(): Promise<void>;
}

export function createDbSyncProvider(options: DbSyncProviderOptions): DbSyncProvider {
  if (!options.db && !options.connection) throw new Error('createDbSyncProvider needs a connection or a db');
  const owned = options.db ? undefined : createPgDb(options.connection!);
  const db = owned ?? guardDb(options.db!);
  const ctx = createCtx(db, options.network);
  return {
    chainData: {
      network: createNetworkApi(ctx),
      accounts: createAccountsApi(ctx),
      governance: createGovernanceApi(ctx),
      transactions: createTransactionsApi(ctx),
      system: createSystemApi(ctx),
    },
    close: async () => {
      await owned?.end();
    },
  };
}

export type { Db, PgDbOptions } from './db';
export { capabilities } from './system';
