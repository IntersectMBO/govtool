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
  /**
   * The network the database follows. Decides stake address prefixes: only
   * `mainnet` is network id 1, so a custom name such as `devnet` is a testnet.
   */
  network: NetworkId;
  /** Connection settings; the provider opens and owns a pool. */
  connection?: PgDbOptions;
  /** Or a database the caller owns, such as a test double. */
  db?: Db;
  /**
   * Path to the Shelley genesis file db-sync was started with. Optional:
   * db-sync keeps no genesis constants, so `network.getGenesisParams` exists
   * only when this is set. A custom network (a devnet) needs it to date
   * epochs, because its system start and epoch length are not public.
   */
  shelleyGenesisPath?: string;
  /**
   * The `meta.network_name` the database must report. Defaults to `network`
   * for a public network; a custom network accepts any non-public name
   * unless this pins one.
   */
  dbNetworkName?: string;
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
      network: createNetworkApi(ctx, {
        shelleyGenesisPath: options.shelleyGenesisPath,
        dbNetworkName: options.dbNetworkName,
      }),
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
