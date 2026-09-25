import { Global, Module, type Provider } from '@nestjs/common';
import { createDbSyncProvider } from '@govtool/provider-dbsync';
import { createKoiosProvider } from '@govtool/provider-koios';
import { createFixtureProvider } from '@govtool/provider-fixture';
import { createBlockfrostProvider } from '@govtool/provider-blockfrost';
import { createPinataPinning } from '@govtool/pinning-pinata';
import { createHttpMetadataService } from '@govtool/metadata-http';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';

import { ConfigService } from '../config/config.service';

/**
 * Injection tokens for the data-layer contracts. Services depend on the
 * contract, never on the implementation, so swapping db-sync for another
 * provider is a change to this module and nothing else.
 */
export const CHAIN_DATA = 'CHAIN_DATA';
export const PINNING = 'PINNING';
export const METADATA = 'METADATA';

/**
 * Builds whichever implementation GOVTOOL_CHAIN_DATA_PROVIDER names.
 *
 * This factory is the only place in the backend that knows a provider exists;
 * every service depends on the contract. Adding a source means a case here and
 * a dependency in package.json, and nothing else.
 *
 * The providers differ in what they can serve — a route may answer 501 under
 * one and 200 under another. That is reported per route by the provider's
 * capability document rather than being a surprise at the call site.
 */
function createChainData(configService: ConfigService): ChainDataApiV1 {
  const config = configService.get();

  switch (config.chainDataProvider) {
    case 'fixture':
      return createFixtureProvider().chainData;

    case 'dbsync': {
      // config.service guarantees dbSync is set when the provider is dbsync.
      const db = config.dbSync!;
      return createDbSyncProvider({
        network: db.network,
        connection: {
          host: db.host,
          port: db.port,
          database: db.dbname,
          user: db.user,
          password: db.password,
        },
      }).chainData;
    }

    case 'blockfrost': {
      // An empty baseUrl means hosted Blockfrost for the configured network.
      const bf = config.blockfrost;
      return createBlockfrostProvider({
        network: bf.network,
        ...(bf.baseUrl ? { baseUrl: bf.baseUrl } : {}),
        ...(bf.projectId ? { projectId: bf.projectId } : {}),
      }).chainData;
    }

    case 'koios': {
      // No token means Koios' free public tier; no baseUrl the public
      // deployment for the configured network.
      const k = config.koios;
      return createKoiosProvider({
        network: k.network,
        ...(k.baseUrl ? { baseUrl: k.baseUrl } : {}),
        ...(k.token ? { token: k.token } : {}),
      }).chainData;
    }

    default:
      throw new Error(
        `GOVTOOL_CHAIN_DATA_PROVIDER is '${String(config.chainDataProvider)}', which no provider implements.`,
      );
  }
}

const chainDataProvider: Provider = {
  provide: CHAIN_DATA,
  inject: [ConfigService],
  useFactory: createChainData,
};

/**
 * `null` when no Pinata JWT is configured. Pinning is optional — the backend
 * must start and serve every read route without it, and `/ipfs/upload`
 * answers 503 the way it always did.
 */
const pinningProvider: Provider = {
  provide: PINNING,
  inject: [ConfigService],
  useFactory: (configService: ConfigService): PinningServiceV1 | null => {
    const jwt = configService.get().pinataApiJwt;
    return jwt ? createPinataPinning({ jwt }) : null;
  },
};

/**
 * `null` when GOVTOOL_METADATA_SERVICE_URL is unset. Like pinning, the metadata
 * service is optional: every other route works without it, and the metadata
 * resolve, retry and report routes answer 503.
 */
const metadataProvider: Provider = {
  provide: METADATA,
  inject: [ConfigService],
  useFactory: (configService: ConfigService): MetadataServiceV1 | null => {
    const baseUrl = configService.get().metadataServiceUrl;
    return baseUrl ? createHttpMetadataService({ baseUrl }) : null;
  },
};

@Global()
@Module({
  providers: [
    ConfigService,
    chainDataProvider,
    pinningProvider,
    metadataProvider,
  ],
  exports: [ConfigService, CHAIN_DATA, PINNING, METADATA],
})
export class ProvidersModule {}

export type { ChainDataApiV1, MetadataServiceV1, PinningServiceV1 };
