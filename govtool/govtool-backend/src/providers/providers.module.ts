import {
  Global,
  Inject,
  Module,
  type OnApplicationShutdown,
  type Provider,
} from '@nestjs/common';
import { createDbSyncProvider } from '@govtool/provider-dbsync';
import { createKoiosProvider } from '@govtool/provider-koios';
import { createFixtureProvider } from '@govtool/provider-fixture';
import { createBlockfrostProvider } from '@govtool/provider-blockfrost';
import { createPinataPinning } from '@govtool/pinning-pinata';
import { createTestPinning } from '@govtool/pinning-test';
import { createHttpMetadataService } from '@govtool/metadata-http';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';

import { ConfigService } from '../config/config.service';
import { DOCUMENT_FETCH_TIMEOUT_MS } from '../metadata/document-store';

/**
 * Injection tokens for the data-layer contracts. Services depend on the
 * contract, never on the implementation, so swapping db-sync for another
 * provider is a change to this module and nothing else.
 */
export const CHAIN_DATA = 'CHAIN_DATA';
export const PINNING = 'PINNING';
export const METADATA = 'METADATA';
/** The metadata service for fetches nobody waits on; see `DocumentStore`. */
export const BACKGROUND_METADATA = 'BACKGROUND_METADATA';
/** The chain data plus whatever it holds open (the db-sync pool). */
const CHAIN_DATA_HANDLE = 'CHAIN_DATA_HANDLE';

type ChainDataHandle = {
  chainData: ChainDataApiV1;
  close?: () => Promise<void>;
};

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
function createChainData(configService: ConfigService): ChainDataHandle {
  const config = configService.get();

  switch (config.chainDataProvider) {
    case 'fixture':
      return createFixtureProvider();

    case 'dbsync': {
      // config.service guarantees dbSync is set when the provider is dbsync.
      const db = config.dbSync!;
      return createDbSyncProvider({
        network: db.network,
        ...(db.shelleyGenesisPath
          ? { shelleyGenesisPath: db.shelleyGenesisPath }
          : {}),
        ...(db.networkName ? { dbNetworkName: db.networkName } : {}),
        connection: {
          host: db.host,
          port: db.port,
          database: db.dbname,
          user: db.user,
          password: db.password,
        },
      });
    }

    case 'blockfrost': {
      // An empty baseUrl means hosted Blockfrost for the configured network.
      const bf = config.blockfrost;
      return createBlockfrostProvider({
        network: bf.network,
        ...(bf.baseUrl ? { baseUrl: bf.baseUrl } : {}),
        ...(bf.projectId ? { projectId: bf.projectId } : {}),
      });
    }

    case 'koios': {
      // No token means Koios' free public tier; no baseUrl the public
      // deployment for the configured network.
      const k = config.koios;
      return createKoiosProvider({
        network: k.network,
        ...(k.baseUrl ? { baseUrl: k.baseUrl } : {}),
        ...(k.token ? { token: k.token } : {}),
      });
    }

    default:
      throw new Error(
        `GOVTOOL_CHAIN_DATA_PROVIDER is '${String(config.chainDataProvider)}', which no provider implements.`,
      );
  }
}

const chainDataHandleProvider: Provider = {
  provide: CHAIN_DATA_HANDLE,
  inject: [ConfigService],
  useFactory: createChainData,
};

const chainDataProvider: Provider = {
  provide: CHAIN_DATA,
  inject: [CHAIN_DATA_HANDLE],
  useFactory: (handle: ChainDataHandle) => handle.chainData,
};

/**
 * GOVTOOL_PINNING_PROVIDER picks the implementation. Under `pinata` (the
 * default) it is `null` when no Pinata JWT is configured. Pinning is optional —
 * the backend must start and serve every read route without it, and
 * `/ipfs/upload` answers 503 the way it always did. Under `test` it pins to the
 * local test metadata service, for isolated test environments.
 */
const pinningProvider: Provider = {
  provide: PINNING,
  inject: [ConfigService],
  useFactory: (configService: ConfigService): PinningServiceV1 | null => {
    const config = configService.get();
    if (config.pinningProvider === 'test') {
      // config.service guarantees the url is set when the provider is test.
      return createTestPinning({ baseUrl: config.testPinningUrl! });
    }
    const jwt = config.pinataApiJwt;
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

/**
 * The same service with a timeout above its own fetch's, for the document
 * store's background fetches. Request-time reads keep the client's default,
 * which stays inside the frontend's 30 s request timeout.
 */
const backgroundMetadataProvider: Provider = {
  provide: BACKGROUND_METADATA,
  inject: [ConfigService],
  useFactory: (configService: ConfigService): MetadataServiceV1 | null => {
    const baseUrl = configService.get().metadataServiceUrl;
    return baseUrl
      ? createHttpMetadataService({
          baseUrl,
          timeoutMs: DOCUMENT_FETCH_TIMEOUT_MS,
        })
      : null;
  },
};

@Global()
@Module({
  providers: [
    ConfigService,
    chainDataHandleProvider,
    chainDataProvider,
    pinningProvider,
    metadataProvider,
    backgroundMetadataProvider,
  ],
  exports: [ConfigService, CHAIN_DATA, PINNING, METADATA, BACKGROUND_METADATA],
})
export class ProvidersModule implements OnApplicationShutdown {
  constructor(
    @Inject(CHAIN_DATA_HANDLE) private readonly chainData: ChainDataHandle,
  ) {}

  async onApplicationShutdown(): Promise<void> {
    await this.chainData.close?.();
  }
}

export type { ChainDataApiV1, MetadataServiceV1, PinningServiceV1 };
