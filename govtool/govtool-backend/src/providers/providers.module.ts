import { Global, Module, type Provider } from '@nestjs/common';
import {
  DbSyncChainDataProvider,
  createDbSyncProvider,
} from '@govtool/provider-dbsync';
import { createKoiosProvider } from '@govtool/provider-koios';
import { createBlockfrostProvider } from '@govtool/provider-blockfrost';
import { createPinataPinning } from '@govtool/pinning-pinata';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';

import { ConfigService } from '../config/config.service';

/**
 * Injection tokens for the two data-layer contracts. Services depend on the
 * contract, never on the implementation, so swapping db-sync for another
 * provider is a change to this module and nothing else.
 */
export const CHAIN_DATA = 'CHAIN_DATA';
export const PINNING = 'PINNING';

/**
 * Builds whichever implementation VVA_CHAINDATAPROVIDER names.
 *
 * This factory is the only place in the backend that knows a provider exists;
 * every service depends on the contract. Adding a source means a case here and
 * a dependency in package.json, and nothing else.
 *
 * The providers differ in what they can serve — see
 * docs/api/provider-gap-report.md — so a route may answer 501 under one and
 * 200 under another. That is reported per route by the provider's capability
 * document rather than being a surprise at the call site.
 */
function createChainData(configService: ConfigService): ChainDataApiV1 {
  const config = configService.get();

  switch (config.chainDataProvider) {
    case 'koios':
      return createKoiosProvider({
        network: config.koios.network,
        ...(config.koios.token === null ? {} : { token: config.koios.token }),
        ...(config.koios.baseUrl === null
          ? {}
          : { baseUrl: config.koios.baseUrl }),
      });

    case 'blockfrost':
      if (config.blockfrost.baseUrl === '') {
        throw new Error(
          'VVA_CHAINDATAPROVIDER is blockfrost, so VVA_BLOCKFROSTBASEURL is required',
        );
      }
      return createBlockfrostProvider({
        baseUrl: config.blockfrost.baseUrl,
        ...(config.blockfrost.projectId === null
          ? {}
          : { projectId: config.blockfrost.projectId }),
      });

    case 'dbsync':
      return createDbSyncProvider(configService.getDbConnectionConfig());
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

@Global()
@Module({
  providers: [ConfigService, chainDataProvider, pinningProvider],
  exports: [ConfigService, CHAIN_DATA, PINNING],
})
export class ProvidersModule {}

export type { ChainDataApiV1, PinningServiceV1 };
export { DbSyncChainDataProvider };
