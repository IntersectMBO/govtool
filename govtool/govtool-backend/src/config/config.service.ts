import { Injectable } from '@nestjs/common';
import 'dotenv/config';
import * as fs from 'fs';
import * as path from 'path';

import {
  BackendConfig,
  BackendConfigFile,
  ChainDataProviderName,
  DbSyncConfig,
} from './config.types';

const CHAIN_DATA_PROVIDERS: ChainDataProviderName[] = [
  'dbsync',
  'koios',
  'blockfrost',
  'fixture',
];

@Injectable()
export class ConfigService {
  private readonly config: BackendConfig;

  constructor() {
    this.config = this.loadConfig();
  }

  get(): BackendConfig {
    return this.config;
  }

  /** Throws unless db-sync is the configured provider; see `BackendConfig.dbSync`. */
  getDbConnectionConfig() {
    const dbSync = this.config.dbSync;
    if (dbSync === null) {
      throw new Error(
        `GOVTOOL_CHAIN_DATA_PROVIDER is '${this.config.chainDataProvider}', so no db-sync connection is configured`,
      );
    }
    return {
      host: dbSync.host,
      database: dbSync.dbname,
      user: dbSync.user,
      password: dbSync.password,
      port: dbSync.port,
    };
  }

  private loadConfig(): BackendConfig {
    const configPath = this.getConfigPath();
    const rawConfig = this.readJsonConfig(configPath);

    const chainDataProvider = this.chainDataProviderName();

    return {
      chainDataProvider,
      // Demanded only when db-sync is the source, so a Koios or Blockfrost
      // deployment starts with no database credentials present at all.
      dbSync: chainDataProvider === 'dbsync' ? this.dbSyncConfig() : null,
      koios: {
        network: this.envString('GOVTOOL_KOIOS_NETWORK', 'mainnet') as
          'mainnet' | 'preprod' | 'preview' | 'guild',
        token: this.envString('GOVTOOL_KOIOS_TOKEN', '') || null,
        baseUrl: this.envString('GOVTOOL_KOIOS_BASE_URL', '') || null,
      },
      blockfrost: {
        network: this.dbSyncNetwork('GOVTOOL_BLOCKFROST_NETWORK'),
        baseUrl: this.envString('GOVTOOL_BLOCKFROST_BASE_URL', ''),
        projectId: this.envString('GOVTOOL_BLOCKFROST_PROJECT_ID', '') || null,
      },
      cacheMaxEntries: this.positiveInteger('GOVTOOL_CACHE_MAX_ENTRIES', 256),
      ipfsGateway: this.envString('IPFS_GATEWAY', ''),
      ipfsProjectId: this.envString('IPFS_PROJECT_ID', ''),
      pinataApiJwt:
        this.envString(
          'GOVTOOL_PINATA_API_JWT',
          rawConfig.pinataapijwt ?? '',
        ) || null,
      metadataServiceUrl:
        this.envString('GOVTOOL_METADATA_SERVICE_URL', '').trim() || null,
      metadataAllowPrivateUrls:
        this.envString('GOVTOOL_METADATA_ALLOW_PRIVATE_URLS', 'false')
          .trim()
          .toLowerCase() === 'true',
      port: this.envNumber('GOVTOOL_PORT', rawConfig.port),
      host: this.envString('GOVTOOL_HOST', rawConfig.host),
      cacheDurationSeconds: this.envNumber(
        'GOVTOOL_CACHE_DURATION_SECONDS',
        rawConfig.cachedurationseconds,
      ),
      drepListCacheDurationSeconds: this.envNumber(
        'GOVTOOL_DREP_LIST_CACHE_DURATION_SECONDS',
        rawConfig.dreplistcachedurationseconds,
      ),
      sentryDsn: this.envString('GOVTOOL_SENTRY_DSN', rawConfig.sentrydsn),
      sentryEnv: this.envString('GOVTOOL_SENTRY_ENV', rawConfig.sentryenv),
    };
  }

  private positiveInteger(name: string, fallback: number): number {
    const value = this.envNumber(name, fallback);
    if (!Number.isSafeInteger(value) || value < 1) {
      throw new Error(`${name} must be a positive safe integer`);
    }
    return value;
  }

  private chainDataProviderName(): ChainDataProviderName {
    const raw = this.envString(
      'GOVTOOL_CHAIN_DATA_PROVIDER',
      'dbsync',
    ).toLowerCase();
    if (!CHAIN_DATA_PROVIDERS.includes(raw as ChainDataProviderName)) {
      throw new Error(
        `GOVTOOL_CHAIN_DATA_PROVIDER must be one of ${CHAIN_DATA_PROVIDERS.join(', ')}; got '${raw}'`,
      );
    }
    return raw as ChainDataProviderName;
  }

  private dbSyncConfig(): DbSyncConfig {
    return {
      host: this.requiredEnvString('GOVTOOL_DBSYNC_HOST'),
      dbname: this.requiredEnvString('GOVTOOL_DBSYNC_DATABASE'),
      user: this.requiredEnvString('GOVTOOL_DBSYNC_USER'),
      password: this.requiredEnvString('GOVTOOL_DBSYNC_PASSWORD'),
      port: this.envNumber('GOVTOOL_DBSYNC_PORT', 5432),
      network: this.dbSyncNetwork(),
    };
  }

  /** A mainnet/preprod/preview setting; also read for Blockfrost's network. */
  private dbSyncNetwork(
    name = 'GOVTOOL_DBSYNC_NETWORK',
  ): DbSyncConfig['network'] {
    const raw = this.envString(name, 'mainnet').toLowerCase();
    if (raw !== 'mainnet' && raw !== 'preprod' && raw !== 'preview') {
      throw new Error(
        `${name} must be mainnet, preprod or preview; got '${raw}'`,
      );
    }
    return raw;
  }

  private getConfigPath(): string {
    const args = process.argv;
    const confiFlagIndex = args.findIndex(
      (arg) => arg === '-c' || arg === '--config',
    );

    if (confiFlagIndex >= 0 && args[confiFlagIndex + 1]) {
      return path.resolve(args[confiFlagIndex + 1]);
    }

    return path.resolve('config.json');
  }

  private readJsonConfig(configPath: string): BackendConfigFile {
    const file = fs.readFileSync(configPath, 'utf8');
    return JSON.parse(file) as BackendConfigFile;
  }

  private envNumber(name: string, fallback: number): number {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
      return fallback;
    }

    const parsed = Number(value);

    if (Number.isNaN(parsed)) {
      throw new Error(`${name} must be a valid number`);
    }

    return parsed;
  }

  private requiredEnvString(name: string): string {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
      throw new Error(`${name} is required`);
    }
    return value;
  }

  private envString(name: string, fallback: string): string {
    const value = process.env[name];

    if (value === undefined || value.trim() === '') {
      return fallback;
    }

    return value;
  }
}
