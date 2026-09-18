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
        `VVA_CHAINDATAPROVIDER is '${this.config.chainDataProvider}', so no db-sync connection is configured`,
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
        network: this.envString('VVA_KOIOSNETWORK', 'mainnet') as
          'mainnet' | 'preprod' | 'preview' | 'guild',
        token: this.envString('VVA_KOIOSTOKEN', '') || null,
        baseUrl: this.envString('VVA_KOIOSBASEURL', '') || null,
      },
      blockfrost: {
        baseUrl: this.envString('VVA_BLOCKFROSTBASEURL', ''),
        projectId: this.envString('VVA_BLOCKFROSTPROJECTID', '') || null,
      },
      cacheMaxEntries: this.positiveInteger('VVA_CACHEMAXENTRIES', 256),
      ipfsGateway: this.envString('IPFS_GATEWAY', ''),
      ipfsProjectId: this.envString('IPFS_PROJECT_ID', ''),
      pinataApiJwt:
        this.envString('VVA_PINATAAPIJWT', rawConfig.pinataapijwt ?? '') ||
        null,
      port: this.envNumber('VVA_PORT', rawConfig.port),
      host: this.envString('VVA_HOST', rawConfig.host),
      cacheDurationSeconds: this.envNumber(
        'VVA_CACHEDURATIONSECONDS',
        rawConfig.cachedurationseconds,
      ),
      drepListCacheDurationSeconds: this.envNumber(
        'VVA_DREPLISTCACHEDURATIONSECONDS',
        rawConfig.dreplistcachedurationseconds,
      ),
      sentryDsn: this.envString('VVA_SENTRYDSN', rawConfig.sentrydsn),
      sentryEnv: this.envString('VVA_SENTRYENV', rawConfig.sentryenv),
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
    const raw = this.envString('VVA_CHAINDATAPROVIDER', 'dbsync').toLowerCase();
    if (!CHAIN_DATA_PROVIDERS.includes(raw as ChainDataProviderName)) {
      throw new Error(
        `VVA_CHAINDATAPROVIDER must be one of ${CHAIN_DATA_PROVIDERS.join(', ')}; got '${raw}'`,
      );
    }
    return raw as ChainDataProviderName;
  }

  private dbSyncConfig(): DbSyncConfig {
    return {
      host: this.requiredEnvString('VVA_DBSYNCCONFIG_HOST'),
      dbname: this.requiredEnvString('VVA_DBSYNCCONFIG_DBNAME'),
      user: this.requiredEnvString('VVA_DBSYNCCONFIG_USER'),
      password: this.requiredEnvString('VVA_DBSYNCCONFIG_PASSWORD'),
      port: this.envNumber('VVA_DBSYNCCONFIG_PORT', 5432),
    };
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
