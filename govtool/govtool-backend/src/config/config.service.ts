import { Injectable } from '@nestjs/common';
import 'dotenv/config';
import * as fs from 'fs';
import * as path from 'path';

import {
  BackendConfig,
  BackendConfigFile,
  ChainDataProviderName,
  DbSyncConfig,
  PinningProviderName,
} from './config.types';

const CHAIN_DATA_PROVIDERS: ChainDataProviderName[] = [
  'dbsync',
  'koios',
  'blockfrost',
  'fixture',
];

const PINNING_PROVIDERS: PinningProviderName[] = ['pinata', 'test'];

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
        token: this.secretString('GOVTOOL_KOIOS_TOKEN', '') || null,
        baseUrl: this.envString('GOVTOOL_KOIOS_BASE_URL', '') || null,
      },
      blockfrost: {
        network: this.dbSyncNetwork('GOVTOOL_BLOCKFROST_NETWORK'),
        baseUrl: this.envString('GOVTOOL_BLOCKFROST_BASE_URL', ''),
        projectId:
          this.secretString('GOVTOOL_BLOCKFROST_PROJECT_ID', '') || null,
      },
      cacheMaxEntries: this.positiveInteger('GOVTOOL_CACHE_MAX_ENTRIES', 256),
      ipfsGateway: this.envString('IPFS_GATEWAY', ''),
      ipfsProjectId: this.envString('IPFS_PROJECT_ID', ''),
      pinataApiJwt:
        this.secretString(
          'GOVTOOL_PINATA_API_JWT',
          rawConfig.pinataapijwt ?? '',
        ) || null,
      ...this.pinningConfig(),
      ipfsUpload: {
        perClientLimit: this.positiveInteger(
          'GOVTOOL_IPFS_UPLOAD_PER_CLIENT_LIMIT',
          10,
        ),
        globalLimit: this.positiveInteger(
          'GOVTOOL_IPFS_UPLOAD_GLOBAL_LIMIT',
          300,
        ),
        windowSeconds: this.positiveInteger(
          'GOVTOOL_IPFS_UPLOAD_WINDOW_SECONDS',
          3600,
        ),
      },
      trustProxy: this.envString(
        'GOVTOOL_TRUST_PROXY',
        'loopback, linklocal, uniquelocal',
      ),
      metadataServiceUrl:
        this.envString('GOVTOOL_METADATA_SERVICE_URL', '').trim() || null,
      metadataAllowPrivateUrls:
        this.envString('GOVTOOL_METADATA_ALLOW_PRIVATE_URLS', 'false')
          .trim()
          .toLowerCase() === 'true',
      pdfApiUrl: this.httpUrl('GOVTOOL_PDF_API_URL'),
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

  /** An optional http(s) base url, trailing slashes dropped; null when unset. */
  private httpUrl(name: string): string | null {
    const raw = this.envString(name, '').trim();
    if (raw === '') return null;
    let url: URL;
    try {
      url = new URL(raw);
    } catch {
      throw new Error(`${name} must be an http(s) url`);
    }
    if (url.protocol !== 'http:' && url.protocol !== 'https:') {
      throw new Error(`${name} must be an http(s) url`);
    }
    return raw.replace(/\/+$/, '');
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

  /**
   * GOVTOOL_PINNING_PROVIDER: `pinata` (default) or `test`. `test` demands
   * GOVTOOL_TEST_PINNING_URL, an http(s) url of tests/test-metadata-api, and
   * is for isolated test environments only.
   */
  private pinningConfig(): Pick<
    BackendConfig,
    'pinningProvider' | 'testPinningUrl'
  > {
    const raw = this.envString('GOVTOOL_PINNING_PROVIDER', 'pinata')
      .trim()
      .toLowerCase();
    if (!PINNING_PROVIDERS.includes(raw as PinningProviderName)) {
      throw new Error(
        `GOVTOOL_PINNING_PROVIDER must be one of ${PINNING_PROVIDERS.join(', ')}; got '${raw}'`,
      );
    }
    const pinningProvider = raw as PinningProviderName;
    if (pinningProvider !== 'test') {
      return { pinningProvider, testPinningUrl: null };
    }
    const url = this.requiredEnvString('GOVTOOL_TEST_PINNING_URL').trim();
    let protocol: string;
    try {
      protocol = new URL(url).protocol;
    } catch {
      protocol = '';
    }
    if (protocol !== 'http:' && protocol !== 'https:') {
      throw new Error(
        `GOVTOOL_TEST_PINNING_URL must be an http(s) url; got '${url}'`,
      );
    }
    return { pinningProvider, testPinningUrl: url };
  }

  private dbSyncConfig(): DbSyncConfig {
    return {
      host: this.requiredEnvString('GOVTOOL_DBSYNC_HOST'),
      dbname: this.requiredEnvString('GOVTOOL_DBSYNC_DATABASE'),
      user: this.requiredEnvString('GOVTOOL_DBSYNC_USER'),
      password: this.requiredSecretString('GOVTOOL_DBSYNC_PASSWORD'),
      port: this.envNumber('GOVTOOL_DBSYNC_PORT', 5432),
      network: this.dbSyncNetwork(),
      shelleyGenesisPath:
        this.envString('GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH', '').trim() ||
        null,
      networkName:
        this.envString('GOVTOOL_DBSYNC_NETWORK_NAME', '').trim() || null,
    };
  }

  /**
   * A mainnet/preprod/preview/devnet setting; also read for Blockfrost's
   * network. `devnet` is any custom testnet (network id 0).
   */
  private dbSyncNetwork(
    name = 'GOVTOOL_DBSYNC_NETWORK',
  ): DbSyncConfig['network'] {
    const raw = this.envString(name, 'mainnet').toLowerCase();
    if (
      raw !== 'mainnet' &&
      raw !== 'preprod' &&
      raw !== 'preview' &&
      raw !== 'devnet'
    ) {
      throw new Error(
        `${name} must be mainnet, preprod, preview or devnet; got '${raw}'`,
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

  /**
   * A secret: `process.env[name]` wins; when it is missing or blank, read
   * the file at `process.env[name_FILE]`, defaulting to the Swarm secret
   * mount `/run/secrets/<lowercase name>`. A missing file counts as unset,
   * and a whitespace-only file counts as unset, so an optional secret can be
   * created as `" "` while a required one still throws below.
   */
  private secretValue(name: string): string | undefined {
    const direct = process.env[name];
    if (direct !== undefined && direct.trim() !== '') {
      return direct;
    }
    const file =
      process.env[`${name}_FILE`] ?? `/run/secrets/${name.toLowerCase()}`;
    let content: string;
    try {
      content = fs.readFileSync(file, 'utf8');
    } catch {
      return undefined;
    }
    const value = content.replace(/\r?\n$/, '');
    return value.trim() === '' ? undefined : value;
  }

  private secretString(name: string, fallback: string): string {
    return this.secretValue(name) ?? fallback;
  }

  private requiredSecretString(name: string): string {
    const value = this.secretValue(name);
    if (value === undefined) {
      throw new Error(`${name} is required (or set ${name}_FILE)`);
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
