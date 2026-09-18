export type DbSyncConfig = {
  host: string;
  dbname: string;
  user: string;
  password: string;
  port: number;
};

/**
 * Which implementation of the chain-data contract to construct. Only this
 * setting decides; no service knows which one it is talking to.
 */
export type ChainDataProviderName = 'dbsync' | 'koios' | 'blockfrost';

export type KoiosConfig = {
  network: 'mainnet' | 'preprod' | 'preview' | 'guild';
  /** Optional: the free public tier works without one, at a lower rate limit. */
  token: string | null;
  /** Overrides the network-derived URL, for a self-hosted Koios. */
  baseUrl: string | null;
};

export type BlockfrostConfig = {
  baseUrl: string;
  /** Optional: a self-hosted blockfrost-ryo usually needs no credential. */
  projectId: string | null;
};

export type BackendConfigFile = {
  pinataapijwt?: string | null;
  port: number;
  host: string;
  cachedurationseconds: number;
  dreplistcachedurationseconds: number;
  sentrydsn: string;
  sentryenv: string;
};

export type BackendConfig = {
  chainDataProvider: ChainDataProviderName;
  /**
   * Null unless `chainDataProvider` is 'dbsync'. The credentials are only
   * demanded when db-sync is the configured source, so a Koios or Blockfrost
   * deployment needs no database at all.
   */
  dbSync: DbSyncConfig | null;
  koios: KoiosConfig;
  blockfrost: BlockfrostConfig;
  cacheMaxEntries: number;
  ipfsGateway: string;
  ipfsProjectId: string;
  pinataApiJwt: string | null;
  port: number;
  host: string;
  cacheDurationSeconds: number;
  drepListCacheDurationSeconds: number;
  sentryDsn: string;
  sentryEnv: string;
};
