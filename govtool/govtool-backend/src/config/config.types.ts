export type DbSyncConfig = {
  host: string;
  dbname: string;
  user: string;
  password: string;
  port: number;
  /**
   * The network the database follows; decides stake address prefixes.
   * `devnet` is a custom local testnet (network id 0) whose genesis changes
   * on every run.
   */
  network: 'mainnet' | 'preprod' | 'preview' | 'devnet';
  /**
   * The Shelley genesis file db-sync was started with. Dates epochs on a
   * network with no built-in schedule (devnet); null leaves them undated.
   */
  shelleyGenesisPath: string | null;
  /**
   * The `meta.network_name` the database must report. Null: the network
   * name itself on a public network, any non-public name on devnet.
   */
  networkName: string | null;
};

/**
 * Which implementation of the chain-data contract to construct. Only this
 * setting decides; no service knows which one it is talking to.
 *
 * `fixture` reads a frozen mainnet capture out of the package: no network, no
 * database, no credentials, so the whole stack runs locally and
 * deterministically.
 */
export type ChainDataProviderName =
  'dbsync' | 'koios' | 'blockfrost' | 'fixture';

/** The pinning implementation behind `/ipfs/upload`; see `BackendConfig.pinningProvider`. */
export type PinningProviderName = 'pinata' | 'test';

export type KoiosConfig = {
  network: 'mainnet' | 'preprod' | 'preview' | 'guild';
  /** Optional: the free public tier works without one, at a lower rate limit. */
  token: string | null;
  /** Overrides the network-derived URL, for a self-hosted Koios. */
  baseUrl: string | null;
};

export type BlockfrostConfig = {
  /**
   * Decides stake address prefixes and, with no baseUrl, the hosted URL.
   * `devnet` has no hosted URL, so it needs baseUrl.
   */
  network: 'mainnet' | 'preprod' | 'preview' | 'devnet';
  /** Empty = hosted Blockfrost for `network`; set it for a self-hosted blockfrost-ryo. */
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

/** Fixed-window budgets for the anonymous `/ipfs/upload` route. */
export type IpfsUploadConfig = {
  perClientLimit: number;
  globalLimit: number;
  windowSeconds: number;
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
  /**
   * Which pinning implementation serves `/ipfs/upload`. `pinata` (default)
   * needs `pinataApiJwt`; `test` pins to the local test metadata service at
   * `testPinningUrl`, for isolated test environments only.
   */
  pinningProvider: PinningProviderName;
  ipfsUpload: IpfsUploadConfig;
  /**
   * Express "trust proxy" setting, which resolves the client IP the upload
   * rate limit counts against. Defaults to private-network proxies only.
   */
  trustProxy: string;
  /** Root url of tests/test-metadata-api; required when `pinningProvider` is `test`. */
  testPinningUrl: string | null;
  /**
   * Root url of the private metadata service (govtool-metadata-service). Null when
   * unset: the backend still starts, and the `/metadata/resolve`, `/retry`
   * and `/reports` routes answer 503.
   */
  metadataServiceUrl: string | null;
  /**
   * Local testing only: lets the backend's own metadata fetch reach loopback and
   * private addresses, which it otherwise refuses (D122, amended by D137). Off
   * unless GOVTOOL_METADATA_ALLOW_PRIVATE_URLS is exactly "true".
   */
  metadataAllowPrivateUrls: boolean;
  /**
   * Base url of the proposal discussion (pdf) API, such as
   * http://pdf-backend:1337/api. The governance action route that links an action to
   * its discussion answers 503 without it.
   */
  pdfApiUrl: string | null;
  port: number;
  host: string;
  cacheDurationSeconds: number;
  drepListCacheDurationSeconds: number;
  sentryDsn: string;
  sentryEnv: string;
};
