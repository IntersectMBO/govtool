export type DbSyncConfig = {
  host: string;
  dbname: string;
  user: string;
  password: string;
  port: number;
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

export type IpfsUploadConfig = {
  perClientLimit: number;
  globalLimit: number;
  windowSeconds: number;
};

export type BackendConfig = {
  dbSync: DbSyncConfig;
  cacheMaxEntries: number;
  ipfsGateway: string;
  ipfsProjectId: string;
  pinataApiJwt: string | null;
  ipfsUpload: IpfsUploadConfig;
  trustProxy: string;
  port: number;
  host: string;
  cacheDurationSeconds: number;
  drepListCacheDurationSeconds: number;
  sentryDsn: string;
  sentryEnv: string;
};
