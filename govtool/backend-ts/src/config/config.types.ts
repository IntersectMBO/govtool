export type DbSyncConfig = {
    host: string;
    dbname: string;
    user: string;
    password: string;
    port: number;
};

export type BackendConfigFile = {
    dbsyncconfig: DbSyncConfig;
    pinataapijwt?: string | null;
    port: number;
    host: string;
    cachedurationseconds: number;
    dreplistcachedurationseconds: number;
    sentrydsn: string;
    sentryenv: string;
}

export type BackendConfig = {
  dbSync: DbSyncConfig;
  pinataApiJwt: string | null;
  port: number;
  host: string;
  cacheDurationSeconds: number;
  drepListCacheDurationSeconds: number;
  sentryDsn: string;
  sentryEnv: string;
};