{
    "dbsyncconfig" : {
        "host"     : "postgres",
        "dbname"   : "<DBSYNC_POSTGRES_DB>",
        "user"     : "<DBSYNC_POSTGRES_USER>",
        "password" : "<DBSYNC_POSTGRES_PASSWORD>",
        "port"     : 5432
      },
    "port" : 9876,
    "host" : "0.0.0.0",
    "cachedurationseconds": 20,
    "sentrydsn": "<SENTRY_DSN>",
    "sentryenv": "<SENTRY_ENV>",
    "metadatavalidationhost": "http://metadata-validation",
    "metadatavalidationport": "3000",
    "metadatavalidationmaxconcurrentrequests": 10,
    "redisconfig" : {
        "host"    : "http://redis",
        "port"    : 8094,
        "password": "<REDIS_PASSWORD>"
    },
    "websocketlifetimeseconds": 60
}
