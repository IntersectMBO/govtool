'use strict';

const http = require('node:http');
const { Pool } = require('pg');
const { createHandler } = require('./app');

function required(name) {
  const value = process.env[name];
  if (!value || value.trim() === '') {
    console.error(`${name} is required`);
    process.exit(1);
  }
  return value.trim();
}

const port = Number(process.env.PORT || 3000);
const host = process.env.HOST || '0.0.0.0';
const kuberUrl = required('KUBER_URL').replace(/\/+$/, '');

const db = new Pool({
  host: required('DBSYNC_HOST'),
  port: Number(process.env.DBSYNC_PORT || 5432),
  database: required('DBSYNC_DATABASE'),
  user: required('DBSYNC_USER'),
  password: process.env.DBSYNC_PASSWORD || '',
  max: 5,
  statement_timeout: 10000,
});
db.on('error', (err) => console.error('db-sync pool error:', err.message));

const server = http.createServer(createHandler({ db, kuberUrl }));
server.requestTimeout = 60000;
server.listen(port, host, () => {
  console.log(`blockfrost-shim listening on ${host}:${port}, kuber ${kuberUrl}`);
});

function shutdown() {
  server.close(() => db.end().finally(() => process.exit(0)));
  setTimeout(() => process.exit(0), 5000).unref();
}
process.on('SIGTERM', shutdown);
process.on('SIGINT', shutdown);
