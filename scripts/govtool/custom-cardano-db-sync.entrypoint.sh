#!/bin/sh
echo "Custom Cardano DB Sync entrypoint"
set -euo pipefail
mkdir -p -m 1777 /tmp
mkdir -p /configuration
CARDANO_NODE_SOCKET_PATH=/node-ipc/node.socket
CARDANO_DB_SYNC_CONFIG_PATH=/configuration/db-sync-config.json

# set pgpass file
echo "-> Generating PGPASS file"
SECRET_DIR=/run/secrets
POSTGRES_DB=${POSTGRES_DB:-$(< ${SECRET_DIR}/postgres_db)}
POSTGRES_USER=${POSTGRES_USER:-$(< ${SECRET_DIR}/postgres_user)}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD:-$(< ${SECRET_DIR}/postgres_password)}
echo "${POSTGRES_HOST}:${POSTGRES_PORT}:${POSTGRES_DB}:${POSTGRES_USER}:${POSTGRES_PASSWORD}" > /configuration/pgpass
chmod 0600 /configuration/pgpass
export PGPASSFILE=/configuration/pgpass

# wait for cardano node to start
echo -n "-> Waiting for $CARDANO_NODE_SOCKET_PATH"
until [ -S "$CARDANO_NODE_SOCKET_PATH" ]; do
  echo -n "."
  sleep 10
done
echo

# find schema directory
echo "-> Finding schema directory"
SCHEMA_DIR=$(find /nix/store -type d -name '*-schema')

echo "-> Running Cardano DB Sync"
exec cardano-db-sync \
    --config "$CARDANO_DB_SYNC_CONFIG_PATH" \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --schema-dir "$SCHEMA_DIR" \
    --state-dir /state \
    $@

echo "-> Cleaning up"
