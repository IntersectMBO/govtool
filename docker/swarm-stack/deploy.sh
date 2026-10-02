#!/usr/bin/env bash
## Deploy GovTool to Docker Swarm.
##
## Usage: ./deploy.sh <command>
##   prepare           label this node govtool=true (single-node swarm only)
##   secrets           create the db-sync password and Pinata JWT secrets
##   init-metadata-db  create the metadata role and database on the proposal
##                     stack's Postgres, and the secret holding its url
##   check             verify .env, node label, networks and secrets
##   deploy [args]     check, then docker stack deploy (extra args, e.g.
##                     --prune, are passed through)
##   status            list the stack's services and failing tasks
##   rm                remove the stack (secrets, volumes and data stay)
set -euo pipefail

cd "$(dirname "$0")"
STACK_FILE=docker-stack.yml

if [ ! -f .env ]; then
  echo ".env is missing: cp .env.example .env and fill it in" >&2
  exit 1
fi
set -a
# shellcheck disable=SC1091
. ./.env
set +a

: "${STACK_NAME:?set STACK_NAME in .env}"

SECRETS=(dbsync_password pinata_api_jwt metadata_database_url)

die() { echo "error: $*" >&2; exit 1; }

secret_exists() { docker secret inspect "$1" >/dev/null 2>&1; }

# Prompt without echo and create the secret; skip one that already exists,
# since swarm secrets are immutable.
create_secret() {
  local name="${STACK_NAME}_$1" prompt="$2" optional="${3:-}" value
  if secret_exists "$name"; then
    echo "secret $name exists, keeping it"
    return
  fi
  read -r -s -p "$prompt: " value
  echo
  if [ -z "$value" ]; then
    [ -n "$optional" ] || die "$1 is required"
    # Whitespace-only secrets are skipped by the entrypoint, i.e. unset.
    value=" "
  fi
  printf %s "$value" | docker secret create "$name" - >/dev/null
  echo "secret $name created"
}

cmd_prepare() {
  local nodes
  nodes=$(docker node ls -q | wc -l | tr -d ' ')
  if [ "$nodes" -ne 1 ]; then
    echo "This swarm has $nodes nodes; label the one to run GovTool:"
    echo "  docker node update --label-add govtool=true <node>"
    exit 1
  fi
  docker node update --label-add govtool=true "$(docker node ls -q)" >/dev/null
  echo "labelled this node govtool=true"
}

cmd_secrets() {
  create_secret dbsync_password "db-sync Postgres password for ${DBSYNC_POSTGRES_USER:-the db-sync user}"
  create_secret pinata_api_jwt "Pinata API JWT (empty: /ipfs/upload answers 503)" optional
}

cmd_init_metadata_db() {
  local service="${METADATA_DB_SERVICE:-preview-proposal_postgres}"
  local superuser="${METADATA_DB_SUPERUSER:-postgres}"
  local host="${METADATA_DB_HOST:-postgres}"
  local secret="${STACK_NAME}_metadata_database_url"
  local container password

  if secret_exists "$secret"; then
    echo "secret $secret exists; the metadata database is already set up"
    return
  fi

  container=$(docker ps -q \
    --filter "label=com.docker.swarm.service.name=$service" | head -n 1)
  [ -n "$container" ] ||
    die "no running $service container on this node; run this on its node"

  # Hex, so it is URL-safe inside DATABASE_URL and needs no SQL quoting.
  password=$(openssl rand -hex 32)

  # Fed on stdin so the password is not on any command line.
  docker exec -i "$container" psql -v ON_ERROR_STOP=1 -q -U "$superuser" -d postgres <<SQL
\set pw '$password'
SELECT 'CREATE ROLE metadata LOGIN'
  WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'metadata') \gexec
ALTER ROLE metadata LOGIN PASSWORD :'pw';
SELECT 'CREATE DATABASE metadata OWNER metadata'
  WHERE NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'metadata') \gexec
SQL

  printf %s "postgresql://metadata:${password}@${host}:5432/metadata" |
    docker secret create "$secret" - >/dev/null
  echo "metadata role and database ready on $service; secret $secret created"
}

cmd_check() {
  local missing=0 v s n
  for v in BASE_DOMAIN GATEWAY_NETWORK METADATA_DB_NETWORK; do
    [ -n "${!v:-}" ] || { echo "missing in .env: $v" >&2; missing=1; }
  done
  if [ "${CHAIN_DATA_PROVIDER:-dbsync}" = dbsync ]; then
    for v in DBSYNC_POSTGRES_HOST DBSYNC_DATABASE DBSYNC_POSTGRES_USER DBSYNC_NETWORK; do
      [ -n "${!v:-}" ] || { echo "missing in .env: $v" >&2; missing=1; }
    done
  fi
  for n in "$GATEWAY_NETWORK" "$METADATA_DB_NETWORK"; do
    docker network inspect "$n" >/dev/null 2>&1 ||
      { echo "network $n does not exist" >&2; missing=1; }
  done
  for s in "${SECRETS[@]}"; do
    secret_exists "${STACK_NAME}_$s" ||
      { echo "secret ${STACK_NAME}_$s does not exist" >&2; missing=1; }
  done
  [ "$(docker node ls -q --filter node.label=govtool=true | wc -l)" -gt 0 ] ||
    { echo "no node is labelled govtool=true (./deploy.sh prepare)" >&2; missing=1; }
  [ "$missing" -eq 0 ] || die "fix the above, then retry"
  echo "ok: $STACK_NAME on $BASE_DOMAIN, images tagged ${GOVTOOL_TAG:-dev}"
}

cmd_deploy() {
  cmd_check
  docker stack deploy --with-registry-auth --detach=false \
    -c "$STACK_FILE" "$@" "$STACK_NAME"
}

cmd_status() {
  docker stack services "$STACK_NAME"
  docker stack ps "$STACK_NAME" --no-trunc \
    --filter desired-state=running \
    --format '{{.Name}}\t{{.CurrentState}}\t{{.Error}}'
}

case "${1:-}" in
  prepare) cmd_prepare ;;
  secrets) cmd_secrets ;;
  init-metadata-db) cmd_init_metadata_db ;;
  check) cmd_check ;;
  deploy) shift; cmd_deploy "$@" ;;
  status) cmd_status ;;
  rm) docker stack rm "$STACK_NAME" ;;
  *) sed -n 's/^## \{0,1\}//p' "$0"; exit 1 ;;
esac
