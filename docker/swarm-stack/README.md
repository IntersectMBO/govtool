# GovTool Swarm stack

A Docker Swarm stack for running GovTool on a server, from the images CI
publishes to GHCR. It runs three services:

| Service    | Image                                         | Reached at                       |
| ---------- | --------------------------------------------- | -------------------------------- |
| `frontend` | `ghcr.io/intersectmbo/govtool-frontend`         | `https://${BASE_DOMAIN}/`        |
| `backend`  | `ghcr.io/intersectmbo/govtool-backend`          | `https://${BASE_DOMAIN}/api/`, `/swagger-ui` |
| `metadata` | `ghcr.io/intersectmbo/govtool-metadata-service` | internal only, from the backend  |

The backend also serves the outcomes API (`/api/outcomes`) and metadata
validation (`/api/metadata`), so the separate metadata-validation and outcomes
services are no longer needed.

Deployment is done with
[docker-stack](https://github.com/mesudip/docker-stack)
(`pip install docker-stack`), which resolves the stack's secrets from the
deploying shell's environment and versions them: changed values roll out as
new secret versions, and `versions`/`checkout` inspect and restore earlier
ones.

It assumes what is already on the server:

- an nginx-proxy gateway (`mesudip/nginx-proxy`, as in
  `tests/test-infrastructure`) on a swarm network, `frontend` by default. It
  routes by the services' `VIRTUAL_HOST` labels and handles TLS.
- the proposal discussion stack's Postgres on an attachable network,
  `preview-proposal_postgres` by default. The metadata service keeps its own
  `metadata` database and role there.
- a db-sync Postgres the backend container can reach at `DBSYNC_POSTGRES_HOST`.

## Files

- `docker-stack.yml`: the stack, deployed with `docker-stack deploy`.
- `.env.example`: every non-secret setting; copy to `.env` (gitignored) and
  export it before deploying. Secrets are exported separately and never
  stored in the file.

## First deploy

On a manager node with `docker-stack` installed, from this folder:

```bash
cp .env.example .env
```

Fill in `.env`; at least `STACK_NAME`, `BASE_DOMAIN`, the `DBSYNC_*` values and
`NETWORK_FLAG`. `DBSYNC_NETWORK` must match the database, or every route
answers 500.

Label the node that will run GovTool: every service is placed with the
constraint `node.labels.govtool == true`, so the stack deploys nothing
until at least one node carries it.

```bash
docker node update --label-add govtool=true <node>
```

On a single-node swarm `<node>` is the id from `docker node ls -q`.

Export the settings and the required db-sync password (prompted without
echo, so it never lands in a file; the deploy fails before creating
anything if it is unset or empty):

```bash
set -a; . ./.env; set +a
read -r -s -p "db-sync Postgres password: " DBSYNC_PASSWORD; echo
export DBSYNC_PASSWORD
```

The Pinata JWT is optional: export `PINATA_API_JWT` the same way, or leave
it unset to run without `/ipfs/upload`.

Create the `metadata` role and database once on the proposal stack's
Postgres, with a generated password. Run this on the node where that
Postgres runs:

```bash
export METADATA_DB_PASSWORD="$(openssl rand -hex 32)"
container=$(docker ps -q \
  --filter "label=com.docker.swarm.service.name=${METADATA_DB_SERVICE:-preview-proposal_postgres}" | head -n 1)
docker exec -i "$container" psql -v ON_ERROR_STOP=1 -q \
  -U "${METADATA_DB_SUPERUSER:-postgres}" -d postgres <<SQL
\set pw '$METADATA_DB_PASSWORD'
SELECT 'CREATE ROLE metadata LOGIN'
  WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'metadata') \gexec
ALTER ROLE metadata LOGIN PASSWORD :'pw';
SELECT 'CREATE DATABASE metadata OWNER metadata'
  WHERE NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = 'metadata') \gexec
SQL
```

Hex keeps the password URL-safe inside the connection string the stack
assembles. Keep this shell: the exported password is reused by the deploy
below. The metadata service applies its migrations on start.

Deploy:

```bash
docker-stack deploy "$STACK_NAME" docker-stack.yml
```

The backend reads a full DRep and proposal snapshot before it listens, so
its first start takes a few minutes. Check convergence with:

```bash
docker stack services "$STACK_NAME"
docker stack ps "$STACK_NAME" --no-trunc --filter desired-state=running
```

## Replacing the old preview-govtool stack

The old stack ran the Haskell backend from `config.json`, plus
`metadata-validation`. With `STACK_NAME=preview-govtool`, deploy over it and
drop the services this file no longer defines:

```bash
docker-stack deploy --prune "$STACK_NAME" docker-stack.yml
```

Then check the frontend's outcomes pages and remove the old outcomes stack:

```bash
docker stack rm preview-outcome
```

## Updating

Set `GOVTOOL_TAG` in `.env` (`dev`, `test`, `latest`, a `vX.Y.Z` tag or a
commit sha), export it, and run `docker-stack deploy` again. Updates start
the new task before stopping the old one and roll back when a task fails
within 60 s. Pinning a sha or version keeps redeploys reproducible; `dev`
moves with every merge to develop.

Changing a secret value (a password, the JWT) and redeploying creates a new
secret version and repoints the services at it; nothing has to be removed by
hand. `docker-stack versions "$STACK_NAME"` lists the history.

## Secrets

Only true secrets are Swarm secrets: the db-sync password, the Pinata JWT
and the metadata `DATABASE_URL`. Hostnames, database names, users and urls
stay in `environment:`, where they help debugging and expose nothing
sensitive.

Each app reads a secret from its environment variable first and falls back
to the file at `<NAME>_FILE`, defaulting to the Swarm mount
`/run/secrets/<lowercase name>` (e.g. `GOVTOOL_DBSYNC_PASSWORD` from
`/run/secrets/govtool_dbsync_password`). The stack mounts each secret at
that default path, so no `*_FILE` variable is needed and no secret value
appears in the service definition: `docker service inspect` shows only
non-sensitive config.
