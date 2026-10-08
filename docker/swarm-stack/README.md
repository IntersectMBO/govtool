# GovTool Swarm stack

One Docker Swarm stack for running GovTool on any server and network, from
the images CI publishes to GHCR. Every environment (dev, preview, mainnet)
deploys the same `docker-stack.yml`; only `.env` and the exported secrets
differ.

| Service    | Image                                           | Reached at                                   |
| ---------- | ----------------------------------------------- | -------------------------------------------- |
| `frontend` | `ghcr.io/intersectmbo/govtool-frontend`         | `https://${BASE_DOMAIN}/`                    |
| `backend`  | `ghcr.io/intersectmbo/govtool-backend`          | `https://${BASE_DOMAIN}/api/`, `/swagger-ui` |
| `pdf`      | `ghcr.io/intersectmbo/govtool-pdf-backend`      | `https://${BASE_DOMAIN}/pdf/`                |
| `metadata` | `ghcr.io/intersectmbo/govtool-metadata-service` | internal only, from the backend              |
| `postgres` | `postgres:16-alpine`                            | internal only: the `metadata` and `pdf` databases |

The backend also serves governance action records (`/api/governance-actions`)
and metadata validation (`/api/metadata`). `pdf` is the proposal discussion
forum backend; the frontend calls it on its own origin, so it needs no CORS
setup or extra DNS name.

## What the server needs

- Docker in swarm mode, and
  [docker-stack](https://github.com/mesudip/docker-stack)
  (`pip install docker-stack`) on a manager node. It resolves the stack's
  secrets from the deploying shell and versions them, so a changed value
  rolls out as a new secret version.
- An nginx-proxy gateway (`mesudip/nginx-proxy`, as in
  `tests/test-infrastructure`) on a swarm network, `frontend` by default. It
  routes by the services' `VIRTUAL_HOST` and handles TLS. Point
  `BASE_DOMAIN`'s DNS at it.
- A chain data source for the backend, chosen by `CHAIN_DATA_PROVIDER`: a
  db-sync Postgres the backend can reach at `DBSYNC_POSTGRES_HOST`
  (`dbsync`, the default), or a Koios or Blockfrost API (`koios`,
  `blockfrost`), which needs no database.

Everything else, including the Postgres for the metadata service and the
forum, is part of the stack.

## Files

- `docker-stack.yml`: the stack.
- `postgres-init.sh`: run by Postgres once, on an empty volume, to create the
  `metadata` and `pdf` roles and databases.
- `.env.example`: every non-secret setting, with defaults; copy it to `.env`
  (gitignored). Secrets are exported in the shell and never stored there.

## What differs per environment

All of it is in `.env`:

| Setting | What it decides |
| --- | --- |
| `STACK_NAME` | the stack's name, e.g. `dev-govtool`, `preview-govtool` |
| `BASE_DOMAIN` | the public domain, e.g. `dev.gov.tools` |
| `APP_ENV` | the environment name shown to Sentry and the frontend |
| `GOVTOOL_TAG` | the image tag: `dev`, `test`, `latest`, `vX.Y.Z` or a commit sha |
| `CHAIN_DATA_PROVIDER` and its `DBSYNC_*`, `KOIOS_*` or `BLOCKFROST_*` | where chain data comes from, and for which network |
| `NETWORK_FLAG` | `0` testnet or `1` mainnet; must match the chain data, or wallets refuse to connect |
| `GATEWAY_NETWORK` | the gateway's swarm network |
| `POSTGRES_NODE_CONSTRAINT` | the node the stack's Postgres stays on, when more than one node is labelled `govtool` |
| `IPFS_*`, `SENTRY_*`, `CHATWOOT_*`, `UMAMI_*` | optional integrations |

`DBSYNC_NETWORK` must match the db-sync database, or every route answers 500.

## First deploy

On a manager node, from this folder:

```bash
cp .env.example .env
```

Fill in `.env` for the environment, as above.

Label the node that runs GovTool. Every service is placed with
`node.labels.govtool == true`, so the stack deploys nothing until a node
carries it:

```bash
docker node update --label-add govtool=true <node>
```

On a single-node swarm `<node>` is the id from `docker node ls -q`. With
more than one node labelled, set `POSTGRES_NODE_CONSTRAINT` (e.g.
`node.hostname == govtool1`) so the database never moves away from its data.

Export the settings, then the secrets. Generate the five marked "once" a
single time, store them somewhere safe, and export the same values on every
later deploy:

```bash
set -a; . ./.env; set +a

# Chain data: the db-sync password with dbsync. With koios, KOIOS_TOKEN
# (optional); with hosted blockfrost, BLOCKFROST_PROJECT_ID.
read -r -s -p "db-sync Postgres password: " DBSYNC_PASSWORD; echo; export DBSYNC_PASSWORD
# Optional: enables /ipfs/upload.
read -r -s -p "Pinata API JWT (empty: none): " PINATA_API_JWT; echo; export PINATA_API_JWT

# Once: the stack's Postgres superuser and the two app roles.
export POSTGRES_PASSWORD="$(openssl rand -hex 32)"
export METADATA_DB_PASSWORD="$(openssl rand -hex 32)"
export PDF_DB_PASSWORD="$(openssl rand -hex 32)"
# Once: the forum's token signing keys. A new value signs every user out.
export PDF_JWT_SECRET="$(openssl rand -hex 32)"
export PDF_REFRESH_SECRET="$(openssl rand -hex 32)"
```

Deploy:

```bash
docker-stack deploy "$STACK_NAME" docker-stack.yml
```

On the first deploy Postgres creates the `metadata` and `pdf` databases, the
metadata service and the forum apply their migrations, and the forum seeds
its lookup tables. The backend reads a full DRep and proposal snapshot
before it listens, so it takes a few minutes. Check with:

```bash
docker stack services "$STACK_NAME"
docker stack ps "$STACK_NAME" --no-trunc --filter desired-state=running
curl -s "https://$BASE_DOMAIN/pdf/health"
```

Every service should show `1/1`.

## The database

The `postgres` service keeps its data in the stack's `postgres` volume, on
the node it runs on. `postgres-init.sh` runs only when that volume is empty:
it creates a `metadata` and a `pdf` role, each owning its own database, with
`METADATA_DB_PASSWORD` and `PDF_DB_PASSWORD`. Each app connects only as its
own role.

The roles keep the passwords they were created with. Exporting a different
value later only breaks the app's connection; to change one, change it in
Postgres first, then deploy with the new value:

```bash
docker exec -it "$(docker ps -q -f name="${STACK_NAME}_postgres")" \
  psql -U postgres -c "ALTER ROLE pdf PASSWORD '<new password>'"
```

Back up the forum's data, which only exists here, with:

```bash
docker exec "$(docker ps -q -f name="${STACK_NAME}_postgres")" \
  pg_dump -U postgres -Fc pdf > "pdf-$(date +%F).dump"
```

The metadata database is a cache of fetched documents and reports; losing
it only means fetching them again.

## Updating

Set `GOVTOOL_TAG` in `.env`, export everything as for the first deploy, and
run `docker-stack deploy` again. The app services start the new task before
stopping the old one and roll back when a task fails within 60 s; Postgres
stops first, so two never share its data directory. Pinning a sha or
version keeps redeploys reproducible; `dev` moves with every merge to
develop.

To drop services from an older version of this stack that this file no
longer defines (such as `metadata-validation`), deploy once with `--prune`:

```bash
docker-stack deploy --prune "$STACK_NAME" docker-stack.yml
```

## Secrets

Only true secrets are Swarm secrets: the chain data credentials, the Pinata
JWT, the Postgres passwords, the two database urls built from them, and the
forum's signing keys. Hostnames, database names, users and urls stay in
`environment:`, where they help debugging and expose nothing sensitive.

Every secret is created on each deploy, whichever provider is chosen. An
unset optional one is stored as a single space, which every app reads as
unset. `POSTGRES_PASSWORD`, `METADATA_DB_PASSWORD`, `PDF_DB_PASSWORD`,
`PDF_JWT_SECRET` and `PDF_REFRESH_SECRET` have no default: without them the
deploy fails before creating anything. The db-sync password is not checked
at deploy time; without it the backend refuses to start, logging
`GOVTOOL_DBSYNC_PASSWORD is required`.

Each app reads a secret from its environment variable first and falls back
to the file at `<NAME>_FILE`, defaulting to the Swarm mount
`/run/secrets/<lowercase name>`. The stack mounts each secret at that
default path, so no secret value appears in the service definition:
`docker service inspect` shows only non-sensitive config. A changed value
and a redeploy create a new secret version; `docker-stack versions
"$STACK_NAME"` lists the history.
