# GovTool Swarm stack

A Docker Swarm stack for running GovTool on a server, from the images CI
publishes to GHCR. It runs four services:

| Service    | Image                                         | Reached at                       |
| ---------- | --------------------------------------------- | -------------------------------- |
| `frontend` | `ghcr.io/intersectmbo/govtool-frontend`         | `https://${BASE_DOMAIN}/`        |
| `backend`  | `ghcr.io/intersectmbo/govtool-backend`          | `https://${BASE_DOMAIN}/api/`, `/swagger-ui` |
| `metadata` | `ghcr.io/intersectmbo/govtool-metadata-service` | internal only, from the backend  |
| `pdf`      | `ghcr.io/intersectmbo/govtool-pdf-backend`      | `https://${BASE_DOMAIN}/pdf/`    |

The backend also serves governance action records (`/api/governance-actions`)
and metadata validation (`/api/metadata`), so the separate metadata-validation
and outcomes services are no longer needed. `pdf` is the proposal
discussion forum backend that replaces Strapi; the frontend calls it on its
own origin, so it needs no CORS setup or extra DNS name.

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
- a Postgres on an attachable network, `preview-proposal_postgres` by
  default. The metadata service and the pdf backend each keep their own
  database and role there (`metadata`, `pdf`).
- a chain data source for the backend, chosen by `CHAIN_DATA_PROVIDER`: a
  db-sync Postgres the backend container can reach at `DBSYNC_POSTGRES_HOST`
  (`dbsync`, the default), or a Koios or Blockfrost API (`koios`,
  `blockfrost`), which needs no database.

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

Fill in `.env`; at least `STACK_NAME`, `BASE_DOMAIN`, `NETWORK_FLAG` and the
settings of the chosen `CHAIN_DATA_PROVIDER`: the `DBSYNC_*` values for
`dbsync`, `KOIOS_NETWORK` for `koios`, `BLOCKFROST_NETWORK` for `blockfrost`.
`DBSYNC_NETWORK` must match the database, or every route answers 500.

Label the node that will run GovTool: every service is placed with the
constraint `node.labels.govtool == true`, so the stack deploys nothing
until at least one node carries it.

```bash
docker node update --label-add govtool=true <node>
```

On a single-node swarm `<node>` is the id from `docker node ls -q`.

Export the settings, then the chain data provider's secret, prompted
without echo so it never lands in a file. With `dbsync`, the db-sync
password is required:

```bash
set -a; . ./.env; set +a
read -r -s -p "db-sync Postgres password: " DBSYNC_PASSWORD; echo
export DBSYNC_PASSWORD
```

The deploy does not check it: every secret the stack does not need is
created blank, so a missing password deploys and the backend then refuses
to start, logging `GOVTOOL_DBSYNC_PASSWORD is required`.

With `koios`, export `KOIOS_TOKEN` the same way to raise the rate limit, or
leave it unset for the public tier. With `blockfrost`, export
`BLOCKFROST_PROJECT_ID` for hosted Blockfrost; a self-hosted blockfrost-ryo
set in `BLOCKFROST_BASE_URL` usually needs none.

The Pinata JWT is optional with any provider: export `PINATA_API_JWT` the
same way, or leave it unset to run without `/ipfs/upload`.

Create the `metadata` and `pdf` roles and databases once on that Postgres,
each with a generated password. Run this on the node where that Postgres
runs:

```bash
export METADATA_DB_PASSWORD="$(openssl rand -hex 32)"
export PDF_DB_PASSWORD="$(openssl rand -hex 32)"
container=$(docker ps -q \
  --filter "label=com.docker.swarm.service.name=${METADATA_DB_SERVICE:-preview-proposal_postgres}" | head -n 1)
create_db() {
  docker exec -i "$container" psql -v ON_ERROR_STOP=1 -q \
    -U "${METADATA_DB_SUPERUSER:-postgres}" -d postgres <<SQL
\set pw '$2'
SELECT 'CREATE ROLE $1 LOGIN'
  WHERE NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = '$1') \gexec
ALTER ROLE $1 LOGIN PASSWORD :'pw';
SELECT 'CREATE DATABASE $1 OWNER $1'
  WHERE NOT EXISTS (SELECT 1 FROM pg_database WHERE datname = '$1') \gexec
SQL
}
create_db metadata "$METADATA_DB_PASSWORD"
create_db pdf "$PDF_DB_PASSWORD"
```

Rerunning it for a role that exists only resets that role's password, so
on a server where `metadata` is already set up, run just
`create_db pdf "$PDF_DB_PASSWORD"` and export the metadata password you
already have.

Hex keeps the passwords URL-safe inside the connection strings the stack
assembles. Keep this shell: the exported passwords are reused by the deploy
below. The metadata service and the pdf backend apply their migrations on
start; the pdf backend also seeds its lookup tables.

The forum signs its access and refresh tokens with two keys, at least 32
characters each and different. Generate them once and keep them: a new
value signs every forum user out.

```bash
export PDF_JWT_SECRET="$(openssl rand -hex 32)"
export PDF_REFRESH_SECRET="$(openssl rand -hex 32)"
```

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
`metadata-validation`, with the forum on a separate Strapi stack. With `STACK_NAME=preview-govtool`, deploy over it and
drop the services this file no longer defines:

```bash
docker-stack deploy --prune "$STACK_NAME" docker-stack.yml
```

Then check the frontend's governance action history pages and remove the old
outcomes stack:

```bash
docker stack rm preview-outcome
```

## Updating

Set `GOVTOOL_TAG` in `.env` (`dev`, `test`, `latest`, a `vX.Y.Z` tag or a
commit sha), export it, and run `docker-stack deploy` again. Updates start
the new task before stopping the old one and roll back when a task fails
within 60 s. Pinning a sha or version keeps redeploys reproducible; `dev`
moves with every merge to develop.

Changing a secret value (a password, a token, the JWT) and redeploying
creates a new secret version and repoints the services at it; nothing has to
be removed by hand. `docker-stack versions "$STACK_NAME"` lists the history.

## Secrets

Only true secrets are Swarm secrets: the db-sync password, the Koios token,
the Blockfrost project id, the Pinata JWT, the metadata and pdf
`DATABASE_URL`s and the forum's two token signing keys.
Hostnames, database names, users and urls stay in `environment:`, where
they help debugging and expose nothing sensitive.

All of them are created on every deploy, whichever provider is chosen. An
unset optional one is stored as a single space, which every app reads as
unset. `METADATA_DB_PASSWORD`, `PDF_DB_PASSWORD`, `PDF_JWT_SECRET` and
`PDF_REFRESH_SECRET` have no default: without them the deploy fails before
creating anything.

Each app reads a secret from its environment variable first and falls back
to the file at `<NAME>_FILE`, defaulting to the Swarm mount
`/run/secrets/<lowercase name>` (e.g. `GOVTOOL_DBSYNC_PASSWORD` from
`/run/secrets/govtool_dbsync_password`). The stack mounts each secret at
that default path, so no `*_FILE` variable is needed and no secret value
appears in the service definition: `docker service inspect` shows only
non-sensitive config.
