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

It assumes what is already on the server:

- an nginx-proxy gateway (`mesudip/nginx-proxy`, as in
  `tests/test-infrastructure`) on a swarm network, `frontend` by default. It
  routes by the services' `VIRTUAL_HOST` labels and handles TLS.
- the proposal discussion stack's Postgres on an attachable network,
  `preview-proposal_postgres` by default. The metadata service keeps its own
  `metadata` database and role there.
- a db-sync Postgres the backend container can reach at `DBSYNC_POSTGRES_HOST`.

## Files

- `docker-stack.yml`: the stack.
- `.env.example`: every setting; copy to `.env` (gitignored).
- `deploy.sh`: loads `.env` and wraps the swarm commands. `docker stack deploy`
  does not read `.env`, so deploy through the script.

## First deploy

On a manager node, from this folder:

```bash
cp .env.example .env
```

Fill in `.env`; at least `STACK_NAME`, `BASE_DOMAIN`, the `DBSYNC_*` values and
`NETWORK_FLAG`. `DBSYNC_NETWORK` must match the database, or every route
answers 500.

```bash
./deploy.sh prepare
```

Labels the node `govtool=true`. On a multi-node swarm, label the node yourself
with `docker node update --label-add govtool=true <node>`.

```bash
./deploy.sh secrets
```

Asks for the db-sync password and the Pinata JWT (leave the JWT empty to run
without `/ipfs/upload`), and stores them as swarm secrets.

```bash
./deploy.sh init-metadata-db
```

Creates the `metadata` role and database on the proposal stack's Postgres with
a random password, and stores the connection url as a secret. Run it on the
node where that Postgres runs. The metadata service applies its migrations on
start.

```bash
./deploy.sh deploy
```

Checks the settings, node label, networks and secrets, then deploys and waits
for the services to converge. The backend reads a full DRep and proposal
snapshot before it listens, so its first start takes a few minutes.

```bash
./deploy.sh status
```

## Replacing the old preview-govtool stack

The old stack ran the Haskell backend from `config.json`, plus
`metadata-validation`. With `STACK_NAME=preview-govtool`, deploy over it and
drop the services this file no longer defines:

```bash
./deploy.sh deploy --prune
```

Then check the frontend's outcomes pages and remove the old outcomes stack:

```bash
docker stack rm preview-outcome
```

## Updating

Set `GOVTOOL_TAG` in `.env` (`dev`, `test`, `latest`, a `vX.Y.Z` tag or a
commit sha) and run `./deploy.sh deploy` again. Updates start the new task
before stopping the old one and roll back when a task fails within 60 s.
Pinning a sha or version keeps redeploys reproducible; `dev` moves with every
merge to develop.

## Secrets

Swarm secrets are immutable, so `secrets` and `init-metadata-db` never change
an existing one, and a secret in use cannot be removed. To rotate one, stop
the stack first (a short outage):

```bash
./deploy.sh rm
docker secret rm preview-govtool_dbsync_password
./deploy.sh secrets
./deploy.sh deploy
```

Each secret is mounted under `/run/secrets` named after the variable it sets
(`GOVTOOL_DBSYNC_PASSWORD`, `GOVTOOL_PINATA_API_JWT`, `DATABASE_URL`); the
stack's entrypoint exports them before starting the image's own command, so
none appear in the service definition.
