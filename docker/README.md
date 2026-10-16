# GovTool Docker Compose

This folder contains a compose file for running GovTool services locally.

## Prerequisites
- Docker and Docker Compose
- A reachable db-sync Postgres instance

## Configure environment
From the repo root

```bash
cd docker
cp .env.example .env
```
Also create the frontend environment file:

```bash
cp ../govtool/frontend/.env.example ../govtool/frontend/.env
```

Docker Compose loads the frontend container's runtime configuration from `govtool/frontend/.env`. Edit that file to configure the backend URL (metadata validation is served by the backend under `/metadata`), network, and optional frontend integrations.

Fill in the db-sync details and required service URLs in `docker/.env`.

The `govtool-backend` service reads `docker/.env`; the compose file maps its values onto the backend's `GOVTOOL_*` settings. Edit it with real values:
- DBSYNC_POSTGRES_HOST
- DBSYNC_POSTGRES_PORT
- DBSYNC_DATABASE
- DBSYNC_POSTGRES_USER
- DBSYNC_POSTGRES_PASSWORD
- DBSYNC_NETWORK: mainnet, preprod, preview or devnet. It must match the database, or every route answers 500.
- IPFS_GATEWAY
- PDF_API_URL
- PINATA_API_JWT (optional; without it `/ipfs/upload` answers 503)

Every backend setting, with its default, is listed in [`govtool-backend/.env.example`](../govtool/govtool-backend/.env.example).

Validate the resolved Compose configuration before starting services:

```bash
docker compose config
```

## Start services

Option A: build locally (uses Dockerfiles)
```bash 
docker compose up -d --build
```

Option B: use images only (no build)
```bash
docker compose pull
docker compose up -d --no-build
```

## Service endpoints
- Frontend: http://localhost
- Backend API: http://localhost:9999, including governance action records under `/governance-actions` and metadata validation under `/metadata`
- Metadata service and its Postgres (`metadata`, `metadata-db`): internal only, reached by the backend. Set `METADATA_DB_PASSWORD` in `docker/.env` first.
