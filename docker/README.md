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
> [!NOTE]
> Update the backend config file used by the `govtool-backend` service in [`example-config.json`](../govtool/backend/example-config.json) before starting the stack.

Also fill in the db-sync details and required URLs in the outcomes env file. Create env files for the frontend and metadata-validation services from their examples if they are not already present.

Note: The `.env.example` in this folder is for the outcomes service only.

Edit `docker/.env` with real values:
- DBSYNC_POSTGRES_HOST
- DBSYNC_POSTGRES_PORT
- DBSYNC_DATABASE
- DBSYNC_POSTGRES_USER
- DBSYNC_POSTGRES_PASSWORD
- IPFS_GATEWAY
- PDF_API_URL

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
- Backend API: http://localhost:9999
- Metadata validation: http://localhost:3000
- Outcomes API: http://localhost:3001

