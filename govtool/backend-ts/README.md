# GovTool Backend TS

NestJS GovTool backend.

## Setup

Install dependencies:

```bash
npm install
```

Copy the environment example:

```bash
cp .env.example .env
```

Edit `config.json` for different configuration:

```json
{
  "port": 9999,
  "host": "0.0.0.0",
  "cachedurationseconds": 20,
  "dreplistcachedurationseconds": 600,
  "sentryenv": "dev"
}
```
## Start Locally

```bash
npm run start
```

or:

```bash
nest start
```

The backend runs on:

```txt
http://localhost:9999
```

## Start With Docker

```bash
docker-compose up --build
```

## Metadata and cache configuration

Set `IPFS_GATEWAY` to a public HTTP(S) gateway base URL (including `/ipfs` if required). Set `IPFS_PROJECT_ID` only for gateways that require a `project_id` header. These variables are forwarded by Docker Compose; keep credentials in deployment secrets.

Metadata requests reject non-public destinations and redirects, time out after 10 seconds, and accept at most 1 MiB. Blocked destinations return `URL_BLOCKED`.

`VVA_CACHEMAXENTRIES` bounds the shared cache to 256 entries by default, evicting the least recently used entry. Stale entries remain available for background refresh while retained in the cache.
