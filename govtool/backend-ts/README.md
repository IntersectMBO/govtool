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
