# govtool-pdf-backend

The proposal discussion forum (pdf) backend: NestJS, Prisma, Postgres. It
replaces the Strapi backend of govtool-proposal-pillar and serves the pdf-ui
vendored in `../frontend/src/pdf-ui` unchanged, on the same `/api/...` paths
and the Strapi v4 response envelope.

- `SPEC.md`: the decided behaviour, including every deliberate difference
  from Strapi (§13).
- `src/README.md`: the building blocks a resource module uses.
- `../../docs/api/pdf-api.md`: the endpoint index.
- `src/config/config.ts`: every environment variable; `.env.example` mirrors it.

## Run

```bash
docker compose up -d --build
curl http://127.0.0.1:1337/health
```

This runs Postgres (published on 127.0.0.1:5442) and the backend on
127.0.0.1:1337. Migrations and the lookup seed apply on start.
`npm run seed:demo` adds sample proposals and budget discussions. Point the
frontend at it with `VITE_PDF_API_URL=http://127.0.0.1:1337/` in
`frontend/.env.local`. `../docker-compose.fixture.yml` also runs it as part
of the whole local stack.

On the host instead: `cp .env.example .env`, `docker compose up -d db`, then
`npm install && npm run prisma:deploy && npm run start:dev`.

## Test

`npm run verify` runs lint, typecheck, unit tests and build. `npm run test:e2e`
runs the HTTP suite against a `pdf_test` database on the compose Postgres,
logging in with real CIP-8 signatures. It refuses any database whose name
does not end in `_test`.
