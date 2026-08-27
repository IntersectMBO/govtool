<p align="center">
  <img width="750" src=".github/images/cardano-govtool-header.png"/>
</p>

<p align="center">
  <big><strong>Monorepo containing Cardano GovTool and supporting utilities</strong></big>
</p>

<div align="center">

[![npm](https://img.shields.io/npm/v/npm.svg?style=flat-square)](https://www.npmjs.com/package/npm) [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) [![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

[![Lines of Code](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=ncloc)](https://sonarcloud.io/summary/overall?id=intersect-govtool) [![Coverage](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=coverage)](https://sonarcloud.io/summary/overall?id=intersect-govtool) [![Technical Debt](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=sqale_index)](https://sonarcloud.io/summary/overall?id=intersect-govtool)

</div>

<hr/>

## 🌄 Purpose

The Cardano GovTool enables ada holders to use the governance features described in
[CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md):
register as a DRep, delegate voting power, submit governance actions and vote on them.

### Instances

- Mainnet: [gov.tools](https://gov.tools/)
- Preview testnet: [preview.gov.tools](https://preview.gov.tools/)

### Documentation

- User documentation: [docs.gov.tools](https://docs.gov.tools/cardano-govtool/using-govtool)
- In-repo documentation: [`docs/`](./docs/) - [architecture](./docs/architecture/), [style guides](./docs/style-guides/), [operations](./docs/operations/)
- AI agent guide: [`AGENTS.md`](./AGENTS.md) - orientation, playbooks and conventions for AI coding agents

## 🗂️ Repository layout

| Path | What lives there |
| --- | --- |
| [`govtool/frontend`](./govtool/frontend/) | React + TypeScript + Vite web app behind gov.tools. All UI, routes, forms, wallet connection and transaction building. |
| [`govtool/backend`](./govtool/backend/) | Haskell + Servant read-only REST API over [cardano-db-sync](https://github.com/IntersectMBO/cardano-db-sync). |
| [`govtool/metadata-validation`](./govtool/metadata-validation/) | NestJS service that validates CIP-100/108/119 off-chain metadata. |
| [`govtool/analytics-dashboard`](./govtool/analytics-dashboard/) | Next.js internal usage dashboard, not part of the gov.tools user flow. |
| [`tests`](./tests/) | Playwright end-to-end tests, Python API tests, Gatling load tests, test infrastructure. |
| [`docker`](./docker/) | Docker Compose setup for running the whole stack locally. |
| [`gov-action-loader`](./gov-action-loader/) | Dev utility for bulk-submitting governance actions to a testnet. |

The frontend talks to the backend over REST and to Cardano wallets over the
[CIP-30](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0030/README.md) and
[CIP-95](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0095/README.md) standards.
Every on-chain write is a transaction the frontend builds and the wallet signs; the
backend only reads.

## 🚀 Getting started

Pick the smallest setup that covers the change you want to make.

### Frontend only - the fastest path

No Cardano node and no database needed: run the app locally against an already
deployed backend. This covers most UI work.

```bash
cd govtool/frontend
nvm use          # Node 22.22.0, pinned by .nvmrc
npm ci
cp .env.example .env
# then set VITE_BASE_URL in .env to a deployed backend,
# e.g. https://govtool.cardanoapi.io/api
npm run dev      # http://localhost:5173
```

To connect a wallet you need a browser extension that supports CIP-95, see the
[compatible wallets list](https://docs.gov.tools/cardano-govtool/using-govtool/getting-started/compatible-wallets).
More detail in the [frontend README](./govtool/frontend/README.md).

### 🐳 Whole stack with Docker

Requires Docker and a reachable `cardano-db-sync` Postgres instance.

```bash
cd docker
cp .env.example .env   # fill in DBSYNC_POSTGRES_*, IPFS_GATEWAY, PDF_API_URL
docker compose up -d --build
```

Frontend on port 80, backend on 9999 (Swagger UI at `/swagger-ui`),
metadata-validation on 3000, outcomes on 3001.
See the [Docker Compose README](./docker/README.md) for the config files the stack
expects.

### Backend on its own

Haskell, GHC 9.2.8, needs access to a `cardano-db-sync` Postgres database; a Nix shell
is the supported setup. See the [backend README](./govtool/backend/README.md).

## ✅ Verifying a change

Run the checks for the package you touched, CI runs the same ones.

```bash
# govtool/frontend
npm run lint && npm run tsc && npx vitest run   # note: npm run test is watch mode

# govtool/metadata-validation
npm run lint && npm test

# govtool/backend (inside the nix shell)
cabal build all && pre-commit run --all-files hlint && pre-commit run --all-files stylish-haskell
```

End-to-end, API and load suites live in [`tests/`](./tests/) and run against a deployed
environment rather than your working tree.

## 🤝 Contributing

Thanks for considering contributing and helping us on creating GovTool! 😎

Start with the [Contributing Documentation](./CONTRIBUTING.md). The short version:

- Branch off `develop`, never `main`, and name the branch `type/issue-or-feature-description`.
- Add an entry to [`CHANGELOG.md`](./CHANGELOG.md) under `[Unreleased]` with a link to the issue.
- Open the PR against `develop` and fill in the PR template.

Good first issues are labelled [`🐛 Bug`](https://github.com/IntersectMBO/govtool/issues?q=is%3Aissue+is%3Aopen+label%3A%22%F0%9F%90%9B+Bug%22)
or [`💡 Feature idea`](https://github.com/IntersectMBO/govtool/issues?q=is%3Aissue+is%3Aopen+label%3A%22%F0%9F%92%A1+Feature+idea%22).

## 💬 Support

Questions and setup help: [`SUPPORT.md`](./SUPPORT.md) and
[GitHub Discussions](https://github.com/IntersectMBO/govtool/discussions).
Security vulnerabilities: [`SECURITY.md`](./SECURITY.md).
Everyone participating is expected to follow the [Code of Conduct](./CODE-OF-CONDUCT.md).

## 📄 License

[Apache 2.0](./LICENSE)
