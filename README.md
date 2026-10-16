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

The Cardano GovTool enables ada holders to use the governance features described in [CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md): register as a DRep, delegate voting power, submit governance actions and vote on them.

### Instances

#### Mainnet

- [gov.tools](https://gov.tools/)

#### Preview Testnet

- [preview.gov.tools](https://preview.gov.tools/)

### Documentation

Learn more; [docs.gov.tools](https://docs.gov.tools/cardano-govtool/using-govtool).

Working on the code with an AI coding agent: [`AGENTS.md`](./AGENTS.md) at the root, and in `govtool`, `govtool/frontend`, `govtool/govtool-backend` and `tests`, is written for it.

## 📍 Navigation

- [Backend](./govtool/govtool-backend/README.md)
- [Frontend](./govtool/frontend/README.md)
- [Documentation (docs.gov.tools source)](./docs/)
- [Tests](./tests/)

### Utilities

- [Governance Action Loader](./gov-action-loader/)

### Backend

GovTool backend is a NestJS service that serves governance data over REST.
It reads chain data through a provider: [DB-Sync](https://github.com/IntersectMBO/cardano-db-sync) following a [Cardano Node](https://github.com/IntersectMBO/cardano-node), Koios, Blockfrost, or a frozen mainnet capture for local development.

### Frontend

GovTool frontend web app communicates with the backend over a REST interface, reading and displaying on-chain governance data.
Frontend is able to connect to Cardano wallets over the [CIP-30](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0030/README.md) and [CIP-95](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0095/README.md) standards.

## 🐳 Running locally with docker

This repository includes a Docker Compose setup for running GovTool services locally.
For local setup instructions, see the [Docker Compose README](docker/README.md).

To run without a Cardano node or DB-Sync, [`govtool/docker-compose.fixture.yml`](./govtool/docker-compose.fixture.yml) runs the backend on frozen mainnet data, with the frontend, metadata service and forum backend; its header lists the ports.
[`tests/devnet`](./tests/devnet/) runs the whole stack on a local Cardano devnet, for tests that submit transactions.

## 🤝 Contributing

Thanks for considering contributing and helping us on creating GovTool! 😎

Please checkout our [Contributing Documentation](./CONTRIBUTING.md).

## 💬 Support

Questions and setup help: [`SUPPORT.md`](./SUPPORT.md).
Security vulnerabilities: follow [`SECURITY.md`](./SECURITY.md) rather than opening a public issue.
Everyone participating is expected to follow the [Code of Conduct](./CODE-OF-CONDUCT.md).

## 📄 License

[Apache 2.0](./LICENSE)
