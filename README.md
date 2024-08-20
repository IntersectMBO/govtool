<p align="center">
  <img width="750" src=".github/images/voltaire-govtool-header.png"/>
</p>

<p align="center">
  <big><strong>Monorepo containing Voltaire GovTool and supporting utilities</strong></big>
</p>

<div align="center">

[![npm](https://img.shields.io/npm/v/npm.svg?style=flat-square)](https://www.npmjs.com/package/npm) [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) [![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

[![Lines of Code](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=ncloc)](https://sonarcloud.io/summary/overall?id=intersect-govtool) [![Coverage](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=coverage)](https://sonarcloud.io/summary/overall?id=intersect-govtool) [![Technical Debt](https://sonarcloud.io/api/project_badges/measure?project=intersect-govtool&metric=sqale_index)](https://sonarcloud.io/summary/overall?id=intersect-govtool)

</div>

<hr/>

## 🌄 Purpose

The Voltaire GovTool enables ada holders to experience the governance features described in [CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md).

### Instances

#### Mainnet

- *Coming soon*

#### SanchoNet

- [sanchogov.tools](https://sanchogov.tools/)

#### Preview Testnet

- [preview.gov.tools](https://preview.gov.tools/)

### Documentation

Learn more; [docs.gov.tools](https://docs.gov.tools/).

## 📍 Navigation

- [Backend](./govtool/backend/README.md)
- [Frontend](./govtool/frontend/README.md)
- [Infrastructure](./infra/terraform/README.md)
- [In repo documentation](./docs/)
- [Tests](./tests/)

### Utilities

- [Governance Action Loader](./governance-action-loader/)

### Backend

GovTool backend implements an API wrapper around an instance of [DB-Sync](https://github.com/IntersectMBO/cardano-db-sync) which interfaces with a [Cardano Node](https://github.com/IntersectMBO/cardano-node).
The API exposes endpoints making the querying of governance related data from DB-Sync straight forward.

### Frontend

GovTool frontend web app communicates with the backend over a REST interface, reading and displaying on-chain governance data.
Frontend is able to connect to Cardano wallets over the [CIP-30](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0030/README.md) and [CIP-95](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0095/README.md) standards.

## 🤝 Contributing

Thanks for considering contributing and helping us on creating GovTool! 😎

Please checkout our [Contributing Documentation](./CONTRIBUTING.md).
