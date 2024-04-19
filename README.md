<p align="center">
  <img width="750" src=".github/images/sanchonet-govtool-header.png"/>
</p>

<p align="center">
  <big><strong>Monorepo containing SanchoNet GovTool and supporting utilities</strong></big>
</p>

<div align="center">

[![npm](https://img.shields.io/npm/v/npm.svg?style=flat-square)](https://www.npmjs.com/package/npm) [![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=flat-square)](http://makeapullrequest.com) [![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

![Statements](https://img.shields.io/badge/statements-29.28%25-red.svg?style=flat) ![Branches](https://img.shields.io/badge/branches-86.39%25-yellow.svg?style=flat) ![Functions](https://img.shields.io/badge/functions-14.28%25-red.svg?style=flat) ![Lines](https://img.shields.io/badge/lines-29.28%25-red.svg?style=flat)

</div>

<hr/>

## 🌄 Purpose

The SanchoNet GovTool enables ada holders to experience some of the governance features described in [CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md) and to test governance features on [SanchoNet](https://sancho.network/) through a guided and straightforward experience.
The SanchoNet GovTool is currently open for beta testing and can be accessed at [sanchogov.tools](https://sanchogov.tools/).

Learn more; [docs.sanchogov.tools](https://docs.sanchogov.tools/).

## 📍 Navigation

- [GovTool Backend](./govtool/backend/README.md)
- [GovTool Frontend](./govtool/frontend/README.md)
- [GovTool Infrastructure](./infra/)
- [Documentation](./docs/)
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

⚠️ We are in the process of improving our contributing documentation, improvements to come.

Please checkout our [Contributing Documentation](./CONTRIBUTING.md).
