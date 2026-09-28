# GovTool Software Architecture Documentation

**Valid as of: 2026-09-28**

## Overview

GovTool is a decentralized application for [CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md) governance. The [`IntersectMBO/govtool`](https://github.com/IntersectMBO/govtool) repository contains:

- **Frontend** (`govtool/frontend`): React + Vite web app. It talks to the backend and metadata validation services over REST, and to wallets over CIP-30 / CIP-95. It embeds the Proposal Discussion pillar (`@intersect.mbo/pdf-ui`) and the Governance Outcomes pillar (`@intersect.mbo/govtool-outcomes-pillar-ui`), enabled with the `VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED` and `VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED` flags.
- **Backend** (`govtool/backend`): Haskell (Servant) read-only API over cardano-db-sync. This is the service built by CI and deployed today (`ghcr.io/intersectmbo/govtool-backend`).
- **Backend TS** (`govtool/backend-ts`): NestJS (TypeScript) port of the backend. It exposes the same API on port 9999 (Swagger at `/swagger-ui`) and is configured through `VVA_*` environment variables. It is in the repository but not yet used by the published images or deployment manifests.
- **Metadata validation** (`govtool/metadata-validation`): NestJS service (`POST /validate`) that fetches off-chain metadata anchors and checks their hash and format against CIP-100 / CIP-108 / CIP-119.
- **Analytics dashboard** (`govtool/analytics-dashboard`): Next.js internal dashboard. It is not part of the core deployment.

External services, each in its own repository: the Proposal Pillar backend (Strapi + PostgreSQL, [`IntersectMBO/govtool-proposal-pillar`](https://github.com/IntersectMBO/govtool-proposal-pillar)) and the Outcomes Pillar backend (reads db-sync, [`IntersectMBO/govtool-outcomes-pillar`](https://github.com/IntersectMBO/govtool-outcomes-pillar)). Deployment manifests live in [`IntersectMBO/govtool-argo`](https://github.com/IntersectMBO/govtool-argo) (Helm + Argo CD). For local development, use [`docker/docker-compose.yaml`](https://github.com/IntersectMBO/govtool/blob/develop/docker/README.md).

## Frontend

### Technology Stack

- [React](https://react.dev/): The library for web user interfaces.
- [Vite](https://vitejs.dev/): The build tool for modern web development.
- [TypeScript](https://www.typescriptlang.org/): The language for static typing.
- [@mui](https://mui.com/): The library for React components.
- [@tanstack/react-query](https://tanstack.com/query/latest): The library for data fetching.
- [react-router](https://reactrouter.com/): The library for routing.
- [react-hook-form](https://react-hook-form.com/) and [yup](https://github.com/jquense/yup): Forms and validation.
- [i18next](https://www.i18next.com/): Translations.
- [cardano-serialization-lib](https://github.com/Emurgo/cardano-serialization-lib) (`@emurgo/cardano-serialization-lib-asmjs`): Serialization and deserialization of Cardano data structures, and transaction building.
- `@intersect.mbo/pdf-ui` and `@intersect.mbo/govtool-outcomes-pillar-ui`: The embedded pillars.

### Description

Frontend is a React application using Vite as a build tool to enhance development speed and optimize production builds. Frontend interacts with the backend service via REST API and with the Cardano blockchain via cardano-serialization-lib and connected supported wallets (for the list of compatible wallets go [here](../../cardano-govtool/using-govtool/getting-started/compatible-wallets.md)). Transactions are built in the frontend, and signed and submitted by the user's wallet.

### Components

- **Direct voter** - direct voter is a DRep which does not have a metadata and have all the ADA delegated to themselves. This component combines the registration and delegation process into one step. Direct voters are not visible in the DRep directory.
  Direct voters ui components are (no specific file names are provided as they might be continuously updated):

  - Direct voter registration card - UI component visible on the dashboard allowing to navigate to the Direct voter registration form. It displays current Direct voter status and amount of ADA.
  - Direct voter registration form - UI component allowing to register as a Direct voter and delegate all the ADA to themselves. Under the hood metadata anchor is mocked with provided default values.
    Direct voter uses following CSL services:
  - TransactionBuilder - to build the transaction
  - CertificatesBuilder - to build the delegation certificate
  - DRepRegistration - to build the DRep registration certificate

- **DRep** - DRep is a Delegated Representative which has metadata and can receive delegated Voting Power from other ADA holders. DRep registration and delegation are separate processes. DReps are visible in the DRep directory.

  DRep ui components are (no specific file names are provided as they might be continuously updated):

  - DRep registration card - UI component visible on the dashboard allowing to navigate to the DRep registration form. It displays current DRep status and amount of ADA.
  - DRep registration form - UI component allowing to register as a DRep. The form builds CIP-119 metadata (JSON-LD) that the DRep stores at a public URL; GovTool checks the URL and hash before building the registration certificate.
    DRep uses following CSL services:
  - TransactionBuilder - to build the transaction
  - CertificatesBuilder - to build the DRep registration certificate
  - DRepRegistration - to build the DRep registration certificate
  - DRepDeregistration - to build the DRep deregistration certificate
  - DRepUpdate - to build the DRep update certificate

**Note**

- A Direct Voter can become a DRep by providing the metadata.
- A DRep without metadata is shown as a Direct Voter.

- **DRep directory** - DRep directory is a list of all registered DReps. It displays DRep metadata, and amount of Voting Power. DRep directory is visible for all users. DRep directory is the part of delegation pillar.

  DRep directory allows to delegate ADA to DReps. It uses following CSL services:

  - Credential
  - DRep
  - Certificate
  - VoteDelegation

- **GA Submission** - GA Submission is a form allowing to submit a Governance Action. GA Submission is the part of governance pillar. GA Submission uses following CSL services:

  - TransactionBuilder - to build the transaction
  - CertificatesBuilder - to build the governance action certificate
  - GovernanceAction - to build the governance action certificate

  Additionally, GA Submission uses Metadata validation service to validate the metadata.

## Backend

### Technology Stack

- [Haskell](https://www.haskell.org/) (Servant): The current production backend (`govtool/backend`).
- [NestJS](https://nestjs.com/) / TypeScript with `pg`: The backend-ts port (`govtool/backend-ts`), the migration target.
- [cardano-db-sync](https://github.com/IntersectMBO/cardano-db-sync): A component that follows the Cardano chain (via a [cardano-node](https://github.com/IntersectMBO/cardano-node)) and stores blocks and transactions in PostgreSQL.

### Description

The backend is a read-only API. It runs SQL queries (`govtool/backend/sql`, mirrored in `govtool/backend-ts/sql`) against db-sync, caches the results in memory, and returns governance data (DReps, proposals, votes, epoch parameters, transaction status) to the frontend. It does not talk to cardano-node directly and does not store its own data. Transactions are built in the frontend and signed and submitted by the user's wallet.

The backend also offers an IPFS upload endpoint used for pinning vote rationale (via Pinata) when the user chooses "GovTool pins data to IPFS".

## Data Storage

The only persistent store used by the core GovTool services is db-sync's PostgreSQL database, which GovTool reads but does not write to. The Proposal Pillar keeps its off-chain discussion data in its own PostgreSQL database.

## Architecture diagram

**Valid as of: 2024-06-06** (does not yet show backend-ts or the embedded pillars)
![Architecture diagram](./architecture-diagram.png)
