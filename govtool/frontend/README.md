# GovTool Frontend

## Prerequisites

- [Git](https://git-scm.com/)
- [nvm](https://github.com/nvm-sh/nvm)
- Node.js 22.22.0, as specified by [`.nvmrc`](./.nvmrc) and `package.json`
- npm, using the committed `package-lock.json`

## Local development

Clone the repository and enter the frontend package:

```bash
git clone https://github.com/IntersectMBO/govtool.git
cd govtool/govtool/frontend
```

Install and activate the required Node.js version:

```bash
nvm install
nvm use
```

Create the local environment file and install the locked dependencies:

```bash
cp .env.example .env
npm ci
```

Start the frontend development server:

```bash
npm run dev
```

Vite prints the local URL when it starts, normally `http://localhost:5173`.

### Environment variables

The values copied from `.env.example` are suitable for a local frontend connected to the standard local services:

- `VITE_BASE_URL`: GovTool backend API URL. The local Docker setup uses `http://localhost:9999`.
- `VITE_METADATA_API_URL`: Metadata validation service URL. The local Docker setup uses `http://localhost:3000`.
- `VITE_NETWORK_FLAG`: Cardano network selector; use `0` for a test network and `1` for mainnet.
- `VITE_IS_DEV`: Keep this `true` locally to enable development behavior and skip the production maintenance check.
- `VITE_IPFS_GATEWAY`: Gateway used to load `ipfs://` content.

The following integrations are optional and may remain blank:

- `VITE_SENTRY_DSN`: Sentry error reporting. `VITE_APP_ENV` labels the Sentry environment when a DSN is configured.
- `VITE_CHATWOOT_URL` and `VITE_CHATWOOT_WEBSITE_TOKEN`: Chatwoot feedback widget.
- `VITE_PDF_API_URL`: Proposal discussion service API.
- `VITE_OUTCOMES_API_URL`: Governance outcomes service API.
- `VITE_IPFS_PROJECT_ID`: Project identifier for gateways that require it.

The two feature flags can remain `false` when their companion services are not running:

- `VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED`
- `VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED`

For backend setup, see the [backend README](../backend/README.md). To run the complete service stack, see the [Docker Compose instructions](../../docker/README.md).

## Troubleshooting

### Wrong Node.js version

Run `nvm install` followed by `nvm use` in this directory. Confirm that `node --version` reports `v22.22.0`.

### Missing `.env`

Create it again from the tracked example:

```bash
cp .env.example .env
```

### Port already in use

Vite will normally choose another available port automatically. To choose one explicitly, run `npm run dev -- --port 5174`.

### Backend or API is not running

The page can start without the APIs, but data requests will fail. Start the required services using the [backend instructions](../backend/README.md) or the [Docker Compose stack](../../docker/README.md), then confirm that the URLs in `.env` match those services.

## Contributing

See the repository [contributing guide](../../CONTRIBUTING.md) before submitting a pull request.

### Users

The GovTool application can read and display data from the Cardano chain using REST API.
We distinguish two types of users:

#### without a connected wallet who can:

1. See the governance actions along with their details and the number of votes
<!-- 2. See the list of DReps. -->

#### with connected wallet who can:

1.  See the governance actions along with their details and the number of votes.
2.  Display the wallet status.
3.  Delegate his or her voting power in a form of ADA to dReps.
4.  Register as DRrep or Direct Voter.
5.  Vote for the Governance Actions of his or her choice (if the user is registered).
6.  Create their own Governance Action.
<!-- 7. See the list of DReps from which they can submit their vote. -->
