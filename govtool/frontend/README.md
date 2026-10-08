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
- `VITE_NETWORK_FLAG`: Cardano network selector; use `0` for a test network and `1` for mainnet.
- `VITE_IS_DEV`: Keep this `true` locally to enable development behavior and skip the production maintenance check. Any non-empty value, `false` included, turns it on; leave it empty to turn it off.
- `VITE_IPFS_GATEWAY`: Gateway used to load `ipfs://` content.

The following integrations are optional and may remain blank:

- `VITE_SENTRY_DSN`: Sentry error reporting. `VITE_APP_ENV` labels the Sentry environment when a DSN is configured.
- `VITE_CHATWOOT_URL` and `VITE_CHATWOOT_WEBSITE_TOKEN`: Chatwoot feedback widget.
- `VITE_PDF_API_URL`: Proposal discussion service API.
- `VITE_IPFS_PROJECT_ID`: Project identifier for gateways that require it.
- `VITE_IS_CIP179_ENABLED`: CIP-179 surveys on governance actions and votes. On when unset; any value other than `true` turns it off. The surveys are also hidden when the backend reports `survey.linkedVoting` unavailable.

`VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED` can remain `false` when the proposal discussion service is not running. Turning it on also needs `VITE_PDF_API_URL`, the forum backend's origin, such as `http://localhost:1337`.

Governance action history uses the GovTool backend configured by `VITE_BASE_URL`. Its pages and navigation do not depend on an environment flag. The voting panel consumes the action's `vote_aggregates`, displaying each supported voter group independently without requesting network metrics. An unsupported applicable group has a direct provider-support message and no pass/fail indicator.

For backend setup, see the [backend README](../govtool-backend/README.md). To run the complete service stack, see the [Docker Compose instructions](../../docker/README.md).

## Checks

CI runs `npm run tsc`, `npm run lint` and `npm test` on pushes that change the frontend. Locally, run:

```bash
npm run tsc
npm run lint
npx vitest run
```

`npm test` runs the same tests, but stays in watch mode in a terminal. Each test run rewrites the tracked `junit-report.xml`; restore it with `git checkout -- junit-report.xml` before committing.

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

The page can start without the APIs, but data requests will fail. Start the required services using the [backend instructions](../govtool-backend/README.md) or the [Docker Compose stack](../../docker/README.md), then confirm that the URLs in `.env` match those services.

## Contributing

See the repository [contributing guide](../../CONTRIBUTING.md) before submitting a pull request.

### Users

The GovTool application can read and display data from the Cardano chain using REST API.
We distinguish two types of users:

#### without a connected wallet who can:

1. See the governance actions along with their details and the number of votes.
2. Browse the DRep directory.
3. Read the governance action history, the 2025 budget proposals archive and, when enabled, the proposal discussions.

#### with connected wallet who can:

1.  See the governance actions along with their details and the number of votes.
2.  Display the wallet status.
3.  Delegate his or her voting power in a form of ADA to dReps.
4.  Register as DRrep or Direct Voter.
5.  Vote for the Governance Actions of his or her choice (if the user is registered).
6.  Create their own Governance Action.
<!-- 7. See the list of DReps from which they can submit their vote. -->
