# AGENTS.md - tests

Four suites, none of which is the frontend unit-test suite. That one lives in
govtool/frontend, runs with npx vitest run, and is what gates frontend PRs.

- govtool-frontend/playwright: Playwright + TS E2E against a deployed GovTool
- govtool-backend: Python + pytest against a deployed backend /api
- load-testing: Gatling + Maven, JDK 17, against a deployed /api
- test-metadata-api: Node support service that hosts JSON metadata during tests
- test-infrastructure: compose files and scripts that deploy the test stack

Every suite here needs a running deployment, a real testnet, funded wallets and API
keys. None runs offline, so do not claim one did.

## Which suite gets my change

- A util, hook, component, context or service: govtool/frontend vitest, not here
- A backend endpoint or response shape: govtool-backend
- A user-visible flow such as registration, delegation, voting or submission: the
  matching numbered folder in govtool-frontend/playwright/tests
- Metadata validation rules: govtool/metadata-validation jest tests

## Playwright E2E

```bash
cp .env.example .env && npm install && npx playwright install
npm run test              # all, UI mode; npm run test:headless for CI-style
npm run test:delegation-pillar / :voting-pillar / :proposal-pillar / :outcomes
npm run allure:serve      # report
```

lib/constants/environments.ts is the source of truth for env. Required: HOST_URL, the
deployed frontend under test, with the README listing dev, QA, preview, pre-prod and
mainnet URLs; NETWORK, preview by default, which derives the Blockfrost, faucet and
Kuber URLs and networkId; BLOCKFROST_API_KEY for the matching network; KUBER_API_KEY;
and FAUCET_ADDRESS, FAUCET_PAYMENT_PRIVATE and FAUCET_STAKE_PRIVATE from
npm run generate-faucet-wallet. Optional: TX_TIMEOUT default 240000, TEST_WORKERS,
CARDANOAPI_METADATA_URL, DOCS_URL, CI.

Fund the faucet wallet. An unfunded one produces wallet-bootstrap failures that read
as flaky tests. Timeout is 90s per test, 180s on preview, with zero retries, so a
failure is real - but so is an empty wallet.

lib/pages holds page objects; use those over raw selectors. lib/_mock holds JSON
fixtures. The fake CIP-30/95 wallet is not local: it is the npm dependency
@cardanoapi/cardano-test-wallet, injected by lib/fixtures/loadExtension.ts from
node_modules/@cardanoapi/cardano-test-wallet/script.js, so wallet behaviour changes
mean bumping that package, not editing this repo. The package-wallet script in
package.json is dead - it points at lib/_mock/cardano-test-wallet/types.ts, which no
longer exists. playwright.config.ts wires the setup and teardown
files as Playwright projects with dependencies: wallet bootstrap, then auth setups,
then feature suites, then teardowns. A new suite needing an authenticated wallet
declares that dependency instead of duplicating login. Details in
govtool-frontend/playwright/DEVELOPERS_GUIDE.md and
govtool-frontend/playwright/README.md.

## Backend API tests

```bash
python -m venv venv && source venv/bin/activate && pip install -r requirements.txt
export KUBER_API_URL=… KUBER_API_KEY=… && python3 ./setup.py   # writes test_data.json
export BASE_URL=…/api METRICS_URL=… METRICS_API_SECRET=… && pytest -v
```

setup.py registers the DReps and stakes the tests assume, and needs its main wallet
funded; the address is in govtool-backend/README.md. Add a case to test_cases whenever
you add or change a public backend endpoint.

## Load tests and infrastructure

load-testing needs JDK 17: export API_URL then run via mvnw. Not in PR CI.

test-infrastructure stands up cardano-node, db-sync, govtool and gov-action-loader.
gen-configs.sh renders configs_template and secrets_template into the rendered yml
files. Env files here hold secrets, so check .gitignore before adding one. An
untracked .env-mainet currently exists in this working tree and should not be
committed.
