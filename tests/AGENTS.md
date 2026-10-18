# GovTool test suites

No suite here gates a pull request. pytest and the full Playwright suite run
against deployed environments, nightly or on dispatch (test_backend.yml, which
also runs after a test-stack deploy, and test_integration_playwright.yml).
test_integration_devnet.yml runs on dispatch and on push to the branches it
lists, and tests/load-testing runs in no workflow. test:cip179 and
test:governance-action-aggregates run on every push that touches the frontend or
the Playwright suite (code_check_frontend.yml).

## tests/devnet

GovTool and both suites against a local Cardano devnet, with no secrets;
tests/devnet/README.md has the commands, ports and knobs.

Traps:
up.sh deletes the previous devnet and generates a new wallet mnemonic unless
SKIP_CHAIN=1, so wallets from an earlier chain are gone.
Re-running the seed adds another full set of proposals.
up.sh writes .state/playwright.env and .state/pytest.env, which override each
suite's own .env because dotenv never replaces a set variable. Anything they do
not set, such as METRICS_URL, still comes from the suite's .env.
.env.devnet.local is read before .env.devnet and the first value set wins, so
values derived in .env.devnet follow a local override.
run-tests.sh does not read .env.devnet, so its DEVNET_PLAYWRIGHT_* settings take
effect only from the shell. Setting DEVNET_PLAYWRIGHT_FILES replaces the default
filter, which also drops the chatwoot exclusion.
A frontend image build that dies with a bare SIGKILL ran out of memory; the
README's prebuilt.Dockerfile route builds on the host instead.
Specs that open external links still need internet.
Known failures, on devnet and elsewhere, are listed in docs/known-issues.md;
remove an entry when you fix it.

## tests/govtool-backend (pytest)

BASE_URL is required. Kuber comes from KUBER_URL (config.py ignores the older
KUBER_API_URL) plus KUBER_API_KEY; NETWORK only builds the default Kuber and
faucet URLs. config.py runs git rev-parse on import, so run it inside a
checkout.

The DRep and ADA holder test wallets are in the committed test_data.json.
setup.py has its own copy of the keys and never reads or writes that file. The
wallets exist on preview and in the devnet seed but not in the fixture
backend's mainnet capture, so run the suite against a db-sync-backed backend,
never the fixture.

Survey cases are skipped unless RUN_SURVEY_TESTS=1. Newer routes
(/governance-actions, /system/*, /metadata/*) have no cases yet.

## tests/govtool-frontend/playwright

lib/constants/environments.ts loads .env and reads most variables;
TEST_WALLET_MNEMONIC, read in lib/wallet/testWallets.ts, is required by any test
that uses a wallet. .env.example is for a deployed environment and
.env.devnet.example for devnet.

The wallet is in this repository: lib/wallet/pageWallet.ts injects a CIP-30/95
wallet into the page. Test code builds its own transactions with Kuber, and
every signed transaction, the app's included, is submitted through Blockfrost,
so a Blockfrost key is required off devnet (devnet uses blockfrost-shim).

An underfunded faucet (FAUCET_*) shows up as failures that look flaky; the
README gives per-suite amounts.

The file suffix picks the project in playwright.config.ts: .pd, .loggedin.pd,
.ga, .loggedin, .dRep, .delegation and .wallet each have one, cip179/ runs on
desktop and mobile, and anything else runs in independent and mobile, except
walletConnect.spec.ts (desktop only) and governance-actions.aggregates.ui.spec.ts
(its own config). A .tx.spec.ts file matches no project and never runs. Reuse an
existing suffix and the page objects in lib/pages.

retries is 0, so a failure is real unless the faucet is empty. CI=true, set in
.env.example, turns on forbidOnly, TEST_WORKERS, the allure reporter and the
dRep setup dependency. Without it, tests register the shared DReps on first use
(lib/wallet/sharedDReps.ts, under a file lock); --project='dRep setup' does it
up front.

## tests/test-metadata-api

govtool-pinning-test and the devnet metadata bucket run on it, so a change to
its API breaks both.
