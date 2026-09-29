# Devnet test environment

Runs GovTool and its integration suites against a local Cardano devnet. Once
images are pulled or built, nothing reaches a public network, hosted Kuber,
Blockfrost, a faucet, Pinata or an IPFS gateway, and no secrets are needed.
CI: `.github/workflows/test_integration_devnet.yml` (push to `dev` and
`draft/govtool-provider-layer`, manual).

- Chain: adaup (`cardano devnet up --docker`) runs cardano-node 11.0.1
  (magic 42, protocol 10), Kuber, db-sync 13.7 with its Postgres and an
  anchor file server on the Docker network `adaup-devnet`.
- App (`docker-compose.yml`, all ports on loopback): govtool-backend on the
  db-sync provider (`GOVTOOL_DBSYNC_NETWORK=devnet`, genesis from adaup's
  config volume; serves `/outcomes` too), the metadata service and its
  Postgres, the metadata bucket (`tests/test-metadata-api`: anchor uploads,
  IPFS pinning via `GOVTOOL_PINNING_PROVIDER=test`, and the `/ipfs/<cid>`
  gateway for the backend, metadata service, db-sync and browser), the pdf
  backend and its Postgres, the frontend image, and `blockfrost-shim`.
  Chatwoot, Sentry and analytics are off.
- `blockfrost-shim/` answers the three Blockfrost calls the suites make:
  `POST /v0/tx/submit` (to Kuber), `GET /v0/accounts/{stake}` and
  `GET /v0/governance/dreps/{id}` (from db-sync). `npm test` there.

## Use

```sh
pip install "adaup>=0.3.0"   # the `cardano` CLI
tests/devnet/up.sh                      # chain, faucet, app, seed, env files
tests/devnet/run-tests.sh pytest        # PYTHON=<venv python> if needed
tests/devnet/run-tests.sh playwright    # extra args go to playwright
tests/devnet/run-tests.sh playwright --last-failed   # re-run failures only
tests/devnet/down.sh                    # everything, volumes included
```

`up.sh` steps: a fresh devnet (the previous one is removed); the faucet
(`scripts/faucet-env.js` derives `FAUCET_*` from adaup's faucet key, adds a
stake key and moves `FAUCET_FUND_ADA`, default 100M ADA, to the base
address); the app stack, built and healthy; `seed.sh`; then
`.state/playwright.env` and `.state/pytest.env` (mode 600; the run scripts
export them, overriding each suite's own `.env`). Skip steps with
`SKIP_CHAIN=1`, `SKIP_FAUCET=1`, `SKIP_SEED=1`; `DEVNET_COMPOSE_BUILD=0`
starts existing images without rebuilding. Step timings go to
`.state/timings.txt`.

`seed.sh`: fixture anchors into the bucket; `cardano devnet smoke` twice
(round 1 ratifies `DEVNET_SEED_RATIFY` plus extra treasury withdrawals and
waits for enactment, for outcomes data; round 2 ratifies nothing, so one proposal of every type stays live);
`scripts/seed-pytest-wallets.sh` registers the backend suite's fixed DReps
and ADA holders from `tests/govtool-backend/test_data.json`; then waits for
the backend to list every type and for DRep voting power (next epoch).

## Settings

All in `.env.devnet`; local overrides in `.env.devnet.local` (gitignored,
read first, so derived URLs follow it). Useful knobs:

- `ADAUP_CARDANO`: path to adaup's `cardano` CLI.
- Timings (adaup): `ADAUP_DEVNET_SLOT_LENGTH` 0.2 s,
  `ADAUP_DEVNET_EPOCH_LENGTH` 300 slots (60 s epochs),
  `ADAUP_DEVNET_LIVE_SECONDS` 172800: proposal lifetime and DRep activity
  (2880 epochs, 48 h). It must outlast the whole run, and the frontend shows
  expiry as a date, so seeded proposals must expire on a later day (4H).
- Ports: `FRONTEND_PORT` 8080, `BACKEND_PORT` 9999, `PDF_PORT` 1337,
  `BLOCKFROST_SHIM_PORT` 3100, `METADATA_BUCKET_PORT` 3001; adaup's Kuber
  8081, Postgres 5433, anchors 8090.
- `FRONTEND_DOCKERFILE`: `Dockerfile` builds from source (needs about 8 GB
  of Docker memory); `../../tests/devnet/frontend/prebuilt.Dockerfile`
  serves a host build (`cd govtool/frontend && npm ci && npm run build`).
- `DEVNET_SEED_ACTIONS` / `DEVNET_SEED_RATIFY`, `DEVNET_SEED_EXTRA_TREASURY`
  (10 more enacted withdrawals: outcomes need a second page), `FAUCET_FUND_ADA`,
  `DEVNET_TEST_WORKERS` (4), `DEVNET_TX_TIMEOUT` (120000 ms).
- Playwright selection: `DEVNET_PLAYWRIGHT_PROJECTS`,
  `DEVNET_PLAYWRIGHT_FILES`, `DEVNET_PLAYWRIGHT_GREP_INVERT`.

The bucket URL (`http://metadata-bucket.localhost:<port>`) must work from
the host and the containers: it is a network alias in Docker and loopback on
the host (macOS and systemd-resolved map `*.localhost`; otherwise add it to
`/etc/hosts`). On macOS `up.sh` also publishes it on `[::1]`.

## Excluded

Playwright runs every project, the proposal discussion forum and `mobile`
included; the pdf backend in the stack serves the forum.

- `10-feedback/chatwoot.spec.ts`: Chatwoot is disabled.
- pytest survey tests: opt-in upstream (`RUN_SURVEY_TESTS=1`).

## Workarounds in the suites

- Wallet connect first requests `<protocol>//<hostname>/is-maintenance-mode-on`
  without the page's port, which fails off port 80 and blocks every connect;
  `lib/wallet/pageWallet.ts` answers it (`false`) in the page.
- `registeredDRepWallet()` registers with CIP-119 metadata unless given an
  anchor (without it the app treats the wallet as a direct voter); 2F and
  2W pre-register their ADA holders' stake keys (fresh HD wallets are
  unregistered, and the app reads registration only on connect).
- pytest `test_epoch_boundary` dates epochs as `epochLength × slotLength`
  (devnet slots are 0.2 s).
- The backend's DRep-list cache is 20 s here (600 s in `config.json`, sized
  for day-long epochs).

## Still external

Only links and optional assets: Google Fonts in the frontend's
`index.html` (falls back to system fonts), docs.gov.tools and other links
(tests that open them need internet), ada handle lookups (handle.me), and
cexplorer links.
