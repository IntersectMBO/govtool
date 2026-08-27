# Architecture map

Orientation only. Conventions and traps are in the per-package AGENTS.md; recipes in
docs/ai/PLAYBOOKS.md. Paths whose purpose is obvious from the name are omitted.

## Topology

frontend to wallet extension, CIP-30/95: all chain writes happen here
frontend to backend, REST /api/*: backend is read-only
frontend to metadata-validation: POST /validate
frontend to outcomes-pillar-backend and pdf/budget API: external, other repos
backend to cardano-db-sync Postgres: read-only SQL, TTL-cached
backend to Pinata: IPFS pin, the one backend write
cardano-node to cardano-db-sync: follows the chain, not run by us

The backend never writes to chain. Every state change is a transaction the frontend
builds with cardano-serialization-lib and the wallet signs.

## govtool/frontend

Vite aliases: @ for src, plus @pages @consts @context @hooks @models @services @utils
@mock, and @atoms @molecules @organisms for src/components/*. There is no @types or
@config alias, so use @/types/... and @/config/env. Every folder has an index.ts
barrel and new files must be added to it.

- App.tsx: the route table. Every route registers here.
- config/env.ts: the only env accessor. Reads window.__ENV__, then import.meta.env.
- consts/paths.ts: PATHS, PDF_PATHS, OUTCOMES_PATHS, USER_PATHS,
  BUDGET_DISCUSSION_PATHS
- consts/queryKeys.ts: QUERY_KEYS and MUTATION_KEYS. Every new key registers here.
- consts/governanceAction/fields.ts: GA form schemas, driving both rendering and
  hashing
- services/API.ts: the single axios instance, with the 500-to-error-page interceptor
- i18n/locales/en.json: all user-facing copy, about 925 lines, the only locale
- components: atomic design. atoms have no GovTool dependencies, molecules compose
  atoms without fetching, organisms are feature blocks that may use contexts and hooks.

Contexts in src/context:

- wallet.tsx: about 1600 lines. CIP-30/95 connection plus every buildXCert and
  buildXGovernanceAction, funnelling into buildSignSubmitConwayCertTx. The chain-write
  boundary.
- appContext.tsx: protocol phase. isInBootstrapPhase, isFullGovernance,
  isAppInitializing.
- featureFlag.tsx: the VITE_IS_X_ENABLED flags, and phase-derived visibility for which
  vote totals show for which GA type
- pendingTransaction: in-flight txs, so the UI can poll /transaction/status/:txId
- governanceAction.tsx: GA list and detail state
- contextProviders.tsx composes all of them. modal, snackbar, pagination and
  dataActionsBar are UI plumbing; adaHandle, usersnap and proposalDiscussion are
  integrations.

## govtool/backend

Layers, outermost first: route in src/VVA/API.hs, handler in the same file, the
domain function in the matching src/VVA module using withPool plus a query, then
sql/*.sql.

Handlers return API types from API/Types.hs; domain modules return internal types from
Types.hs. The explicit mapping between them keeps the public OpenAPI schema decoupled
from db-sync's shape. Do not shortcut it.

- app/Main.hs: config, connection pool, CacheEnv, CORS, Sentry, error-to-status map,
  Swagger mount
- src/VVA/API.hs: the VVAApi route type and every handler
- src/VVA/Types.hs: internal types, the App monad, AppError, CacheEnv
- src/VVA/Pool.hs: withPool, borrow a connection
- src/VVA/Cache.hs: cacheRequest, the only caching primitive
- src/VVA/Ipfs.hs: Pinata upload, the only write path
- sql/views.sql: shared views the other queries build on
- Domain modules: DRep, AdaHolder, Proposal, Epoch, Transaction, Network, Account
- Config.hs uses Conferer; CommandLine.hs defines -c/--config, start-app, show-config

Route groups. Read src/VVA/API.hs line 59, or /swagger-ui, for the authoritative list:
/drep/* for list, info, getVotes, get-voting-power and voting-power-list;
/ada-holder/* for get-current-delegation and get-voting-power; /proposal/* for list,
get and enacted-details; /epoch/params; /transaction/status;
/network/metrics, /network/info, /network/total-stake; /account/:stakeKey;
/ipfs/upload; and /throw500 as a deliberate error probe.

Error to status, in handleErrors in app/Main.hs: ValidationError 400, NotFoundError
404, CriticalError and InternalError 500, AppIpfsError OtherIpfsError 400, other
AppIpfsError 503.

CacheEnv is built once in app/Main.hs, each field a Data.Cache with a TTL from config.
cachedurationseconds defaults to 20s, dreplistcachedurationseconds to 600s.

Pool is 2 stripes of 60 connections with a 1s idle timeout; Warp has a 300s request
timeout and 60s graceful shutdown. Long queries are a real failure mode, so push work
into SQL and cache the result rather than holding a connection.

## govtool/metadata-validation

NestJS. One job: fetch metadata at a URL, canonize the JSON-LD, hash it, validate the
body against a CIP standard. POST /validate is in app.controller.ts, orchestration in
app.service.ts, rules in utils/getStandard.ts, utils/validateCIP108body.ts and
utils/validateMetadataStandard.ts. enums/ValidationError.ts holds the error codes the
frontend switches on, so grep the frontend before renaming one.

utils/canonizeJSON.ts is duplicated at govtool/frontend/src/utils/canonizeJSON.ts and
must behave identically: the frontend hashes metadata before submission and this
service re-hashes it. Change one, change both.

## Environment variables

Frontend. .env.example, src/config/env.ts and docker-entrypoint.sh must all agree.

- VITE_BASE_URL: backend REST base URL
- VITE_METADATA_API_URL: metadata-validation service
- VITE_PDF_API_URL, VITE_OUTCOMES_API_URL: pillar APIs
- VITE_IPFS_GATEWAY, VITE_IPFS_PROJECT_ID: IPFS reads
- VITE_NETWORK_FLAG: Cardano network id, 0 is testnet
- VITE_APP_ENV, VITE_IS_DEV: environment banners and dev affordances
- VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED,
  VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: pillar flags
- VITE_SENTRY_DSN, VITE_GTM_ID, VITE_USERSNAP_SPACE_API_KEY

Backend. example-config.json, or the same keys as env vars via Conferer:
dbsyncconfig with host, dbname, user, password and port; port 9999; host;
cachedurationseconds; dreplistcachedurationseconds; pinataapijwt; sentrydsn;
sentryenv.

## CI gates, in .github/workflows

- code_check_frontend.yml: on push touching govtool/frontend. Runs npm run test,
  npm run lint and npm run tsc as parallel jobs.
- code_check_backend.yml: on push touching govtool/backend. hlint then
  stylish-haskell via pre-commit.
- pr.yaml: on PR to develop, test, staging or main. hadolint per Dockerfile, optional
  lint.sh and unit-test.sh per package, docker build, Dockle image scan.
- test_storybook.yml: Storybook must build
- test_integration_playwright.yml and test_backend.yml: E2E and Python API tests
  against a deployment
- frontend_sonar_scan.yml, lighthouse.yml: SonarCloud, Lighthouse
- update-govtool-version.yml: manual dispatch, bumps every version. Never by hand.
