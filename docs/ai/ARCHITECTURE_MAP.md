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
@config alias, so use @/types/... and @/config/env. Many feature directories expose files through index.ts barrels. When a directory already uses a barrel, add new public exports to it; otherwise follow the surrounding direct-import pattern.

- App.tsx: the route table. Every route registers here.
- config/env.ts: the only env accessor. Reads window.__ENV__, then import.meta.env.
- consts/paths.ts: PATHS, PDF_PATHS, OUTCOMES_PATHS, USER_PATHS,
  BUDGET_DISCUSSION_PATHS
- consts/queryKeys.ts: QUERY_KEYS and MUTATION_KEYS. Every new key registers here.
- consts/governanceAction/fields.ts: GA form schemas, driving both rendering and
  hashing
- services/API.ts:is the shared Axios instance for the GovTool backend and redirects backend HTTP 500 responses to the error
  page.Metadata validation uses a separate Axios client because it has a separate base URL.
- i18n/locales/en.json: is the only locale and should be the source of new user-facing copy. Some existing hardcoded strings
  remain.
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
- contextProviders.tsx: composes the main application providers. ChatwootProvider is mounted separately in main.tsx, and 
  pending-transaction state is integrated through the wallet context.

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

A NestJS service that validates off-chain metadata. It fetches metadata from a URL,
parses the JSON, accepts a supplied CIP standard or attempts to identify CIP-108 or
CIP-119, validates the required fields for a recognized standard, and compares a
Blake2b-256 hash of the exact fetched content with the submitted hash.

POST /validate is defined in app.controller.ts, orchestration is handled by
app.service.ts, and validation logic lives in utils/getStandard.ts,
utils/validateCIP108body.ts, and utils/validateMetadataStandard.ts.
enums/ValidationError.ts defines the validation statuses consumed by the frontend,
so check frontend usage before renaming or removing one.

Both metadata validation and the frontend contain utils/canonizeJSON.ts. The
metadata-validation copy is currently unused by the production validation flow. The
frontend uses canonicalization for signature verification, not for the metadata hash submitted to POST /validate.

## Environment variables

### Frontend application configuration

The frontend reads configuration from window.__ENV__ when running in a container,
falling back to Vite's import.meta.env values for local development and build-time
configuration.

When adding a frontend runtime variable, update all applicable configuration points:

1. govtool/frontend/.env.example
2. govtool/frontend/src/config/env.ts
3. govtool/frontend/docker-entrypoint.sh
4. The govtool-frontend service in docker/docker-compose.yaml

Active frontend variables:

- VITE_BASE_URL: GovTool backend REST API base URL.
- VITE_METADATA_API_URL: metadata-validation service base URL.
- VITE_PDF_API_URL: Proposal Discussion and Budget Discussion API base URL.
- VITE_OUTCOMES_API_URL: Governance Outcomes API base URL.
- VITE_IPFS_GATEWAY: gateway used to resolve ipfs:// resources.
- VITE_IPFS_PROJECT_ID: optional project identifier sent when accessing the
  configured IPFS gateway.
- VITE_NETWORK_FLAG: Cardano network ID; 0 selects a test network and 1
  selects mainnet.
- VITE_APP_ENV: deployment environment name supplied to services such as Sentry.
- VITE_IS_DEV: enables development behavior, including React Query devtools and
  bypassing production maintenance checks.
- VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: enables the Proposal Discussion
  pillar.
- VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: enables the Governance Outcomes
  pillar.
- VITE_SENTRY_DSN: optional Sentry data source name.
- VITE_CHATWOOT_URL: base URL from which the Chatwoot SDK is loaded.
- VITE_CHATWOOT_WEBSITE_TOKEN: Chatwoot website token used to initialize the
  feedback widget.

### Frontend container and analytics configuration
The frontend container also accepts variables that are handled by
docker-entrypoint.sh and Nginx rather than exposed to React through window.__ENV__:

- UMAMI_URL: base URL of the Umami analytics service.
- UMAMI_WEBSITE_ID: Umami website identifier.
- UMAMI_SSL_VERIFY: controls TLS certificate verification for the Umami proxy;
  defaults to true.
- TRUSTED_PROXY_CIDRS: trusted proxy address ranges used when resolving the
  original client IP. Set it to none to disable real-IP resolution.
- REAL_IP_HEADER: header used to obtain the original client IP; defaults to
  X-Forwarded-For.

Umami is enabled only when both UMAMI_URL and UMAMI_WEBSITE_ID are configured.

### Metadata-validation service

The metadata-validation service reads these variables directly from process.env:

- PORT: HTTP port used by the NestJS service.
- IPFS_GATEWAY: gateway used to resolve ipfs:// metadata URLs.
- IPFS_PROJECT_ID: optional project identifier sent to the configured IPFS
  gateway.

The local template is govtool/metadata-validation/.env.example.

### Backend configuration

The Haskell backend loads configuration from example-config.json, or from the file
supplied with --config / -c. Conferer also supports environment-variable overrides
prefixed with VVA_.

Backend configuration keys:

- dbsyncconfig.host: cardano-db-sync PostgreSQL host.
- dbsyncconfig.dbname: database name.
- dbsyncconfig.user: database user.
- dbsyncconfig.password: database password.
- dbsyncconfig.port: PostgreSQL port.
- port: backend HTTP port; example-config.json uses 9999.
- host: backend bind address.
- cachedurationseconds: default endpoint-cache lifetime in seconds.
- dreplistcachedurationseconds: DRep-list cache lifetime in seconds.
- pinataapijwt: optional Pinata API JWT used by the IPFS upload endpoint.
- sentrydsn: backend Sentry data source name.
- sentryenv: backend Sentry environment name.

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
