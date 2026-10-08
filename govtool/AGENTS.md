# GovTool data layer

Pre-release: v2.1.0-alpha.2 is the first release built on these packages;
describe none of it as shipped to production.

## What it is

The Haskell backend queried db-sync directly, so a db-sync schema change was a
GovTool breaking change. govtool-backend instead puts a provider-agnostic
contract between the HTTP surface and the data source: one contract package,
one package per source, one per satellite service. Its services depend only on
the contract; src/providers/providers.module.ts in govtool-backend is the one
place that names a provider. Compatibility is still measured against the Haskell
backend's responses, which the frontend was written against.

## Where the truth is

govtool/govtool-data-providers/SPEC.md: the decided state. Where src disagrees,
src is the backlog item.
govtool/govtool-data-providers/src: exact shapes.
docs/api/decisions.md: why. Append-only, numbered D1.. and F1..; later entries
beat earlier ones and name what they amend. New decisions go there as they are
made.
docs/api/README.md: an index of the planned /api/v1 surface (rest-api-v1.md,
which also maps every current path), the metadata service spec, the forum
backend's endpoints (pdf-api.md) and the open questions.
Each package README: what that package serves, omits and costs.
govtool/govtool-backend/src/config/config.service.ts: every environment
variable; that package's .env.example mirrors it.
govtool/docker-compose.fixture.yml and govtool/docker-compose.koios.yml: local
runs, with the knobs and the expected behaviour in each file's header.

## Packages

govtool-data-providers: the contract. Types plus two error classes and a few
helpers, with zero runtime dependencies; a new one means something leaked.
govtool-provider-fixture: all six components over a committed mainnet capture.
No network, database or credentials; develop and test against this.
govtool-provider-dbsync, -koios, -blockfrost: chain data only.
govtool-pinning-pinata: the pinning contract.
govtool-pinning-test: the pinning contract over tests/test-metadata-api, for
isolated test runs (GOVTOOL_PINNING_PROVIDER=test).
govtool-metadata-http: the metadata contract as a client of
govtool-metadata-service, which is private to the backend.
govtool-backend: the backend (D151). It serves the legacy routes and bodies plus
/system/capabilities, /system/features, the /metadata routes, the governance
action records under /governance-actions with their supporting routes under
/misc (D162), and /survey/definition (D164). It takes the db-sync connection
from GOVTOOL_DBSYNC_* (the password also from its _FILE path or Swarm secret),
never from config.json.
govtool-pdf-backend: the proposal discussion forum backend, NestJS + Prisma + its
own Postgres database (its own container locally; a database on the Postgres it
shares with the metadata service in docker/swarm-stack, D165, D166). It is
wire-compatible with the Strapi v4 surface the vendored forum UI calls, and
standalone: no file: deps. Its SPEC.md is the decided state; src/README.md maps
the shared building blocks.

Edges are file: paths: every provider and client depends on the contract; the
backend depends on the contract, the four chain-data providers, both pinning
packages and metadata-http; providers never depend on each other.

## Frontend and backend

On any 500 the frontend's API client (govtool/frontend/src/services/API.ts, with
the interceptor App.tsx installs) navigates to the error page, which throws the
user out of a vote or a form. The backend never answers 500 for an expected
condition.

govtool/frontend/src/models/featureSet.ts is a hand-kept copy of the feature set
and the FeatureId union in govtool/govtool-backend/src/system/capabilities.ts.
CI builds the frontend from its own folder, so it cannot take a file: dependency
on a sibling package, and nothing checks that the ids match. Anything else the
frontend needs from the contract arrives published or vendored.

VITE_NETWORK_FLAG (1 mainnet, 0 testnet) must match the network the backend
serves (GOVTOOL_DBSYNC_NETWORK, GOVTOOL_KOIOS_NETWORK or
GOVTOOL_BLOCKFROST_NETWORK; KOIOS_NETWORK in docker-compose.koios.yml and
NETWORK_FLAG in the Swarm stack). A mismatch, or an unset flag, shows as every
wallet being refused as the wrong network, not as a config error.

## Build order

file: deps resolve to built dist, not source, so nothing typechecks until its
dependencies are built: contract, then providers and clients, then backend.
Rebuild the contract before touching a provider or the errors make no sense. A
stale dist also lets a package typecheck against the current contract while
implementing an earlier one; it compiles and throws at runtime, so compiling is
not evidence until rebuilt. The backend loads each provider's dist, so rebuild a
provider before starting the backend.

First build, from govtool/, in this order:

```bash
for p in govtool-data-providers govtool-provider-fixture govtool-provider-dbsync \
         govtool-provider-koios govtool-provider-blockfrost govtool-pinning-pinata \
         govtool-pinning-test govtool-metadata-http govtool-backend; do
  (cd $p && npm install && npm run build)
done
```

## Verify

npm run verify in each package listed above chains what it has of format
check, lint, typecheck, tests (jest in the backend and pinning, node --test in
the providers) and build, offline. CI compiles the contract and providers but
runs only the backend's tests, so a provider change is checked only by its own
npm run verify. govtool-metadata-service has no verify script.

Changing the contract: edit govtool-data-providers/src and SPEC.md together, npm
run build there, then npm run verify in every provider, every client and the
backend. Loosening a field breaks every consumer that reads it unguarded, and
the compiler is the only thing that finds them.

Changing a mapper: npm run verify in that provider, then npm run live against a
real source (db-sync's runs four of its scripts/live-*.mjs; live-surveys.mjs
runs on its own). Only live runs find mapping bugs: unit tests pin the mapping
you intended, and a fixture written by the mapper's author encodes the same
misunderstanding.

Before calling a capability unsupported: check whether required data derives it
(the constitution comes from getEnacted on every provider), test the deployment
you mean rather than the API (self-hosted and hosted Blockfrost differ), and
look for dynamic indexing before calling a field unused (protocolParams[key]).

## Seeing it run

Cheapest first:

1. Backend alone, on frozen data, restarting on every save: in
   govtool/govtool-backend, GOVTOOL_CHAIN_DATA_PROVIDER=fixture npm run
   start:dev, then curl localhost:9999/drep/list, /proposal/list,
   /network/metrics and so on. /drep/list reports a total of 18 from the
   fixture's 30 rows: the two predefined DReps have no CIP-129 id, and the
   directory rule hides 10 anonymous ones.
2. Frontend against that backend: in govtool/frontend, put
   VITE_BASE_URL=http://127.0.0.1:9999 in .env.local, which is gitignored and
   overrides .env, then npm run dev. Metadata validation goes to the same
   backend under /metadata.
3. The whole stack in Docker, with the metadata service, the forum backend and
   their Postgres: docker compose -f govtool/docker-compose.fixture.yml up -d
   --build, then http://localhost:8080. Its frontend is the Vite dev server over
   govtool/frontend, so frontend edits show as saved; backend or metadata edits
   need up -d --build backend (or metadata).
4. Live data: GOVTOOL_CHAIN_DATA_PROVIDER=koios in step 1, or docker compose -f
   govtool/docker-compose.koios.yml up -d for the stack. Live totals and ids
   move.

Traps:
Under dbsync, GOVTOOL_DBSYNC_NETWORK must match the database: /network/info and
the governance action routes that read the network answer 500, and the rest
encode stake addresses and epochs for the wrong network. On a local devnet use
devnet plus GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH, or expiry dates are null
(D142).
On an HTTP provider the backend takes a while to listen: the cache warmer
awaits a full DRep and proposal snapshot first. The Swarm stack's healthcheck
allows 300 s for it.
On a public provider the warmer sometimes logs a full stack trace and recovers
on the next pass, because a failed refresh keeps serving the previous snapshot.
Chase it only if it repeats or the live script fails the same call.
The frontend's production build needs about 8 GB of memory and is OOM-killed
with less, so the compose files here run the Vite dev server instead of
building the frontend image.
The backend image builds from govtool/, not govtool-backend/, because the file:
paths cannot resolve from a narrower context.

## Adding a chain-data provider

Seven places, each failing differently when missed. In govtool/govtool-backend:
1. The case in src/providers/providers.module.ts.
2. The union in src/config/config.types.ts. Miss it and typecheck fails.
3. CHAIN_DATA_PROVIDERS in src/config/config.service.ts. Miss it and startup
   rejects the name with a message that looks like a typo.
4. The file: dependency in package.json.
5. A COPY and build pair in its Dockerfile.
Outside it:
6. A ! line in govtool/.dockerignore, which ignores everything else.
7. The dependency loop in .github/workflows/code_check_backend.yml.
Missing 5 to 7 builds locally and fails in the image or in CI. Then write the
package README: what it serves and omits.

## Load-bearing rules

Chain data never resolves a URL; it emits anchors and the metadata service
fetches. The one exception is an action title denormalized onto a DRep vote
row, and it does not generalize to names, bios or abstracts.

Anchored documents are CIP-100 metadata, which CIP-108 extends for governance
actions and CIP-119 for DRep profiles. POST /metadata/validate checks the hash
and the body, then the fields of the standard the caller names or the document
declares through its CIP108 or CIP119 namespace (src/metadata in
govtool-backend); a document declaring neither gets no field checks.

Availability is the interface: an absent optional method means not supported,
and there is no availability boolean. The provider's declaration carries only
which option values are honoured. This is why entities use optional members,
not a capability table.

Refuse rather than fabricate. A provider that cannot compute a value raises
CAPABILITY_UNSUPPORTED, never 0 or an empty list. Never serve a capability
half-filled; required-ness follows the displayed computation.

Contract types are a floor: a provider may add fields, never narrow. Types hide
extras but JSON.stringify does not, so a consumer forwarding a provider object
serializes only contract fields.

One bech32 id per entity: CIP-129 for DRep, action and committee credentials,
pool1 for pools, stake1 (stake_test1 off mainnet) for accounts. The backend
translates to and from the forms the frontend sends, at its edge, and that is
where GovTool's existing wire format lives. Providers report ledger names,
lovelace as decimal strings and propagating errors.

Committee members are identified by the cold credential; hot rotates. A vote
carries only hot, so cold is optional on a vote and unresolved means no voter
info.

No aggregate counter for anything a filtered list's total answers. That is why
there is no metrics resource; the backend's /network/metrics assembles its
thirteen counters from the committee, DRep counts, proposal total and stake
distribution, and reports 0 for the five nothing renders.

Providers do no caching; the backend owns the cache and warmer. The db-sync
provider owns its SQL; a changed statement needs a test pinning what the
backend relies on.

## Traps

CIP-105 and CIP-129 DRep ids share the drep1 prefix and differ by a header
byte; decode and validate, never prefix-match.

getEnacted is keyed by lineage: UpdateCommittee and NoConfidence share the
committee lineage, and a per-type answer gives a prevGovActionId the ledger
rejects.

Committee membership is assembled from genesis plus every enacted
UpdateCommittee delta plus hot-key auth and cold-key resignation certificates
plus term expiry; it cannot be read off the latest action. The constitution is
the opposite: getEnacted('constitution') then body.anchor.

DRep inactivity is the ledger's expiry epoch, pushed by drepActivity on each
vote or re-registration; do not reconstruct it from vote timestamps.

Paging is 1-based page and size with total optional; no cursors. Walk to total
or a short page; a provider omitting total must never return a short page
except the last. govtool/govtool-backend/src/common/snapshot.ts does this; use
it for whole-set reads. Random ordering is unpaged: size only, page beyond 1
refused.

Promise-returning methods must reject, not throw synchronously; a notFound()
thrown inside a non-async arrow bypasses .catch().

Unaliased SQL columns (bare encode(), CONCAT(), LOWER()) come back under
duplicate names and a name-based reader gets undefined; alias every computed
column.

Koios rejects request bodies over about 5 KB, a byte limit not an id count;
batch by measured size. An explicit sort on an already-sorted endpoint can make
a provider sort a whole table and never return.
