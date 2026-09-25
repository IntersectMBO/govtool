# GovTool data layer

Loads for anything under govtool/. Early-stage trial: nothing here is deployed,
and nothing should be described as shipped.

## What it is

GovTool's backend queries db-sync directly, so a db-sync schema change is a
GovTool breaking change. The trial puts a provider-agnostic contract between
the HTTP surface and the data source: one contract package, one package per
source, one per satellite service, and a backend that depends only on the
contract.

## Where the truth is

govtool-data-providers/SPEC.md: the decided state. Where src disagrees, src is
the backlog item.
govtool-data-providers/src: exact shapes.
../docs/api/decisions.md: why. Append-only, numbered D1.. and F1..; later entries beat
earlier ones and name what they amend. New decisions go there as they are made.
../docs/api/README.md: the /api/v1 path surface, the metadata service spec,
what the frontend calls, what each provider serves, and the open questions.
Each package README: what that package serves, omits and costs.
govtool-backend/src/config/config.service.ts: every environment variable;
govtool-backend/.env.example mirrors it.
docker-compose.fixture.yml and docker-compose.koios.yml: local runs, with the
knobs and the expected behaviour in each file's header.

## Packages

govtool-data-providers: the contract. Interfaces only, zero runtime deps.
govtool-provider-fixture: all six components over a committed mainnet capture.
No network, database or credentials; develop and test against this.
govtool-provider-dbsync, -koios, -blockfrost: chain data only.
govtool-pinning-pinata: the pinning contract.
govtool-metadata-http: the metadata contract as a client of
govtool-metadata-service, which is private to the backend.
govtool-backend: backend-ts forked onto the contract; same routes and bodies
plus /system/capabilities, /system/features and four metadata routes.

Edges are file: paths: every provider and client depends on the contract; the
backend depends on the contract, the four chain-data providers, pinning and
metadata-http; providers never depend on each other.

The frontend keeps its own copy of the feature-set type in
frontend/src/models/featureSet.ts because CI builds it from frontend/ alone, so
it cannot take a file: dependency on a sibling package. Anything else it needs
from the contract arrives published or vendored.

## Three backends, one ships

govtool/backend is the Haskell service the deployment compose runs; compatibility
is measured against it. govtool/backend-ts is its TypeScript port, actively
developed, not deployed. govtool-backend is the fork of backend-ts on the
contract. So: a backend-ts bug may already be fixed on develop, and the fork
drifts, so reconcile it against backend-ts before proposing either replaces the
other.

## Build order

file: deps resolve to built dist, not source, so nothing typechecks until its
dependencies are built: contract, then providers and clients, then backend.
Rebuild the contract before touching a provider or the errors make no sense.
A stale dist also lets a package typecheck against the current contract while
implementing an earlier one; it compiles and throws at runtime, so compiling is
not evidence until rebuilt.

Adding a chain-data provider touches four places, and each miss fails
differently: the case in govtool-backend/src/providers/providers.module.ts, the
union in src/config/config.types.ts, the validation in
src/config/config.service.ts, and the file: dependency in package.json. Missing
the config pair rejects the name at startup with a message that looks like a
typo.

## Working on it

First build, from this folder, in this order:

```bash
for p in govtool-data-providers govtool-provider-fixture govtool-provider-dbsync \
         govtool-provider-koios govtool-provider-blockfrost govtool-pinning-pinata \
         govtool-metadata-http govtool-backend; do
  (cd $p && npm install && npm run build)
done
```

The inner loop in any package is npm test (jest in the backend and pinning,
node --test in the providers, both fast and offline), then npm run verify
before claiming it works. A provider's dist is what the backend loads, so
rebuild it before starting the backend.

Seeing a change run, cheapest first:

1. Backend alone, on frozen data, restarting on every save:
   (cd govtool-backend && GOVTOOL_CHAIN_DATA_PROVIDER=fixture npm run start:dev),
   then curl localhost:9999/drep/list, /proposal/list, /network/metrics and
   so on. Nothing else needs to be running.
2. Frontend against that backend: in frontend/, put
   VITE_BASE_URL=http://127.0.0.1:9999 and
   VITE_METADATA_API_URL=http://127.0.0.1:9999/metadata in .env.local, which
   is gitignored and overrides .env (leave .env alone; it points at preview),
   then npm run dev. Vite hot-reloads frontend edits.
3. The whole stack in Docker, including the metadata service and its
   Postgres: docker compose -f docker-compose.fixture.yml up -d --build, then
   http://localhost:8080. The frontend there is the Vite dev server over
   ./frontend, so frontend edits show as saved; backend or metadata edits need
   up -d --build backend (or metadata).
4. Live data: the same with GOVTOOL_CHAIN_DATA_PROVIDER=koios in step 1, or
   docker compose -f docker-compose.koios.yml up -d for the stack.

Frontend checks, in frontend/: npm run tsc, npm run lint, npm test.

The integration suite in ../tests/govtool-backend is pytest against a running
backend: BASE_URL=http://localhost:9999 NETWORK=preview pytest -v, from a
fresh venv made from its requirements.txt. Its test data is registered on
preview, so it is the check for a db-sync-backed run, not for the fixture.

Changing the contract: edit govtool-data-providers/src and SPEC.md together,
npm run build there, then npm run verify in every provider and in
govtool-backend. The compiler is the only thing that finds consumers of a
loosened field.

Changing a provider mapper: npm run verify in that package, then its live
script against a real source (below).

Changing the backend: npm run verify there. test/legacy-shape.spec.ts pins
every response body with toEqual, so an added or dropped key fails it, and
test/capabilities.spec.ts fails when a declared feature stops matching the
code behind it.

Adding a provider: the four places under Build order, then a README in the
package saying what it serves and omits.

Local-run traps: under dbsync, GOVTOOL_DBSYNC_NETWORK must match the database
or every route answers 500. Two backends run as the identical command line
node dist/main.js, so stop one by PID from its cwd, not by pkill on the path.
A stale frontend/node_modules shows as Vite failing to resolve an import;
npm install there fixes it.

## Verify

npm run verify in any package, before claiming it works. It chains what the
package has of format check, lint, typecheck, tests and build, and never
touches the network.

Koios and Blockfrost carry npm run live, db-sync carries
scripts/live-*.mjs; both need a real source and are the only thing that finds
mapping bugs, because a fixture is written by whoever wrote the mapper and
encodes the same misunderstanding (one test asserted a negative balance as
expected). Run them after changing any mapper. Unit tests pin the mapping you
intended; live scripts say whether the source agrees.

Proof the backend serves data rather than compiling:
GOVTOOL_CHAIN_DATA_PROVIDER=fixture GOVTOOL_PORT=9123 node dist/main.js, then
curl localhost:9123/drep/list answers 18 of 30 DReps, the other 12 anonymous
and hidden by the directory rule.

Three method rules from wrong claims: a missing endpoint is not a missing
capability if the value is derivable from required data (the constitution is
derivable from getEnacted on all three providers); a capability claim is about
a deployment, not an API (self-hosted blockfrost-ryo findings reversed on
hosted mainnet); a static grep undercounts usage, so check dynamic indexing
before calling a field unused (protocolParams[key] nearly lost the thresholds).

## Load-bearing rules

Chain data never resolves a URL; it emits anchors and the metadata service
fetches. The one exception is an action title denormalized onto a DRep vote
row, and it does not generalize to names, bios or abstracts.

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
pool1 for pools, stake1 for accounts. The backend translates to and from the
forms the frontend sends, at its edge, and that is where GovTool's existing
wire format lives. Providers report ledger names, lovelace as decimal strings
and propagating errors.

Committee members are identified by the cold credential; hot rotates. A vote
carries only hot, so cold is optional on a vote and unresolved means no voter
info.

No aggregate counter for anything a filtered list's total answers. That is why
there is no metrics resource; the backend's /network/metrics assembles its
thirteen counters from the committee, DRep counts, proposal total and stake
distribution, and reports 0 for the five nothing renders.

The contract package has zero dependencies; a new one means something leaked.
Providers do no caching; the backend owns the cache and warmer. The db-sync
provider owns its SQL; a changed statement needs a test pinning what the
backend relies on.

## Traps

Loosening a contract field breaks every consumer reading it unguarded; after
editing the contract, typecheck every provider and the backend.

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
except the last. govtool-backend/src/common/snapshot.ts does this; use it for
whole-set reads. Random ordering is unpaged: size only, page beyond 1 refused.

Promise-returning methods must reject, not throw synchronously; a notFound()
thrown inside a non-async arrow bypasses .catch().

Casting a test stub to the contract type disables the check that matters; the
backend's typed stub helper exists so a stub cannot be an invalid page.

Unaliased SQL columns (bare encode(), CONCAT(), LOWER()) come back under
duplicate names and a name-based reader gets undefined; alias every computed
column.

Koios rejects request bodies over about 5 KB, a byte limit not an id count;
batch by measured size. An explicit sort on an already-sorted endpoint can make
a provider sort a whole table and never return.

On a public provider the cache warmer logs a full stack now and then and
recovers next pass, because a failed refresh keeps serving the previous
snapshot. One trace is weather; chase it only if it repeats or the live script
fails the same call.

On an HTTP provider the backend takes a while to listen: the warmer awaits a
full DRep and proposal snapshot first. The healthcheck allows for it.

Changing KOIOS_NETWORK without VITE_NETWORK_FLAG (1 mainnet, 0 testnet) shows
as a wallet refusing to connect, not as a config error.

Building the frontend image needs more than Docker Desktop's default memory;
the minifier dies with a bare SIGKILL. The compose files pull the published
image or run the Vite dev server for that reason.

The backend image builds from govtool/, not govtool-backend/, because the file:
paths cannot resolve from a narrower context.

Live data moves; totals and ids in any example will differ.
