# GovTool data layer

This file sits in govtool/ so it loads whenever anything under it is touched,
which is where every package below lives.

Early-stage planning. Every package described here is a trial implementation,
none is committed to git, and none is deployed. Treat all of it as a spike that
exists to answer design questions, not as production code. Nothing here should
be presented to a reader as shipped.

## What is being tried

GovTool's backend talks to db-sync directly. The experiment is to put a
provider-agnostic contract between the HTTP surface and whatever supplies the
data, so a deployment can choose db-sync, Koios, Blockfrost or something else
without the backend or the frontend knowing.

The shape is one contract package plus one package per data source, and a
backend that depends only on the contract.

## Where things are

All paths are relative to the repository root.

govtool/govtool-data-providers: the contract. Interfaces and types only.
govtool/govtool-provider-dbsync: implements the chain-data contract over db-sync SQL.
govtool/govtool-provider-koios: implements it over the Koios REST API.
govtool/govtool-provider-blockfrost: implements it over a Blockfrost-compatible API.
govtool/govtool-pinning-pinata: implements the pinning contract over Pinata.
govtool/govtool-backend: the HTTP surface, forked from govtool/backend-ts.

Dependency edges, all via file: paths:

- every provider depends on govtool-data-providers
- govtool-backend depends on govtool-data-providers, govtool-provider-dbsync, govtool-pinning-pinata

Nothing depends on a provider except the backend, and providers never depend on
each other.

## Three backends exist, and only one of them ships

This is the single most misread thing in the repository.

govtool/backend is the Haskell service. It is what the deployment compose
builds and runs, and it is still actively maintained on the develop branch.
Compatibility is measured against this, not against the TypeScript port.

govtool/backend-ts is a TypeScript port of it, tracked and under active
development. It is not in the deployment compose.

govtool/govtool-backend is the untracked fork of backend-ts that this
experiment rewired onto the contract. It carries the same HTTP routes and the
same response bodies.

Two consequences. First, a bug found in backend-ts may already be fixed
upstream, so check the develop branch before fixing it again. Second, the fork
drifts: reconcile govtool-backend against backend-ts before proposing that
either replaces the other.

## Build order, and the trap in it

The file: dependencies resolve to each package's built dist, not to its
source. A package therefore cannot typecheck until everything it depends on has
been built at least once. After changing the contract, rebuild it before
touching any provider, or the provider will typecheck against a stale contract
and the errors will make no sense.

```bash
cd govtool/govtool-data-providers && npm install && npm run build
```

Then the same install and build in each provider, then in govtool-backend.

## Verifying

Every package has the same entry point, and it is what to run before claiming
anything works:

```bash
npm run verify
```

It chains format check, lint, typecheck, unit tests and build. It never touches
the network.

Providers that talk to an HTTP API also carry live scripts, deliberately
excluded from verify because they need network access and their results move
with chain state:

- npm run smoke asks whether each route answers at all
- npm run conformance asks whether the answers are valid instances of the
  contract, which is the one that finds bugs

Run the live scripts after changing any mapper. Configure them through
environment variables rather than editing them; read the script for the names it
accepts.

## Running a local instance

govtool/docker-compose.koios.yml builds and runs the backend and the frontend
against Koios. Koios is a public API, so this needs no database, no Cardano
node and no credentials, which makes it the fastest way to exercise the whole
stack.

```bash
cd govtool
docker compose -f docker-compose.koios.yml up -d
```

That builds the backend and pulls the published frontend image. After changing
backend code, rebuild only that service with: up -d --build backend

Frontend on port 8080, backend on 9999, both overridable with FRONTEND_PORT
and BACKEND_PORT. Other knobs, all optional: KOIOS_NETWORK (default mainnet),
KOIOS_TOKEN to raise the rate limit, KOIOS_BASE_URL for a self-hosted Koios,
PINATA_API_JWT to make the upload route work.

Changing KOIOS_NETWORK means changing VITE_NETWORK_FLAG with it: 1 for mainnet,
0 for any testnet. The frontend compares that number against the network id the
wallet reports, so a mismatch surfaces as a wallet that refuses to connect,
which reads like a wallet problem rather than a configuration one.

Bring up only the backend by naming it: up -d backend. That is enough for
API work.

The frontend service declares both an image and a build, the same way
docker/docker-compose.yaml does, so passing --build builds it from
govtool/frontend instead of pulling. That needs more memory than Docker Desktop allocates by
default: at 7.8 GB, with nothing else running, the minifier is killed part way
through chunk rendering, and the error says only SIGKILL, which reads like a
crash rather than a memory limit. CI builds it on larger runners. Pulling is the
default because nothing in this compose file changes frontend source. Pin a
different published build with FRONTEND_TAG.

The provider is chosen at runtime, not build time, so the same image serves any
of them. Set VVA_CHAINDATAPROVIDER to dbsync, koios or blockfrost. Under dbsync
the VVA_DBSYNCCONFIG_ group becomes required; under the other two it is not
read at all, and its absence is what demonstrates the backend has no remaining
database dependency.

Things to expect, none of which are faults in the setup:

The backend takes roughly thirty seconds to accept connections. The cache
warmer runs in the module init hook and awaits a full DRep and proposal
snapshot before the server listens, and on a paging HTTP provider that is many
requests. The container healthcheck already allows for it.

/network/metrics answers 501 under Koios and Blockfrost. Six of its thirteen
counters are collection-wide aggregates neither can compute. The frontend
degrades rather than breaking: the committee threshold bar reads zero. This is
the single gap between Koios and a fully working GovTool, and the frontend
reads only three of those thirteen fields, so closing it is a narrowing
exercise rather than new data. See docs/api/frontend-needs-audit.md.

Live data moves. Totals and identifiers in any example will differ from what
you see.

The backend image builds from the govtool directory rather than from
govtool-backend, because the backend depends on the contract and the providers
by relative path and a narrower context cannot resolve them. The frontend image
is the stock one and builds from govtool/frontend, unchanged.

Keep it that way. Three CI workflows build govtool/frontend/Dockerfile with
govtool/frontend as the context, so the frontend cannot take a file: dependency
on a sibling package: npm install inside that build has no way to reach it, and
the failure appears as an unresolved import during the Vite build rather than
as a missing file. Anything the frontend needs from the contract has to arrive
as a published package or be vendored.

The frontend image is configured at run time by govtool/frontend/docker-entrypoint.sh,
not at build time, so the VITE_ values are environment variables on the service
and changing one needs only a restart.

## Testing doctrine

Fixture-based unit tests do not catch provider mapping bugs. Every real defect
found in this experiment so far passed a green unit suite, because the fixture
is written by whoever wrote the mapper and encodes the same misunderstanding. In
one case a test asserted a negative account balance as the expected value.

What does find them:

1. The live conformance script, which walks real responses and checks the
   contract's own rules: required fields present, lovelace a decimal string and
   never negative, identity fields non-empty, page and envelope shapes complete.
2. A capability cross-check, which exercises every route and compares the
   outcome against the provider's own declared capability document. This is how
   a route declared supported while hanging forever gets caught.

So: unit tests pin the mapping you intended, live scripts tell you whether the
source agrees. Neither replaces the other.

## Invariants worth not breaking

The contract package has zero runtime dependencies and contains no
implementation, no transport and no framework. Anything that fetches, queries,
caches or wires dependency injection belongs in a provider. Adding a dependency
to it is the signal that something has leaked.

The db-sync provider's SQL is frozen: it is byte-identical to the SQL the
existing backend ships, and it is verifiable with diff -r. When a contract
field cannot be filled from those statements, the contract loosens; the SQL does
not grow. That is what keeps the provider's results identical to the deployed
API's.

Providers do no caching. The backend owns caching, and it already has a cache
and a cache warmer.

The legacy wire format lives in the backend, not in providers. Providers report
honestly: lovelace as decimal strings, ledger names, errors that propagate. The
backend narrows to the legacy JSON shape at the edge. Keep it that way, or every
provider ends up carrying GovTool's history.

## Traps that have already cost time

Loosening a contract field is safe for a provider and breaks every consumer that
reads the field unguarded. After editing the contract, typecheck every provider
and the backend before moving on. The compiler is the only thing that will find
those sites.

A list call with no limit does not mean "everything" on an HTTP provider. Koios
caps a page, Blockfrost caps it much lower, and both report the cap honestly
through a cursor. A consumer that builds a snapshot must follow the cursor to
exhaustion, or it silently shows a fraction of the data with no error anywhere.
The backend has a helper for this; use it rather than calling a list method
directly when you need the whole set.

Casting a test stub to the contract type switches off the check that matters
most. The backend's stub helper is deliberately typed so a stub cannot be an
invalid page or envelope. Reintroducing a cast there re-opens dozens of silent
violations at once.

SQL columns that are not aliased are read by position, not by name. A SELECT
list with bare encode(...), CONCAT(...) or LOWER(...) produces duplicate
and meaningless column names, and a name-based reader silently gets undefined
for each one. Alias every computed column, or read positionally.

Koios rejects any request body over roughly five kilobytes, which is a limit on
bytes and not on the number of identifiers. Batch by measured size.

Sending an explicit sort to an endpoint that is already sorted can make a
provider sort an entire table and never return. Check whether the default order
is already what you want.

A public provider fails occasionally for no reason you can reproduce. Running
against Koios, the cache warmer logs a full error stack every so often and
recovers on its next pass twenty seconds later, because a refresh only replaces
the cached snapshot after the provider call resolves, so a failed pass keeps
serving the previous one. Treat a single stack trace there as weather, not as a
bug; chase it only if it repeats across several passes or the same call fails
from the conformance script.

## Design notes and audits

These live under docs/api and are opt-in reading. The contract drafts there
predate the packages and their TypeScript files are now duplicated by, and
behind, the contract package source. Treat the packages as the source of truth
and the drafts as history.

docs/api/README.md orients the three components and the route map.
docs/api/provider-gap-report.md compares what each provider can serve, and
records which contract fields had to be loosened and why.
docs/api/frontend-needs-audit.md measures what the current frontend actually
calls, against the spec and against each provider.
docs/api/multi-provider-support.md is the design for the capability layer.

## Configuration

The backend reads the same environment variables the existing TypeScript backend
reads: a VVA_DBSYNCCONFIG_ group for the database connection, VVA_HOST and
VVA_PORT for the listener, cache duration settings, a Pinata credential and
Sentry settings. Read src/config/config.service.ts for the current list rather
than copying one from a document.

Live provider scripts take their own variables for endpoint, credential and
sample identifiers. Read the script.

Never commit a populated env file. Each package gitignores .env.

## Open questions

Whether govtool-backend replaces backend-ts, or whether the provider layer is
folded into backend-ts as a change on top of it. The second is likely less
disruptive while backend-ts is under active development.

Whether the provider packages should move under this folder. They are siblings
today, and moving them would break every file: path and the Docker build
context.

Whether the frozen SQL should be taken from the Haskell backend rather than from
backend-ts, given that Haskell is what ships. The two sets are reported to be
the same statements modulo placeholder syntax, which has not been verified here.
