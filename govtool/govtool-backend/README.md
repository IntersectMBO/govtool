# govtool-backend

The GovTool HTTP API. Same routes, same response bodies, same error shapes as
`backend-ts` — but with no database handle of its own.

Every read goes through the Chain Data contract
([`@govtool/data-providers`](../govtool-data-providers)), satisfied here by
[`@govtool/provider-dbsync`](../govtool-provider-dbsync); the upload route goes
through the pinning contract, satisfied by
[`@govtool/pinning-pinata`](../govtool-pinning-pinata). Swapping db-sync for
Koios or Blockfrost is a change to
[src/providers/providers.module.ts](src/providers/providers.module.ts) and
nothing else.

## What this package still owns

The HTTP surface, the legacy response shapes, and the cache. Controllers and
DTOs are **unchanged** from `backend-ts`; so are `ConfigService`,
`CacheService`, `common/hex.ts` and the whole `metadata/` module (it validates
documents over HTTP and never touched the database).

What changed: the nine services that used to hold SQL now map contract types
back to the legacy wire shapes, `src/db` and `src/sql` are gone along with the
`sql/` directory, and two small modules were added —
[src/common/legacy.ts](src/common/legacy.ts) for the narrowings back to legacy
JSON, and [src/common/errors.ts](src/common/errors.ts) for the single place
`ChainDataError` and `PinningError` become HTTP.

## Run

```bash
npm install          # resolves the three sibling packages by path
npm run start
```

They are `file:` dependencies, so each must be built once before this package
typechecks or runs:

```bash
for p in govtool-data-providers govtool-provider-dbsync govtool-pinning-pinata; do
  (cd "../$p" && npm install && npm run build)
done
```

Configuration is unchanged — the same `VVA_*` variables and `config.json`. See
[.env.example](.env.example).

## Docker

The build context is the **parent** directory, because of those path
dependencies:

```bash
docker build -f govtool-backend/Dockerfile ..
# or, which already sets the context:
docker compose up --build
```

## Compatibility

Compatibility is measured against the **Haskell** backend, since that is what
is deployed and what the frontend was written against.

- All 22 routes are present, at the same paths, with the same methods.
- Response bodies are asserted key-for-key in
  [test/legacy-shape.spec.ts](test/legacy-shape.spec.ts) (33 specs, driven by a
  stubbed provider). Those tests use `toEqual`, so an added or dropped key
  fails.
- The db-sync provider runs the legacy SQL unmodified —
  `diff -r ../govtool-provider-dbsync/sql ../backend-ts/sql` is empty.

Deliberate differences from `backend-ts`, all cases where `backend-ts` diverged
from Haskell:

1. **`GET /drep/getVotes/:drepId` works again.** `get-votes.sql` leaves four
   columns unaliased, so `pg` returns `concat`, `encode` and `lower` where
   `backend-ts` reads `gov_action_id`, `drep_id` and `vote`. Six of nine fields
   were `undefined`, and because the lookup map was keyed on `undefined` the
   endpoint returned `[]` for every DRep. The provider reads the statement
   positionally, as Haskell did.

2. **Internal ids are narrowed at the edge, not carried through.** The contract
   keeps db-sync's row ids opaque (`providerId`); the legacy `id` / `txId` /
   `proposalId` fields are reconstructed here, so the wire format is unchanged
   while the data layer stays free of them.

Two legacy behaviours are preserved on purpose even though the provider is
stricter underneath:

- `GET /ada-holder/get-voting-power/:stakeKey` still answers `0` on failure,
  including a database outage. The frontend renders this number directly and
  has no error path for it. The provider itself distinguishes "no rows" from
  "unavailable" for callers that want the difference.
- `GET /proposal/enacted-details` still substitutes `HardForkInitiation` for
  any type other than itself and `ParameterChange`. The provider refuses an
  unanswerable type; the substitution happens in
  [proposal.service.ts](src/proposal/proposal.service.ts).

## Known limitation

`GET /epoch/params` returns db-sync's `epoch_param` row verbatim, as before —
so it is the one route whose body is provider-shaped rather than contract-shaped.
Pointing this backend at a non-db-sync provider would change that body. Every
other route is provider-independent.

## Tests

```bash
npm run verify    # lint, typecheck, unit tests, build
npm run test:e2e  # the two routes that need no data layer
```

`legacy-shape.spec.ts` needs no database. Booting the full `AppModule` does,
because `ProvidersModule` opens a pool on startup — which is why the e2e spec
covers only `/` and `/health`.
