# govtool-backend

The GovTool backend: the HTTP API on the data-layer contract. It replaced the
Haskell backend (`govtool/backend`) and its TypeScript port
(`govtool/backend-ts`), both since removed, and keeps their routes, error
shapes and response bodies except where noted under
[Known limitations](#known-limitations). It holds no database handle of its
own.

Every chain read goes through the chain-data contract
([`@govtool/data-providers`](../govtool-data-providers)), satisfied by
whichever provider `GOVTOOL_CHAIN_DATA_PROVIDER` names:

| Value        | Package                                                       | Needs                                        |
| ------------ | ------------------------------------------------------------- | -------------------------------------------- |
| `fixture`    | [`@govtool/provider-fixture`](../govtool-provider-fixture)    | nothing: a frozen mainnet capture in the package |
| `dbsync`     | [`@govtool/provider-dbsync`](../govtool-provider-dbsync)      | the `GOVTOOL_DBSYNC_*` connection group      |
| `koios`      | [`@govtool/provider-koios`](../govtool-provider-koios)        | nothing; `GOVTOOL_KOIOS_TOKEN` raises the rate limit |
| `blockfrost` | [`@govtool/provider-blockfrost`](../govtool-provider-blockfrost) | `GOVTOOL_BLOCKFROST_PROJECT_ID` for hosted Blockfrost |

The upload route goes through the pinning contract, satisfied by
[`@govtool/pinning-pinata`](../govtool-pinning-pinata) when
`GOVTOOL_PINATA_API_JWT` is set. Metadata goes through the metadata contract,
satisfied by [`@govtool/metadata-http`](../govtool-metadata-http) talking to
[`govtool-metadata-service`](../govtool-metadata-service) when
`GOVTOOL_METADATA_SERVICE_URL` is set. Both are optional: without them the
backend starts, every read route works, `/ipfs/upload` and the metadata routes
answer `503`, and document-derived fields are `null`.

[src/providers/providers.module.ts](src/providers/providers.module.ts) is the
only place that knows a provider exists. Adding a source is a case there, the
name in `src/config/config.types.ts` and `src/config/config.service.ts`, and a
`file:` dependency in `package.json`.

## What this package owns

The HTTP surface, the response shapes the frontend reads, and the cache.
Controllers and DTOs keep the legacy API's. The services map contract types to
those shapes; [src/common/legacy.ts](src/common/legacy.ts) holds the
narrowings, [src/common/legacy-ids.ts](src/common/legacy-ids.ts) the
identifier translation, [src/common/legacy-network.ts](src/common/legacy-network.ts)
the epoch-to-time schedule, and [src/common/errors.ts](src/common/errors.ts)
the single place a `ChainDataError`, `PinningError` or metadata fault becomes
HTTP.

Beyond the legacy routes it serves `GET /system/capabilities` (the
provider's own declaration), `GET /system/features` (this backend's feature
set, which the frontend reads at boot to decide which controls to render; see
[src/system/capabilities.ts](src/system/capabilities.ts)) and the four
metadata routes below.

## Run

The sibling packages are `file:` dependencies, so each must be built once
before this package typechecks or runs:

```bash
for p in govtool-data-providers govtool-provider-fixture govtool-provider-dbsync \
         govtool-provider-koios govtool-provider-blockfrost govtool-pinning-pinata \
         govtool-pinning-test govtool-metadata-http; do
  (cd "../$p" && npm install && npm run build)
done
npm install
GOVTOOL_CHAIN_DATA_PROVIDER=fixture npm run start:dev
```

Configuration is `config.json` plus `GOVTOOL_`-prefixed environment variables;
[.env.example](.env.example) lists them and
[src/config/config.service.ts](src/config/config.service.ts) is the source of
truth.

## Upload

`POST /ipfs/upload` is anonymous, so it accepts only what GovTool pins:

- a `text/plain` body (anything else is `415`) holding a CIP-100 JSON-LD
  document, with `@context`, `hashAlgorithm: blake2b-256` and a `body` object
  (anything else is `400`, before the pinning service is contacted);
- at most `GOVTOOL_IPFS_UPLOAD_PER_CLIENT_LIMIT` uploads per client IP and
  `GOVTOOL_IPFS_UPLOAD_GLOBAL_LIMIT` in all per
  `GOVTOOL_IPFS_UPLOAD_WINDOW_SECONDS`, then `429` with `retryAfterSeconds`.
  The client IP is resolved through `GOVTOOL_TRUST_PROXY` (Express "trust
  proxy"), which defaults to private-network proxies only.

The bytes are pinned exactly as received, since the on-chain hash is over
them. A pinning failure never passes Pinata's response or connection details
to the client.

## Errors

Every `5xx` is logged with its method, path and body, and reported to Sentry
when `GOVTOOL_SENTRY_DSN` is set. The backend closes its db-sync pool and
timers on `SIGTERM`, so a container stops cleanly.

## Metadata

`POST /metadata/validate` reads up to 2 MB (`METADATA_FETCH_LIMIT_BYTES` in
[src/metadata/config.ts](src/metadata/config.ts), a code constant by design,
D120) and reports a larger document as `EXCEEDS_LIMIT`.

Four routes front the metadata service (spec
[§2.8](../../docs/api/metadata-service-spec.md)). They pass the contract's
values through and expose nothing about caching (D119):

| Route                                | Response                                                                                    |
| ------------------------------------ | ------------------------------------------------------------------------------------------- |
| `GET /metadata/resolve?hash=&url=`   | always `200` with a `MetadataResult`; a failure carries `code`, `category` and `reportId`   |
| `POST /metadata/retry` `{hash, url}` | always `200` with a `MetadataRefreshGovernance action`; `Retry-After` is set with `retryAfterSeconds` |
| `GET /metadata/reports/:id`          | `200` with a `MetadataReport`, or `404`                                                     |
| `GET /metadata/reports?hash=&url=`   | `200` with `MetadataReportSummary[]`, newest first                                          |

- `hash` must be 64 hex characters, `url` must be `http(s)://` or `ipfs://`,
  and a report id must match `[A-Za-z0-9-]{1,64}`. Unknown query or body
  fields are rejected with `400`, so no cache parameter reaches the service.
- The retry window, one real fetch per (url, hash) per minute, is enforced by
  the service (D125); the backend relays its `retryAfterSeconds`.
- With `GOVTOOL_METADATA_SERVICE_URL` unset, all four answer `503`. If the service
  cannot be reached they answer a generic `502` that does not name it.

The DRep and proposal services resolve anchors through the same service, so a
DRep's name, objectives and image and an action's title, abstract, motivation
and rationale are filled when it is configured and `null` when it is not.

## Docker

The build context is the **parent** directory, because of those path
dependencies:

```bash
docker build -f govtool-backend/Dockerfile ..
```

`../docker-compose.fixture.yml` and `../docker-compose.koios.yml` already set
the context; see `../AGENTS.md`. CI publishes the image as
`ghcr.io/intersectmbo/govtool-backend`.

The image runs `node dist/main.js` on port 9999. Unlike the Haskell image, it
takes the db-sync connection only from the environment, never from
`config.json`, so a deployment sets at least:

| Variable | |
| --- | --- |
| `GOVTOOL_DBSYNC_HOST`, `_PORT`, `_DATABASE`, `_USER`, `_PASSWORD` | the db-sync connection |
| `GOVTOOL_DBSYNC_NETWORK` | `mainnet`, `preprod`, `preview` or `devnet`; must match the database, or every route answers 500 |

`GOVTOOL_PINATA_API_JWT`, `GOVTOOL_METADATA_SERVICE_URL` and
`GOVTOOL_PDF_API_URL` are optional; without them the upload, metadata and
discussion-link routes answer `503`. `-c <file>` points it at a `config.json`
for port, host, cache durations and Sentry, which the environment overrides.
[`docker/docker-compose.yaml`](../../docker/docker-compose.yaml) is a working
example.

## Compatibility

Compatibility is measured against the **Haskell** backend this replaced,
since that is what the frontend was written against.

- All 22 legacy routes are present, at the same paths, with the same
  methods.
- Response bodies are asserted key-for-key in
  [test/legacy-shape.spec.ts](test/legacy-shape.spec.ts), driven by a stubbed
  provider. Those tests use `toEqual`, so an added or dropped key fails.

Deliberate differences from the removed `backend-ts`, both cases where it
diverged from Haskell:

1. **`GET /drep/getVotes/:drepId` answers.** `backend-ts` reads four unaliased
   SQL columns by name, gets `undefined` for six of nine fields, keys its
   lookup map on `undefined` and returns `[]` for every DRep. The db-sync
   provider aliases its columns.

2. **Internal ids do not travel.** The `id` / `txId` / `proposalId` fields
   were db-sync row ids. Where a canonical identifier can stand in it does
   (`/proposal/list`'s `id` is the CIP-129 action id) and where none can, the
   field is `null` rather than the text `"undefined"` that `backend-ts` emits
   under a provider without row ids.

Behaviours preserved on purpose even though the provider is stricter
underneath:

- Identifiers. Routes accept the forms the frontend sends (raw hex or CIP-105
  DRep ids, `txHash#index`, 58-hex reward addresses) as well as the contract's
  forms, and respond with the forms the frontend reads (`drepId`, `hashRaw`,
  `drepHash`: hex hash; `view`, `drepView`: CIP-105).
  [src/common/legacy-ids.ts](src/common/legacy-ids.ts) translates both ways and
  answers 400 for anything that does not decode; a bare DRep hash is tried as
  a key credential, then as a script credential.
- `GET /ada-holder/get-voting-power/:stakeKey` still answers `0` on failure,
  including a provider outage. The frontend renders this number directly and
  has no error path for it. A failure is logged and never cached, so the next
  request asks again.
- With a `drepId`, `GET /proposal/list` leaves out the actions that DRep has
  voted on and `GET /proposal/get` returns its vote. A `drepId` that names no
  DRep, such as the text `undefined` a disconnected frontend sends, is
  ignored.
- Documents are fetched ahead, not while a request waits. After each
  snapshot refresh the warmer fills a store with every DRep's and every
  governance action's anchored document, 30 at a time from the metadata
  service ([document-store.ts](src/metadata/document-store.ts), D167).
  `/drep/list`, `/proposal/list`, their searches and `/drep/getVotes` read
  only from it. A failure the service reports is retried after 5, 10 and
  20 minutes and then kept, with its reason in a DRep row's `metadataError`.
  A name or text search answers `503` until every document has had an
  answer, rather than a partial result.
- `GET /network/metrics` still answers its thirteen counters. The contract has
  no metrics resource, so [network.service.ts](src/network/network.service.ts)
  assembles them from the committee, the DRep counts, the proposal total and
  the stake distribution, and reports `0` for the five that count rows no
  screen renders.

## Known limitations

`GET /epoch/params` keeps the snake_case `epoch_param` shape the frontend
reads, mapped from the contract's typed `ProtocolParams` in
[epoch.service.ts](src/epoch/epoch.service.ts). db-sync bookkeeping columns
(`id`, `block_id`, `cost_model_id`, `nonce`, `extra_entropy`) and parameters
the ledger no longer has (`decentralisation`, `min_utxo_value`) are `null`;
`cost_model.costs` is served and its `id` and `hash` are `null`.

A bare 56-hex stake key hash on the stake-key routes is read as a key-hash
reward address on the served network (db-sync's configured network, otherwise
the provider's `getNetworkInfo`, asked once). A proposal stamp the provider
dates by epoch alone gets that epoch's start time from the network's genesis
schedule, which is what `expiryDate` needs; see
[legacy-network.ts](src/common/legacy-network.ts).

Fields with no source are reported as `null` or an empty list rather than
dropped from the shape:

- **Everything from an anchored document**, when no metadata service is
  configured.
- **db-sync's internal row ids**: `/account`'s `id`, and `id` / `txId` on
  `/proposal/enacted-details`. No provider carries one.
- **`/transaction/status`'s `votingProcedure`**: a vote is reachable only
  through its DRep or its action, so a transaction hash cannot be asked what it
  voted on.

`GET /survey/definition/:txId/:index` serves a CIP-179 survey definition from
the provider's optional `surveys` namespace (SPEC.md §5.6): the transaction's
whole label-17 metadata as a singleton CBOR map, for any index 0–65535; the
frontend selects the definition. A missing label is a 404, a provider without
the namespace a 501 (and `/system/features` reports `survey.linkedVoting`
unavailable, so the frontend hides CIP-179). A definition is cached for 60
seconds per transaction and sent with `Cache-Control: public, max-age=60`;
an error is `no-store` and never cached.

## Tests

```bash
npm run verify    # format check, lint, typecheck, unit tests, build; never fixes
npm run test:e2e  # the two routes that need no data layer
```

`legacy-shape.spec.ts` stubs the contract directly and needs no provider;
`capabilities.spec.ts` pins the feature set to the code that implements it;
`metadata-routes.spec.ts`, `snapshot.spec.ts` and `cache.spec.ts` cover the
metadata gateway, the whole-collection reader and the cache. The e2e spec
covers `/` and `/health` only; to exercise the rest, run the server against
the fixture.

For a read-only Governance action comparison, build the contract, providers and backend,
then run from this directory with the db-sync and Koios environment settings
for the same network (`.env` supplies defaults):

```bash
GOVTOOL_AGGREGATES_REPORT=/tmp/govtool-governance action-live.json node scripts/live-governanceAction-aggregates.mjs
```

This samples current, enacted and expired actions, compares each supported
voter group's complete tally and threshold, and checks the Governance action detail
projection. Unsupported historical Koios SPO/committee groups are recorded
separately. Any mismatch or read failure exits nonzero. Chain data changes, so
the resulting report describes that run rather than a frozen release guarantee.

To check desktop and mobile rendering with those sampled tallies, run from
`../../tests/govtool-frontend/playwright`:

```bash
GOVTOOL_AGGREGATES_REPORT=/tmp/govtool-governance action-live.json npm run test:governance action-aggregates
```

These browser checks replay the captured aggregate data through mocked HTTP
responses; they do not replace the full release suite against a deployment.
