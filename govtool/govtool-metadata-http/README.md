# @govtool/metadata-http

HTTP client implementing the GovTool Metadata Service contract
([`@govtool/data-providers/metadata`](../govtool-data-providers)) against
[`govtool-metadata-service`](../govtool-metadata-service). It speaks the
service routes of
[`docs/api/metadata-service-spec.md`](../../docs/api/metadata-service-spec.md)
§2.8.

One HTTP call per method, and nothing else: no caching, no retries, no rate
limiting. The service owns the cache and enforces the one-fetch-per-anchor-per-
minute retry window (D125); [`govtool-backend`](../govtool-backend) is the only
caller and the only public door (D119).

## Usage

```ts
import { createHttpMetadataService } from '@govtool/metadata-http';

const metadata = createHttpMetadataService({
  baseUrl: 'http://metadata:3000', // the service root, no trailing /api
  timeoutMs: 30_000, // optional; the default
});

const result = await metadata.getMetadata(hash, url);
if (!result.ok) console.log(result.code, result.category, result.reportId);
```

Zero HTTP dependencies: it uses the global `fetch` Node 20 ships. A custom
`fetch` can be injected for tests.

## Mapping

| Method           | Request                                  | Response                                                                         |
| ---------------- | ---------------------------------------- | -------------------------------------------------------------------------------- |
| `getMetadata`    | `GET /api/metadata?hash=&url=`           | the service's `200 {hash, fetchedAt, url, metadata}` becomes a `MetadataSuccess` |
|                  |                                          | a §2.4 failure status with `{code, …}` becomes a `MetadataFailure`               |
| `getCipMetadata` | `GET /api/metadata?hash=&url=&cip=`      | `501` throws: the service does not validate CIPs yet                             |
| `refresh`        | `POST /api/metadata/{hash}/refresh?url=` | `200 MetadataRefreshOutcome`, passed through                                     |
| `getReport`      | `GET /api/metadata/reports/{id}`         | `200 MetadataReport`, or `null` on `404`                                         |
| `listReports`    | `GET /api/metadata/reports?hash=&url=`   | `200 MetadataReportSummary[]`                                                    |

A failure's `category` is taken from the service when it sends one of the
three, and otherwise derived from `code` with `METADATA_FAILURE_CATEGORY`.
`checkedAt` is the service's `fetchedAt`.

## What throws

A metadata failure is a value. These are infrastructure faults and throw a
`MetadataHttpError` (with `status` when a response arrived):

- the service is unreachable, or does not answer within `timeoutMs`
- `400` on the resolve route, which is a caller error (bad input, or a miss
  with no url)
- `501` for `cip=`
- any status or body outside the contract

## Request hygiene

- Only `Accept: application/json` is sent. **Never `Cache-Control`**: the
  service reads `Cache-Control: invalidate` as a cache-buster, and cache
  mechanics are not something a GovTool caller controls (D119).
- Redirects are refused rather than followed.
- Path segments (hash, report id) are percent-encoded.
- `baseUrl` must be `http(s)` and must not carry credentials.

## Checks

```bash
npm run verify   # format:check, lint, typecheck, test, build
```

Tests use `node:test` against a local `node:http` stub server; `npm test`
compiles them into `.test-build/` first.
