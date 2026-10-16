# @govtool/pinning-test

The GovTool pinning contract
([`@govtool/data-providers/pinning`](../govtool-data-providers)) over the test
metadata service in `tests/test-metadata-api`, for isolated integration-test
environments with no internet. Never use it in a deployment: the test service
has no authentication and no durability.

## Usage

The backend selects it with `GOVTOOL_PINNING_PROVIDER=test` and
`GOVTOOL_TEST_PINNING_URL=http://<test-metadata-api>:3000`. Directly:

```ts
import { createTestPinning } from '@govtool/pinning-test';

const pinning = createTestPinning({ baseUrl: 'http://localhost:3000' });
const cid = await pinning.pinData(bytes, owner); // bafkrei...
```

## What it does

- `pinData()` posts the exact bytes to `POST /ipfs`, and checks the CID the
  service answers against the one computed locally.
- `getDataCid()` computes locally: CIDv1, raw codec, sha2-256, base32
  (`bafkrei…`). That is the CID IPFS and Pinata assign content that fits in one
  256 KiB block, so anchors pinned here look like anchors pinned on Pinata.
  Above one block a real IPFS node would chunk into a dag-pb root; this service
  keeps the raw-block CID for any size up to the 512 KiB cap.
- `fetch()` reads `GET /ipfs/<cid>`; `unpin()` is `DELETE /ipfs/<cid>`.
- `getHealth()` is `GET /ipfs`: `healthy` on 200, `degraded` on any other
  status, `unavailable` when unreachable.

The same service answers `GET /ipfs/<cid>` for everything else that reads IPFS,
so point the frontend (`VITE_IPFS_GATEWAY`), the metadata service
(`IPFS_PRIMARY_GATEWAY`, `IPFS_GATEWAYS`) and db-sync (`ipfs_gateway`) at it
too; `tests/test-metadata-api/README.md` has the exact values.

## Tests

```bash
npm run verify   # format, lint, typecheck, test, build
```
