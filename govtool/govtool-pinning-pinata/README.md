# @govtool/pinning-pinata

Pinata implementation of the GovTool pinning contract
([`@govtool/data-providers/pinning`](../govtool-data-providers)).

The author-side write path: pin a user-authored metadata document and return
the `url` + `dataHash` pair to submit on chain. It is optional — GovTool works
fully with it switched off, and an author who hosts their own document never
touches it.

## Usage

```ts
import { createPinataPinning } from '@govtool/pinning-pinata';

const pinning = createPinataPinning({ jwt: process.env.VVA_PINATAAPIJWT! });

const pin = await pinning.pin({
  content: JSON.stringify(document),
  contentType: 'application/ld+json',
  fileName: 'drep.jsonld',
});

// exactly what goes on chain
const anchor = { url: pin.url, dataHash: pin.dataHash };
```

`toAnchor(pin)` from the contract package does that last step. Never put a
gateway url on chain — `gatewayUrls` is for display only, and an anchor
pinned to one gateway's availability outlives that gateway.

## What it does

`pin()` is a faithful port of the legacy backend's `POST /ipfs/upload`: the
same endpoint (`https://upload.pinata.cloud/v3/files`), the same multipart
shape (`network` + `file`), the same 512 KiB cap, the same default file name
(`data.txt`), and failures in the same four classes. It additionally computes
`dataHash` — blake2b-256 over the exact bytes — so the author never has to.

`prepare()` hashes and sizes **without** pinning, which lets an author preview
the `dataHash` they will commit when hosting the document themselves.

`getPolicy()` and `getHealth()` are answered locally; health asks Pinata
whether the JWT is still accepted (`healthy` on 200, `degraded` on any other
status, `unavailable` when unreachable).

## What it does not do

`getPin()`, `listPins()`, `repin()` and `unpin()` throw
`UNSUPPORTED_OPERATION`. GovTool never needed them, and shipping an unverified
call to Pinata's file-management API would be worse than an honest gap.

**CIP validation is not performed.** `PinRequest.standard` is accepted and
ignored, and `prepare().valid` reflects size and content-type only. Validating
a document against CIP-108/119 belongs to the metadata service, which re-fetches
and re-checks from the public url anyway — this service is not a trusted source
for it.

`PinningPolicy.rateLimit` and `.quota` are omitted rather than invented: a thin
adapter does not know the account's plan limits.

## Errors

Every failure is a `PinningError` with a `reason` and a `terminal` flag, so a
consumer maps it onto its own transport without knowing the backend:

| `reason`                   | when                                                   | terminal |
| -------------------------- | ------------------------------------------------------ | -------- |
| `TOO_LARGE`                | over 512 KiB, checked before any request leaves        | yes      |
| `UNSUPPORTED_CONTENT_TYPE` | content type outside the policy                        | yes      |
| `BACKEND_UNAVAILABLE`      | `fetch` threw — Pinata unreachable                     | no       |
| `BACKEND_ERROR`            | Pinata answered non-2xx; `details` has status and body | no       |
| `BACKEND_INVALID_RESPONSE` | 2xx whose body carried no cid                          | yes      |
| `UNSUPPORTED_OPERATION`    | one of the four file-management calls                  | yes      |

Prefer `PinningError.is(e)` to `instanceof`; two copies of the contract package
can coexist in one process and `instanceof` fails across them.

## Tests

16 specs, no network: `fetch` and the clock are injected, so a test asserts the
exact multipart body sent and every failure mapping.

```bash
npm run verify   # format, lint, typecheck, test, build
```
