# GovTool API contracts — working drafts

Drafts for [#4221](https://github.com/IntersectMBO/govtool/issues/4221) (Chain
Data API v1), [#4224](https://github.com/IntersectMBO/govtool/issues/4224) /
[#4225](https://github.com/IntersectMBO/govtool/issues/4225) (metadata), and the
pinning path currently living in the Haskell backend.

## Three components

| Component | Owns | Source of truth | Fails by | Spec |
|-----------|------|-----------------|----------|------|
| **Chain Data API** | Everything derivable from the ledger: chain state, accounts, DReps, proposals, votes, committee, transactions | The ledger, via a swappable provider | Provider lag or outage → degraded mode | [`chain-data/`](./chain-data/index.ts) |
| **Metadata Service** | Off-chain document retrieval, hash verification, CIP validation, persistence, cache lifecycle | The public internet (HTTP/IPFS) | Unreachable or invalid documents → typed metadata status | [`metadata/`](./metadata/index.ts) |
| **Pinning Service** | Author-side write path: pin user-authored metadata, return the `url` + `dataHash` to submit on chain | Its own pin store | Quota or backend outage → pin rejected; author can self-host instead | [`pinning/`](./pinning/index.ts) |

Why they are separate:

1. **Different freshness clocks.** A chain-data response's `meta.asOf` is a chain
   point. A metadata document's freshness is "when did HTTP last answer". One
   envelope cannot honestly describe both.
2. **Different trust.** The Chain Data API reads deterministic ledger data. The
   metadata service reads arbitrary third-party documents. The pinning service
   *accepts* arbitrary user content — the only write path in the stack.
3. **Different failure handling.** Provider lag degrades a whole chain-data
   response; a metadata fetch failure degrades one field of one entity; a pin
   failure is recoverable by the author hosting the file themselves.
4. **Provider count.** Four chain-data providers are planned (#4222, #4223,
   #4234, #4235). Metadata fetching and validation must be implemented once, not
   four times.

Rules that follow:

- The Chain Data API **never fetches a url**. It embeds a `MetadataProjection`
  — `{ id, anchor, standard, status, body?, failureReason?, fetchedAt? }` — and
  nothing else crosses that line.
- The Metadata Service **does not trust the Pinning Service.** Content GovTool
  pinned is re-fetched and re-hashed from its public url like anyone else's, so
  "pinned but not publicly reachable" is reported honestly.
- The Pinning Service is **optional**. GovTool must work fully with it disabled.

## Chain Data API route map

| Route | Module | Notes |
|-------|--------|-------|
| `GET /network` | [`network.ts`](./chain-data/network.ts) | tip, era, current epoch |
| `GET /network/epochs` | `network.ts` | historical list |
| `GET /network/epochs/{epoch}/params` | `network.ts` | any epoch, not just current |
| `GET /network/blocks` | `network.ts` | confirmation depth |
| `GET /network/stake-distribution` | `network.ts` | `active` and `live` totals |
| `GET /network/treasury` | `network.ts` | |
| `GET /accounts/{stakeAddress}` | [`accounts.ts`](./chain-data/accounts.ts) | registration, balance, both delegations |
| `GET /accounts/{stakeAddress}/delegations` | `accounts.ts` | governance + pool history |
| `GET /accounts/{stakeAddress}/stake-events` | `accounts.ts` | registration / deregistration certs |
| `GET /accounts/{stakeAddress}/voting-power` | `accounts.ts` | |
| `GET /governance/dreps` | [`governance/dreps.ts`](./chain-data/governance/dreps.ts) | directory |
| `GET /governance/dreps/{id}` | `dreps.ts` | profile |
| `GET /governance/dreps/{id}/votes` | `dreps.ts` | |
| `GET /governance/dreps/{id}/delegators` | `dreps.ts` | `basis=active\|live` |
| `GET /governance/dreps/{id}/delegation-events` | `dreps.ts` | joined / left |
| `GET /governance/dreps/{id}/history` | `dreps.ts` | registration / update / retire |
| `GET /governance/dreps/{id}/voting-power` | `dreps.ts` | current or by epoch range |
| `GET /governance/proposals` | [`governance/proposals.ts`](./chain-data/governance/proposals.ts) | |
| `GET /governance/proposals/{id}` | `proposals.ts` | CIP-129 id |
| `GET /governance/proposals/{id}/votes` | `proposals.ts` | |
| `GET /governance/proposals/{id}/tallies` | `proposals.ts` | stake + count + threshold |
| `GET /governance/proposals/{id}/activity` | `proposals.ts` | lifecycle timeline |
| `GET /governance/proposals/enacted?type=` | `proposals.ts` | for diff views |
| `GET /governance/votes` | [`governance/votes.ts`](./chain-data/governance/votes.ts) | cross-cutting feed |
| `GET /governance/votes/{txHash}` | `votes.ts` | |
| `GET /governance/pools` | [`governance/pools.ts`](./chain-data/governance/pools.ts) | SPOs as voters |
| `GET /governance/committee` | [`governance/committee.ts`](./chain-data/governance/committee.ts) | from gov-state |
| `GET /governance/constitution` | `committee.ts` | |
| `GET /governance/voters` | [`governance/index.ts`](./chain-data/governance/index.ts) | role-agnostic resolve |
| `GET /governance/metrics` | [`governance/metrics.ts`](./chain-data/governance/metrics.ts) | dashboard counters |
| `GET /transactions/{txHash}` | [`transactions.ts`](./chain-data/transactions.ts) | status + governance effects |
| `GET /system/capabilities` | [`common.ts`](./chain-data/common.ts) | per-provider matrix |
| `GET /system/health` | `common.ts` | tip lag, 503 threshold |

Module dependency rule: modules may import `common.ts` and `refs.ts` freely, but
may only reference each other through the **refs** (`VoterRef`, `GovActionRef`,
`AccountRef`). Only the owning module returns the full entity. The one exception
is the type-only cycle between `proposals` and `votes` — a proposal carries
`myVote`, a vote carries its proposal — which TypeScript resolves without a
runtime dependency.

## Documents

- [`multi-provider-support.md`](./multi-provider-support.md) — how a provider declares what it can serve, and how the frontend switches features on it: the `Subject × Facet × Temporality` model, the binary-support rule, and the three ways declaration-vs-runtime drift is tested.
- [`data-inventory.md`](./data-inventory.md) — every data item GovTool consumes, with its current source, consumer and tier.
- [`frontend-needs-audit.md`](./frontend-needs-audit.md) — what the current frontend actually calls, versus the spec, versus what each provider can serve; and which providers could run GovTool today.
- [`provider-gap-report.md`](./provider-gap-report.md) — what implementing the contract three times (db-sync, Koios, Blockfrost) taught us: coverage side by side, the loosenings already made, the five structural changes worth considering, and one retracted finding.
- [`provider-comparison.md`](./provider-comparison.md) — what [cardanoapi/dbsync-api](https://github.com/cardanoapi/dbsync-api) has that this draft adopted, and what it lacks.

## Open decisions

1. **Lovelace as `string`.** Total supply (4.5e16 lovelace) exceeds `Number.MAX_SAFE_INTEGER` (9.0e15). Today's API returns these as JSON numbers, which is already lossy.
2. **CIP-129 bech32 is the only identifier** for gov actions, DReps and CC members. The raw hash (and `txHash` + `index`) is returned alongside because it is not derivable from the bech32 without decoding, but no composite or legacy id form appears in the contract.
3. **`UpdateCommittee`**, the ledger name, replaces today's `NewCommittee` — a breaking rename that needs a conscious call.
4. **`live` vs `active` named in every field that has both.** Never a bare `votingPower` whose snapshot semantics depend on which endpoint produced it.
5. **Expensive fields are opt-in** via `expand`, never computed unconditionally on list reads.
6. **No internal database ids** in the contract.
7. **`undefined`** = the provider cannot serve it, as declared once at `/system/capabilities`. `null` = known absent on chain.
8. **`meta` carries the chain point only, and it is optional.** Providers that cannot report the tip without an extra round trip omit it; absent means "not reported", not "unknown". Tip lag is derivable from it, capability gaps are declared per provider, and cache/poll policy belongs to the layer facing the client.
