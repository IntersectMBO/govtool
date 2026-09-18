# @govtool/data-providers

Provider-agnostic TypeScript contracts for GovTool's data layer.

This package is **interfaces and types only**. It declares _what_ GovTool reads
and writes; it contains no implementation, no transport and no runtime
dependencies. An implementer may satisfy these contracts from SQL, HTTP, a
file or a fixture, and consumers are written against the contract rather than
against any one provider.

The only runtime export in the whole package is `pinning.toAnchor()`, a
two-field projection. Everything else erases at compile time.

## Why it exists

Four chain-data providers are planned — db-sync
([#4222](https://github.com/IntersectMBO/govtool/issues/4222)), Koios
([#4223](https://github.com/IntersectMBO/govtool/issues/4223)), Blockfrost
([#4234](https://github.com/IntersectMBO/govtool/issues/4234)) and Kupo
([#4235](https://github.com/IntersectMBO/govtool/issues/4235)). Without a
shared contract each one grows its own response shapes and every consumer
learns which provider it is talking to. This package is the seam: providers
implement it, consumers import it, and neither depends on the other.

## Install

Consumed from inside the monorepo by path — no registry or workspace setup:

```jsonc
// consumer package.json
{
  "dependencies": {
    "@govtool/data-providers": "file:../govtool-data-providers",
  },
}
```

The package ships compiled declarations, so `npm run build` must have run in
this directory before a consumer typechecks against it.

## The three components

| Component    | Owns                                                                                                          | Source of truth                      | Contract                                             |
| ------------ | ------------------------------------------------------------------------------------------------------------- | ------------------------------------ | ---------------------------------------------------- |
| `chain-data` | Everything derivable from the ledger: chain state, accounts, DReps, proposals, votes, committee, transactions | The ledger, via a swappable provider | [`src/chain-data/index.ts`](src/chain-data/index.ts) |
| `metadata`   | Off-chain document retrieval, hash verification, CIP validation, cache lifecycle                              | The public internet (HTTP/IPFS)      | [`src/metadata/index.ts`](src/metadata/index.ts)     |
| `pinning`    | Author-side write path: pin user-authored metadata, return the `url` + `dataHash` to submit on chain          | Its own pin store                    | [`src/pinning/index.ts`](src/pinning/index.ts)       |

They are separate because they have different freshness clocks (a chain point
versus "when did HTTP last answer"), different trust levels (deterministic
ledger data versus arbitrary third-party documents versus accepted user
content), and different failure modes. The full rationale is in the header
comment of each module — worth reading once before implementing against them.

Two rules follow from the split:

- **Chain data never fetches a url.** It embeds a `MetadataProjection` produced
  by the metadata service and nothing else crosses that line.
- **The metadata service does not trust the pinning service.** Content GovTool
  pinned is re-fetched and re-hashed from its public url like anyone else's, so
  "pinned but not publicly reachable" is reported honestly.

## Importing

Prefer the subpath imports. They are collision-free and make the component
boundary visible at the import site:

```ts
import type { ChainDataApiV1, DRep } from '@govtool/data-providers/chain-data';
import type { DRepsApi } from '@govtool/data-providers/chain-data/governance';
import type {
  Anchor,
  MetadataProjection,
} from '@govtool/data-providers/metadata';
import type { PinRecord } from '@govtool/data-providers/pinning';
```

The root entry point exposes the same three components as namespaces, plus the
three service contracts flat:

```ts
import type { chainData, ChainDataApiV1 } from '@govtool/data-providers';
import { pinning } from '@govtool/data-providers';

declare const drep: chainData.DRep;
```

The root deliberately does **not** flatten the components: `chain-data` and
`metadata` each define their own `Hex` and `Timestamp` — by design, so
`metadata` stands alone — and a flat re-export would make those names
ambiguous.

## Conventions an implementer must honour

These are contract, not style. Breaking one silently produces wrong numbers in
the UI rather than a type error.

- **Lovelace is a decimal `string`.** Total supply (4.5e16) exceeds
  `Number.MAX_SAFE_INTEGER` (9.0e15). Returning it as a JSON number is lossy.
- **`undefined` means the provider structurally cannot serve this field;
  `null` means known-absent on chain.** Which fields a given provider cannot
  serve is declared once at `/system/capabilities`, not repeated per response.
- **`active` versus `live` is named in every field that has both.** `active` is
  the epoch-boundary snapshot the ledger counts votes against and the only
  valid tally denominator; `live` is the current un-snapshotted value. There is
  no bare `votingPower` whose meaning depends on which route produced it.
- **CIP-129 bech32 is the canonical identifier** for governance actions, DReps
  and committee members. Raw hashes (and `txHash` + `index`) are returned
  alongside because they are not derivable without decoding, but no composite
  or legacy id form appears in the contract.
- **No internal database ids**, ever.
- **Ratios stay `{ numerator, denominator }`**, never a float.
- **Expensive fields are opt-in via `expand`**, never computed unconditionally
  on a list read.
- **`meta.asOf` is optional and carries the chain point only.** A provider that
  cannot report the tip without an extra round trip omits it. Absent means "not
  reported", never "assume current".

## Implementing a provider

[`test/conformance.ts`](test/conformance.ts) contains a stub that implements all
three services by throwing `CAPABILITY_UNSUPPORTED`. Copy one and replace the
`unsupported` calls route by route, declaring the remaining gaps at
`/system/capabilities` until it is complete.

That file is also the test suite. It is type-checked, never executed, and it
covers the two ways a contracts package breaks: a signature that admits no
implementation, and a domain type that cannot be constructed. The fixtures in
its second half build each central entity from literals, which is exactly what
a provider's mapping layer does.

## Scripts

```bash
npm run build       # emit dist/ (JS + .d.ts + maps)
npm run typecheck   # check src/ and the conformance suite
npm run lint        # eslint
npm run format      # prettier --write
npm test            # build, then the runtime smoke test
npm run verify      # format:check + lint + typecheck + test
```

## Implementations

| Package                                                  | Component    | Notes                                                                                                                         |
| -------------------------------------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| [`@govtool/provider-dbsync`](../govtool-provider-dbsync) | `chain-data` | Runs the legacy GovTool SQL unmodified. Roughly half the contract is `CAPABILITY_UNSUPPORTED`.                                |
| [`@govtool/pinning-pinata`](../govtool-pinning-pinata)   | `pinning`    | Port of the legacy `POST /ipfs/upload`.                                                                                       |
| [`@govtool/provider-koios`](../govtool-provider-koios)   | `chain-data` | Over the public Koios REST API. Wider coverage than db-sync; no surveys, no aggregate DRep metrics, no per-vote voting power. |
| —                                                        | `metadata`   | Not implemented yet. `govtool-backend` still validates documents inline in its own `metadata/` module.                        |

## What 0.2.0 changed, and why

0.1.0 was drafted before anything implemented it. Writing the db-sync provider
against the legacy SQL showed where the draft assumed data that no real source
returns, so this version **loosens** rather than adds. Every change below has
the same cause: a field was required that a provider cannot honestly fill.

Required → optional, because the legacy SQL has no column for it:

- `ChainPoint.slot`, `.blockHash`, `.time` — db-sync's tip read is
  `MAX(epoch)` and `MAX(block_no)`; the rest needs another query.
- `EpochStamp.epoch` — some sources record only a wall-clock time. Deriving an
  epoch from a timestamp is wrong on any network with a non-standard epoch
  length.
- `VotingPower.epoch` — a per-credential distribution read returns the latest
  amount without the epoch it was taken in.
- `NetworkInfo.networkMagic`, `.era`, and the epoch's start/end times.
- `ProtocolParams` scalars and both threshold groups. `epoch` and `raw` stay
  required; everything else is lifted only when a provider can read it without
  interpretation. db-sync stores thresholds as doubles, and a float cannot be
  turned back into the on-chain ratio.
- `StakeDistribution.epoch`, `.totalActiveStake`.
- `Registration.status` and the four lifecycle fields; `DRepActivity.lastVotedAt`,
  `.inactiveFromEpoch`; `DRep.isCip119Compliant`; `Delegation.since`.
- `GovAction.deposit`, `.depositReturnAddress`, `.proposedBy`, and `.body`.
- `Account.votingPower` and `.delegation` — each is its own read on every
  provider surveyed, so they became `expand` fields.
- `TransactionState.effects`.
- `PinningPolicy.rateLimit`, `.quota` — a thin adapter over a hosted backend
  does not know the plan's limits.

`RoleTally.stake` and `.count` both became optional, which is a correction
rather than a loosening: whether a role is counted by stake or by head is a
ledger fact (DReps and SPOs by stake, the committee by head), so requiring
both forced every provider to invent one.

Added, because a real consumer needs it:

- `VoterRef.cip105Id` — the pre-CIP-129 bech32 that db-sync stores as
  `drep_hash.view` and that the legacy API returns. Not derivable from `hash`
  without knowing the old encoding, so it is carried when the provider has it.
- `TxRef.providerId`, `GovActionRef.providerId`, `Account.providerId` — an
  opaque provider-native id. The rule "no internal database ids" still holds
  for _identity_; this exists only so a consumer reproducing an older API that
  leaked them can do so without the contract pretending they are canonical.
- `GovAction.rawBody`, `EnactedActionSummary.rawBody`,
  `MetadataProjection.raw` — the source's own rendering, for the variants a
  provider cannot type.
- `MetadataProjection.failureMessage` — db-sync's free-text fetch error.
- `PageRequest.offset`, and `limit` became optional meaning "everything", for
  providers that materialise the whole result set (all the legacy SQL does) and
  consumers that page in memory behind their own cache.
- `DRepKindRegistration` and `DRep.registrationByKind` — the same credential
  can hold a DRep registration and a direct-voter registration over its
  lifetime, which the legacy API exposed as four booleans and the draft could
  not represent.
- `DRepVotingPowerEntry`, `DRepsApi.getVotingPowers` — the batch read, now
  returning entries that echo each DRep's identity.
- `AccountsApi.getDelegation` — the hottest read after wallet connect, and a
  single lookup everywhere; it should not require fetching a whole account.
- `SurveysApi` (`chain-data/surveys`) — CIP-179 surveys. **Optional** on
  `ChainDataApiV1`, since it is GovTool-specific.
- `ChainDataError` and `PinningError` — the runtime error classes. The draft
  had `ApiError` as a wire shape only, which meant every implementation would
  invent its own exception and every consumer would sniff for it. Use the
  static `is()` rather than `instanceof`: two copies of this package can
  coexist in one process and `instanceof` fails across them.

Tightened:

- `MetadataId` now specifies the exact construction
  (`blake2b-256(utf8(url) ‖ 0x23 ‖ bytes(dataHash))`). It was described but not
  pinned down, and two services computing it differently would not agree.
- `NetworkId` accepts any string, because db-sync's `meta.network_name` is
  free text (`sanchonet`, a custom testnet).

The one draft decision now settled: `UpdateCommittee` is the contract's name.
db-sync and the legacy API call it `NewCommittee`, and the provider translates
in both directions (`toContractType` / `toDbSyncType`).

## What 0.3.0 changed, and why

Same cause as 0.2.0, a different source: writing providers over HTTP APIs
(Koios, Blockfrost) found two more fields that were required and are not
universally recorded.

- **`EpochStamp.time` is optional**, so both halves of the stamp now are, and
  at least one is always set. The two halves come from different kinds of
  source and neither is universal: db-sync records a DRep's
  `last_register_time` with no epoch, while Koios records every governance
  action's ratification, enactment, expiry and drop as an _epoch number and
  nothing else_. Deriving the missing half is unsafe in either direction.
  `hasTime()` and `hasEpoch()` narrow an `EpochStamp` for a consumer that
  needs one of them.
- **`StakeRegistrationEvent.slot` and `.block` are optional.** Koios'
  `/account_updates` reports the absolute slot but no block height. A provider
  that has one and not the other fills what it has rather than inventing the
  rest.

Then Blockfrost ([#4234](https://github.com/IntersectMBO/govtool/issues/4234))
found five more, all the same shape — a field that one source records and
another simply does not:

- **`GovActionLifecycle.submitted` is optional.** Blockfrost's proposal record
  carries the ratified / enacted / dropped / expired epochs and the expiry, but
  nothing about submission. `submittedTx` stays required, since the submitting
  transaction is the record's own key.
- **`VoteRecord.at` is optional.** A provider that indexes votes per proposal
  returns the voter, the choice and the vote's transaction; dating it means
  joining that transaction to its block, one read per vote.
- **`StakeDistribution`'s four governance totals are optional.** Summing the
  whole DRep distribution is one query on db-sync and one request per DRep on a
  per-entity HTTP API. A provider serving only `totalActiveStake` is still
  useful as a tally denominator.
- **`DRepHistoryEvent.at` and `.anchor` are optional**, and so are
  **`DelegationHistoryEvent.at` and `.from`.** Certificate listings name the
  transaction and date nothing; a history listing gives each certificate's
  target, not the one it replaced.

Loosening a field is safe for producers and **breaks consumers that read it
unguarded** — which is the point. The compiler then shows every place that had
quietly assumed one provider's shape; that is how `govtool-backend`'s
`/network/total-stake` learnt to report a 501 instead of a silent `0`. After
editing this package, typecheck every provider _and_ `govtool-backend`.

The wider analysis — coverage of db-sync and Blockfrost side by side, and the
three structural changes worth considering — is in
[`docs/api/provider-gap-report.md`](../../docs/api/provider-gap-report.md).

## Status

Version 0.3.0. The contracts carry the issue links they were drafted for
([#4221](https://github.com/IntersectMBO/govtool/issues/4221) for chain data,
[#4224](https://github.com/IntersectMBO/govtool/issues/4224) /
[#4225](https://github.com/IntersectMBO/govtool/issues/4225) for metadata), and
remaining open decisions are in
[`docs/api/README.md`](../../docs/api/README.md).

`docs/api/` is where these drafts were written and it still holds the route
map, the data inventory and the provider comparison. Its `.ts` files are now
duplicated by — and behind — `src/` here; worth collapsing to one source of
truth.
