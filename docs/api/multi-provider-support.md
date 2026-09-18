# Multi-provider support — how a provider declares what it can serve

Status: **2026-09-18.** The design doc for the capability layer in
[`govtool-data-providers/src/chain-data/capabilities/`](../../govtool/govtool-data-providers/src/chain-data/capabilities/),
implemented by all three providers and consumed by the backend and frontend.

Companion to [`provider-gap-report.md`](./provider-gap-report.md), which asks
"what can each provider serve?". This asks the operational question that follows:

> **How does a provider say so, in a form the frontend can switch features on?**

---

## 1. Why the old shape failed

The contract shipped `ProviderCapabilities` from 0.1.0:

```ts
export type CapabilityLevel = "supported" | "partial" | "unsupported";
export interface ProviderCapabilities {
  capabilities: Record<string, CapabilityLevel>; // 'governance.dreps.list' | 'route#field'
}
```

Three implementations later it had rotted, and the rot was measurable. A
mechanical cross-check of each provider's table against every `unsupported(...)`
site in the same package found:

- **90 distinct keys** across three providers (db-sync 57, Koios 75,
  Blockfrost 56), in **five incompatible syntaxes** — `route#field`,
  `route{param}`, `route{param=value}`, `route{param:value}`, and dotted
  sub-paths. Koios wrote `{basis:live}` where the other two wrote `{basis=live}`.
  Nothing type-checked any of it.
- **47 runtime refusals matched no declared key.**
- **16 keys declared `unsupported` never refuse anything** — the route answers
  and the field is silently missing from a 200.
- Of 50 keys carrying a `#`, **only 4 were declared by all three providers.**

And `'partial'` meant "fewer fields", "one request per element" and "only the
first page was filtered" interchangeably — so a consumer could not act on it.

Nothing consumed any of this. `govtool-backend` had no capabilities route; the
frontend contained zero occurrences of the string "capabilit".

## 2. The model: `Subject × Facet × Temporality`

A provider does not declare routes. It declares where it sits in a small
orthogonal space, and routes and features are derived from that.

| Axis            | Members                                                                                                                             |
| --------------- | ----------------------------------------------------------------------------------------------------------------------------------- |
| **Subject**     | `network` `account` `drep` `pool` `committee` `constitution` `proposal` `vote` `voter` `transaction` `survey`                       |
| **Facet**       | `identity` `registration` `stake` `delegation` `ballot` `tally` `body` `params` `aggregate` `treasury` `chain` `outcome` `metadata` |
| **Temporality** | `current` `asAt` `series` `events`                                                                                                  |

A _dataset_ is one point in that space — `drep.delegation.current`,
`proposal.ballot.current`, `network.params.asAt` — and `DATASETS` is a closed
registry of 41 of them.

**Temporality is the axis that earns the model.** `drep.delegation.current`
(who delegates now) and `drep.delegation.events` (who joined and left) are
different capabilities of one subject. Every source has the first; **no source
has the second**. The old table could not express that difference at all, so
"we cannot show a delegation timeline on any provider, ever" was not a
statement it could make.

`StakeBasis` (`active` / `live`) is deliberately **not** a fourth axis. It is
already in the contract and already load-bearing, and the axis has two sides:
Koios refuses `live` on voting power, Blockfrost refuses `active` on its
delegator list. It stays a per-dataset `EnumSupport<StakeBasis>`.

## 3. Support is binary

**A dataset is served or it is not.** There is no "expensive", "degraded" or
"partial" grade. A provider that cannot afford a read declares it `refused`
with `kind: 'tooExpensive'` and a typed `fallback` dataset — it does not serve
it with a warning label.

This was a deliberate reversal. An earlier draft graded reads on a five-member
`CostClass` (`indexed | batched | scan | fanout | walk`). It was removed:
`scan` was used **once** across all three providers, and a cost grade invites
exactly the ambiguity that killed `'partial'` — a consumer cannot act on
"works, but expensive".

One operational fact survives, because it is not a statement about support:

```ts
readonly pollable: boolean;
```

A read can be served, cheap, and still be something a provider will not
tolerate on a 20-second interval. `pollable` drives `refresh: 'poll' |
'userInitiated'`, which the frontend feeds to react-query. Blockfrost's DRep
directory is _served_ and `pollable: false`.

### What `partial` decomposed into

The one three-valued word became separately actionable facts, each of which a
consumer can branch on:

| Fact                              | Says                                                                                       |
| --------------------------------- | ------------------------------------------------------------------------------------------ |
| `OptionSupport`                   | per **member** of a sort/filter/expand union — see below                                   |
| `FilterSupport.maxSelected`       | Koios allows exactly **one** proposal status at a time                                     |
| `FieldSupport.serves`             | `always` / `onExpand` / `never`, per field                                                 |
| `FieldSupport.whenRequested`      | `throws` vs `ignored` when a `never` field is asked for                                    |
| `PagingSupport.omittedLimitMeans` | `everything` vs `oneMaxPage` — the bug in §6                                               |
| `Caveat[]`                        | the answer is served but **means something else**                                          |
| `Unavailability.kind`             | `notInSource` `noIndex` `tooExpensive` `notImplemented` `deploymentFault` `representation` |

### `OptionSupport` has four members, and all four are used

```ts
export type OptionSupport =
  "honoured" | "rejected" | "ignored" | "approximated";
```

Usage across the three shipped declarations: `honoured` 135, **`ignored` 86**,
`rejected` 74, `approximated` 29.

`ignored` is not a synonym for `rejected`, and it is the second most common
value. db-sync, `proposals.api.ts`:

```ts
switch (sort) {
  case 'newest': …
  case 'mostYesVotes': …
  default:
    return copied;   // highestParticipation lands here. Unsorted. Status 200.
}
```

`rejected` throws — a consumer sees an error and handles it. `ignored` returns
**200 OK with the request silently discarded**, and the user reads an unsorted
list as though it were sorted. `approximated` is a third real state:
Blockfrost filters one hydrated page, so the answer is usable but incomplete.

**The frontend never sees these words.** `isOfferable()` collapses them to a
boolean (`honoured | approximated` → offered) before the response leaves the
backend. They exist so the provider author can be precise and so the drift
audit can catch a provider that lies.

## 4. The layers

`src/chain-data/capabilities/`, each depending only on the ones above it, and
none depending on a framework, a provider or a consumer:

| File             | Holds                                                                                                                                                                                    |
| ---------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `axes.ts`        | the vocabulary: the three axes, `Reachability`, `Absence`, `OptionSupport`, `FilterSupport`, `SearchSupport`, `PagingSupport`, `BatchSupport`, `Caveat`                                  |
| `datasets.ts`    | `RouteId` **computed** from `ChainDataApiV1` by a recursive mapped type (40 routes, none hand-written), and the 41-entry `DATASETS` registry                                             |
| `fields.ts`      | `OptionalFieldsOf<T>` derives the declarable fields from the entity type; `EntityFieldTable<E>` is a **required exhaustive** record over them                                            |
| `declaration.ts` | what a provider writes: `DatasetCapability`, `ProviderCapabilityDocument`, `declareCapabilities`, `composeCapabilities`, plus the runtime dual `CapabilityRefusal` / `refusalIsDeclared` |
| `features.ts`    | `deriveFeatures(doc, digest): FeatureSet` — a pure JSON→JSON function over a 34-entry `FeatureCatalog`                                                                                   |

Two type-level assertions fail the build if the contract and the registry drift:
`NamespacesAreInSync` (a namespace added to `ChainDataApiV1`) and
`EveryRouteHasADataset`.

## 5. What a consumer reads

`GET /system/features` returns a `FeatureSet`; each feature is:

```ts
interface FeatureState<F> {
  available: boolean; // show / hide
  tier: FeatureTier; // 'core' → a broken one is a banner, not a hidden tab
  permanentlyAbsent: boolean; // no known source records this — DELETE the component
  refresh: RefreshPolicy; // 'poll' | 'userInitiated'
  options: FeatureOptions<F>; // { sort: { allowed: [...], defaultTo, maxSelected }, … }
  mode: ModeOf<F> | null;
  paging: PagingFacts | null;
  caveats: readonly Caveat[]; // served, but means something else
  blockedBy: FeatureBlock | null;
}
```

`permanentlyAbsent` is the distinction that stops dead UI accumulating: a
toggle that is off on every provider _and always will be_ should not ship as a
toggle.

### The three worked cases

Derived from the three real declarations:

|                             | db-sync                                       | Koios                               | Blockfrost                  |
| --------------------------- | --------------------------------------------- | ----------------------------------- | --------------------------- |
| `drep.delegationTimeline`   | `available: false`, **`permanentlyAbsent`**   | same                                | same                        |
| `govAction.voterList`       | `false`, blocked by `proposal.ballot.current` | `true`, sort `[newest, oldest]`     | `true`, sort `[]`           |
| `drepDirectory.browse` sort | all five members                              | **`[]`** → hide the control         | **`[]`** → hide the control |
| `govActionList.browse` sort | `[…, mostYesVotes]`                           | `[newest, oldest, soonestToExpire]` | `[]`                        |

Blockfrost additionally reports `brokenCore: [account.votingPower,
transaction.confirmation]` — a deployment banner, not two silently missing
screens.

### Frontend integration

1. `getSystemFeatures()` — typed against the contract, not the backend handler.
   It passes `validateStatus: () => true` because the shared axios interceptor
   navigates the app to an error page on any 500, and an advisory capability
   fetch must never do that. Rejects `schemaVersion !== 2`.
2. Fetched in the existing `AppContextProvider` bootstrap.
3. Exposed through the existing `useFeatureFlag()` — no parallel mechanism.
4. A component reads `sortOptions={dRepDirectorySort.options}` and
   `isSorting={dRepDirectorySort.isAvailable}`.

The label↔member translation is typed: GovTool's control sends `"VotingPower"`,
the contract says `"votingPower"`, and the map's value type is
`OptionOf<'drepDirectory.browse', 'sort'>` — so a contract rename **fails the
frontend build** instead of silently emptying a dropdown.

**Everything fails open.** Loading, fetch failure, or an unknown feature id all
yield the full pre-capability UI.

## 6. The backend both raises and lowers capability

Proxying a provider's document to the browser would be wrong in both
directions, so the backend publishes its own document via
`composeCapabilities(providerDatasets, backendCapabilityPatch(doc))`:

- **Raises.** GovTool's backend reads the whole DRep directory into a snapshot
  and sorts, filters and pages **in memory**. Koios rejects every `DRepSort`
  member — and the UI sort still works. Gating on the raw provider document
  would disable a working feature.
- **Lowers.** `ada-holder.service.ts` catches every error on the voting-power
  route and returns `0`. A capability the backend destroys must not be
  advertised.

`GET /system/capabilities` returns the composed document (operators, support
bundles); `GET /system/features` returns the small derived set for the browser.

## 7. Drift is the failure mode, so it is tested three ways

The old table rotted because nothing checked it. Three layers now do:

1. **Type level** — 11 negative cases with `@ts-expect-error`, two of which
   were flipped to valid values to confirm they produce `TS2578` and are
   therefore real. Plus the two registry-sync assertions.
2. **`declarationProblems(doc)`** — served-but-carries-an-unavailability,
   not-served-without-a-reason, a `deploymentFault` baked into the static
   table, a `refusedRoutes` entry naming a foreign route, `unreviewed`
   contradicting `served`. Empty is the gate in each provider's
   `capabilities.spec.ts`.
3. **`refusalIsDeclared(table, entities, refusal)`** — because a provider
   throws the _same typed object_ the table declares, a test collects every
   refusal site and asserts each is predicted.

This is not theoretical. Auditing the three declarations immediately after they
were written found **32 drifts across 106 refusal sites** (24 fixed). The
sharpest: db-sync declared **zero** governance-action detail tabs, because a
needed filter declaration had been argued away in a code comment.

Live conformance remains a fourth layer the type system cannot replace: only a
real response proves a field declared `serves: 'always'` is actually populated.

## 8. Current state

| Package                       | Declaration                                  | Tests |
| ----------------------------- | -------------------------------------------- | ----- |
| `govtool-data-providers`      | the layer itself, 3,564 lines                | 19    |
| `govtool-provider-dbsync`     | `src/capabilities.ts`                        | 175   |
| `govtool-provider-koios`      | `src/capabilities.ts`                        | 197   |
| `govtool-provider-blockfrost` | `src/capabilities.ts`                        | 89    |
| `govtool-backend`             | `src/system/capabilities.ts` + two endpoints | 88    |
| `frontend`                    | request, hook, context, four gates           | 226   |

## 9. Known gaps

- **The three providers export the document under three different names**
  (`dbSyncCapabilities`, `koiosCapabilities`,
  `BLOCKFROST_CAPABILITY_DOCUMENT`). Should be one convention.
- **Nothing has been run against a live backend over HTTP.** Both sides are
  typed against `FeatureSet`, and the backend returns it unwrapped, but the
  round trip is unproven.
- **The backend's compensation patch is a hand-maintained claim.** "The
  directory is sorted in memory" is pinned by a test, not by a type.
- **`DeploymentOverlay` has a slot and no prober.** Until the cache-warmer
  emits overrides, db-sync's missing `utxo_view` is exactly as invisible as
  before — the model has a place for the fix, not the fix.
- **31 of 34 catalogue features are ungated** in the frontend, by design. The
  gates are UI-only: narrowing a dropdown does not stop a refused sort key
  being sent.
- **`narrowOptions`' `keyMap` types its key side as `string`**, so a stale UI
  key is not a build error — it silently drops that option.
