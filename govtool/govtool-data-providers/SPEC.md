# GovTool Governance Data API — Specification

**Version 1 draft · 2026-09-22**

This specifies the interface a **data provider** implements so that the GovTool
backend can read Cardano governance data from it. If you are building a
governance data source and want GovTool to run on it, this is the contract you
satisfy.

It is written to be read without access to GovTool's source. The TypeScript in
[`src/`](./src) states the same thing precisely and is where you read exact
shapes.

**This document is the decided state.** Where `src/` disagrees with it, `src/`
has not caught up yet — that is a backlog item, not a correction to this
document.

## Terms

| Term                  | Meaning                                                                                                                |
| --------------------- | ---------------------------------------------------------------------------------------------------------------------- |
| **DRep**              | A delegated representative. A registered credential that votes on behalf of delegated stake.                           |
| **Governance action** | A proposal on chain. Seven kinds; see [§5.2](#52-proposals).                                                           |
| **Vote aggregate**    | Per-role totals of votes cast on one action, with the threshold that applies.                                          |
| **Stake basis**       | `active` = the epoch-boundary snapshot the ledger decides by. `live` = the current value, which moves within an epoch. |
| **Anchor**            | An on-chain `{ url, dataHash }` pointing at an off-chain document.                                                     |
| **Lineage**           | A governance action _purpose_ — the chain of actions of one kind.                                                      |

---

## 1. Why a provider interface exists

GovTool's deployed backend reads the chain by querying a `cardano-db-sync`
database directly. That makes db-sync a hard dependency for anyone running
GovTool, and a db-sync schema change a breaking change in GovTool.

This interface replaces that with one seam. GovTool asks a provider for
governance data; the provider answers from db-sync, Koios, Blockfrost, a custom
indexer, or a fixture. Nothing above the seam knows which.

---

## 2. Components

Six components. Two are required; four are optional and each maps to a product
surface that disappears when it is absent.

| Component                  | Required | Source of truth              |
| -------------------------- | -------- | ---------------------------- |
| **Chain data**             | yes      | the ledger                   |
| **Metadata service**       | yes      | anchored off-chain documents |
| **Index provider**         | no       | a text index                 |
| **Committee info**         | no       | a curated list               |
| **Pinning service**        | no       | its own pin store            |
| **Transaction monitoring** | no       | the mempool                  |

A component exists where the **source of truth** differs — not where data is
merely inconvenient. That is the test for adding another.

```
chain data ──────────┐
                     ├──> index provider
metadata service ────┘
     ^
     └── resolves the anchors chain data emits
```

**Chain data never resolves a URL.** It emits anchors; the metadata service
fetches them. There is one documented exception, in [§5.2](#52-proposals).

---

## 3. Conventions

Every rule here applies across all components.

### 3.1 Identifiers

**One bech32 identifier per entity, in the latest CIP-recommended form.** No
alternate forms travel in this interface.

| Entity                          | Form      |
| ------------------------------- | --------- |
| DRep credential                 | CIP-129   |
| Governance action               | CIP-129   |
| Committee cold / hot credential | CIP-129   |
| Stake pool                      | `pool1…`  |
| Stake address                   | `stake1…` |

Other renderings — CIP-105, raw hex, `txHash#index`, shortened display forms —
are the consumer's business, not this interface's.

> **Decode, never string-match.** CIP-105 and CIP-129 DRep ids share the `drep1`
> bech32 prefix; CIP-129 prepends a header byte encoding the credential type.
> They cannot be told apart by prefix. A provider must decode and validate the
> header, and reject a non-conforming identifier rather than guessing. Accepting
> "anything starting with `drep1`" silently mis-resolves CIP-105 input.

### 3.2 Values

- **Lovelace is an integer string** — base-10 digits, no decimal point, no
  units. Never a number: ada totals exceed what a 64-bit float represents
  exactly, so a numeric type silently loses precision at the top of the range.
- **`Ratio` is two integers**, `{ numerator, denominator }`. `denominator: 0` is
  illegal. Normalization is not required, so **compare ratios by
  cross-multiplication**, never structurally.
- **An omitted field and an explicitly empty one mean different things.**
  A field **present with an explicit empty value** means the fact is known and
  the answer is "none" — this DRep never retired, this account delegates to
  nobody, this action has no predecessor. A field **omitted entirely** means
  this provider does not supply it, and the consumer knows nothing either way.

  Conflating them produces a wrong answer, not a vague one: "you are not
  delegated" and "we cannot tell whether you are delegated" are different
  statements. In JSON these are a present `null` and an absent key; in a typed
  language they are a nullable field and an optional one; in OpenAPI,
  `nullable: true` versus not being in `required`.

> Governance thresholds are exact rationals in the ledger. Some sources serve
> them as floats. Prefer a source that carries the rational; otherwise
> reconstruct by bounded-denominator continued fractions (cap ≈ 1000, which
> recovers `0.67 → 67/100` and `0.666… → 2/3` exactly) and say so.

### 3.3 Contract types are a floor, not a ceiling

A provider may return **more** than the contract defines. Use interface
extension; TypeScript method return types are covariant, so this needs no
contract change:

```ts
export interface KoiosProtocolParams extends ProtocolParams { extra: string }
getProtocolParams(): Promise<Envelope<KoiosProtocolParams>>   // legal
```

A provider may never **narrow**: it cannot drop a required field or retype one.

> Types hide extra fields; `JSON.stringify` does not. A consumer that forwards a
> provider object onward must serialize only contract fields, or provider-specific
> data reaches places that must not depend on it.

### 3.4 Paging

**One paging model, supported by every provider: `page` and `size`.**

```ts
interface PageRequest {
  page: number; // 1-based
  size: number;
}

interface Page<T> {
  elements: T[];
  total?: number;
} // whole result set, not this page
```

- **`page` starts at 1.**
- **`total` is optional but strongly recommended.** It is the count across the
  whole filtered result set, not the size of this page. With it, a consumer can
  render a numbered paginator and knows how many pages remain.
- There is no cursor and no opaque continuation token. This is sufficient for
  every read in this interface.

**When `total` is omitted**, a consumer detects the end of a collection by
receiving a **short page** — fewer than `size` rows — or an empty one. That is
the only signal available, so a provider must not return a short page for any
other reason. Supply `total` unless counting the filtered set is genuinely
infeasible: without it a numbered paginator cannot be drawn, and a consumer
reading a whole collection cannot know how far it has to go.

**Offset-style paging is not stable under mutation.** If the underlying set
changes between two page reads, a row can be seen twice or missed. Governance
data changes slowly relative to a user paging a list, so this is accepted.

**Random ordering is not paged** — see [§5.3](#53-dreps). It takes `size` only,
and a provider must reject a `page` other than 1 rather than shuffling again.

### 3.5 Errors

Providers throw a typed error with a stable code: `NOT_FOUND`, `INVALID_INPUT`,
`CAPABILITY_UNSUPPORTED`, `PROVIDER_UNAVAILABLE`, `PROVIDER_RATE_LIMITED`,
`PROVIDER_TIMEOUT`, `STALE_DATA`, `INTERNAL`.

**Refuse rather than fabricate.** A provider that cannot compute a value raises
a refusal. It does not return `0`, an empty list, or any placeholder that reads
as data.

### 3.6 No caching in a provider

The consumer owns caching. The one exception is the metadata service, whose
cache is part of its contract ([§6](#6-metadata-service)).

---

## 4. Declaring what you support

Two mechanisms, and they answer different questions.

| Question                              | Answered by                                                                            |
| ------------------------------------- | -------------------------------------------------------------------------------------- |
| **Is this available?**                | **The interface.** A present method or field means available; an absent one means not. |
| **Which option values are honoured?** | **The provider's spec JSON.**                                                          |

An optional member (`listDelegators?`, `committee?`) is how the interface says
a provider may not have something. A consumer checks for it — an absent member
is a `TypeError`, not a rejected promise, and the compiler enforces the check.

The spec JSON carries only what structure cannot express: which sort keys,
filter values, search modes and representations are honoured, and whether an
optional argument such as `epoch` is accepted. **There are no availability
booleans in it.** Option sets are arrays; a value absent from the array is not
supported, and `[]` is a complete refusal of that control.

**Declare accurately.** This interface states obligations; it does not defend
against a provider that advertises a capability and fails to deliver it. That
is an implementation bug, and the conformance suite is where it surfaces.

### Identifying yourself

A provider also states who it is, for attribution:

| Field  | Status                                       |
| ------ | -------------------------------------------- |
| `id`   | **required** — a free-form string you choose |
| `name` | **required** — human-readable                |
| `icon` | optional — a base64-inlined PNG              |

`id` is not drawn from a fixed list; a new provider needs no change here to
name itself. The icon is **inlined rather than a URL** so that attribution
renders with no second fetch.

---

## 5. Chain data

Everything derivable from the ledger. See [`src/chain-data/`](./src/chain-data)
for signatures.

### 5.1 Network

| Read                           | Status                                             |
| ------------------------------ | -------------------------------------------------- |
| `getNetworkInfo()`             | **required**                                       |
| `getProtocolParams()`          | **required**                                       |
| `getProtocolParams({ epoch })` | optional — declare it                              |
| `getStakeDistribution()`       | `totalActiveStake` **required**; the rest optional |
| `getTreasury()`                | optional                                           |
| `getGenesisParams()`           | optional                                           |

`ProtocolParams` is a **typed, named, camelCase** object — never a raw source
row — carrying **every protocol parameter the ledger holds in the current
era**, all required:

| Group           | Parameters                                                                                                                                                          |
| --------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Version         | `protocolVersion`                                                                                                                                                   |
| Governance      | `govActionLifetime`, `govActionDeposit`, `drepDeposit`, `drepActivity`, `committeeMinSize`, `committeeMaxTermLength`, the DRep and pool **threshold groups**        |
| Fees, deposits  | `minFeeA`, `minFeeB`, `minFeeRefScriptCostPerByte`, `keyDeposit`, `poolDeposit`, `coinsPerUtxoByte`                                                                 |
| Limits          | `maxBlockBodySize`, `maxBlockHeaderSize`, `maxTxSize`, `maxValSize`, `maxTxExecutionUnits`, `maxBlockExecutionUnits`, `collateralPercentage`, `maxCollateralInputs` |
| Plutus          | `executionUnitPrices`, `costModels`                                                                                                                                 |
| Pools, monetary | `poolRetireMaxEpoch`, `stakePoolTargetNum`, `poolPledgeInfluence`, `monetaryExpansion`, `treasuryCut`, `minPoolCost`                                                |

The whole set is required because each part is load-bearing somewhere:

- **Thresholds**: every action detail screen renders one.
- **Cost models and execution prices**: a wallet cannot build a Plutus-script
  transaction without them. That includes proposing a treasury withdrawal or a
  parameter change, both of which run the guardrails script.
- **Everything else**: a parameter-change screen shows the current value of
  whichever parameter the action changes.

Rational parameters (thresholds, prices, `a0`, `rho`, `tau`, the reference-script
fee) are `Ratio`s. `costModels` holds one integer array per Plutus language,
in the ledger's parameter order, and a language with no model in force is
absent. Source bookkeeping (row ids, the epoch nonce) and parameters the ledger
no longer has (`decentralisation`, `extraEntropy`, `minUtxoValue`) are not
carried.

**Genesis parameters** are the network's fixed constants from the Shelley
genesis: `networkMagic`, `networkId`, `systemStart`, `epochLength`,
`slotLength`, `activeSlotsCoefficient`, `securityParam`, `slotsPerKesPeriod`,
`maxKesEvolutions`, `updateQuorum`, `maxLovelaceSupply`. They are optional
because not every source keeps the genesis file (db-sync does not). They are
the Shelley values: on a network that began in Byron, such as mainnet, an
epoch-to-time conversion also needs the era boundaries.

`StakeDistribution` separates `totalActiveStake` (the only valid tally
denominator) from `totalLiveStake` (moves within an epoch, never a denominator),
and carries the governance breakdowns — DRep-controlled, pool-controlled, and
the two predefined targets — as optional fields.

Epoch and block listings are **not** in this interface. GovTool is not a block
explorer; chain data is in scope only where a governance decision depends on it.

### 5.2 Proposals

| Read                                | Status                   |
| ----------------------------------- | ------------------------ |
| `list()`, `get(id)`                 | **required**             |
| `getEnacted(lineage)`               | **required** — see below |
| vote aggregates                     | **required**             |
| `listVotes(id)` — individual votes  | optional                 |
| `listActivity(id)` — lifecycle feed | optional                 |

**The typed body is required for all seven action types** — `ParameterChange`,
`HardForkInitiation`, `TreasuryWithdrawals`, `NoConfidence`, `UpdateCommittee`,
`NewConstitution`, `InfoAction`. A source that cannot decode what an action
proposes cannot serve proposals. There is no untyped fallback field.

**`getEnacted` is a transaction-construction API, not a display feature.**
Submitting a governance action requires the `prevGovActionId` of the last
enacted action in the same **lineage**, and lineage is by _purpose_, not by type:

| Purpose        | Types                                    | Needs prev |
| -------------- | ---------------------------------------- | ---------- |
| `PParamUpdate` | `ParameterChange`                        | yes        |
| `HardFork`     | `HardForkInitiation`                     | yes        |
| `Committee`    | `UpdateCommittee` **and** `NoConfidence` | yes        |
| `Constitution` | `NewConstitution`                        | yes        |
| —              | `TreasuryWithdrawals`, `InfoAction`      | no         |

`UpdateCommittee` and `NoConfidence` **share one lineage**. A per-type
implementation returns the wrong id and the ledger rejects the transaction.
`null` means nothing of that lineage has ever been enacted — the genesis case.

**Vote aggregates.** Required, but the **representation is declared**:
`percent`, `stake`, `count`, one or more. `stake` is preferred — it is what the
ledger decides by for DReps and pools. The denominator must be **the total as it
stood for that action**, not the current total, so a percentage is reproducible
after the fact. A declared representation is served completely or not declared.

**Lifecycle** (`live`, `ratified`, `enacted`, `expired`, `dropped`) is returned
on the entity. Filtering by status is **optional**, and a provider that does not
support it must **declare and reject** it, never accept and ignore it.

**Sorting.** `newest` and `oldest` are **required**. `soonestToExpire`,
`mostYesVotes` and `highestParticipation` are optional and declared.
`soonestToExpire` is a distinct ordering, not a synonym for `oldest` —
`govActionLifetime` is itself a changeable parameter, so the two diverge across
a change. A consumer must never relabel one as the other.

**Voter context.** A caller may ask what a particular voter did, and the
obligation splits by shape:

- **On a single action — required.** Given an action and a voter, a provider
  says how that voter voted. It is one lookup at a time, so no source has an
  excuse.
- **On a listing — optional, declared.** Annotating or filtering a whole page
  by voter is the efficiency claim, and a provider may decline it.

A provider that declines the listing form causes the consumer to **hide its
"voted / not voted" filter entirely** — not to fall back to one request per row.

**Protocol-param snapshots** at submission and enactment are both optional.
Absent, a consumer falls back to `getProtocolParams({ epoch })`; absent that
too, it shows only the proposed values.

> **The one metadata exception.** A vote listing row may carry the governance
> action's **title**, denormalized, so a listing renders without N metadata
> lookups. This is the only resolved metadata anywhere in chain data. It is
> optional and deliberately narrow — the action `id` and `type` are required,
> the title is not, and nothing else from the document travels.

### 5.3 DReps

| Read                         | Status       |
| ---------------------------- | ------------ |
| `list()`, `get(id)`          | **required** |
| voting power, `active` basis | **required** |
| `liveVotingPower`            | optional     |
| `listVotes(id)`              | optional     |
| `listDelegators(id)`         | optional     |
| update history               | optional     |
| counts                       | optional     |
| search by exact id           | **required** |

**Anonymous DReps.** A DRep that registered with **no anchor** is _anonymous_.
That is the definition — an observable fact, not an inference about intent.
Anonymous DReps must not appear in search results unless their id is typed
directly. (This is self-enforcing for text search, which matches metadata they
do not have; it must be enforced explicitly for handle-based search.)

**Voting power** on the `active` basis is required. `live` is optional.

**Delegators**, when served, require `stakeAddress` and `activeVotingPower` per
row. `delegatedAt`, `liveVotingPower` and `previousDRepId` are optional.

**Votes.** The listing contains rows for actions **voted and not voted**, with a
filter between them. A not-voted row has no vote choice and no anchor.

**Activity** is `x / y` — actions voted out of actions votable — **since the
DRep registered**. Not a rolling window. `y` is the length of the unfiltered
vote listing, so the headline number and the list explaining it cannot disagree.

**Active vs inactive is a ledger fact.** The ledger tracks a DRep's expiry
epoch, pushed forward by `drepActivity` whenever the DRep votes or re-registers;
inactive means `currentEpoch > expiry`. **Read the expiry. Do not reconstruct
activity from vote timestamps.**

**Registration.** The latest registration and the latest registration-update are
**required**, each with a date. Full history is optional. Update history is a
**metadata-change feed** — each row an anchor and a date, sorted `asc` or `desc`.

**Counts**, if served, are all-or-nothing: `totalRegistered`, `totalActive`,
`totalInactive`. `anonymous` is optional within them.

**Sorting** is entirely optional — a provider supporting none is conformant.
The default ordering is **random**, so the directory does not become a rich
list. `status` is a filter, not a sort.

**A randomly ordered read is not paged.** It returns `size` rows (default 20)
and a provider rejects any `page` beyond the first, rather than reshuffling —
paging a freshly shuffled set would repeat some rows and omit others.

**Search** is a single term. **The caller does not name a mode** — it passes
what the user typed and the provider applies whatever it supports.

What the provider _declares_ is which kinds of input will match: `exactId`
(**required**), `substring`, `freeText`, `adaHandle`. That declaration exists so
a consumer can tell the user what is searchable — the difference between a box
labelled "Search by DRep ID" and one labelled "Search by name or ID". Without
it a user types a name into a box that only matches credentials and gets silence.

`substring`, `freeText` and `adaHandle` generally belong to the index provider,
which has an index. `exactId` is a credential lookup and needs none.

### 5.4 Accounts

The connected wallet's own stake key.

| Read                               | Status       |
| ---------------------------------- | ------------ |
| `get(stakeAddress)`                | **required** |
| `getDelegation()`                  | **required** |
| `getVotingPower()`                 | optional     |
| `balance`                          | optional     |
| delegation history (pool and DRep) | optional     |

Required core: `stakeAddress`, `stakeKeyHash`, `isRegistered`. `isScriptBased`
is optional — it is derivable from the CIP-129 header.

`alwaysAbstain` and `alwaysNoConfidence` are **not DReps.** They are predefined
delegation targets with no credential, anchor or registration. They appear in
exactly two places: as a delegation target, and as stake totals on the network.

**If you serve `balance`, serve its components** — `total` alone tells a
consumer nothing a wallet could not already supply. The point is `rewards` and
`rewardsRest`, which most wallet UIs omit and which are the usual explanation
for voting power exceeding an apparent balance.

### 5.5 Committee, constitution, pools, transactions

**Committee membership is required**: cold credential, hot credential, quorum,
term start and expiry. It is genuine ledger state — `UpdateCommittee` actions
are add/remove **deltas**, so current membership is assembled from the genesis
committee, every enacted `UpdateCommittee`, any enacted `NoConfidence`, the
`AuthCommitteeHotCert` and `ResignCommitteeColdCert` certificates, and term
expiry against the current epoch. It cannot be read off the latest action.

**A committee member is identified by the cold credential.** The hot credential
is rotatable — an id that changes on rotation breaks every reference. Hot is a
separate nullable field, absent until authorised.

On a **vote**, though, the chain carries only the hot credential. Resolving it
to the cold one is a join, so a vote's voter reference carries the cold
credential as an **optional** field. **Providers should resolve and supply it**;
there is no lookup method to fall back on, and a consumer that does not receive
it simply shows no voter information for that vote.

**The constitution is required, and derivable.** Each `NewConstitution` action
_replaces_ the previous one, and its body carries the anchor — so
`getEnacted(Constitution)` → `body.anchor` is sufficient. No dedicated
constitution resource is needed.

**Pool details are required.** Ticker and name are **not** part of them: pool
metadata is anchored by the pool registration certificate and is resolved by the
metadata service like any other document.

**A pool's own voting history is optional**, like every individual-vote listing
(only the per-action aggregate is required). It is what a pool's page renders,
and it is also what the index provider makes searchable — see [§7](#7-index-provider).

**Transactions.** The required surface is **whether the transaction is on
chain**. Nothing more.

---

## 6. Metadata service

Resolves anchors into documents. Required.

```ts
getMetadata(hash: Hex, url?: string): Promise<MetadataResult>;
getCipMetadata(cip: number, hash: Hex, url?: string): Promise<CipMetadataResult>;
refresh(hash: Hex, url: string): Promise<MetadataResult>;
```

```ts
getReport(reportId: string): Promise<MetadataReport | null>;
listReports(hash: Hex, url: string): Promise<MetadataReportSummary[]>;
```

`getCipMetadata` additionally validates against the named CIP. The CIP is a
**number** (100, 108, 119), so a new standard needs no interface change.

**Every failure has a fetch report** (D113–D115). A failure carries a one-line
message, a display `category` derived from its code (`NETWORK`,
`INVALID_CONTENT`, `SCHEMA_INVALID`), and a `reportId`. The report holds the
detail: DNS answers, each address tried and its outcome, redirects, the
response, every byte received up to the fetch limit, and content issues with
source ranges for highlighting. Reports are never deleted (D123) and hide
nothing from the user (D116).

**The service connects only to public unicast addresses** (D122), checked on
every resolved address and every redirect hop, and it connects to the address
it checked. A refusal is reported, not hidden.

**Content is cached permanently, keyed by hash.** This is safe because the hash
_is_ the content identity: if it matches, the bytes cannot have changed.
**Errors are cached for a bounded duration** — a failure describes the network at
a moment, not the content.

The fetch sequence is normative:

> Caller asks for hash **H** with url **U** → cache miss → fetch **U** → bytes
> **B** → compute **H′ = hash(B)** → **cache B permanently under H′** → if
> **H′ ≠ H**, return `HASH_MISMATCH`.

A mismatch still populates the cache under the hash actually served. The fetch
is not wasted, and the service accumulates a record of what a url really served
versus what the chain claimed.

**The hash is authoritative; the url is only a hint.** A cached hash is answered
whatever url accompanies it, and content is never looked up by url. The
_mismatch_ is a different fact from the content: it says what url **U** served at
one moment, and **U** can change. So `HASH_MISMATCH` is cached like any other
error, under **(U, H)** for the bounded duration, never permanently (D112).

`refresh` forces a re-fetch past a cached error, so a publisher who fixed their
hosting can verify it without waiting for expiry. It cannot be needed for a
cached success. It returns a `MetadataRefreshOutcome`: at most one real fetch
per (url, hash) per minute, whoever asks, and inside that window the latest
result with `retryAfterSeconds` (D125). It is expensive and amplifying, so a
backend should expose it with care (D93).

**Failure codes**, one per pipeline stage:

| Code               | Stage                                                                    |
| ------------------ | ------------------------------------------------------------------------ |
| `FETCH_ERROR`      | could not retrieve — DNS, timeout, refusal, TLS; detail in the message   |
| `EXCEEDS_LIMIT`    | larger than the service accepts                                          |
| `JSON_PARSE_ERROR` | retrieved, not parseable                                                 |
| `HASH_MISMATCH`    | parsed, hash does not match                                              |
| `SCHEMA_INVALID`   | hash-correct, violates the named CIP; message names the field and reason |

---

## 7. Index provider

Optional. Search over governance actions, DReps and pools: a **term query** plus
entity-specific filters. Consumes chain data _and_ the metadata service — the
searchable text lives in resolved documents.

Search is its own component because a text index is a different kind of thing
from a ledger read. A chain-data provider may implement it, but is not obliged
to, and neither half obliges the other:

```ts
interface GovernanceIndexV1 {
  dreps?: DRepSearchApi;
  proposals?: ProposalSearchApi;
  pools?: PoolSearchApi;
}
```

An implementer supplies what it has — one of the three, two, or all of them.

**Stake pools are indexed so a pool's historical voting record can be found and
shown.** That pairs with the optional pool vote history in
[§5.5](#55-committee-constitution-pools-transactions): chain data serves one
pool's votes, the index makes pools findable in the first place.

---

## 8. Committee info provider

Optional. Supplies the human identity behind a committee credential.

```ts
getMemberInfo(coldCredentialId: Bech32): Promise<CommitteeMemberInfo | null>;
```

Fields: `coldCredentialId` (required), then `name`, `organisation`,
`avatarUrl`, `country`, `bio`, `contactLinks`, `source`, `lastUpdatedAt` — all
optional. `null` means no curated record, which is normal and not an error.

This cannot be part of the metadata service, because **a committee member has
no anchor.** Every other off-chain document here is pointed at by the chain;
committee identity is not, so it must come from a curated source. `source`
records provenance, which — absent any authoritative registry — is the only
basis a consumer has for judging a name.

---

## 9. Pinning service

Optional. The only write path.

```ts
pinData(data: Buffer, owner: string): Promise<Cid>;   // resolves when pinned
getDataCid(data: Buffer): Promise<Cid>;               // compute only
unpin(cid: Cid): Promise<void>;
fetch(cid: Cid): Promise<Buffer>;
```

`pinData` **blocks until the pin is complete**. There is no pin status to poll
and no state machine: it resolves with the CID or rejects with a failure reason.

`getDataCid` computes without pinning, so an author can obtain the hash to
anchor before committing to storage.

**`owner`** — a DRep id, stake address or similar — is what makes quota, usage
monitoring and abuse blocking possible. It is supplied by the caller, which
knows the connected wallet; the pinning service cannot authenticate it. It is a
public chain identity, so recording it exposes nothing new.

The service also reports **backend health**. Authentication is a constructor
concern.

---

## 10. Transaction monitoring

Optional. For transactions GovTool itself just helped construct.

```ts
add(txHash: Hex, callback: (update: TxUpdate) => void): void;
```

Reports mempool presence, confirmation, and confirmation depth to 5, plus an
explorer link. The only push interface here; everything else is
request/response. It exists because **mempool visibility is something no
ledger-derived source has** — a transaction in the mempool is not on chain yet.

---

## 11. Conformance

A provider is conformant when it implements every required method in
[§5](#5-chain-data), declares its option sets accurately, and refuses rather
than fabricates.

**Verify against a live instance.** Fixture tests pass with mapping bugs in
place — four real mapping bugs once survived a green 147-test fixture suite and
were caught only against live data.

**Name the deployment.** A capability claim is about a _deployment_, not an
API. A self-hosted instance that 500s on an endpoint says nothing about the
hosted service. This has caused wrong conclusions more than once.

**Check derivability before reporting a gap.** A missing endpoint is not a
missing capability if the value is derivable from data already required — the
constitution is the worked example.

### Authentication, rate limiting and wire format

Out of scope. This interface is between a provider and the GovTool backend. How
the backend authenticates, rate-limits, or shapes its own HTTP responses is the
backend's business.
