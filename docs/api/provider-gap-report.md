# Provider gap report — what implementing the contract three times taught us

Status: **2026-09-18.** Written from three finished implementations of
`chain-data`:

| Package                                                                     | Source                             | Verified against            |
| --------------------------------------------------------------------------- | ---------------------------------- | --------------------------- |
| [`@govtool/provider-dbsync`](../../govtool/govtool-provider-dbsync)         | the legacy GovTool SQL, unmodified | preview db-sync, epoch 1424 |
| [`@govtool/provider-koios`](../../govtool/govtool-provider-koios)           | public Koios REST API              | mainnet, epoch 656          |
| [`@govtool/provider-blockfrost`](../../govtool/govtool-provider-blockfrost) | blockfrost-ryo 3.1.1               | mainnet, epoch 656          |

Two headlines.

**The contract is well-targeted.** Of 40 routes, only **two** are unsupported
by all three providers. Everything else is served by at least one.

**Coverage is wildly uneven, and not in the direction the draft assumed.**
Koios serves 22 routes fully; db-sync and Blockfrost serve 4 each. The draft
was written against db-sync's shape, and db-sync turns out to be the _least_
complete of the three for everything except aggregates.

> This report supersedes the two-provider version. One finding in it —
> "neither provider can serve `governance.committee.*`" — was **wrong**, and
> was wrong precisely because it generalised from two data points. Koios
> serves the committee and the constitution in full. §5 records the retraction.

---

## 1. Contract changes made so far

All were **loosenings** — a required field became optional. Safe for
producers, and they break consumers that read the field unguarded, which is
the point: the compiler then shows every place that had assumed one source's
shape.

`0.1.0 → 0.2.0` (db-sync): ~25 fields. `0.2.0 → 0.3.0` (Koios, then
Blockfrost): nine more.

| Field                                        | Who could not fill it          | Who can            |
| -------------------------------------------- | ------------------------------ | ------------------ |
| `EpochStamp.time`                            | Koios, Blockfrost (epoch-only) | db-sync            |
| `EpochStamp.epoch`                           | db-sync (time-only)            | Koios, Blockfrost  |
| `GovActionLifecycle.submitted`               | Blockfrost                     | db-sync, **Koios** |
| `VoteRecord.at`                              | Blockfrost                     | db-sync, **Koios** |
| `StakeDistribution`'s four governance totals | Blockfrost                     | db-sync, Koios     |
| `DRepHistoryEvent.at`, `.anchor`             | Blockfrost                     | Koios              |
| `StakeRegistrationEvent.at`/`.slot`/`.block` | Blockfrost, Koios (partly)     | db-sync            |
| `DelegationHistoryEvent.at`, `.from`         | Blockfrost                     | Koios              |

The right-hand column is the useful one: **every loosening was needed by
exactly one provider**, and in each case another provider fills the field. So
none of these should be re-tightened, and none of them is a hole in the data
model — they are holes in specific sources.

---

## 2. Coverage, three ways

`Y` served · `~` partial · `N` declared `CAPABILITY_UNSUPPORTED` · `–` namespace absent

Generated from the providers' own `*_CAPABILITIES` tables, each cross-checked
against live behaviour.

| Route                                          | db-sync | Koios | Blockfrost |
| ---------------------------------------------- | :-----: | :---: | :--------: |
| `network.getNetworkInfo`                       |    ~    |   Y   |     Y      |
| `network.getProtocolParams`                    |    ~    |   ~   |     ~      |
| `network.getStakeDistribution`                 |    ~    |   ~   |     ~      |
| `network.getTreasury`                          |    N    | **Y** |     N      |
| `network.listEpochs`                           |    N    | **Y** |     ~      |
| `network.listBlocks`                           |    N    | **Y** |     ~      |
| `accounts.get`                                 |    ~    |   ~   |     ~      |
| `accounts.getDelegation`                       |    Y    |   Y   |     ~      |
| `accounts.getVotingPower`                      |    ~    | **Y** |     N      |
| `accounts.listDelegationHistory`               |    N    | **Y** |     ~      |
| `accounts.listStakeEvents`                     |    N    |   ~   |     ~      |
| `governance.dreps.list`                        |    Y    |   ~   |     ~      |
| `governance.dreps.get`                         |    ~    | **Y** |     ~      |
| `governance.dreps.getVotingPower`              |    ~    | **Y** |     ~      |
| `governance.dreps.getVotingPowers`             |    Y    |   Y   |     ~      |
| `governance.dreps.listDelegators`              |    N    |   ~   |   **Y**    |
| `governance.dreps.listHistory`                 |    N    | **Y** |     ~      |
| `governance.dreps.listVotes`                   |    ~    | **Y** |     N      |
| `governance.dreps.listDelegationEvents`        |    N    |   N   |     N      |
| `governance.proposals.list`                    |    ~    |   ~   |     ~      |
| `governance.proposals.get`                     |    ~    | **Y** |     ~      |
| `governance.proposals.listVotes`               |    N    |   ~   |   **Y**    |
| `governance.proposals.getTallies`              |    N    |   ~   |     ~      |
| `governance.proposals.listActivity`            |    N    |   ~   |     N      |
| `governance.proposals.getEnacted`              |    ~    | **Y** |     ~      |
| `governance.proposals.listByTx`                |    N    | **Y** |   **Y**    |
| `governance.votes.list`                        |    N    |   ~   |     N      |
| `governance.votes.get`                         |    N    | **Y** |     N      |
| `governance.pools.get`                         |    N    | **Y** |     N      |
| `governance.pools.list`                        |    N    |   ~   |     N      |
| `governance.pools.listVotes`                   |    N    | **Y** |     N      |
| `governance.committee.getCommittee`            |    N    | **Y** |     N      |
| `governance.committee.getMember`               |    N    | **Y** |     N      |
| `governance.committee.getConstitution`         |    N    |   ~   |     N      |
| `governance.committee.listConstitutionHistory` |    N    |   ~   |     N      |
| `governance.voters.resolve`                    |    N    | **Y** |     N      |
| `governance.voters.list`                       |    N    |   N   |     N      |
| `governance.metrics.get`                       |    ~    |   N   |     N      |
| `transactions.get`                             |    ~    | **Y** |     N      |
| `surveys.getDefinition`                        |  **Y**  |   N   |     –      |

|            | supported | partial | unsupported |
| ---------- | :-------: | :-----: | :---------: |
| db-sync    |     4     |   13    |     23      |
| **Koios**  |  **22**   |   14    |    **4**    |
| Blockfrost |     4     |   17    |     18      |

### The two universal gaps

- **`governance.dreps.listDelegationEvents`** — join/leave history per DRep.
  All three can list _current_ delegators; none records the transitions.
- **`governance.voters.list`** — a role-agnostic voter directory. Each
  provider indexes DReps, pools and committee members separately, and none
  has a union.

Both are genuinely absent from every source, not artefacts of one shape.

### Where each provider is alone

- **db-sync only:** `governance.metrics.get` (one SQL query; see §3.2) and
  `surveys.getDefinition` (CIP-179 tx metadata).
- **Koios only:** the committee, the constitution, pools as voters, the
  cross-cutting vote feed, treasury, `transactions.get`, and a DRep's own
  voting record joined to the actions.
- **Blockfrost only:** nothing exclusively — `dreps.listDelegators` and
  `proposals.listVotes` are `supported` there and `partial` on Koios, which is
  a difference of degree, not of kind.

---

## 3. Findings

### 3.1 Aggregates are the one thing an HTTP API cannot do, and the contract has no way to say "partly"

`GovernanceMetrics` has 13 required fields. db-sync answers all of them in one
query. Koios can compute seven cheaply and needs **a walk over every DRep** for
the other six (`uniqueDelegators`, `totalDelegations`, `totalActiveDReps`,
`totalInactiveDReps`, `totalActiveCip119CompliantDReps`,
`totalRegisteredDirectVoters`). Blockfrost can compute none.

Because the fields are required, Koios' only contract-legal options were to
refuse the whole route or to fill six numbers with zeros that a dashboard
would render as fact. It refused — and then added **`getAvailable()`, a method
outside `ChainDataApiV1`**, returning `Partial<GovernanceMetrics>`.

That escape hatch is the right call and the wrong place. A provider inventing
non-contract methods is how a contract stops being one.

**Recommendation.** Put the partial read in the contract:

```ts
export interface MetricsApi {
  /** Every counter, or CAPABILITY_UNSUPPORTED. */
  get(q?: { epoch?: EpochNo }): Promise<Envelope<GovernanceMetrics>>;
  /** Whatever this provider can compute cheaply. Always answers. */
  getAvailable(q?: {
    epoch?: EpochNo;
  }): Promise<Envelope<Partial<GovernanceMetrics>>>;
}
```

db-sync implements `getAvailable` as `get`; Koios and Blockfrost implement what
they have. GovTool's dashboard then degrades per tile instead of per page.

### 3.2 Cost is invisible, and it inverts between providers

The same call has wildly different cost depending on the source:

| Read                            | db-sync | Koios             | Blockfrost                |
| ------------------------------- | ------- | ----------------- | ------------------------- |
| `dreps.list` (one page)         | 1 query | 2–3 requests      | **2 per element**         |
| `metrics.get`                   | 1 query | >1,000 requests   | impossible                |
| `proposals.getTallies`          | —       | 1 request         | 1 + one per page of votes |
| `dreps.getVotingPowers()` (all) | 1 query | 1 per 1,000 DReps | refused: 1 per DRep       |

Nothing in the signatures hints at this. A consumer written against db-sync
will melt a Blockfrost deployment, and the contract's "omit `limit` to get
everything" is actively dangerous on two of the three.

**Recommendation.** Extend `CapabilityLevel` with a cost class:
`'indexed' | 'fanout' | 'scan' | 'unsupported'`. `fanout` means one request
per element; `scan` means a bounded crawl (Blockfrost's `getEnacted`). This is
additive, breaks nothing, and remains the highest-value change on the list.
Also make `limit` required on list routes, or declare a per-provider maximum —
all three providers already clamp it, silently.

### 3.3 `RoleTally` needs to say what its numbers _are_

Three providers, three shapes for the same tally:

- **db-sync**: stake sums, no counts. Threshold-relevant.
- **Koios**: stake sums **and** counts, plus `notVotedStake` and
  `totalEligibleStake` — everything a threshold bar needs.
- **Blockfrost**: counts only. For DReps and SPOs the ledger decides by
  _stake_, so a Blockfrost DRep tally is **turnout**, not weight.

A consumer receiving `{ role: 'drep', count: {...} }` cannot tell which of
these it has. Making both fields optional (0.2.0) removed the type error and
left the ambiguity.

**Recommendation.**

```ts
export interface RoleTally {
  role: VoterRole;
  /** What the ledger counts for this role: stake for drep/spo, heads for cc. */
  decidedBy: 'stake' | 'count';
  stake?: Record<VoteChoice, Lovelace>;
  count?: Record<VoteChoice, number>;
  /** True when the figure present is the one `decidedBy` names. */
  isAuthoritative: boolean;
  …
}
```

### 3.4 A vote is indexed differently per source, and one shape only fits two of three

`dreps.listVotes` returns `VotedGovAction` (`vote` + the full `proposal`),
which bakes a join into the read model. db-sync and Koios both do that join
happily. Blockfrost cannot: `/governance/dreps/{id}/votes` returns
`{tx_hash, cert_index, vote}` where the pair locates the **vote**, and nothing
says which action it was cast on. Reconstructing it is ~1,500 requests per
DRep.

So the data — "this DRep voted yes 40 times" — exists on Blockfrost and cannot
be expressed.

**Recommendation** (unchanged, but now known to serve one provider of three,
so lower priority than the two-provider report claimed): add a join-free
`VoteCast` and let `listVotes` return it, with the joined `VotedGovAction`
behind `expand: ['proposal']`.

### 3.5 Identifier encodings: the contract's insistence on one form is earning its keep

- **Koios** speaks CIP-129 natively and consistently — `drep_id`, `cc_hot_id`,
  `proposal_id` all arrive canonical. The least work of the three.
- **Blockfrost** is inconsistent _within itself_: `/governance/dreps` returns
  CIP-129, `/accounts/{stake}` returns CIP-105 for the same credential, and
  committee voters come back as raw hex.
- **db-sync** stores raw hashes plus the pre-CIP-129 `view`.

`VoterRef.cip105Id` (added 0.2.0) is carrying real weight. Worth stating in
`refs.ts` as a rule rather than a convention: **a provider must normalise, and
`id` is always CIP-129.** It was a live bug in two of the three providers until
real data caught it.

### 3.6 Fields a provider is _forced_ to misreport

Three places where the contract leaves no honest answer:

- **`DRep.registration.deposit`** — Blockfrost has no deposit field, so it
  sends `null`, which the contract defines as "known absent on chain". False.
- **`DRepActivity.votesCast`** — required, so Blockfrost sends `0` when it
  knows only the last-active epoch. Reads as "never voted".
- **`GovAction.previousAction`** — required-nullable, and Blockfrost never
  reports it, so `null` claims the action has no predecessor.

All three should be optional. `null` and "unknown" are different statements and
the contract already has a word for each — the required-nullable pattern just
forces the wrong one.

---

## 4. Smaller items

- **`ProtocolParams.dvt`/`pvt` are unfillable by all three.** db-sync, Koios
  and Blockfrost every one store the thresholds as floating point, and the
  contract wants exact ratios. The typed groups are dead weight: either accept
  floats in a separate field or let `raw` carry them.
- **`meta.asOf` works well.** Providers fill it where the tip was already
  fetched and omit it otherwise, exactly as designed.
- **`surveys` being optional on `ChainDataApiV1` was right.** db-sync serves
  it, Koios declares it unsupported, Blockfrost omits the namespace entirely —
  three different, all legitimate, answers.
- **`accounts.get`'s `expand` model works.** Blockfrost gets balance,
  delegation and pool delegation in one read; db-sync needs three. The
  consumer asks for what it needs and neither provider over-fetches.

---

## 5. Retraction

The two-provider report said:

> **3.4 Neither provider can serve `governance.committee.*`** … A contract
> namespace that nothing can implement is a liability.

**This was wrong.** Koios serves `getCommittee`, `getMember` and
`getConstitution` from `/committee_info` and `/constitution`, verified live: 7
members, quorum 2/3. The namespace is not aspirational and should not be moved
behind a separate `GovStateApi`.

The error came from generalising a gap shared by two sources into a property
of the data. Worth keeping in mind for the next provider: **a gap is only
structural once a source that ought to have the data also lacks it.**

---

## 6. What to do, in order

1. **§3.2 cost classes.** Additive, breaks nothing, and prevents a consumer
   written against db-sync from melting Blockfrost.
2. **§3.1 `getAvailable()` into `MetricsApi`.** Koios already implements it
   outside the contract; this only moves it inside.
3. **§3.6 three fields to optional.** One-line changes that stop three
   providers from asserting things they do not know.
4. **§3.3 `RoleTally.decidedBy`.** Needs a consumer-side migration, so it
   wants a deliberate slot.
5. **§3.4 `VoteCast`.** The only breaking change proposed, and it serves one
   provider of three. Do it when Blockfrost's DRep voting record is actually
   wanted.

Leave every loosening in §1 alone: each is load-bearing for a provider that
exists today.

### Method note

The bugs in this round were **not** found by unit tests. All three providers
have fixture-based suites that passed while the defects were live, because a
fixture is written by the same person as the mapper and encodes the same
misunderstanding — in one case a test asserted a negative account balance as
expected output.

What found them: a **live conformance script** that walks real responses and
checks them against the contract's required fields, lovelace-as-string rule and
the `EpochStamp` invariant; and a **capability cross-check** that exercises
every route and compares the outcome to the provider's own declared table.
[`govtool-provider-koios/scripts/conformance.mjs`](../../govtool/govtool-provider-koios/scripts/conformance.mjs)
is the reference implementation — it belongs in a shared package so every
provider runs the same checks.
