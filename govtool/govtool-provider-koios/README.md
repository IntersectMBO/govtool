# @govtool/provider-koios

Koios implementation of the GovTool Chain Data contract
([`@govtool/data-providers/chain-data`](../govtool-data-providers)), over the
public [Koios](https://koios.rest) REST API.

It is a read model and nothing else: no caching, no request coalescing, no
background refresh. Those are policy, they differ per deployment, and the
consumer owns them — [`govtool-backend`](../govtool-backend) keeps its cache
and cache warmer in front of this. That matters more here than it does over a
local database: **every read is an HTTP round trip against a shared public
service with a rate limit.**

## Usage

```ts
import { createKoiosProvider } from '@govtool/provider-koios';

const provider = createKoiosProvider({
  network: 'mainnet', // or 'preprod' / 'preview'
  token: process.env.KOIOS_TOKEN, // omit for the free public tier
});

const { data } = await provider.governance.proposals.list({ limit: 10 });
```

Against a self-hosted instance, pass the base URL instead:

```ts
const provider = createKoiosProvider({
  baseUrl: 'https://koios.internal/api/v1',
  network: 'preprod', // skips the /genesis read used to name the network
});
```

Zero HTTP dependencies: it uses the global `fetch` Node 20 ships. A custom
`fetch` can be injected for tests or for an agent-backed client.

## What it costs

| Read                                                       | Requests                              |
| ---------------------------------------------------------- | ------------------------------------- |
| `dreps.get`, `accounts.get`, `proposals.get`, `votes.list` | 1–3                                   |
| `dreps.list` (a page)                                      | 2, plus 1 with `expand: ['metadata']` |
| `accounts.listDelegationHistory`                           | 2, whatever the length                |
| `proposals.list` with `expand: ['tallies']`                | 1 + one per row                       |
| `dreps.getVotingPowers()` with no ids                      | one per 1000 DReps, twice             |

`proposal_voting_summary` takes a single `_proposal_id`, so tallies cannot be
joined into a listing. Expanding them on a 20-row page is 21 requests; on the
public tier, expect to meet the rate limiter. `PROVIDER_RATE_LIMITED` carries
`retryAfterSeconds` when Koios sends `Retry-After`.

---

# What Koios cannot serve

Every gap is declared at `system.getCapabilities()`, which returns the
`ProviderCapabilityDocument` built in
[`src/capabilities.ts`](./src/capabilities.ts): one entry per dataset, with the
sort keys, filter values, expands, bases, search modes and paging behaviour
each one carries, plus a field-level table over every optional field of every
entity.

Three things that document says which a route-level `supported` /
`partial` / `unsupported` table could not:

- **refused vs ignored.** `dreps.listVotes` and `pools.listVotes` accept a
  `VoteListQuery` and read nothing off it but the page, so a `vote` filter or a
  `votingPower` sort is silently dropped rather than refused. Those are
  declared `ignored`, and a UI must not offer them — a refusal-only model
  reported them as working.
- **no search at all on `dreps.list`.** Not just no free text: an exact CIP-129
  id is refused too, because `/drep_list` has no id filter. Only the empty
  string is tolerated, since GovTool's backend always sends the parameter.
- **an omitted `limit` is one page, not everything.** PostgREST caps a response
  at 1000 rows, so `omittedLimitMeans: 'oneMaxPage'`; `total` is `estimated`,
  because every paged read asks for `count: 'estimated'`.

Every `CAPABILITY_UNSUPPORTED` this package can throw is listed as a typed
refusal in `KOIOS_REFUSALS`, and `test/capabilities.spec.ts` asserts the
declaration predicts each one — so a refusal site added without a matching
declaration fails the test run.

The shape of the gaps is different from db-sync's. There, the limit is a frozen
set of 17 SQL statements written for GovTool's screens. Here, every endpoint
Koios publishes is available, and what is missing is what **Koios itself does
not record**.

## 1. Missing outright — no endpoint, no workaround

| Contract route                          | Why                                                                                                                |
| --------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| `surveys.getDefinition`                 | See §2. The only GovTool feature with no Koios path at all.                                                        |
| `governance.dreps.listDelegationEvents` | `/drep_delegators` is a snapshot. There is no join/leave stream, and no delegation timestamps anywhere.            |
| `governance.voters.list`                | No combined voter index. DReps, pools and committee members are three unrelated endpoints with no common ordering. |
| `governance.metrics.get`                | Six required fields need a walk over every DRep. See §4.                                                           |
| `accounts.get{expand:drep}`             | Koios cannot say whether a stake credential is _also_ registered as a DRep — the two never appear in one response. |
| `accounts.get{expand:adaHandles}`       | Needs an asset lookup; out of scope for a chain-data provider.                                                     |

## 2. CIP-179 surveys — the one hard blocker

The contract returns the label-17 metadata payload as **CBOR, hex-encoded**,
and is explicit about why: the payload format is versioned by the CIP, so the
consumer decodes it.

Koios only ever exposes metadata **decoded**. `/tx_metadata` returns the
label's value as JSON, which has already lost the CBOR's type information — map
key types, definite vs. indefinite length, byte strings vs. text — so it cannot
be re-encoded to the bytes the survey was published as.

`/tx_cbor` does return raw bytes, but for the whole transaction. Extracting the
auxiliary data means a CBOR parser and a transaction-layout decoder inside the
provider: implementation the contract deliberately keeps out, and a correctness
risk the feature does not justify.

**A deployment that needs surveys keeps the db-sync provider for this route.**

## 3. Fields Koios does not record

These routes work; the named field is absent.

| Field                                               | Note                                                                                                                                                                                                                                                                             |
| --------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `VoteRecord.votingPower`                            | **The biggest one.** Koios has per-role totals (`/proposal_voting_summary`) and per-DRep snapshots (`/drep_voting_power_history`), but never the power applied to an individual vote. A "who voted, with how much weight" table cannot be built.                                 |
| `ProtocolParams.dvt` / `.pvt`                       | Thresholds arrive as IEEE-754 doubles (`0.67`, `0.51`); the contract's `Ratio` is the exact on-chain numerator/denominator. `0.67` is not `2/3`. The floats stay reachable in `raw`. **Same gap db-sync has** — the loss happens at the ledger→db-sync hop, not in the provider. |
| `RoleTally.threshold` / `.passing`                  | Follows from the above. Koios publishes a `*_pct`, but a percentage is a rendering of a comparison the consumer makes from the ratio.                                                                                                                                            |
| `RoleTally.totalEligibleStake`                      | No per-action eligible-stake denominator.                                                                                                                                                                                                                                        |
| `DRepDelegator.since` / `.txRef`                    | `/drep_delegators` gives address, epoch and amount only.                                                                                                                                                                                                                         |
| `DRep.kind`                                         | GovTool's "direct (sole) voter" is a stake credential registered as its own DRep. Koios has no way to ask that, so `kind` is always `'drep'` — declared `unsupported`, never guessed.                                                                                            |
| `DRepActivity.notVotedCount` / `.participationRate` | Needs the set of actions votable while the DRep was registered; not expressible as a Koios query. `votesCast` **is** available (exact count, one row-free request) — but over the DRep's whole history, where db-sync counts a trailing 365 days.                                |
| `CommitteeMember.termStartEpoch`                    | `/committee_info` reports expiry only.                                                                                                                                                                                                                                           |
| `StakeRegistrationEvent.block`                      | `/account_updates` reports the absolute slot, not the block height.                                                                                                                                                                                                              |
| `StakeDistribution.totalLiveStake`                  | No un-snapshotted network total.                                                                                                                                                                                                                                                 |
| `GovAction.protocolParamsAtSubmission`              | Not joinable; call `network.getProtocolParams({ epoch })` separately — which Koios _can_ serve for any epoch.                                                                                                                                                                    |

## 4. Aggregates: `governance.metrics.get`

Refused as a whole, because six of `GovernanceMetrics`' **required** fields
cannot be computed:

| Field                             | Why                                                            |
| --------------------------------- | -------------------------------------------------------------- |
| `uniqueDelegators`                | `/drep_delegators` once per DRep, de-duplicated                |
| `totalDelegations`                | same walk                                                      |
| `totalActiveDReps`                | the `active` flag is on `/drep_info`, which takes explicit ids |
| `totalInactiveDReps`              | same                                                           |
| `totalActiveCip119CompliantDReps` | needs every DRep's metadata body                               |
| `totalRegisteredDirectVoters`     | Koios does not model the direct voter                          |

Answering the route means well over a thousand requests, or filling required
numbers with zeros a dashboard would render as fact. Neither is acceptable, so
the route throws and names all six.

**`governance.metrics.getAvailable()`** returns the computable two-thirds as a
`Partial<GovernanceMetrics>` — proposal and vote totals, the registered-DRep
count, the DRep distribution, the committee and the treasury, one cheap request
each. It is not part of `ChainDataApiV1`: a caller reaches it through the
concrete provider, having decided what to do about the missing six.

Closing this properly needs a decision, not more code: either loosen those six
fields to optional on the contract (and default them in the backend), or accept
that `/network/metrics` is a db-sync-only route.

## 5. Query capabilities, not data

The data exists; Koios cannot filter or order by it, and the provider refuses
rather than applying the operation to an arbitrary page and presenting it as a
ranking.

- **No text search anywhere** — `dreps.list{search}`, `proposals.list{search}`,
  `votes.list{search}`. There is no index over names, titles or rationales.
- **`dreps.list{sort}`** — `/drep_list` carries only `drep_id`, `hex`,
  `has_script` and the registration flag. Voting power, registration date and
  activity all live on `/drep_info`, which takes explicit ids. Sorting the
  hydrated page would order 1000 arbitrary rows.
- **`proposals.list{sort}`** by `mostYesVotes` / `highestParticipation` — vote
  weights are on `/proposal_voting_summary`, which cannot be joined in.
- **`proposals.list{status}`** with more than one status — status is four
  nullable epoch columns, not a column; one at a time translates cleanly.
- **`accounts.getVotingPower{epoch}`** — balances at the tip only.
- **`votes.get{index}`** — Koios does not number voting procedures within a
  transaction.
- **`votes.list#isCurrent`** is `partial`. Supersede detection is exact when
  the read is scoped to one proposal or one voter, and best-effort over an
  arbitrary page of the global feed.

## 6. Two known wire-level hazards

Found by diffing the published `koiosapi.yaml` (v1.4.2) against a live mainnet
instance. Both are handled defensively in [`src/rows/index.ts`](./src/rows/index.ts):

1. **The spec and the deployment disagree.** `/drep_list` documents a
   `drep_status` enum but serves a `registered` boolean. `/committee_info`
   documents only hex credentials but serves the CIP-129 bech32 ids too.
   `/account_info` documents `proposal-refund` and serves `proposal_refund`.
   Every such field is read both ways.
2. **`proposal_type` says `NewCommittee`, `proposal_description.tag` says
   `UpdateCommittee`.** Koios kept the pre-ratification name on the column
   only. The provider renames the column to the ledger name and trusts the tag.

---

# What Koios does better than db-sync

Switching is not only a loss. Against `@govtool/provider-dbsync` and its frozen
SQL, these routes go from `unsupported` to working:

| Area                                 | Gain                                                                                                                                                                                                                                                                              |
| ------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Typed governance action bodies**   | `proposal_description` is the ledger's own JSON rendering, so all seven variants are typed — `ParameterChange` changes, `TreasuryWithdrawals` targets, `UpdateCommittee` added/removed members and quorum, `NewConstitution` anchors. db-sync hands the consumer an untyped blob. |
| **Votes, pools, committee, tallies** | `votes.*`, `pools.*`, `committee.*` and `proposals.getTallies` / `listVotes` / `listActivity` / `listByTx` are all `unsupported` on db-sync.                                                                                                                                      |
| **Per-epoch voting power**           | `/drep_voting_power_history` is a real series. db-sync's `get-voting-power.sql` is `ORDER BY epoch_no DESC LIMIT 1` — it can only ever answer "now".                                                                                                                              |
| **Protocol params for any epoch**    | A two-year-old ParameterChange action can be rendered against the parameters in force. db-sync serves the current epoch only.                                                                                                                                                     |
| **Metadata status**                  | Koios resolves and validates off-chain metadata and reports `is_valid`, so `MetadataStatus` is a read, not a guess. Still no fetching here — the flag arrives in the same response as the chain data.                                                                             |
| **DRep `active` / `inactive`**       | Koios applies the `drepActivity` rule itself. db-sync's directory query cannot derive it.                                                                                                                                                                                         |
| **Transaction effects**              | `/tx_info` with `_governance: true` classifies votes, proposals, DRep registrations and delegations. `transactions.get#effects` is `unsupported` on db-sync.                                                                                                                      |
| **Network identity & epochs**        | `networkMagic`, `era`, historical epochs, blocks and the treasury — all `unsupported` on db-sync.                                                                                                                                                                                 |
| **Measured staleness**               | The tip carries a timestamp, so `getHealth()` reports real `secondsSinceLastUpdate` against a 300s threshold.                                                                                                                                                                     |
| **Delegation history**               | `/account_updates` plus one batched `/tx_info` reconstructs targets; `unsupported` on db-sync.                                                                                                                                                                                    |

## One deliberate reconstruction

`accounts.getVotingPower` does **not** return Koios' `total_balance`.

GovTool's `get-stake-key-voting-power.sql` computes
`utxo + rewards + reward_rest − withdrawals`, subtracting withdrawals only when
the reward tables exceed them. Koios' `total_balance` is
`utxo + rewards − withdrawals`: no `reward_rest`, no guard — so it returns a
**negative** number for any account that has withdrawn more than its recorded
rewards, which real mainnet accounts do.

The provider recomputes from the component columns Koios does expose
(`reserves + treasury + proposal_refund` being db-sync's `reward_rest`), so
this route agrees with the db-sync provider instead of quietly disagreeing
with it. `test/accounts.spec.ts` pins the formula, including the `>` boundary.

---

## Development

```bash
npm install
npm run verify   # format:check, lint, typecheck, test, build
```

Tests run against `test/fake-http.ts`, a `fetch` double with fixtures taken
from live mainnet responses.

Because a fixture cannot notice that Koios changed, there is a second,
opt-in check that hits the real API:

```bash
npm run build && npm run smoke          # mainnet
KOIOS_NETWORK=preprod npm run smoke     # or another deployment
```

It exercises 26 routes end to end and exits non-zero on any failure. It is not
part of `verify` — it needs the network and mainnet data moves — but it is the
only thing that catches `src/rows` drifting from what Koios actually serves.
All 26 pass against mainnet as of Koios v1.4.2 / epoch 656.

**Still untested:** preprod and preview (only mainnet has been run), and any
Koios deployment older than the one behind `api.koios.rest` — see §6 on the
spec/deployment divergence.

## Verified against live Koios

Three checks, only the first in `npm run verify`:

```bash
npm run verify       # format, lint, typecheck, 148 unit tests, build
npm run smoke        # live: 26 routes answer
npm run conformance   # live: are the answers valid contract instances?
```

`conformance` is the one that finds bugs. It walks each real response and
checks the contract's own rules — required fields present, lovelace as a
decimal string and never negative, `EpochStamp` carrying at least one of its
two halves, `Page`/`Envelope` complete, voter hashes non-empty. A fixture
cannot find these, because the fixture is written by whoever wrote the mapper.

Four defects it caught that 147 passing unit tests did not:

| Defect                                             | Cause                                                                                                                                                                                                                                       |
| -------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `balance.total` was **negative** (−98,218,279,141) | Koios' `total_balance` omits `proposal_refund` from the credit side while still subtracting all withdrawals. `mapVotingPower` already guarded against this; `mapBalance` did not — and a unit test asserted the negative value as expected. |
| SPO and committee `voter.hash` were `''`           | `/vote_list` returns `voter_hex: null` for every role, so the hash must be decoded from the bech32 id. Only the DRep branch did it.                                                                                                         |
| `pools.listVotes` voter hash was `''`              | `/pool_votes` returns no voter columns at all, and the voter was hand-rolled with `hash: ''` instead of going through `toVoterRef`.                                                                                                         |
| `listBlocks` **never returned**                    | `order=block_height.desc` makes Koios sort the whole block table: >70s versus ~1s without it. `/blocks` is already newest-first.                                                                                                            |

Plus two weak assertions: `smoke` and `conformance` both picked
`elements[0]` from the DRep directory, which on mainnet is a _retired_ DRep
carrying `votingPower.amount === "0"`, so the activity and power-history
shapes were never exercised. Both now select a DRep with non-zero power.
