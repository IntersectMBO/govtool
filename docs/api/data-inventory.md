# Chain Data API v1 — data inventory

Working draft for [#4221](https://github.com/IntersectMBO/govtool/issues/4221).

Purpose: enumerate **every piece of data GovTool consumes**, so the canonical domain
types and the provider capability matrix (db-sync #4222, Koios #4223,
Blockfrost #4234, Kupo #4235) can be derived from one list.

Columns:

- **Today** — where the current stack gets it (`backend` = Haskell VVA endpoint, `sql` = db-sync query, `fe` = computed in the frontend, `ext` = external service).
- **Consumer** — which GovTool surface needs it.
- **Tier** — `core` (v1 must-have, every provider must serve it), `ext` (v1 extended, capability-gated), `next` (revamp scope, not in today's API).

Rows marked **⊕** were identified by comparing against
[cardanoapi/dbsync-api](https://github.com/cardanoapi/dbsync-api) — see
[provider-comparison.md](./provider-comparison.md).

---

## 0. Component boundaries

Three separate components, deliberately. Each has a different trust model, a
different failure mode, and a different freshness clock — merging any two makes
all three harder to reason about.

| Component | Owns | Source of truth | Failure mode | Spec |
|-----------|------|-----------------|--------------|------|
| **Chain Data API** | Chain-derived read model: chain state, accounts, DReps, proposals, votes, committee, transactions | The ledger, via a swappable provider | Provider lag / outage → degraded mode | [`chain-data/`](./chain-data/index.ts) |
| **Metadata Service** | Off-chain document retrieval, hash verification, CIP validation, persistence, cache lifecycle | The public internet (HTTP/IPFS) | Unreachable / invalid documents → typed metadata status | [`metadata/`](./metadata/index.ts) |
| **Pinning Service** | Author-side write path: pin user-authored metadata, return the `url` + `dataHash` to submit on chain | Its own pin store | Quota / backend outage → pin rejected, author can self-host instead | [`pinning/`](./pinning/index.ts) |

Rules that follow from the split:

1. The Chain Data API **never fetches a url**. It embeds a `MetadataProjection`
   (status + body, no retry/cache internals) produced by the Metadata Service.
2. The Metadata Service **does not trust the Pinning Service**. Content pinned by
   GovTool is re-fetched and re-hashed from its public url like anyone else's, so
   "pinned but not publicly reachable" is reported honestly.
3. The Pinning Service is **optional**. GovTool must work fully with it disabled;
   authors who host their own metadata never touch it.
4. Freshness is per-component. A chain-data response's `meta.asOf` describes chain
   data only; metadata carries its own `fetchedAt`.

---

## 1. Chain & network — `chain/chain.ts` → `/chain/*`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 1.1 | Network name / magic / era | `GET /network/info` | header, wallet guard | core |
| 1.2 | Chain tip: block no, slot, block hash, absolute time | `GET /network/info` (block + time only) | freshness banner, live metrics | core |
| 1.3 | Current epoch no, epoch start/end time | `GET /network/info` | countdowns, expiry math | core |
| 1.4 | Sync status of the underlying data source (tip lag in seconds) | — | degraded-mode banner | core |
| 1.5 ⊕ | **Historical epoch list** (last *n* epochs: no, start, end, duration) | — | epoch pickers, charts | ext |
| 1.6 | Protocol parameters for an epoch (full set, incl. all DVT/PVT thresholds, deposits, `govActionLifetime`, `drepActivity`, `committeeMinSize`, `committeeMaxTermLength`) | `GET /epoch/params` (current epoch only) | vote-threshold bars, deposit display, PP-change diffs | core |
| 1.7 ⊕ | Protocol parameters **for an arbitrary past epoch** | — | historical action context | ext |
| 1.8 | Protocol parameters **at proposal creation** and **at enactment** | `proposalResponse.protocolParams` | threshold calculation per action | core |
| 1.9 | **Active** stake total (epoch snapshot) | `GET /network/total-stake` | % denominators for tallies | core |
| 1.10 ⊕ | **Live** stake total (current, un-snapshotted) | — | live dashboards; must never be mixed with 1.9 | core |
| 1.11 | Stake controlled by DReps / by SPOs | `GET /network/total-stake` | dashboard metrics | core |
| 1.12 | `alwaysAbstain` / `alwaysNoConfidence` voting power | `GET /network/total-stake` | threshold math | core |
| 1.13 | Aggregate metrics: unique delegators, total delegations, total gov actions, total DRep votes, registered/active/inactive DReps, CIP-119-compliant DReps, registered direct voters | `GET /network/metrics` | home + dashboard stats | ext |
| 1.14 | Committee size, quorum numerator/denominator | `GET /network/metrics` | CC threshold bar | core |
| 1.15 ⊕ | Block lookup (by number, or latest *n*) | — | tx confirmation depth | ext |
| 1.16 | Treasury balance, reserves, per-epoch treasury delta | — | treasury-withdrawal context | next |

## 2. Governance actions — `governance/proposals.ts` → `/governance/proposals/*`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 2.1 | Identity: CIP-129 `gov_action1…` id, tx hash, index | `GET /proposal/list`, `/proposal/get` | routing, search, deep links | core |
| 2.2 | Action type (7 Conway types) | both | filters, icons | core |
| 2.3 | Type-specific body: PP updates, hard-fork version, withdrawals (address + amount), new committee members + terms + removals + quorum, new constitution anchor + guardrails script hash, `NoConfidence`, `Info` | `details` (untyped blob) | action-detail cards | core |
| 2.4 | Deposit amount + return address | — (`govActionDeposit` from PP only) | detail page | ext |
| 2.5 | Submitter / proposer stake address | — | detail page, filters | ext |
| 2.6 | `prevGovAction` pointer (tx hash + index) | both | lineage chain | core |
| 2.7 | Lifecycle: created date/epoch, expiry date/epoch, ratified/enacted/dropped/expired epoch + tx | partial (`created*`, `expiry*`) | status chip, timeline | core |
| 2.8 | Status enum (`live` / `ratified` / `enacted` / `expired` / `dropped`) | `fe` derived from dates | filters, tabs | core |
| 2.9 | Metadata anchor: url + hash | both | handed to the Metadata Service | core |
| 2.10 | Projected CIP-108 metadata: title, abstract, motivation, rationale, references, authors | both (inlined, unvalidated) | detail page | core |
| 2.11 | Metadata status (`pending`/`valid`/`invalid`/`unavailable`) | `ext` service, not persisted | "unverified metadata" warning | core |
| 2.12 | Vote tallies by role (DRep/SPO/CC × yes/no/abstain), as **stake** and as **count** | stake only | vote bars | core |
| 2.13 | Not-voted / abstained-by-default power per role | `fe` derived | threshold bars | ext |
| 2.14 | Applicable thresholds for *this* action type, and computed pass/fail per role | `fe` derived from PP | "will it pass" indicator | ext |
| 2.15 | Individual votes cast on the action (voter, role, vote, tx, time, epoch, voting power at vote, rationale anchor) | — (only per-DRep via `/drep/getVotes`) | "who voted" tab | core |
| 2.16 | Activity timeline (submitted → votes → ratified → enacted) | — | detail page timeline | next |
| 2.17 | Enacted-action details per type (for "currently enacted" comparison) | `GET /proposal/enacted-details` | PP diff, constitution diff | ext |
| 2.18 ⊕ | Lookup of all gov actions **created by a given tx hash** | — | post-submission confirmation | ext |
| 2.19 | List controls: pagination, sort (newest/soonest-to-expire/most-yes), filter by type/status, free-text + id search, `voterId` context | `GET /proposal/list` | governance actions page | core |
| 2.20 ⊕ | **Expansion flags** — tallies and voting power are expensive; the caller opts in (`expand=tallies,myVote`) rather than paying for them on every list read | — | list performance | core |

## 3. Voters — `governance/dreps.ts`, `pools.ts`, `committee.ts`

Common voter shape covering **DRep, SPO, CC member, direct/sole voter, and the two
predefined voters** — the current API models only DReps and sole voters.

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 3.1 | Voter identity: CIP-129 id, hash (hex), script-based flag | `GET /drep/list`, `/drep/info` | everywhere | core |
| 3.2 | Voter role (`drep` / `spo` / `cc` / `direct`) | implicit (`type: DRep \| SoleVoter`) | badges, vote grouping | core |
| 3.3 | Registration: status (`active`/`inactive`/`retired`), latest registration tx + date, retirement tx + date, deposit, ever-registered flags | `/drep/info`, `/drep/list` | profile, own-status guard | core |
| 3.4 | Registration/update **history** (each tx, epoch, anchor, what changed) | — | profile activity (#4226) | core |
| 3.5 | Metadata anchor url + hash | both | handed to the Metadata Service | core |
| 3.6 | Projected CIP-119 metadata: givenName, objectives, motivations, qualifications, paymentAddress, image, identity/link references, `doNotList` | `/drep/list` (+ `fe` IPFS resolution) | DRep directory & profile | core |
| 3.7 | Metadata status + CIP-119 compliance flag | `ext` | "verified" badge | core |
| 3.8 | **Active** voting power (epoch snapshot) + the epoch it belongs to | `/drep/get-voting-power`, `/drep/voting-power-list` | tallies, vote weight | core |
| 3.9 ⊕ | **Live** voting power + live delegator count (current, un-snapshotted) | — | DRep directory "live" view | core |
| 3.10 | Influence / share of total stake | `fe` derived | profile | ext |
| 3.11 | Voting-power history by epoch | — | profile chart (#4228) | next |
| 3.12 | Delegator **count**, live and active | `sql` only | profile | ext |
| 3.13 ⊕ | Delegator **list**: stake address, delegated-at (tx/epoch/time), and balance broken into **utxo / rewards / rewards-rest** | `sql` only (count) | profile delegators tab | ext |
| 3.14 ⊕ | Delegation **events** for a DRep (`joined` / `left`, stake address, tx, epoch, time) | — | profile activity | ext |
| 3.15 | Activity: votes cast, **voted vs not-voted counts** against votable actions, last-active epoch, inactivity countdown (`drepActivity`) | `/drep/getVotes` (votes only) | profile, status | core |
| 3.16 | Vote rationale (CIP-100 anchor + projected body + status) | anchor only | vote list (#4228) | core |
| 3.17 | Ada Handle for a DRep/wallet | `ext` handle.me API | search, display | ext |
| 3.18 | Directory controls: pagination, sort (voting power / registration date / activity / status / random+seed), filter by status & type, search by name/id/handle | `GET /drep/list` | DRep directory | core |
| 3.19 | SPO-specific: pool id, ticker, pledge, live stake, margin | — | SPO votes (#4228) | next |
| 3.20 | CC-specific: cold/hot credential pair, term start/expiry epoch, resigned flag | — | CC votes | next |

## 4. Accounts — `chain-data/accounts.ts` → `/accounts/*`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 4.1 | Stake key: registered flag, script-based flag | `GET /account/{stakeKey}` | dashboard gating | core |
| 4.2 ⊕ | Stake **registration / deregistration events** (epoch, slot, block, time, tx) | — | account history | ext |
| 4.3 | Current DRep delegation: voter ref, tx, `alwaysAbstain`/`alwaysNoConfidence` sentinel | `GET /ada-holder/get-current-delegation` | dashboard card | core |
| 4.4 ⊕ | Current **stake-pool** delegation (pool id, epoch, tx, time) | — | SPO context, "your pool voted…" | ext |
| 4.5 | Delegation history (tx, epoch, from → to), for both DRep and pool | — | activity (#4226) | ext |
| 4.6 | Ada holder voting power (lovelace) | `GET /ada-holder/get-voting-power` | dashboard card | core |
| 4.7 | Is this stake key also a DRep / direct voter (own-profile resolution) | `/drep/info` | dashboard | core |
| 4.8 ⊕ | Account balance breakdown (utxo / rewards / rewards-rest) | wallet only | voting-power explainer | ext |

## 5. Votes — `governance/votes.ts` → `/governance/votes/*`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 5.1 | Vote record: proposal id, voter id + role, `yes`/`no`/`abstain`, tx hash, epoch, timestamp | `/drep/getVotes` (DRep only) | vote lists | core |
| 5.2 | Voting power applied at the time of the vote | — | weighted display | ext |
| 5.3 | Superseded/overridden votes (a voter re-voting on the same action) | — | history correctness | ext |
| 5.4 | Rationale anchor + projected CIP-100 body + status | anchor only | rationale modal | core |
| 5.5 | Vote-list controls: filter by type, sort, search, pagination | `/drep/getVotes` params | profile vote tab | core |

## 6. Off-chain metadata — **separate service**

Owned by the Metadata Service ([`metadata/index.ts`](./metadata/index.ts)),
epics #4224 and #4225. The Chain Data API contributes only the anchor; everything
below is out of its contract.

| # | Data | Today | Tier |
|---|------|-------|------|
| 6.1 | Raw fetched document + observed content hash + byte size + content type | `ext`, not persisted | core |
| 6.2 | Standard expected vs. detected (CIP-100 / 108 / 119) | `ext` | core |
| 6.3 | **Retrieval** failures (`URL_NOT_FOUND`, `FETCH_TIMEOUT`, `GATEWAY_ERROR`, …) kept distinct from **validation** failures (`INVALID_HASH`, `INVALID_JSONLD`, `INCORRECT_FORMAT`, …) | `ext` (flat enum, mixed) | core |
| 6.4 | Provenance: source url, resolved gateway, fetched-at, attempts, next-retry, terminal-failure flag | — | core |
| 6.5 | Cache lifecycle (`fresh` / `stale` / `refreshing` / `failed`), expiry, forced refresh | — | core |
| 6.6 | Author signature verification (CIP-100 `authors[]`) | — | ext |
| 6.7 | Ops stats: counts by status, queue depth, oldest pending | — | ext |

**Boundary contract:** chain-data responses embed `MetadataProjection` —
`{ id, anchor, standard, status, body?, failureReason?, fetchedAt? }`. Nothing
else crosses.

## 7. Author-side pinning — **separate component**

Owned by the Pinning Service ([`pinning/index.ts`](./pinning/index.ts)),
replacing `POST /upload` in the current backend.

| # | Data | Today | Tier |
|---|------|-------|------|
| 7.1 | Pin a document → CID, `ipfs://` url, **blake2b-256 `dataHash`**, byte size | `POST /upload` (url only) | core |
| 7.2 | Hash-and-validate **without** pinning, so self-hosting authors get the same `dataHash` | — | core |
| 7.3 | Pin status + replicas across backends (kubo / Pinata / …) | — | ext |
| 7.4 | Policy: max size, allowed content types, rate limit, per-owner quota, retention window | — | core |
| 7.5 | Owner's pin list, re-pin, unpin | — | ext |
| 7.6 | Backend health + quota usage | — | ext |

**Boundary contract:** pinning emits `{ url, dataHash }` — an `Anchor` the user
submits on chain. The Metadata Service later reads it back over the public
internet, with no special trust in the fact that GovTool pinned it.

## 8. Transactions — `chain-data/transactions.ts` → `/transactions/*`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 8.1 | Tx confirmation status by hash | `GET /transaction/status` | post-submit polling | core |
| 8.2 | Resulting governance effect of a tx (vote / registration / delegation / proposal) | partial (`votingProcedure`) | success screens | core |
| 8.3 | Pending (mempool) governance events | — | optimistic UI | next |

> Wallet interaction (CIP-30/CIP-95, cert & vote building, signing) stays client-side and is **out of scope** for all three components.

## 9. Constitution & committee — `governance/committee.ts`

| # | Data | Today | Consumer | Tier |
|---|------|-------|----------|------|
| 9.1 | Current constitution: anchor url + hash, guardrails script hash, projected text | `fe` static/off-chain | constitution page | ext |
| 9.2 | Constitution history (per enacted `NewConstitution` action) | — | diff view | next |
| 9.3 | Current committee from gov-state: members (cold/hot), term expiry, quorum, the gov action that set it | count only via metrics | CC page | ext |

## 10. Envelope — `chain-data/common.ts`

| # | Concern | Shape |
|---|---------|-------|
| 10.1 | Pagination | cursor only (`cursor` + `limit`); `total` only when cheap to compute |
| 10.2 | Freshness | `asOf` (block, slot, epoch, time) when the provider can report it without an extra round trip; tip lag is derived from it |
| 10.3 | Provenance | which provider answered — injected by infrastructure, not part of the drafted contract |
| 10.4 | Capabilities | machine-readable endpoint/field → `supported`/`partial`/`unsupported` per provider, queryable at runtime |
| 10.5 | Health | provider status, last successful sync, `secondsSinceLastUpdate`, with an explicit staleness threshold that flips the service to `503` |
| 10.6 | Errors | typed, stable `code` + `message` + `retryable` + `details` |
| 10.7 | Live data | polling policy belongs to the layer facing the client; SSE/WS deferred to #4236 |
| 10.8 ⊕ | Expansion | expensive fields (tallies, voting power, delegator balances) are opt-in per request, never unconditional |

## 11. Decisions to lock before the contract freezes

1. **Lovelace as `string`.** Total supply in lovelace (4.5e16) exceeds `Number.MAX_SAFE_INTEGER` (9.0e15); the current API returns these as JSON numbers, which is already lossy for totals. v1 serializes all lovelace/stake values as decimal strings.
2. **CIP-129 bech32 is the only identifier** for gov actions, DReps and CC members. Raw hashes are returned alongside (not derivable without decoding); no legacy or composite id form appears in the contract.
3. **Metadata is a separate service**, embedded only as a status-carrying projection — so "not fetched yet", "fetch failed" and "fetched but empty" stay distinguishable, and no chain-data provider re-implements fetching.
4. **Pinning is a separate, optional component** — the only write path, the only place that accepts arbitrary user content, and not a trusted source for reads.
5. **One voter model, discriminated by role**, replacing today's DRep-only shape plus the `type: "DRep" | "SoleVoter"` overload.
6. **`live` vs `active` is named in every field that has both** (stake, voting power, delegator count). Never a bare `votingPower` whose snapshot semantics depend on the endpoint.
7. **Tallies carry both stake and count**, plus the denominator used and the applicable threshold, so the UI never re-derives percentages from mismatched sources.
8. **Time is always both epoch and ISO-8601 UTC timestamp**; never one without the other.
9. **Optional vs. capability-gated**: a field the provider *cannot* serve is `undefined`, with the gap declared once at `/system/capabilities` — not `null`, and not re-listed on every response.
10. **No internal database ids in the contract.** Today's `id: number` fields (and db-sync row ids generally) are provider implementation detail and must not leak.

---

- Component boundaries & route map: [`README.md`](./README.md)
- Chain Data API: [`chain-data/index.ts`](./chain-data/index.ts)
- Metadata Service: [`metadata/index.ts`](./metadata/index.ts)
- Pinning Service: [`pinning/index.ts`](./pinning/index.ts)
- Comparison against cardanoapi/dbsync-api: [`provider-comparison.md`](./provider-comparison.md)
