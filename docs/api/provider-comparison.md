# Comparison: cardanoapi/dbsync-api vs. the Chain Data API v1 draft

Source reviewed: [`swagger.yaml`](https://github.com/cardanoapi/dbsync-api/blob/main/swagger.yaml)
(1382 lines, 24 endpoints) plus `src/controllers/*` and `src/types/*` on `main`.

It is an Express + Prisma read API directly over a db-sync database, used by the
Cardano API test tooling. Worth mining because it has been run against mainnet
db-sync for a while and has solved several problems the current GovTool backend
has not.

---

## 1. Its endpoint surface

| Group | Endpoints |
|-------|-----------|
| Stake | `/stake-address`, `/delegation`, `/address/balance` |
| DRep | `/drep`, `/drep/{id}`, `/drep/{id}/vote`, `/drep/{id}/registration`, `/drep/{id}/delegation`, `/drep/{id}/live-delegators`, `/drep/{id}/active-delegators`, `/drep/{id}/stats/live` |
| Blockchain | `/blockchain/epoch`, `/blockchain/block`, `/blockchain/epoch/params`, `/blockchain/gov-state/committee` |
| Governance | `/governance/live-stake`, `/governance/active-stake` |
| Gov action | `/proposal`, `/proposal/{id}`, `/proposal/{id}/votes`, `/proposal/{id}/vote-count`, `/proposal/{id}/vote-count/{voter}`, `/gov-action`, `/gov-action/count` |
| Ops | `/health` |

---

## 2. What it has that the draft was missing — **adopted**

These are now in [`data-inventory.md`](./data-inventory.md) (marked ⊕) and in
[`chain-data/`](./chain-data/index.ts).

| # | What | Why it matters | Where it landed |
|---|------|----------------|-----------------|
| 1 | **`live` vs `active` as a first-class distinction** — separate endpoints for live/active stake and live/active delegators | This is the biggest one. `active` is the epoch-boundary snapshot the ledger counts votes against; `live` is current. Today's GovTool returns a bare `votingPower` whose meaning depends on which endpoint produced it — so a percentage computed from two different sources is quietly wrong. | `common.ts` → `StakeBasis`, `VotingPower.basis`; `chain.ts` → `StakeDistribution.totalLiveStake`; `dreps.ts` → `liveVotingPower`, `delegators.{live,active}` |
| 2 | **Delegator balance breakdown**: `utxoBalance` / `rewardBalance` / `rewardRestBalance` | Voting power is the sum of these three; showing only the total makes "why is my power lower than my wallet balance" unanswerable. `reward_rest` in particular is invisible in most wallets. | `common.ts` → `StakeBalance` |
| 3 | **Stake-pool delegation alongside DRep delegation** (`/api/delegation` returns both) | GovTool only models DRep delegation, but SPO votes are shown on every action — "your pool voted X" needs the pool link. | `accounts.ts` → `PoolDelegation` |
| 4 | **Stake registration *and* deregistration events** with epoch/slot/block/time/tx | GovTool has a boolean `isRegistered` and cannot explain when or how that changed. | `accounts.ts` → `StakeRegistrationEvent` |
| 5 | **DRep delegation events as `joined` / `left`** | A count delta is not an activity feed; #4226 needs the events. | `dreps.ts` → `DRepDelegationEvent` |
| 6 | **Expansion flags** — `?vote_count=true` on proposals, `?voting_power=true` on votes, `?balance=true` on delegators | Explicit opt-in for the expensive joins. The current GovTool list endpoint computes tallies unconditionally. | `expand` on every list query |
| 7 | **Historical epoch list** (`/blockchain/epoch?limit=n` with start/end/duration) | Needed for any time-series or "as of epoch N" view. | `chain.ts` → `EpochSummary`, `listEpochs()` |
| 8 | **Protocol params for an arbitrary epoch** (`?epoch_no=`) | GovTool's `/epoch/params` only serves the current epoch, so a two-year-old ParameterChange action is rendered against today's params. | `chain.ts` → `getProtocolParams({ epoch })` |
| 9 | **Gov actions by tx hash** (`/gov-action?id=<txhash>`) | The post-submission confirmation path: user submits, gets a tx hash, needs the resulting action id. | `proposals.ts` → `listByTx()` |
| 10 | **`voted` / `notVoted` counts per DRep** (`/drep/{id}/stats/live`) | Participation needs a denominator, not just a numerator. | `dreps.ts` → `activity.notVotedCount` |
| 11 | **Concrete health semantics**: `secondsSinceLastUpdate`, and `503` once the tip is >300 s old | The draft said "report health"; this says what health *means*. Adopted as an explicit threshold rather than a vague status. | `common.ts` → `ProviderHealth.secondsSinceLastUpdate` |
| 12 | **Committee read from ledger gov-state**, not reconstructed from enacted actions | Gov-state is authoritative for current membership, terms and quorum; replaying `UpdateCommittee` actions is error-prone. | `committee.ts` header note |

## 3. What the draft has that it does not

Not criticism — it is a db-sync-specific test API, not a public contract. But
these are exactly the gaps that make it unusable as GovTool's Chain Data API:

| Area | Missing there |
|------|---------------|
| **Provider independence** | Hard-wired to db-sync via Prisma. Internal row ids (`id: 418`) leak into responses, `epoch_no`/`block_no` snake_case sits next to `epochNo`/`blockNo` camelCase, and there is no capability or provider concept at all. |
| **Freshness per response** | Only a global `/health`. No response carries the chain point it reflects, so a client cannot tell whether two calls are mutually consistent. |
| **Typed errors** | `400`/`404`/`500` with free-text `message`. No stable codes, no `retryable`, no rate-limit signalling. |
| **Metadata lifecycle** | Returns `url` + `dataHash` and a bare `title` / `drepName`, with no validation state, no hash verification, no fetch provenance. A spoofed or unreachable document is indistinguishable from a valid one. This is the entire scope of #4224/#4225. |
| **Typed gov action body** | `details: { additionalProperties: true }` — an untyped blob, same weakness as today's GovTool. |
| **Vote rationales** | Anchor url/hash on a DRep's vote only; no resolved CIP-100 body. |
| **Voter model** | DRep + sole voter only. No CC member or SPO voter entity, so "who voted" cannot be rendered for two of the three roles. |
| **Thresholds / outcome** | Raw vote counts only. No DVT/PVT threshold application, no pass/fail. Every consumer re-derives it. |
| **Superseded votes** | No notion of a voter re-voting; vote history can double-count. |
| **Constitution** | Absent. |
| **Transaction status & effects** | Absent — no post-submission polling path. |
| **Predefined delegation** | `alwaysAbstain` / `alwaysNoConfidence` are not modelled as delegation targets. |
| **Pagination** | `page`/`size` offset only — no cursor, so a list that changes under you skips or repeats rows. |
| **Lovelace typing** | Inconsistent: `liveVotingPower: '20027234084683'` (string) but `votingPower: integer` and `deposit: integer`. The integer ones overflow `Number.MAX_SAFE_INTEGER` for large totals. |
| **CIP-129** | Accepts bech32 on input, but returns `hash#index` (`'abc123#1'`) as the id. |

## 4. Inconsistencies to avoid inheriting

Worth naming, since a mapping layer is likely to copy them by accident:

1. **`/api/gov-action` overloads `govActionType`** to mean the *lifecycle state*
   (`'ratified'`, `'enacted'`) while `/api/proposal` uses the same-sounding field
   for the *action type* (`'NoConfidence'`). Two different vocabularies, one name.
2. **`/proposal/{id}/vote-count` documents a `noconfidence` vote option.** There
   is no such vote — votes are `yes` / `no` / `abstain`; *NoConfidence* is an
   action type.
3. **`expiryEpochNon`** — typo'd field name in the proposal response.
4. **`NewCommittee`** as the action type name; the ledger calls it
   `UpdateCommittee`. The draft uses the ledger name (a deliberate breaking rename
   from today's GovTool — flagged for a decision).
5. **`influence: '19.9043%'`** — a pre-formatted percentage string. Contracts
   return numbers; formatting is the client's job and locale-dependent.
6. **Several response schemas are structurally broken in the YAML** (`content:`
   with `application/json` mis-indented under it at `/drep/{id}/live-delegators`,
   `/drep/{id}/stats/live`, `/drep/{id}/delegation`), so the published docs do
   not describe the real payloads. An argument for generating the spec from
   types rather than hand-maintaining YAML — which is what #4220 should set up.

## 5. Net position

After folding in section 2, the draft covers everything dbsync-api exposes except
`/address/balance` (wallet concern, out of scope) and `/blockchain/block`
(kept as `ext`, only needed for confirmation depth).

The remaining risk is not coverage but **provider capability**: items 1, 2 and 5
above are cheap on db-sync and genuinely hard on Koios, Blockfrost and Kupo.
Those belong in the capability matrix with honest `partial` / `unsupported`
entries rather than being quietly dropped from the contract.
