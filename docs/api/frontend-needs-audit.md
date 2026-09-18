# Frontend-needs audit — what GovTool actually uses today

Status: **2026-09-18.** Companion to
[`provider-gap-report.md`](./provider-gap-report.md), which asks "what can each
provider serve?". This asks the narrower, shipping-relevant question:

> **What does the current GovTool frontend actually need, and which providers
> can run it today?**

Method: every backend call in `govtool/frontend/src/services/requests`, traced
through `govtool-backend` to the contract routes it calls, then each of those
run live against all three providers. Not inferred from the spec — measured.

Answer up front:

| Provider       | Routes the frontend needs | Verdict                                                                                        |
| -------------- | :-----------------------: | ---------------------------------------------------------------------------------------------- |
| **db-sync**    |        **15 / 15**        | Ships today. One route is broken on the _preview deployment_ for an environmental reason (§4). |
| **Koios**      |        **14 / 15**        | One gap, and it dissolves if `/network/metrics` is narrowed to what the frontend reads (§3).   |
| **Blockfrost** |        **9 / 15**         | Six gaps, four of them structural. Not viable for the current frontend.                        |

---

## 1. The frontend's real surface

The frontend makes **17** backend calls. Fifteen are chain data; two are other
services (IPFS pinning, metadata validation).

It also ships **six request modules that are never invoked** —
`postDRepRegister`, `postDRepVote`, `postDRepRetire`, `postDRepRemoveVote`,
`postAdaHolderDelegate`, `postAdaHolderRemoveDelegation`. They target backend
routes that no longer exist; transactions are built client-side over CIP-30/95
now. They are exported from `services/requests/index.ts` and called from
nowhere. **Dead code — safe to delete, and worth deleting so nobody reads them
as a requirement.**

Two backend routes exist that the frontend never calls:
`/proposal/enacted-details` and `/survey/definition`.

## 2. Route-by-route

`Y` works · `~` works with caveats · `N` cannot

| Frontend call                   | Legacy route                            | Contract route                     | In spec | FE uses |           db-sync            | Koios | Blockfrost |
| ------------------------------- | --------------------------------------- | ---------------------------------- | :-----: | :-----: | :--------------------------: | :---: | :--------: |
| `getNetworkInfo`                | `/network/info`                         | `network.getNetworkInfo`           |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getNetworkTotalStake`          | `/network/total-stake`                  | `network.getStakeDistribution`     |    Y    |    Y    |              Y               |   Y   |   **N**    |
| `getNetworkMetrics`             | `/network/metrics`                      | `governance.metrics.get`           |    Y    |    Y    |              Y               | **N** |   **N**    |
| `getEpochParams`                | `/epoch/params`                         | `network.getProtocolParams`        |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getDRepList`                   | `/drep/list`                            | `governance.dreps.list`            |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getVoterInfo`                  | `/drep/info/:id`                        | `governance.dreps.get`             |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getDRepVotingPower`            | `/drep/get-voting-power/:id`            | `governance.dreps.getVotingPower`  |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getDRepVotingPowerList`        | `/drep/voting-power-list`               | `governance.dreps.getVotingPowers` |    Y    |    Y    |              Y               |   Y   |   **N**    |
| `getDRepVotes`                  | `/drep/getVotes/:id`                    | `governance.dreps.listVotes`       |    Y    |    Y    |              Y               |   Y   |   **N**    |
| `getProposals`                  | `/proposal/list`                        | `governance.proposals.list`        |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getProposal`                   | `/proposal/get/:id`                     | `governance.proposals.get`         |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getAccount`                    | `/account/:stakeKey`                    | `accounts.get`                     |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getAdaHolderCurrentDelegation` | `/ada-holder/get-current-delegation/:k` | `accounts.getDelegation`           |    Y    |    Y    |              Y               |   Y   |     Y      |
| `getAdaHolderVotingPower`       | `/ada-holder/get-voting-power/:k`       | `accounts.getVotingPower`          |    Y    |    Y    |             ~ §4             |   Y   |   **N**    |
| `getTransactionStatus`          | `/transaction/status/:tx`               | `transactions.get`                 |    Y    |    Y    |              Y               |   Y   |   **N**    |
| `postIpfs`                      | `/ipfs/upload`                          | `pinning.pin`                      |    Y    |    Y    | — pinning service (Pinata) — |       |            |
| `postValidate`                  | `/validate`                             | metadata service                   |    Y    |    Y    |     — separate service —     |       |            |

Every route the frontend uses **is** in the spec. There is no frontend need
the contract fails to model — the gaps are all provider-side.

### Blockfrost's six

| Route                          | Why                                                                         |
| ------------------------------ | --------------------------------------------------------------------------- |
| `/network/total-stake`         | needs all four governance totals; Blockfrost has only `totalActiveStake`    |
| `/network/metrics`             | no aggregate endpoint of any kind                                           |
| `/drep/voting-power-list`      | batch with no ids = one request per DRep; refused                           |
| `/drep/getVotes`               | `/governance/dreps/{id}/votes` does not say which proposal each vote was on |
| `/ada-holder/get-voting-power` | voting power is reported per DRep, never per account                        |
| `/transaction/status`          | `/txs/{hash}` answers 500 on this deployment                                |

The first three are cost/shape problems that a different query model could
solve. The last three are missing data.

## 3. The spec is broader than the frontend — but that is not the problem

Of the contract's **40 routes, the current frontend needs 15**. The other 25
are unused today:

> `network.getTreasury`, `listEpochs`, `listBlocks` · `accounts.listDelegationHistory`,
> `listStakeEvents` · `dreps.listDelegators`, `listHistory`, `listDelegationEvents` ·
> `proposals.listVotes`, `getTallies`, `listActivity`, `getEnacted`, `listByTx` ·
> `votes.list`, `votes.get` · `pools.*` · `committee.*` · `voters.*` ·
> `surveys.getDefinition` · `system.getCapabilities`

That is 62% unused — but **none of it is what blocks shipping**, and several
are obviously wanted next (`proposals.getTallies` and `listVotes` for a real
vote breakdown, `committee.*` for the CC page). Generality costs nothing here
because an unused route is simply never called.

**The over-provisioning that does cost something is inside one route.**

`/network/metrics` returns 13 counters. The frontend reads **three**:
`noOfCommitteeMembers`, `quorumNumerator`, `quorumDenominator` — from a single
component, `VotesSubmitted.tsx`, to draw the committee threshold bar.

The ten it never reads:

> `uniqueDelegators`, `totalDelegations`, `totalGovernanceActions`,
> `totalDRepVotes`, `totalRegisteredDReps`, `totalDRepDistr`,
> `totalActiveDReps`, `totalInactiveDReps`,
> `totalActiveCIP119CompliantDReps`, `totalRegisteredDirectVoters`

Those ten are exactly the expensive ones — collection-wide aggregates — and
they include **all six Koios cannot compute**. The three the frontend actually
uses all come from committee data, which Koios serves in one cheap request and
already returns from `getAvailable()`.

**So Koios' only blocker is a field set the frontend does not read.** Narrowing
`/network/metrics` to what `VotesSubmitted.tsx` needs — or having the backend
fall back to `getAvailable()` and serve the committee fields — takes Koios to
15/15 with no loss of function.

For contrast, field usage elsewhere is high, so this is a metrics-specific
problem and not a general one:

| Response             | fields read / total |
| -------------------- | :-----------------: |
| `ProposalResponse`   |       29 / 29       |
| `DRepInfoResponse`   |       10 / 10       |
| `NetworkTotalStake`  |        4 / 4        |
| `DRepListItem`       |       17 / 21       |
| **`NetworkMetrics`** |     **3 / 13**      |

## 4. db-sync is not 100% on the deployment GovTool uses

`/ada-holder/get-voting-power` fails on the shared preview db-sync:

```
relation "utxo_view" does not exist
```

`get-stake-key-voting-power.sql` is the only frozen statement that uses
`utxo_view`, and that instance has **no views at all**
(`information_schema.views` is empty; schema_version stage_two = 47).

It is invisible, because the legacy service catches every error on this route
and returns `0`. So every connected wallet shows **0 ₳ voting power** and
nothing reports a fault. `govtool-backend` preserves that behaviour
deliberately — the frontend has no error path for the field — but the provider
now surfaces the real error underneath, which is how this was found.

**This is a deployment question, not a code one:** either the db-sync instance
needs `utxo_view` created, or that statement needs rewriting against `tx_out`.
Until then the route is dead on preview regardless of provider choice.

## 5. A consumer bug this audit found, now fixed

`govtool-backend` builds a _snapshot_ for `/drep/list` and `/proposal/list`:
fetch the full set once, then filter, sort and page in memory. It called
`list()` with no `limit`, taking the first page as the whole set.

That holds only for db-sync. Koios returns **1,000 of 1,684** DReps with
`nextCursor: "1000"`; Blockfrost returns **25**. Both report the truncation
honestly and the backend ignored it — so the directory would have shown a
fraction of the DReps, sorted and paged as if complete, with no error anywhere.

Fixed: `src/common/snapshot.ts` follows the cursor to exhaustion, with a page
guard that throws rather than returning a partial snapshot as though it were
whole. db-sync is unaffected (one page, one request).

## 6. What to do to ship

**To ship on db-sync (status quo):** nothing in code. Fix or replace
`utxo_view` on the preview deployment (§4).

**To ship on Koios — one change:** narrow `/network/metrics`, or have
`network.service.ts` fall back to `metrics.getAvailable()` and serve the three
committee fields the frontend reads. Everything else already works: 14/15
measured live, including `/drep/getVotes`, which Blockfrost cannot do.

**Blockfrost is not a candidate for the current frontend.** Three of its six
gaps are missing data rather than cost, and two (`/transaction/status`,
`/ada-holder/get-voting-power`) sit on the wallet-connect and
post-submission paths that every user hits.

**Independent of provider:** delete the six dead `post*` request modules.

### And for the spec

Nothing here argues for making the contract _less_ general. Every frontend
need is already modelled, the unused routes cost nothing, and several are the
obvious next features. The one lesson is narrower and is already in the gap
report as §3.1: **`GovernanceMetrics` bundles thirteen counters of wildly
different cost into one required record**, and that bundling is the single
thing standing between Koios and a shippable GovTool.
