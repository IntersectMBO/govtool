# @govtool/provider-blockfrost

`ChainDataApiV1` ([`@govtool/data-providers`](../govtool-data-providers)) over
the **hosted Blockfrost API**, verified against
`https://cardano-mainnet.blockfrost.io/api/v0` **v6.8.0**, mainnet epoch 657,
on 2026-09-24. Preprod and preview use their hosted hosts; a self-hosted
blockfrost-ryo is reached with `baseUrl`, but the endpoints this relies on
(`/governance/committee`, `/network`, `/txs/{hash}`, hydrated DRep rows,
proposal ids on DRep votes) were missing there, so expect gaps. A capability
claim is about a deployment (SPEC.md §11).

```ts
import { createBlockfrostProvider } from '@govtool/provider-blockfrost';

const { chainData } = createBlockfrostProvider({
  network: 'mainnet',
  projectId: process.env.BLOCKFROST_PROJECT_ID, // sent as the project_id header
});
```

Options: `network` (required), `projectId`, `baseUrl`, `fetch`, `timeoutMs`
(30 s), `maxConcurrency` (8 in flight), `maxRetries` (3), `rateLimit`
(default: Blockfrost's published 10 req/s with bursts of 500 on the hosted
URL, none for a `baseUrl`). Global `fetch`, no HTTP library, no cache
(SPEC.md §3.6): within one method call a resource is read at most once, and
nothing outlives the call.

The project id lives only in the request header. No error this provider
raises carries a header, a `cause` object or a raw body — only Blockfrost's
own short `message`, with the id redacted should it ever appear — and a 403
(bad key) is `PROVIDER_UNAVAILABLE` with a fixed message.

## What is served

| Area         | Served                                                                                                    | Omitted, and why                                                                                                                                                                                                                                                                                                  |
| ------------ | --------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| network      | `getNetworkInfo`, `getProtocolParams` (+ past epochs), `getStakeDistribution` (all fields), `getTreasury`, `getGenesisParams` | treasury at a past epoch: `/network` is live pots only, so it is refused                                                                                                                                                                                                                                          |
| accounts     | `get`, `getDelegation`, `getPoolDelegation`                                                               | `getVotingPower` (the ledger adds live proposal deposits, which cost every proposal record to find); `balance` (withdrawable rewards are not split into `rewards` / `rewardsRest`); `listDelegationHistory` (no governance-delegation history endpoint)                                                            |
| dreps        | `list`, `get`, `listVotes`, `listUpdateHistory`, `getCounts`                                              | `listDelegators`: `/delegators` `amount` is each account's CURRENT balance (it equals `/accounts/{stake}.controlled_amount`), and the row requires the active figure. Also not on DReps: `liveVotingPower`, `activity`, `delegatorCount`, `expiryEpoch`, `registration.retiredAt`                                     |
| proposals    | `list`, `get` (+ `voterId`), `getEnacted`, `listVotes`, `listActivity`, aggregates for LIVE actions       | aggregates on concluded actions (no per-epoch stake history); `protocolParamsAt*`                                                                                                                                                                                                                                 |
| pools        | `list`, `get`                                                                                             | `listVotes`: `/pools/{id}/votes` rows do not say which action was voted on                                                                                                                                                                                                                                        |
| committee    | `getCommittee`, `getMember`, `getConstitution`                                                            | `termStartEpoch` is `null` (not in `/governance/committee`)                                                                                                                                                                                                                                                       |
| transactions | `get`                                                                                                     |                                                                                                                                                                                                                                                                                                                   |

Declared (`system.getCapabilities()`, `capabilities()`):

```json
{
  "sorts": { "dreps": ["votingPower", "random"], "proposals": ["newest", "oldest", "soonestToExpire"], "votes": ["newest", "oldest"] },
  "filters": { "dreps": ["status", "kind"], "proposals": ["type", "status"] },
  "search": ["exactId"],
  "voteAggregate": ["stake", "count"],
  "optionalArguments": ["protocolParams.epoch"]
}
```

Declined: DRep `registrationDate` (ordering by latest registration needs every
DRep's certificates, ~3.4k requests for any one page) and `activity`; proposal
`mostYesVotes` and `highestParticipation` (aggregates exist for live actions
only); `proposals.voterContextOnList` (a committee voter needs every action's
vote list). Each is refused with `CAPABILITY_UNSUPPORTED`, never approximated.

## How it maps

- **DRep directory.** Hosted `/governance/dreps` rows are hydrated (stake,
  script flag, retired, expired, last active epoch, anchor), so the whole
  directory — 1,712 rows, 18 pages — is read per call and filtered, sorted,
  counted and paged in memory: `total` is exact and no page is short except
  the last. Each DRep on the returned page then costs its registration:
  `/updates` plus one `/txs/{hash}` per dated certificate.
- **Status** is `retired`, else `expired` → inactive, else active: Blockfrost's
  flags, not a reconstruction. The expiry epoch itself is not served.
- **Anonymous = no anchor.** Blockfrost keeps the anchor when it could not
  fetch the document (`json_metadata: null` plus an `error`), so a missing
  anchor really is a missing anchor.
- **Proposals.** `/governance/proposals` is ids only. A GovAction is the record
  (`/governance/proposals/{tx}/{i}`), the submitting transaction's date
  (`/txs/{tx}`; the record has none) and its CBOR (`/txs/{tx}/cbor`, for the
  anchor in the proposal procedure). The typed body is decoded from
  `governance_description`, the ledger's JSON, parsed number-exact.
  `expires` is Blockfrost's `expiration`, the same db-sync column the db-sync
  provider reads (docs/api/decisions.md OPEN-76). Status precedence is enacted >
  ratified > expired > dropped, since an expired action also carries a
  dropped epoch.
- **`getEnacted`** walks predecessor links in the lineage, never epochs;
  UpdateCommittee and NoConfidence share `committee`.
- **Constitution.** `/governance/constitution` answers 400. It is the enacted
  head of the constitution lineage's body anchor (SPEC.md §5.5).
- **Vote anchors** are not in Blockfrost's vote rows, so each vote
  transaction's CBOR is read for the rationale anchor (`src/cbor.ts`, a
  bounded reader). `null` then means the voter attached none.
- **Committee** membership is `/governance/committee` (ledger state on hosted
  v6.8). A vote's hot key is resolved to its cold one through it. A cold-id
  voter lookup on an action with a committee vote under a hot key no current
  member holds is refused: Blockfrost keeps no rotation history.
- **DRep vote listing** follows the db-sync provider's window rules (first
  registration to retirement; votable epochs; bootstrap excludes non-Info
  actions). It is ordered by action, not by vote time — Blockfrost does not
  date a vote without another read per vote.

## Vote aggregates

Live actions only, computed as the db-sync provider does (see the header of
`src/governance/proposals/aggregates.ts`): DRep and SPO as `stake`, the
committee as `count`; `yes + no + abstain + notVoted = totalEligible`; the
threshold from the current parameters by type and parameter group.
Concluded actions carry no `voteAggregates` rather than today's stake under
an old vote. On mainnet the DRep figures match Koios's
`proposal_voting_summary` exactly for all three live actions, except one DRep
that retired this epoch (502 ada) which Koios still counts and the ledger does
not.

**SPO deviations — read before relying on SPO figures:**

1. **Snapshot.** Pools are weighted by `/pools/extended` `active_stake`, the
   epoch's leader-schedule snapshot. The ledger weighs SPO votes with the
   newer snapshot taken at the epoch's start (db-sync
   `pool_stat.voting_power`), which Blockfrost does not serve. Measured: one
   pool 57.19M vs 57.37M ada; the SPO yes total on one action 53.36M vs 53.19M.
2. **Silent pools.** After bootstrap the ledger defaults a silent pool by its
   reward account's DRep delegation. Reading that is `/pools/{id}` +
   `/accounts/{reward}` per silent pool (~6,000 requests), so every silent
   pool is `notVoted`. This is large: on mainnet 623 silent pools holding
   12.1B of 21.4B ada active stake have always-abstain reward accounts, so the
   SPO `abstain` is understated, `notVoted` overstated, and the ledger ratio
   yes / (totalEligible − abstain) reads far lower here. HardForkInitiation is
   unaffected (a silent pool is No there regardless).

## Request cost

Measured on mainnet, 2026-09-24 (158 proposals, 3 live; 1,712 directory rows,
1,685 registered DReps), with `npm run live`:

| Read                                                    | Requests |
| ------------------------------------------------------- | -------: |
| full proposal list (`proposals.list`, size 500)         |      435 |
| full DRep directory (backend snapshot, 4 × size 500)    |    3,821 |
| one DRep detail (`dreps.get`)                           |        6 |
| one live proposal detail with aggregates (`proposals.get`) |    62 |
| DRep page (`dreps.list`, size 20 / size 100)            | 76 / 279 |
| DRep vote listing (`dreps.listVotes`, page of 20)       |      187 |
| `getStakeDistribution` / `getCounts`                    |  22 / 20 |

Where it goes: a proposal is a record + a dated transaction + its CBOR (~2.7
requests; 435 for 158), the aggregates' shared inputs are the DRep directory
(20), `/pools/extended` (~33) and the committee; a DRep is its `/updates` plus
~1.6 dated certificates (~2.6 requests).

The backend caches and refreshes snapshots on a timer, so the per-snapshot
cost is what the quota sees. Blockfrost's free tier is 50,000 requests a day:

| Snapshot            | per refresh | every 20 s (4,320/day) | every 10 min (144/day) |
| ------------------- | ----------: | ---------------------: | ---------------------: |
| proposal list       |         435 |              1,879,200 |                 62,640 |
| DRep directory      |       3,821 |             16,506,720 |                550,224 |
| both                |       4,256 |             18,385,920 |                612,864 |

Neither snapshot fits the free tier even at a 10-minute refresh. The DRep
directory is also slow: past the 500-request burst, Blockfrost's 10 req/s
limit makes it about 6.4 minutes (382 s measured), and the provider paces itself to it rather
than burning retries on 429s. The directory cost is the required
`registration` date on every DRep (two dated reads per DRep); the proposal
cost is the per-proposal record plus its transaction's date and CBOR.

## Tests

```bash
npm run verify     # typecheck, build, fixture tests (node --test test/*.test.mjs)
BLOCKFROST_PROJECT_ID=... npm run live              # every method against mainnet, ~2.3k requests
BLOCKFROST_PROJECT_ID=... LIVE_FULL=1 npm run live  # plus the full DRep snapshot, ~3.8k more
```

Fixture tests use an injected `fetch` answering invented Blockfrost-shaped
JSON. They cannot catch a field the live API spells differently, so run the
live script after touching a mapper: it asserts contract invariants on real
data and prints the request count per method and per route.

## Found only against live data

- The directory lists **credentials that never registered** (25 on mainnet:
  delegated to, `last_active_epoch: null`, empty `/updates`, yet
  `active: true` on the detail read) and the **two predefined targets** as if
  they were DReps. Both are excluded.
- **`counted: false`** marks a superseded vote on `/governance/proposals/…/votes`
  (24 rows on one mainnet action); only counted rows are votes.
- On hosted v6.8 committee voters are **CIP-129 `cc_hot1…`**, not the bare hex
  blockfrost-ryo 3.1.1 sent. Both are handled; `voter_role` is still
  `constitutional_committee`.
- `/accounts/{stake}` returned CIP-129 `drep_id` here, where blockfrost-ryo
  returned CIP-105 for the same credential; both are decoded and re-encoded.
- `/pools/{id}/votes` rows lack the action; `/governance/dreps/{id}/votes`
  rows carry it (`proposal_tx_hash`, `proposal_cert_index`).
- `/pools/{id}/history` stops at the last rewarded epoch (655 in epoch 657),
  and `/epochs/{current+1}/stakes` is 404: the snapshot the ledger votes with
  is not exposed (the SPO deviation above).
