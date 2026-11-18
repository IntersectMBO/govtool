# DRep dashboard reads (planned)

Status: planned. A registered DRep's dashboard (`/dashboard`, tabs Overview,
Governance, Discussions, Activity Report, History Log) already calls these
routes, but only once `GET /system/features` stops listing their feature as
unavailable. Until then each block says the data is not available yet, or shows
the part GovTool can already read. A backend that implements a route and drops
its `NO_ROUTE` entry in `govtool-backend/src/system/capabilities.ts` switches
the block on with no frontend change.

Unlike the other feature gates these fail closed: the dashboard calls a route
only when the feature set has loaded and does not list the feature, so an
older backend never sees a 404 from them.

The routes follow the legacy surface: paths relative to `VITE_BASE_URL`, the
DRep id as `/drep/info/{drepId}` accepts it (the frontend sends the credential
hash in hex), lovelace as decimal strings, 1-based `page` with `pageSize` and a
`total`, and errors in the legacy body. The frontend reads them through the
instance without the 500 redirect, so a failure stays in its panel.

The chain-data contract already has each source:
`DRepsApi.listDelegators`, `DRepsApi.listUpdateHistory`,
`AccountsApi.listDelegationHistory` and `GovAction.depositReturnAddress` in
`govtool/govtool-data-providers/src/chain-data`.

## GET /drep/{drepId}/delegators?page=1&pageSize=12&sort=votingPower

Feature `drep.delegatorList`. Largest delegators first.

```json
{
  "elements": [
    {
      "stakeAddress": "stake1u…",
      "votingPower": "485230000000",
      "delegatedAt": { "epoch": 512, "time": "2026-03-15T10:12:00Z" }
    }
  ],
  "page": 1,
  "pageSize": 12,
  "total": 156
}
```

`delegatedAt` may be null where the source does not date the delegation.
The Delegators table pages through it; the Delegation Distribution bar reads
page 1 with `pageSize=10` and divides by the DRep's voting power; the
representation card shows `total` as the delegator count.

## GET /drep/{drepId}/history?page=1&pageSize=100&sort=desc

Feature `drep.registrationHistory`. Every registration, re-registration,
update and retirement certificate, plus the status changes the ledger makes at
an epoch boundary (inactive when the DRep expires, active again after a vote).

```json
{
  "elements": [
    {
      "kind": "retired",
      "status": "retired",
      "at": { "epoch": 593, "time": "2026-09-12T09:08:00Z" },
      "txHash": "8f31c6e2…",
      "anchor": { "url": "https://…", "hash": "…" }
    }
  ],
  "page": 1,
  "pageSize": 100,
  "total": 18
}
```

- `kind`: `registered`, `reregistered`, `updated`, `retired`, `statusChanged`.
- `status`: the DRep's status after the event, `active`, `inactive` or `retired`.
- `txHash`: null for `statusChanged`, which has no certificate.
- `anchor`: the metadata anchor as of the event, or null.

The History Log reads one page of 100 and filters, sorts and pages it in the
browser. Without the route it shows the latest registration from the DRep
record and says the full history is not available yet.

## GET /account/{stakeAddress}/delegation-history?page=1&pageSize=100

Feature `account.delegationHistory`. The wallet's governance delegations,
newest first; the first row is the current one.

```json
{
  "elements": [
    {
      "target": { "type": "drep", "dRepId": "drep1…" },
      "at": { "epoch": 520, "time": "2026-04-22T08:00:00Z" },
      "txHash": "…"
    }
  ],
  "page": 1,
  "pageSize": 100,
  "total": 3
}
```

`target.type` is `drep`, `abstain` or `noConfidence`; `dRepId` is null for the
last two. The stake address is the hex the ada-holder routes take. Without the
route, My Delegations shows the current delegation only.

## GET /drep/{drepId}/voting-power-history

Feature `drep.votingPowerHistory`. One point per epoch, oldest first; the
Activity Report draws the last points as Delegation Growth.

```json
[
  { "epoch": 513, "votingPower": "3100000000000", "delegators": 1790 }
]
```

`delegators` may be null.

## deposit_return_address on GET /governance-actions

My Submitted Proposals lists the actions whose deposit returns to the
connected wallet: the reward address in `proposal_procedure` is the only
proposer identity on chain. Add `deposit_return_address` (bech32 `stake1…`, or
null) to each row of `GET /governance-actions`. While no row carries the key,
the tab says submitted proposals are not available yet.

## Read today, without a backend change

- Votes: `GET /drep/getVotes/{drepId}`, every vote with its action.
- Outcomes, participation and actions per epoch: every row of
  `GET /governance-actions`, read in pages of 100 until a short page.
- Discussions: the forum backend. A comment made as a DRep carries the DRep's
  key hash, so the dashboard lists the forum threads with the DRep's comments
  (`GET /api/comments?filters[drep_id][$eq]=…`) and the threads that comment's
  author posted. A DRep that never commented as a DRep has no threads there.
