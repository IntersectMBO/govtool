# @govtool/provider-dbsync

db-sync implementation of the GovTool Chain Data contract
([`@govtool/data-providers/chain-data`](../govtool-data-providers)).

It is a read model and nothing else: no caching, no request coalescing, no
background refresh. Those are policy, they differ per deployment, and the
consumer owns them — [`govtool-backend`](../govtool-backend) keeps its cache
and cache warmer in front of this.

## The SQL is the specification

The 17 files in [`sql/`](./sql) are byte-for-byte the ones the legacy GovTool
backend ships. They are **not edited here**. When a contract field cannot be
filled from them, the contract loosens; the SQL does not grow. That is what
keeps this provider's results identical to the legacy API's, and it is why
roughly half the contract reports `CAPABILITY_UNSUPPORTED`.

```bash
# the copy is verifiable
diff -r sql ../backend-ts/sql && echo identical
```

## Usage

```ts
import { createDbSyncProvider } from '@govtool/provider-dbsync';

const provider = createDbSyncProvider({
  host: 'localhost',
  port: 5432,
  database: 'cexplorer',
  user: 'postgres',
  password: '…',
});

const { data } = await provider.governance.dreps.list({ limit: 10 });
await provider.dispose();
```

Already own a pool? Implement `Queryable` over it and construct the class
directly, so there is only one pool in the process:

```ts
import { DbSyncChainDataProvider } from '@govtool/provider-dbsync';

const provider = new DbSyncChainDataProvider({
  query: async (sql, params) => ({
    rows: (await pool.query(sql, [...params])).rows,
  }),
  queryArrays: async (sql, params) => ({
    rows: (
      await pool.query({ text: sql, values: [...params], rowMode: 'array' })
    ).rows,
  }),
});
```

`queryArrays` is not optional. `get-votes.sql` leaves four columns unaliased,
so it must be read positionally — see below.

## What it serves

Fully: `governance.dreps.list`, `governance.dreps.getVotingPowers`,
`accounts.getDelegation`, `surveys.getDefinition`.

Partially — the route works, some fields cannot be filled:
`network.getNetworkInfo` (no magic, no era), `network.getProtocolParams`
(current epoch only; `dvt`/`pvt` thresholds are floats in db-sync and are left
to `raw`), `network.getStakeDistribution` (no `totalActiveStake`),
`accounts.get`, `accounts.getVotingPower`, `governance.dreps.get`,
`governance.dreps.listVotes`, `governance.proposals.*`,
`governance.metrics.get`, `transactions.get` (no classified `effects`).

Not at all: pools as voters, committee membership, the constitution, the
cross-cutting vote feed, delegation and registration history, epoch and block
lists, the treasury. None has a statement in the legacy SQL, and adding one is
out of scope.

The full matrix is `system.getCapabilities()`, which returns a
`ProviderCapabilityDocument`: one entry per dataset, saying what one call costs,
which sort keys / filter values / expands / search modes are honoured, and — for
everything it will not serve — why, in a machine-readable `Absence`. The source
is [src/capabilities.ts](src/capabilities.ts), and
[test/capabilities.spec.ts](test/capabilities.spec.ts) proves every
`CAPABILITY_UNSUPPORTED` the code can throw is already predicted by it.

Three things worth reading there before you build a UI on top:

- **Not every refusal throws.** `governance.proposals.list{sort:
"highestParticipation"}` is accepted and silently not applied, and
  `governance.dreps.listVotes` ignores `sort`, `vote` and `proposalType`
  outright. Those are declared `ignored`, which is the one state a control must
  never be offered for.
- **`dreps.list` and `dreps.get` disagree.** `list` never inspects `expand`, so
  asking it for `liveVotingPower` is a 200 with the key missing; `get` throws
  for the same field. The per-route truth is in `fieldOverrides`.
- **`utxo_view` is a deployment fault, not a code gap.** The shared preview
  db-sync has no such view, so `accounts.getVotingPower` fails there and works
  on mainnet. Pass `missingUtxoViewOverride()` as a `capabilityOverride` where
  that is true; it is deliberately not in the static table, so it clears without
  a release.

## Behaviour worth knowing before you trust a number

- **Lovelace is a decimal string** everywhere. Total supply is 4.5e16 and
  `Number.MAX_SAFE_INTEGER` is 9.0e15, so the legacy API's JSON numbers were
  already lossy. `TreasuryWithdrawals` totals are summed as `BigInt`.
- **`governance.proposals.list` returns live actions only.** The statement's
  first CTE requires `expiration > MAX(epoch)` and no ratified/enacted/
  expired/dropped epoch. A `status` filter for anything but `['live']` is
  refused rather than answered with an empty page.
- **Tallies mix units by role, because the ledger does.** DReps and SPOs are
  weighed by stake (`RoleTally.stake`), the committee by head
  (`RoleTally.count`). Only the field that applies is set.
- **`accounts.getVotingPower` is `basis: 'live'`**: the statement sums current
  UTxO plus rewards, not an epoch snapshot. `governance.dreps.getVotingPower`
  is `basis: 'active'`, from `drep_distr`.
- **`meta.asOf` is always absent.** No statement returns the tip alongside its
  result, and the contract says to omit rather than spend a round trip. Use
  `system.getHealth()` for the tip.
- **Errors are always `ChainDataError`.** A driver failure becomes
  `PROVIDER_UNAVAILABLE` with a generic message and the original on `cause`,
  so a connection string never reaches a response body.
- **`status` and `kind` on a DRep are derived, not stored.** The rules are
  exported as `deriveStatus` and `deriveKind` for a consumer that has to
  reproduce the legacy fields.

## Two places this deliberately differs from backend-ts

Both are bugs in the TypeScript backend that the Haskell backend did not have.
Since compatibility is measured against the Haskell API, this provider matches
Haskell.

1. **`get-votes.sql` must be read positionally.** Its SELECT list has three
   unaliased `encode(...)` expressions and an unaliased `CONCAT`, so `pg`
   names the columns `gov_action_proposal_id, concat, encode, lower, url,
encode, epoch_no, time, vote_tx_hash`. `backend-ts` reads
   `gov_action_id`, `drep_id`, `vote`, `doc_hash`, `proposal_id` and `date` —
   six of nine fields are `undefined`, and because the lookup map is keyed on
   `undefined`, `/drep/getVotes` returns `[]` for every DRep. Read positionally
   here, and pinned by [test/votes.spec.ts](test/votes.spec.ts).

2. **`getEnacted` refuses a type it cannot answer.** The statement only has
   `ParameterChange` and `HardForkInitiation` actions to return;
   `backend-ts` silently substituted `HardForkInitiation` for anything else,
   handing back the wrong action's body. This throws
   `CAPABILITY_UNSUPPORTED` instead.

Two smaller, intentional divergences: `governance.dreps.get` reports
`NOT_FOUND` for a credential that was never registered rather than an
all-empty record, and `accounts.getVotingPower` lets a database failure
surface instead of returning 0 for it.

## Tests

70+ specs, no database required: [test/fake-db.ts](test/fake-db.ts) answers
from canned rows keyed by SQL file and records what was asked, so a test
asserts the provider ran the statement it claims to, with the parameter array
the legacy service used.

```bash
npm run verify   # format, lint, typecheck, test, build
```

What a live database still has to confirm: that the row shapes in
[src/rows/index.ts](src/rows/index.ts) match what `pg` actually returns for
each statement, and that no column name collides the way `get-votes.sql` does.
