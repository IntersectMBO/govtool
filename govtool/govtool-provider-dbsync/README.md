# @govtool/provider-dbsync

`ChainDataApiV1` ([`@govtool/data-providers`](../govtool-data-providers)) over
a **cardano-db-sync PostgreSQL database**, live-checked against preview
db-sync and cross-checked against Koios.

```ts
import { createDbSyncProvider } from '@govtool/provider-dbsync';

const { chainData, close } = createDbSyncProvider({
  network: 'mainnet',
  connection: { host, port, database, user, password },
});
```

Options: `network` (required; decides stake address prefixes) and either
`connection`, in which case the provider opens and owns a `pg` pool and
`close()` ends it, or `db`, a caller-owned object with one `query(sql, params)`
method, which is how tests drive it with a fake. Statements run under a
statement timeout, and a cancelled statement is reported as
`PROVIDER_TIMEOUT`. Every driver failure becomes a `ChainDataError` with a
generic message, so a connection string or table name never reaches a
response.

The provider owns its SQL. A statement may be changed or added when the
contract needs a value; a changed statement needs a test pinning whatever
behaviour the backend relies on. Every computed column is aliased, because
`pg` returns unaliased `encode(...)` or `lower(...)` columns under duplicate
names and a name-based reader silently gets `undefined`.

## What is served

| Area         | Served                                                                                                   | Omitted, and why                                                                                                                                                      |
| ------------ | -------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| network      | `getNetworkInfo`, `getProtocolParams` (+ past epochs), `getStakeDistribution`, `getTreasury`             | `getGenesisParams`: db-sync does not keep the genesis file                                                                                                            |
| accounts     | `get`, `getDelegation`, `getPoolDelegation`, `getVotingPower`, `listDelegationHistory`                   | `balance`: db-sync records withdrawals without saying whether they drew on staking or non-staking rewards, so `rewards` / `rewardsRest` cannot be split                |
| dreps        | `list`, `get`, `listVotes`, `listUpdateHistory`, `getCounts`                                             | `listDelegators`: db-sync holds only the per-DRep total in `drep_distr`; the per-account figure in `epoch_stake` is the pool snapshot, taken at a different boundary and missing every account that delegates to a DRep but not to a pool. `liveVotingPower`: no live per-DRep figure |
| proposals    | `list`, `get` (+ `voterId`), `getEnacted`, `listVotes`, `listActivity`, aggregates                      |                                                                                                                                                                       |
| pools        | `list`, `get`, `listVotes`                                                                               |                                                                                                                                                                       |
| committee    | `getCommittee`, `getMember`, `getConstitution`                                                           |                                                                                                                                                                       |
| transactions | `get`                                                                                                    |                                                                                                                                                                       |

Declared (`system.getCapabilities()`, `capabilities()`):

```json
{
  "sorts": { "dreps": ["votingPower", "registrationDate", "random"], "proposals": ["newest", "oldest", "soonestToExpire", "mostYesVotes", "highestParticipation"], "votes": ["newest", "oldest"] },
  "filters": { "dreps": ["status", "kind"], "proposals": ["type", "status"] },
  "search": ["exactId"],
  "voteAggregate": ["stake", "count"],
  "optionalArguments": ["protocolParams.epoch", "proposals.voterContextOnList"]
}
```

## How it maps

- **Committee membership** is assembled from the genesis committee row, every
  enacted `UpdateCommittee`, the hot-key authorisation and cold-key
  resignation certificates, and term expiry against the current epoch. The
  constitution is the enacted head of its lineage.
- **Vote aggregates** weigh DReps and pools by stake and the committee by
  count, with `notVoted` filled so that `yes + no + abstain + notVoted` is the
  eligible total, and the threshold from the parameters by type and group. A
  pool with no recorded voting power is refused rather than counted as zero.
- **DRep fields** are omitted rather than zeroed when the ledger's
  distribution has no row for the DRep (SPEC.md §3.2).
- **Unknown but well-formed addresses** get an explicit empty answer
  (`isRegistered: false`, `null`, an empty page) rather than `NOT_FOUND`: a
  brand-new wallet is exactly this case.
- **Pages** are capped at 1,000 rows; a larger request is refused, never
  silently shortened.

## Tests

```bash
npm run verify   # typecheck, build, fixture tests (node --test test/*.test.mjs)
```

Fixture tests drive the provider through a fake `db`, so they pin the SQL
sent and the row mapping without a database. They cannot catch a column the
live schema spells differently, so after touching a statement run the live
scripts against a real db-sync:

```bash
npm run build
DBSYNC_POSTGRES_HOST=... DBSYNC_POSTGRES_PORT=... DBSYNC_POSTGRES_USER=... \
DBSYNC_POSTGRES_PASSWORD=... DBSYNC_DATABASE=... \
[NETWORK=preview] [KOIOS_URL=https://preview.koios.rest/api/v1] [SKIP_KOIOS=1] \
node scripts/live-dreps.mjs        # also live-proposals, live-network, live-committee-pools
```

Each script is read-only, asserts the contract's invariants on real rows and
exits non-zero when a check fails. Koios is a second opinion: a mismatch is
reported but does not fail the run.
