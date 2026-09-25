# @govtool/provider-fixture

A complete GovTool governance data layer over a **frozen slice of mainnet**. No
network, no database, no credentials: point the backend at this and the whole
stack runs locally and deterministically.

Implements all six components of [`SPEC.md`](../govtool-data-providers/SPEC.md):
chain data, metadata, index, committee info, pinning, transaction monitoring.

## Use

```ts
import { createFixtureProvider } from '@govtool/provider-fixture';

const govtool = createFixtureProvider();

const { data } = await govtool.chainData.governance.dreps.list({ page: 1, size: 10 });
const hits = await govtool.index.dreps.searchDReps({ page: 1, size: 10, term: 'alice' });
```

`createFixtureProvider()` also returns `data`, the raw dataset, so a test can
assert against it directly rather than inferring what the fixture contains.

## What is in the dataset

Captured from mainnet, chosen for spread rather than size:

| | |
|---|---|
| DReps | 30: active, inactive and retired; a third of them anonymous |
| Proposals | 60: live, expired and enacted; five action types |
| Votes | ~1,000 across 12 proposals |
| Pools | 10 |
| Committee | the real committee, with cold and hot credentials |
| Accounts | 17 real stake addresses with balances and delegations |

Everything in `data/mainnet.json` is **already in contract shapes**. Mapping
happens at capture time, so the provider is a pure reader and a shape bug
fails when the fixture is captured rather than when it is read. The file is
committed, so nothing needs capturing to run the stack.

## Refreshing the dataset

```bash
BLOCKFROST_PROJECT_ID=<key> npm run capture
```

Tunable with `CAPTURE_DREPS`, `CAPTURE_PROPOSALS`, `CAPTURE_VOTED_PROPOSALS`,
`CAPTURE_POOLS`, and `BLOCKFROST_URL` for a non-default endpoint. The capture
converts the source's float thresholds into exact `Ratio`s by
bounded-denominator continued fractions (`0.67 → 67/100`, `0.666… → 2/3`) and
bech32-encodes treasury withdrawal recipients, so the fixture is type-correct
rather than approximately right.

## What it declares, and why that matters

The fixture is a **conformant provider, not a complete one**, which makes it
useful for testing degradation:

- **`voteAggregate: ['count']`**: it holds voting power for the DReps it
  captured, not for every voter on an action, so a stake-weighted total would be
  assembled from partial data. It counts heads and says so. A consumer that
  renders an ada prefix on this is wrong, and the fixture will catch it.
- **`search: ['exactId']`** on chain data: free-text search belongs to the
  index component, which has the resolved documents. Searching a name through
  chain data correctly returns nothing.
- **no `protocolParams.epoch`**: one frozen epoch, so a past-epoch read is
  genuinely unavailable and is refused rather than faked.
- **no `accounts.getVotingPower`, `getPoolDelegation` or delegation history**,
  and no `listUpdateHistory` or `listActivity`: the capture does not carry
  them, so the methods are absent rather than half-filled.

## Tests

```bash
npm run verify
```

21 tests covering the conventions (ratios, integer strings, 1-based paging),
the DRep directory, the voted/not-voted listing and its denominator, typed
bodies, lineage-keyed `getEnacted`, committee identity, the index, and the
satellite services.
