# @govtool/provider-koios

`ChainDataApiV1` ([`@govtool/data-providers`](../govtool-data-providers)) over
the **public Koios REST API**, live-verified against `api.koios.rest` on
mainnet. Preprod, preview and guild use their public hosts; a self-hosted
Koios is reached with `baseUrl`. A capability claim is about a deployment
(SPEC.md §11), so re-run the live script against the instance you deploy on.

```ts
import { createKoiosProvider } from '@govtool/provider-koios';

const { chainData } = createKoiosProvider({
  network: 'mainnet',
  token: process.env.KOIOS_TOKEN, // omit for the free public tier
});
```

Options: `network` (required), `token` (sent as a Bearer token), `baseUrl`
(full base URL including `/api/v1`), `fetch`, `timeoutMs` (30 s),
`maxRetries` (3, on 429, 5xx and timeouts), `maxConcurrency` (4 in flight).
Global `fetch`, no HTTP library, no cache (SPEC.md §3.6).

## What is served

| Area         | Served                                                                                   | Omitted, and why                                                                                                                                       |
| ------------ | ---------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| network      | `getNetworkInfo`, `getProtocolParams` (+ past epochs), `getStakeDistribution`, `getTreasury`, `getGenesisParams` | `totalLiveStake`: Koios has no un-snapshotted network total                                                                                             |
| accounts     | `get`, `getDelegation`, `getPoolDelegation`, `getVotingPower`                            | `balance`: withdrawals are not split into `rewards` / `rewardsRest`, and the contract forbids `total` alone. `listDelegationHistory`: `/account_updates` dates each certificate but does not say what it delegated to |
| dreps        | `list`, `get`, `listVotes`, `listUpdateHistory`, `getCounts`                             | `listDelegators`: `/drep_delegators` reports each account's live balance, not the active snapshot the row requires. `liveVotingPower`: no live per-DRep figure. `activity` on list rows: served on `get` only |
| proposals    | `list`, `get` (+ `voterId`), `getEnacted`, `listVotes`, `listActivity`, aggregates      |                                                                                                                                                        |
| pools        | `list`, `get`, `listVotes`                                                               |                                                                                                                                                        |
| committee    | `getCommittee`, `getMember`, `getConstitution`                                           |                                                                                                                                                        |
| transactions | `get`                                                                                    |                                                                                                                                                        |

Declared (`system.getCapabilities()`, `capabilities()`):

```json
{
  "sorts": { "dreps": ["votingPower", "registrationDate", "random"], "proposals": ["newest", "oldest", "soonestToExpire"], "votes": ["newest", "oldest"] },
  "filters": { "dreps": ["status", "kind"], "proposals": ["type", "status"] },
  "search": ["exactId"],
  "voteAggregate": ["stake", "count"],
  "optionalArguments": ["protocolParams.epoch", "proposals.voterContextOnList"]
}
```

## How it maps

- **DRep directory.** `/drep_list` is the candidate set and only the returned
  page is hydrated, unless a filter or sort needs a fact for every DRep:
  splitting active from inactive or sorting by voting power needs `/drep_info`
  for the whole set (one POST per ~70 DReps), and a `kind` filter or the
  registration-date sort needs every certificate (`/drep_updates` in bulk).
  Everything is then filtered, ordered and paged here, so `total` is exact.
- **Stake distribution.** `totalActiveStake` from `/epoch_info`, the
  predefined targets from `/drep_info`, the SPO figure as the sum of
  `/pool_voting_power_history` for the epoch, and the DRep figure as the stake
  of every active registered DRep, the same definition the db-sync provider
  uses.
- **Delegations** stand only while their target does, checked as the db-sync
  provider checks them: a retiring DRep's delegations are cleared from
  protocol 10, a pool retirement ends delegations to the pool.
- **Committee voters** are resolved from hot to cold credential through the
  committee. A cold-id voter lookup that cannot be resolved is refused rather
  than answered "not voted".
- **Pages** are capped at 1,000 rows, which is also PostgREST's cap on a
  response; a larger request is refused, never silently shortened.
- **Request bodies** over roughly five kilobytes are rejected by Koios, which
  is a limit on bytes and not on the number of identifiers, so batches are
  sized by measured bytes.

## Tests

```bash
npm run verify                        # typecheck, build, fixture tests (node --test test/*.test.mjs)
npm run live                          # every method against mainnet, contract invariants asserted
KOIOS_NETWORK=preprod npm run live
KOIOS_BASE_URL=https://koios.example/api/v1 KOIOS_NETWORK=mainnet npm run live
KOIOS_TOKEN=... npm run live          # the token is never printed
```

Fixture tests use an injected `fetch` answering invented Koios-shaped JSON.
They cannot catch a field the live API spells differently, so run the live
script after touching a mapper: it walks every method sequentially, inside
the public tier's rate limit, asserts the contract's invariants on real data
and exits non-zero on any failed check.
