# @govtool/provider-blockfrost

Blockfrost implementation of the GovTool Chain Data contract
([`@govtool/data-providers/chain-data`](../govtool-data-providers)), for
[#4234](https://github.com/IntersectMBO/govtool/issues/4234).

Verified against the self-hosted **blockfrost-ryo 3.1.1** at
`https://mainnet.blockfrost.sireto.io` (mainnet, epoch 656) on 2026-09-18.
It works against hosted Blockfrost too — pass `projectId`.

## Usage

```ts
import { createBlockfrostProvider } from '@govtool/provider-blockfrost';

const provider = createBlockfrostProvider({
  baseUrl: 'https://mainnet.blockfrost.sireto.io',
  // projectId: process.env.BLOCKFROST_PROJECT_ID,  // hosted Blockfrost only
  stalenessThresholdSeconds: 900,
});

const { data } = await provider.governance.proposals.list({ limit: 25 });
```

No HTTP dependency — it uses the global `fetch`. `fetch` and the clock are
injectable, so the 88 unit tests run without a network.

## What shapes this provider

Blockfrost is a **per-entity store**, and that single fact explains most of
what follows. Directory endpoints return identifiers only, so each element
costs its own request: `/governance/dreps` gives `{drep_id, hex}`, and turning
one into a contract `DRep` takes a detail read _and_ a metadata read. A page of
25 DReps is 50 requests, run 8 at a time.

Three consequences, all deliberate:

- **`limit` is not optional in practice.** The contract says an absent `limit`
  means "everything"; here that would be tens of thousands of requests, so it
  defaults to one Blockfrost page (25).
- **`sort` is refused** on `dreps.list` and `proposals.list`. Sorting needs the
  whole collection; sorting one page would look like a global ordering and
  quietly lie.
- **Filters apply to the hydrated page**, not the collection. A filtered page
  can come back shorter than `limit` while more matches exist further on.
  `nextCursor` tracks the _Blockfrost_ page, so a caller that follows it still
  sees every match.

## What it serves that db-sync does not

- **Every proposal status.** Each proposal record carries its ratified,
  enacted, dropped and expired epoch, so `status` is fully derivable — the
  db-sync provider's SQL returns live actions only.
- **Per-proposal votes with voter identity.** `proposals.listVotes` and
  `getTallies` work, including committee votes. The legacy SQL cannot list a
  proposal's voters at all.
- **Delegators with their stake.** `dreps.listDelegators` is `supported`.
- **A complete typed `UpdateCommittee` body**, quorum included as an exact
  `Ratio` — db-sync renders that threshold as a float and loses it.
- **Richer protocol parameters**: every governance parameter is present and
  named.

## What it cannot serve

Declared per **dataset** at `system.getCapabilities()` — a
`ProviderCapabilityDocument` built in [`src/capabilities.ts`](src/capabilities.ts),
where every refusal carries a cause, a scope and a reason, every listing carries
its cost class, and a served-but-different answer (page-local filters, head-count
tallies) carries a caveat. The full analysis is in
[`docs/api/provider-gap-report.md`](../../docs/api/provider-gap-report.md);
in short:

| Route                                      | Why                                                                                                                                               |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------- |
| `governance.dreps.listVotes`               | `/governance/dreps/{id}/votes` gives the vote choices but **not which proposal** each was cast on. Reconstructing it is ~1,500 requests per DRep. |
| `governance.metrics.get`                   | Every counter is a collection-wide aggregate; there is no aggregate endpoint.                                                                     |
| `transactions.get`                         | `/txs/{hash}` answers **500** on this deployment, for every hash including one from the tip block.                                                |
| `governance.committee.*`                   | No committee or constitution resource exists (400 "Invalid path"). Replaying enacted actions would be a guess, not gov-state.                     |
| `governance.votes.*`, `governance.pools.*` | No cross-cutting vote feed; `/pools/{id}` times out (504).                                                                                        |
| `network.getTreasury`, the stake breakdown | `/network` answers **500** here.                                                                                                                  |
| `surveys`                                  | CIP-179 definitions are transaction metadata, and `/txs` is unavailable. The namespace is omitted rather than present-and-failing.                |

## Behaviour worth knowing

- **Lovelace stays a string** end to end — Blockfrost already returns decimal
  strings, so no precision is lost anywhere.
- **Blockfrost mixes DRep id encodings between endpoints.**
  `/governance/dreps` returns CIP-129 (29 bytes, credential-type header) while
  `/accounts/{stake}` returns CIP-105 (28 bytes, no header) for the _same_
  credential — verified on mainnet. Both are normalised, so `id` is always
  CIP-129 and `cip105Id` always the older form.
- **Committee voters arrive as raw hex**, not bech32, and without saying
  whether the credential is hot, cold or script-based.
- **`lifecycle.submitted` is always absent.** Blockfrost's proposal record has
  no submission epoch or time. It could be guessed from
  `expiration - gov_action_lifetime`, which is wrong whenever that parameter
  has changed, so it is left unset.
- **Tallies are headcount, never stake.** Votes carry no voting power, so a
  DRep or SPO tally here is _turnout_ and cannot be compared to a threshold.
  `threshold` and `passing` are left unset rather than computed from counts.
- **`kind` is inferred**, not read: a credential with a metadata anchor is a
  `drep`, one without is a `directVoter` — the same rule the legacy API used.
- **Epoch stamps get both halves.** Blockfrost dates things by epoch only, so
  `EpochTimeResolver` reads `/epochs/{n}` for the start time and memoises it.
  That is a cache of immutable facts, not of query results.
- **Errors are always `ChainDataError`**, with retries on 429/5xx and generic
  messages so a `project_id` never reaches a response body.

## Tests

```bash
npm run verify   # format, lint, typecheck, 88 unit tests, build
npm run smoke    # live: hits a real deployment, not part of verify
```

`test/fake-http.ts` routes requests from a table and records the path and
query of each, so a test asserts the exact requests made — the part that must
not drift — without the network. `npm run smoke` is what catches a field this
deployment spells differently; it found three bugs the fixtures could not
(committee role naming, committee hex voters, and the mixed id encodings).

```bash
BLOCKFROST_URL=https://mainnet.blockfrost.sireto.io \
BLOCKFROST_STAKE_ADDRESS=stake1u… npm run smoke
```
