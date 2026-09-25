# @govtool/data-providers

Provider-agnostic TypeScript contracts for GovTool's data layer. Interfaces and
types only: no implementation, no transport, no framework, zero runtime
dependencies.

[SPEC.md](./SPEC.md) is the specification: what the data layer is for, the
six components, and the conventions every provider must honour. This file is
only how to use the package.

## Install

Path dependency, as the other packages do it:

```json
{
  "dependencies": {
    "@govtool/data-providers": "file:../govtool-data-providers"
  }
}
```

It publishes built output, so build it before anything that depends on it will
typecheck:

```bash
npm ci && npm run build
```

## Import

Prefer the subpath imports. They are collision-free and make the component
boundary visible at the import site:

```ts
import type { DRep, ChainDataApiV1 } from '@govtool/data-providers/chain-data';
import type { MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';
import type { GovernanceIndexV1 } from '@govtool/data-providers/index-provider';
import type { CommitteeInfoProviderV1 } from '@govtool/data-providers/committee-info';
import type { TransactionMonitorV1 } from '@govtool/data-providers/tx-monitor';
```

The root entry exposes the same six as namespaces (`chainData`, `metadata`,
`pinning`, `governanceIndex`, `committeeInfo`, `txMonitor`) and re-exports the
two error classes. It does not flatten them: `chain-data` and `metadata` each
define their own `Hex` and `Timestamp`, by design, so `metadata` stands alone.

## Layout

```
src/chain-data/       ChainDataApiV1, the ledger read model         required
  common.ts           envelopes, paging, errors, scalars
  refs.ts             the identifiers every entity is addressed by
  network.ts          tip, protocol and genesis params, stake totals, treasury
  accounts.ts         one stake account
  governance/         dreps, pools, proposals, votes, committee
  transactions.ts     whether a transaction is on chain
  capabilities.ts     which option values a provider honours, and the helpers a UI reads it with
src/metadata/         MetadataServiceV1, anchored off-chain documents   required
src/index-provider/   GovernanceIndexV1, search over actions, DReps and pools
src/committee-info/   CommitteeInfoProviderV1, identity behind committee credentials
src/pinning/          PinningServiceV1, the author-side write path
src/tx-monitor/       TransactionMonitorV1, mempool and confirmation depth
```

## What is optional, and why

An optional member (`listDelegators?`, `getTreasury?`) is how the contract says a
provider may not have it at all. Check for it rather than calling it and
catching; an absent member is a `TypeError`, not a rejected promise:

```ts
if (api.governance.dreps.listDelegators) {
  const { data } = await api.governance.dreps.listDelegators(id, {
    page: 1,
    size: 20,
  });
}
```

Required members are the ones a source must serve to be a GovTool provider.
`capabilities.ts` covers only what a type cannot express: which option values
a method honours, such as sort keys, filter values, search modes and vote
aggregate representations.

## Verify

```bash
npm run verify
```

`format:check`, `lint`, `typecheck`, then the tests. `test/conformance.ts` is
type-checked and never executed; for a contracts package that is the
meaningful test, and it is where a fixture goes when an entity is added.
`test/runtime.test.mjs` builds and then checks the little that survives to
runtime: the entry-point wiring, the namespaces and the helpers.

## Contributing

Contract only. Anything that fetches, queries, caches or wires DI belongs in a
provider package that depends on this one. Loosening a required field to
optional is safe for providers and breaks consumers that read it unguarded, so
after editing `src/`, typecheck every provider **and** `govtool-backend`.
