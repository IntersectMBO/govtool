# GovTool data layer

Read AGENTS.md next to this file before working on any of the data-layer packages.
It covers the architecture, the build order, the invariants and the traps.

Three things that are worth knowing before you touch anything, because getting
them wrong is expensive:

This is an early-stage trial. Nothing here is committed or deployed. Do not
describe it as shipped.

There are three backends and only govtool/backend, the Haskell one, is in the
deployment compose. Compatibility is measured against it.

The packages use file: dependencies that resolve to built output, so the
contract package must be built before anything that depends on it will
typecheck.

npm run verify in any package is the check to run before claiming it works.

To run the whole stack locally against Koios, with no database:

```bash
docker compose -f docker-compose.koios.yml up --build
```

Run it from this folder.

See the local instance section of AGENTS.md for the ports, the switches and
what to expect.
