# AGENTS.md - GovTool agent guide

Cardano GovTool monorepo: lets ada holders use the CIP-1694 governance features -
register as DRep, delegate voting power, submit and vote on governance actions.
Production is https://gov.tools.

## Guides

docs/ai/ARCHITECTURE_MAP.md: topology, data flow, env vars, CI gates
docs/ai/PLAYBOOKS.md: which files to touch, per change type
govtool/frontend/AGENTS.md: React/TS conventions and traps
govtool/backend/AGENTS.md: Haskell conventions and traps
tests/AGENTS.md: E2E, API and load suites

## Packages, and what belongs in each

- govtool/frontend: React 19 + Vite + TS, the gov.tools app. All UI, routes, forms,
  wallet connection and transaction building. Main surface.
- govtool/backend: Haskell + Servant, read-only REST over cardano-db-sync. All
  endpoints, db-sync queries, response shapes.
- govtool/metadata-validation: NestJS. Rules for what makes CIP-100/108/119 off-chain
  metadata valid.
- govtool/analytics-dashboard: Next.js internal usage dashboard. Not part of the
  gov.tools user flow; touch only when asked about analytics.
- tests: Playwright E2E, Python API tests, Gatling load tests, test infra.
- docker: compose for the whole stack. docs: architecture, style guides, ops.
- gov-action-loader: dev utility to bulk-submit governance actions to a testnet.

A new governance action type is a frontend change. See PLAYBOOKS, "add a new
governance action type".

Three dependencies cannot be fixed from this repo: @intersect.mbo/pdf-ui and
@intersect.mbo/govtool-outcomes-pillar-ui, npm packages that render the Proposal
Discussion and Outcomes pillars inside the frontend, and
govtool-outcomes-pillar-backend, an image pinned in docker/docker-compose.yaml.

## Workflow rules

From CONTRIBUTING.md, enforced by CI or review:

1. Base branch is develop, never main. Promotion: develop, test, staging, main.
2. Branch name is type/issue-number-description, e.g. feat/123-add-voting-ui. Types:
   feat, fix, chore, docs.
3. Every PR updates CHANGELOG.md under "[Unreleased]", in the right subsection
   (Added, Fixed, Changed, Removed), with a link to the GitHub issue.
4. Never hand-bump versions. package.json, vva-be.cabal and the Dockerfiles are
   bumped together by update-govtool-version.yml on manual dispatch. All at 2.0.29.
5. Commit subject: imperative, 50 chars or fewer, capitalized, no trailing period,
   issue number in subject or body.

## Verification

Run the checks for the package you touched. CI runs exactly these.

```bash
# govtool/frontend - all three gate the PR
npm run lint && npm run tsc && npx vitest run   # NOT npm run test: that is watch mode

# govtool/metadata-validation
npm run lint && npm test

# govtool/backend - GHC 9.2.8 + cabal, easiest inside nix
cabal build all && pre-commit run --all-files hlint && pre-commit run --all-files stylish-haskell
```

The Haskell toolchain is not installed here - no cabal, ghc, stack or nix on PATH -
and the backend needs a real cardano-db-sync Postgres to run, with no fixture path.
Edit and reason about backend code freely; never claim a build passed.

## Docs that are wrong

- docs/operations/HANDLE_NEW_GOVERNANCE_ACTION_TYPE.md: stale paths and line numbers.
  Corrected in PLAYBOOKS, "add a new governance action type".
- docs/architecture/README.md: dated 2024-04-30, predates React 19, TanStack Query v5,
  React Router v8 and the Outcomes pillar. Concepts hold, versions do not.

## Local stack

```bash
cd docker && cp .env.example .env   # fill DBSYNC_POSTGRES_*, IPFS_GATEWAY, PDF_API_URL
docker compose up -d --build
```

Frontend on 80, backend on 9999 with Swagger at /swagger-ui, metadata-validation on
3000, outcomes on 3001. Compose also needs govtool/backend/example-config.json,
govtool/metadata-validation/.env and docker/.envs to exist. See docker/README.md.

Skip all of it for frontend-only work: point VITE_BASE_URL at a deployed environment
and run npm run dev.

## Style

Match the surrounding file first. Guides in docs/style-guides for react, css-in-js and
css-sass, all Airbnb-based. Haskell: .stylish-haskell.yaml plus hlint. Prettier: semi
true, singleQuote false, trailingComma all, printWidth 80.
