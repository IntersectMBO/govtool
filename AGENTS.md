# GovTool

Monorepo for gov.tools, the Cardano governance (CIP-1694) app. Nested files load
when you work in their folder:

govtool/AGENTS.md: data contract and providers, metadata service, forum backend,
local runs, and what couples the frontend to the backend.
govtool/frontend/AGENTS.md: the React app and the vendored forum UI.
govtool/govtool-backend/AGENTS.md: adding a route, environment variable or
governance action type.
tests/AGENTS.md: the pytest and Playwright suites and the local devnet.

Paths in these files are from the repository root unless they sit inside the
folder being described.

## Layout, where it is not obvious

govtool/frontend is GovTool's only UI. The proposal discussion forum (src/pdf-ui)
and governance action history are frontend source, no longer npm packages, so
their bugs are fixed here.

govtool/govtool-backend is GovTool's API, beside the forum backend
(govtool/govtool-pdf-backend) and the metadata service. The Haskell backend
(govtool/backend, vva-be, cabal), backend-ts and govtool/metadata-validation are
removed; a doc that names them as current is stale.

Every chain write is a transaction the frontend builds
(govtool/frontend/src/context/wallet.tsx) and the user's wallet signs; no
GovTool backend submits one.

docker/swarm-stack/docker-stack.yml runs GovTool on a Docker Swarm server from
the published images; docker/docker-compose.yaml runs it locally against your
own db-sync.

docs is the Docusaurus site behind docs.gov.tools (pages in docs/docs). docs/api
is not published: the planned /api/v1 surface, the metadata service and forum
backend specs, and the numbered decision log, docs/api/decisions.md.

gov-action-loader (Vue + FastAPI, submits governance actions to a testnet) is
not built or tested by CI. Touch it only when asked.

flake.nix and govtool/frontend's default.nix, shell.nix and .envrc predate the
current toolchain (Node 18, yarn.lock, paths that no longer exist). Do not set
up from them; use the Node version in govtool/frontend/.nvmrc, which meets every
package's engines.

## Workflow

Branch from develop and target it; main only takes releases and hotfixes. Name
branches type/issue-description (fix/123-short-description), as CONTRIBUTING.md
requires, and rebase on develop rather than merging it in.

Add a CHANGELOG.md line under [Unreleased], in Added, Fixed, Changed or Removed,
and fill .github/pull_request_template.md. Never bump versions by hand:
update-govtool-version.yml rewrites the frontend and backend package.json and
rolls [Unreleased] into a release heading.

Commit subjects follow recent history, lowercase Conventional Commits such as
fix(frontend): ..., not the capitalized style CONTRIBUTING.md describes.

Update any doc, AGENTS.md included, that your change makes wrong.

## Verify before claiming

No pull-request job runs the code's tests or lint. pr.yaml lints the
Dockerfiles and builds and scans the backend and frontend images (the backend
build typechecks the backend and providers; the frontend's does not), its
lint.sh and unit-test.sh steps find no script and pass, and check-docs.yml
builds the docs site when docs/ changes. The code checks run on push to a branch
of this repository, for the paths they watch (code_check_frontend.yml,
code_check_backend.yml, test_storybook.yml, frontend_sonar_scan.yml), so a pull
request from a fork gets none of them. Nothing checks govtool-metadata-service
or govtool-pdf-backend before merge. Run the nested file's checks before saying
a change works.
