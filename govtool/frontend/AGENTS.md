# GovTool frontend

Import from react-router (v8), not react-router-dom, and call useQuery with the
object syntax (TanStack Query 5); the older forms do not compile.

## Checks

From this folder, after npm ci:

```bash
npm run tsc
npm run lint          # any warning fails
npx vitest run        # add a test file's path to run only that file
```

npm test is bare vitest: it exits in a non-interactive shell but watches in a
terminal, so use npx vitest run. Every run rewrites junit-report.xml, which is
tracked; git checkout -- junit-report.xml before committing. A module that
package.json lists but Vite or tsc cannot resolve means node_modules is stale:
npm ci.

prettier is not a dependency, so npm run format fails or runs whatever prettier
is on PATH. Do not format src with it: pdf-ui and many other files are not
prettier-clean, and the diff would bury the change.

CI also runs two Playwright checks on every push that touches this folder or the
Playwright suite (code_check_frontend.yml): npm run test:cip179 and npm run
test:governance-action-aggregates, from tests/govtool-frontend/playwright after
npm ci here and npm ci --ignore-scripts plus npx playwright install --with-deps
chromium there. Each starts its own Vite dev server from this folder and mocks
the backend. test_storybook.yml builds Storybook and runs the play functions in
src/stories; give a new reusable component a story there.

## Imports and barrels

tsconfig maps @atoms, @molecules, @organisms, @hooks, @consts, @context, @models,
@services, @utils, @pages and @mock to that folder's index.ts, and Vite maps them to the
folder. So a new file must be exported from its barrel before it can be imported
by alias, and a deep alias such as @hooks/mutations builds under Vite but fails
tsc; use @/hooks/mutations. Paths without an alias (types, config, cip179,
pdf-ui, theme, i18n) are imported through @/.

Tests mock whole barrels with vi.mock factories (src/context/wallet.test.tsx,
GovernanceActionVoting.test.tsx). A module that starts importing a new symbol
from a mocked barrel makes those tests throw No "x" export is defined on the
mock, until the factory provides it.

Only path for each concern: env through env in src/config/env.ts; copy through
src/i18n/locales/en.json, the only locale, with no check for missing keys (outside
components, I18n.t as consts/governanceAction/fields.ts does); feature gating
through useFeatureFlag; chain writes through useCardano.

## Reading from the backend

Copy getGovernanceActionRecord (src/services/requests/governanceActions.ts) and
useGetGovernanceActionRecordQuery:

1. Response type in src/models, exported from its index.ts.
2. Request in src/services/requests, exported from requests/index.ts.
3. Key in src/consts/queryKeys.ts.
4. Hook in src/hooks/queries, exported from its index.ts.

Pick the Axios instance on purpose: API carries the 500 redirect
(govtool/AGENTS.md, Frontend and backend), and GovernanceActionsAPI uses the
same base URL without it. To keep one API call on the page, pass
validateStatus: () => true and check the status yourself, as
getSystemFeatures.ts does.

main.tsx turns off refetchOnMount and refetchOnWindowFocus, so a remount never
refreshes data: every value a query depends on belongs in its queryKey.

After a transaction, context/pendingTransaction/utils.tsx invalidates by key
prefix [key, transactionHash]. A query that must refresh after a transaction
keeps the pending transaction hash second in its key and its own arguments after
it; see useGetVoterInfo in hooks/queries/useGetVoterInfoQuery.ts.

At boot appContext.tsx reads /system/features; nothing reads
/system/capabilities. Feature gates fail open: a misspelt id, a failed request
or a loading state all show the feature.

## Environment variables

A new VITE_ variable goes in .env.example, src/config/env.ts, the window.__ENV__
block in docker-entrypoint.sh, and docker/swarm-stack/docker-stack.yml, plus
govtool/docker-compose.fixture.yml, govtool/docker-compose.koios.yml and
tests/devnet/docker-compose.yml when those stacks need a value.
docker/docker-compose.yaml reads the frontend's .env through env_file. Miss the
entrypoint and the published image never gets the variable, while npm run dev,
the fixture stack's dev server and an image built from a checkout (which bakes
in the local .env) all still have it.

Container-only settings (TRUSTED_PROXY_CIDRS, REAL_IP_HEADER, UMAMI_*) are read
by docker-entrypoint.sh for nginx and never go through env.ts.

getEnv treats empty, blank and literal "$VITE_X" values as unset, and a runtime
"" falls back to the build-time value. It passes non-strings through, so flags
compare === "true" || === true, as featureFlag.tsx does.

A new flag goes in FeatureFlagContextType, the createContext default and the
value useMemo with its dependency array, all in src/context/featureFlag.tsx. A
deploy toggle reads a VITE_IS_X_ENABLED variable; a protocol-phase toggle is a
useCallback over isInBootstrapPhase or isFullGovernance, as
areDRepVoteTotalsDisplayed is.

Values that trip people:
VITE_IS_DEV: any non-empty value, "false" included, turns on dev behaviour and
skips the maintenance check.
VITE_IS_CIP179_ENABLED: on when unset, off for any value other than true. The
surveys also hide when the backend lists survey.linkedVoting as unavailable.
VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: gates only the forum and needs a
non-empty VITE_PDF_API_URL too. The budget archive uses neither.

Production builds drop every console call, warn and error included
(drop_console in vite.config.ts), so console output is not a production
diagnostic; Sentry is.

## Routes and pages

"Connected" means localStorage holds wallet_data_stake_key and wallet_data_name
(utils/checkIsWalletConnected.ts). The Playwright wallet
(tests/govtool-frontend/playwright/lib/wallet/pageWallet.ts) and the CIP-179
fixtures (tests/govtool-frontend/playwright/tests/cip179/fixtures.ts) seed the
same keys, so renaming one breaks the E2E suite and the CIP-179 check.

Pages take one of two shapes. Governance actions and the DRep directory have a
separate "/connected" path, and PublicRoute, GovernanceActionDetails and
Dashboard rewrite between the two by string. Newer pages keep one path: App.tsx
mounts the public route only while no wallet is enabled and the Dashboard route
always, and the page switches its layout on isEnabled, as
BudgetProposalsArchive.tsx does.

Wallet-only pages are top-level routes that redirect from a useEffect when no
wallet is connected; copy CreateGovernanceAction.tsx.

A new page touches src/consts/paths.ts, src/pages/index.ts, App.tsx (both
branches when it has both), utils/getPageTitle.ts and en.json. It also needs the
title function in pages/Dashboard.tsx when it renders in the Dashboard without a
CONNECTED_NAV_ITEMS entry, and NAV_ITEMS and CONNECTED_NAV_ITEMS in
src/consts/navItems.tsx when it is in the nav.

## Transactions

src/context/wallet.tsx is the chain-write boundary, and it moves real ada. The
builders return a certificate, vote or proposal builder; the caller passes it to
buildSignSubmitConwayCertTx, which owns UTxO selection, signing, submission and
pending-transaction registration.

Traps:
buildSignSubmitConwayCertTx returns undefined, with no error, while another
transaction is pending, though its type says Promise<string>; the governance
action builders catch their own errors and return undefined. Check for
undefined.
The context default is {}, so a component outside CardanoProvider fails with "x
is not a function" rather than a provider error. Consume it through useCardano.
pdf-ui calls builders by name through its walletAPI prop, and nothing
type-checks those calls.

A new builder goes in CardanoContextType, the provider value and its dependency
array. A new transaction type also goes in context/pendingTransaction/types.ts,
usePendingTransaction.ts, pendingTransaction/utils.tsx (getDesiredResult,
getQueryKey, refetchData) and the alerts.<type> strings in en.json.

wallet.test.tsx covers vote and certificate submission, but not the guardrail
script path nor any governance action builder. Check those on tests/devnet.

## pdf-ui

src/pdf-ui is vendored plain JavaScript with two entry points. App.jsx is the
proposal discussion forum, lazy-loaded at /proposal_discussion only when the
forum flag is on; it talks to govtool/govtool-pdf-backend through
VITE_PDF_API_URL. BudgetArchiveApp.jsx is the read-only 2025 budget proposals
archive, lazy-loaded by pages/BudgetProposalsArchive.tsx at /budget_discussion
with no flag.

Lint skips its .js and .jsx (--ext ts,tsx), and tsc compiles them without
type-checking (allowJs, no checkJs). App.d.ts and BudgetArchiveApp.d.ts type
their props by hand, and nothing checks them against the .jsx. pdf-ui imports
GovTool internals: @atoms, @molecules, @consts, @/theme, @/consts/colors,
@/consts/icons and others. Renaming any of those breaks it at runtime while tsc
and lint stay green, and vitest catches only the few paths pdf-ui's tests
import, so grep src/pdf-ui before renaming.
components/ThemeProviderWrapper/theme.test.js fails when pdf-ui uses a palette
path that src/theme.ts does not define.

The forum backend pins the query strings the forum sends, in its SPEC.md
appendix, its *.allowlists.ts files and test/helpers/pdf-ui-corpus.ts. They are
built in lib/api.js and in components such as ProposalsList and CommentCard;
changing a proposal, comment or poll query means changing all of them.

The budget archive never calls the forum backend: its E2E spec fails on any
budget-discussion API request, and the shared CommentCard stays off the forum API only while the
caller passes archivedReplies. Its data, public/budget-proposals-2025, was
written once by scripts/split-bd-archive.mjs from a forum export that is not in
this repository; the script strips creators' credentials, so any new export goes
through it. The path is hard-coded in lib/budgetArchive.js, nginx.conf (404 for
a missing file, 30-day cache), the split script and
tests/govtool-frontend/playwright/lib/pages/budgetDiscussionPage.ts.

Keep its four-space, single-quote style. lib/markdownRenderer.jsx renders raw
HTML from user content; do not copy that into GovTool code.

## Tests

Tests sit next to their source, except in src/utils/tests, src/pdf-ui/lib/tests
and src/pdf-ui/context/tests. Good examples: useGetVoterInfoQuery.test.tsx,
getSystemFeatures.test.ts, GovernanceActionVoting.test.tsx. jsdom by default; a
file that needs WebCrypto opts into node with a @vitest-environment node comment,
as validateSignature.node.test.ts does.

## Lint

--max-warnings 0 with --report-unused-disable-directives: a warning or a stale
eslint-disable comment fails. Easy to hit: console other than warn and error, a
function declaration for a component (arrow functions only), JSX in a .ts file,
a devDependency imported outside *.test.* and *.stories.*, and Airbnb's bans on
for...of and await in a loop. The React and CSS conventions lint does not check
are in docs/docs/developers/style-guides.

## Dependencies

postinstall runs patch-package. The only patch renames .js files to .jsx in
@intersect.mbo/intersectmbo.org-icons-set and its filename carries version
1.1.1, so bumping that package means regenerating the patch.
