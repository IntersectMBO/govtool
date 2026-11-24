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

getEnv reads runtimeEnv[key] || import.meta.env[key], so any falsy runtime
value ("", false, 0) falls back to the build-time value; only
VITE_IS_CIP179_ENABLED honours a runtime false. It then treats blank and literal
"$VITE_X" values as unset and passes other non-strings through, so flags
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

Every page has one URL, with or without a wallet; there is no "/connected"
path. App.tsx lists the pages that have both layouts once, in
walletOptionalRoutes, and mounts them inside the Dashboard route or on their
own as useIsWalletConnected says: a wallet is enabled, or a saved one is still
reconnecting. Pages that pick their own chrome (the forum, budget archive and
history) use the same hook. Every other page needs a wallet; the rule is
utils/walletOnlyPath.ts (/dashboard, …/new and /my/… except /my/votes).

URLs name resources: an action is /actions/gov_action1… (its CIP-129 id;
build it with getGovActionCIP129Id or getGovActionPath, read it with
parseGovActionCIP129Id), a type's list is /actions?type=…, and intents are
/actions/new, /proposals/new or under /my.

Old URLs (underscores, /connected, /actions/category/…, txHash#index, in any
letter case) are rewritten by utils/getCurrentUrl.ts before any route renders.
Only route names are renamed: a renamed first segment goes in
LEGACY_PATH_SEGMENTS and a renamed second one in LEGACY_CHILD_SEGMENTS, both in
src/consts/paths.ts.

Wallet-only pages are top-level routes that redirect from a useEffect when no
wallet is connected; copy CreateGovernanceAction.tsx.

A new page touches src/consts/paths.ts, src/pages/index.ts, App.tsx
(walletOptionalRoutes when it has both layouts), utils/getPageTitle.ts and
en.json. It also needs NAV_ITEMS and CONNECTED_NAV_ITEMS in
src/consts/navItems.tsx when it is in the nav. The redesigned Dashboard layout
has no page title of its own, so nothing else names a page.

Home's news section (organisms/Home/HomeNews, between Outcomes and the FAQ)
lists the newsletter's four latest posts from its Ghost Content API, links to
the newsletter site for the rest, and holds the newsletter sign-up, which left
the footer. Both talk to newsletter.gov.tools directly
(services/requests/newsletter.ts, consts/links.ts NEWSLETTER): the Content API
key is Ghost's public read-only one, and signing up asks Ghost to email a
confirmation link, as its own portal does. Tests that render Home mock
useNewsletterPosts, so no test reaches that site.

Public pages and the connected Dashboard layout share one frame: TopNav (logo,
nav, Connect Wallet or the wallet menu that holds disconnect-button), the page,
Footer. Below 1024px the nav moves into the mobile drawer. Governance Actions is
one nav link; its three destinations are the Proposed, Live Voting and Outcomes
tabs (organisms/GovernanceActionsTabs.tsx), which reuse the old dropdown's test
ids, public or connected. Governance Activity (/activity, pages/GovernanceActivity.tsx,
organisms/GovernanceActivity/) is one public URL for everyone, mounted outside
the Dashboard route like Home, built from the
live list and the newest decided actions. FAQ is a footer link (faqs-link).

TopNav's search button (organisms/GlobalSearch, Ctrl+K or ⌘K) opens one dialog
over six result types: live actions (/proposal/list), DReps (/drep/list,
listed), forum proposals (the forum backend, only while the forum flag is on),
outcomes (/governance-actions), a pasted address (its stake part through
/account and the ada-holder routes) and an exact ADA handle (handle.me, as the
DRep directory search already does). Its reads use GovernanceActionsAPI, so a
failure stays in the dialog. The forum query (utils/globalSearch.ts) must stay
within the forum backend's PROPOSALS_ALLOWLIST. The button is hidden inside
wallet flows.

Notifications (organisms/Notifications, hooks/useNotifications.ts) have two
modes behind one hook. GET /notifications/config answering version 1 means a
notifications service runs (docs/api/notifications.md): the hook signs in with
the wallet's signData and reads the service. Anything else (404 today) means
local mode: utils/notifications.ts derives items from the routes GovTool
already reads and keeps read state, first-seen times and preferences in
localStorage per stake key. Both modes share the model in models/notifications.ts
(kind plus data, copy in en.json), so a new kind needs the model, the
contract table and notificationContent.ts. The bell renders only for connected
wallets outside wallet flows; tests that render a connected TopNav stub it,
since it needs a QueryClient. The Notification Center and its preferences
(/my/notifications, /my/notifications/preferences) are wallet-only pages
inside the Dashboard route.

A DRep directory card (organisms/DRepCard) shows participation from that DRep's
votes, one /drep/getVotes request per card through the profile's query, and
every /governance-actions row (useDRepCardStats). Delegators come from the
row's delegatorCount (db-sync and Koios fill it), else the delegators route once
drep.delegatorList is served; with neither, the registration date stands in.
Its menu's View Voting History opens the profile at #voting-history, which
DRepDetailsCard scrolls to.

A registered DRep's /dashboard is organisms/DRepDashboard, tabbed by the tab
query parameter; DashboardCards still builds its wallet and action cards, and
everyone else keeps the cards. Its figures come from utils/drepDashboard.ts
over the DRep's votes and every /governance-actions row. Delegators, the full
history, delegation history and delegation growth read routes the backend does
not have yet (docs/api/drep-dashboard.md), behind useIsFeatureServed in
hooks/queries/useDRepDashboard.ts, which fails closed, unlike useFeatureFlag.
Its story seeds the query cache, since the dashboard needs a wallet.
A former DRep that is neither a DRep nor a direct voter gets RetiredDRepDashboard
instead (utils/drepDashboard.ts isRetiredDRep). Retiring needs both
acknowledgements ticked (retirement-ack-voting, retirement-ack-deposit) before
continue-retirement-button enables; the Playwright retirement specs tick them.

Creating a governance action (pages/CreateGovernanceAction.tsx) is six steps in
CenteredBoxPageWrapper's frameless mode, built from
organisms/CreateGovernanceActionSteps/parts.tsx. The CIP-179 suite drives it by
continue-button, the ${type}-radio cards, the fields' name attributes,
storing-information-checkbox and metadata-download-button, so keep those.

Connecting goes through organisms/Modal/ChooseWalletModal and
molecules/WalletOption: wallet.tsx's enable wrapper still opens its own modals,
and WalletOption replaces them once enable settles with the walletConnection
modal (organisms/WalletConnect: checking then a summary, or the failure read
from the error text by utils/walletConnect.ts). Its close button keeps
confirm-modal-button, which the E2E login clicks (with several stake keys it
waits for /stake_keys instead, then dismisses the modal Continue opens); Disconnect opens the
disconnectWallet modal (confirm-disconnect-button). The avatar opens the profile
popup (organisms/ProfileMenu) and the wallet pill the wallet popup
(organisms/ConnectedWalletCard, also in the phone drawer); there is no /profile
page. View Profile and the connection summary open the profile popup through
utils/profileMenu.ts, or go to the dashboard where no avatar is shown (phones).
Switch wallet disconnects without the reload and opens the wallet chooser,
because enable does nothing while a wallet is enabled.

Each page has one main landmark, which TopNav's skip link focuses: public page
shells pass component="main" to PageContainer (or a Box), while the Dashboard
layout wraps its outlet in main, so a page rendered inside Dashboard must not
add another. A filter sidebar renders once per breakpoint (the phone panel or
the desktop column), never both, because Playwright's strict getByTestId fails
on a duplicated row id.

## Design system

The redesign's values live in src/consts/designTokens.ts (colour scales ds.*,
RADIUS, SHADOWS, FONT_FAMILY, PILL_TONES) and feed src/theme.ts; text colours
must reach 4.5:1 on their background (designTokens.test.ts pins the pairs the
design gets wrong: orange text uses 700, or 750 on the nav's orange tint, and
green or amber text uses 700, never 600); the palette
exposes them as theme.palette.ds, and the older palette keys keep their names
with the new values. The type scale is src/consts/typography.ts, used by the
Typography atom: the Figma names (paragraphSM, tagline...) plus the old names
mapped to the nearest size. Geist and Geist Mono are self-hosted through
src/fonts.ts; nothing loads Google Fonts. Icons come from lucide-react, pinned to
an exact version. Rebuilt pages sit in the PageContainer atom (1180px). The
governance action lists share GovernanceActionsHeader (title, Create Governance
Action, tabs) and a search, sort and filter-sidebar frame:
GovernanceActionsListLayout for Live Voting, GovernanceActionHistorySearchFiltersSortBar
for Outcomes, whose state lives in the URL (its status filter draws
FilterSidebar's checkbox rows). An outcome's page is organisms/OutcomeDetails:
tabs in ?tab= (the action's CIP-129 id is the last path segment); each
role section keeps the ${prefix}-voting-results-data and ${title}-expand-button
ids and the Overall Outcome rows the ${prefix}-voting-results-indicator ids,
which test:governance-action-aggregates reads. See all data is a modal dialog in
a portal (in place, MUI would hide it from screen readers), so read its
${title}-*-votes ids from the page, not the section. The page shows nothing from
an anchored document that failed its check (utils/outcomeDetails.ts
outcomeContent): the backend returns the body with an INVALID_HASH status. The
Outcomes type rows show short names but keep the long names in their row ids
(FilterSidebar testLabel), which the E2E filter specs click. The forum's
Proposed list copies that frame inside pdf-ui. The header's four figures (GovernanceActionsStats)
come from every /governance-actions row and the live list. On the Proposed tab
the header's button and notice come from useProposeHeaderControls: GovTool
knows the wallet, and pdf-ui reports the forum sign-in through the App prop
onProposeStateChange (pdf-ui lib/proposeState.js). Import
tokens from @/consts/designTokens rather than @consts in components that tests
render with a mocked @consts barrel.

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

Delegating to a DRep goes through organisms/DelegationReviewDialog: it calls
useDelegateTodRep's delegate on "Continue to wallet", then follows
pendingTransaction.delegate and reads the delegation query entry keyed by that
transaction to tell confirmed from not confirmed. The Abstain and No Confidence
options still call delegate directly.

wallet.test.tsx covers vote and certificate submission, but not the guardrail
script path nor any governance action builder. Check those on tests/devnet.

## pdf-ui

src/pdf-ui is vendored plain JavaScript with two entry points. App.jsx is the
proposal discussion forum, lazy-loaded at /proposals only when the
forum flag is on; it talks to govtool/govtool-pdf-backend through
VITE_PDF_API_URL. BudgetArchiveApp.jsx is the read-only 2025 budget proposals
archive, lazy-loaded by pages/BudgetProposalsArchive.tsx at /budget with no
flag.

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
built in lib/api.js, lib/proposalList.js (the proposals list: one request across
the chosen types) and components such as ProposalsList (now only the unfinished
drafts) and CommentCard; changing a proposal, comment or poll query means
changing all of them.

The budget archive never calls the forum backend: its E2E spec fails on any
budget-discussion API request, and the shared CommentCard stays off the forum API only while the
caller passes archivedReplies. Its data, public/budget-proposals-2025, was
written once by scripts/split-bd-archive.mjs from a forum export that is not in
this repository; the script strips creators' credentials, so any new export goes
through it. The path is hard-coded in lib/budgetArchive.js, nginx.conf (404 for
a missing file, 30-day cache), the split script and
tests/govtool-frontend/playwright/lib/pages/budgetDiscussionPage.ts.

A forum proposal's page (pages/ProposedGovernanceActions/SingleGovernanceAction)
follows OutcomeDetails: it imports Card, CardTitle, MonoTag and HEADER_TINTS from
organisms/OutcomeDetails/parts and shares through ShareDialog (kind proposal).
detailsState.js decides which side card shows; the forum E2E specs read the
page's ids, and copy-link-text appears beside the share button for two seconds
after a copy, as spec 8E expects.

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
