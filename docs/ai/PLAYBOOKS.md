# Playbooks

Which files to touch, per change type. Conventions are in the per-package AGENTS.md;
this is the sequence. Where a file already shows the pattern, this points at it
rather than reproducing it, so the exemplar cannot go stale.

## Frontend: consume a new backend endpoint

Exemplar: getNetworkMetrics and useGetNetworkMetrics. Four files:

1. src/models/api.ts: response type, fields snake_case, mirroring backend JSON
2. src/services/requests/getThing.ts: via the shared API instance, never axios
3. src/consts/queryKeys.ts: add to QUERY_KEYS
4. src/hooks/queries/useGetThingQuery.ts

Plus each folder's index.ts. TanStack Query v5 takes object syntax only:
useQuery({ queryKey, queryFn }). The positional v3/v4 form will not compile. Every
value the query depends on goes in the queryKey, not just the closure: see
useGetProposalQuery, keyed on [key, dRepID, proposalId]. Return a named object like
{ networkMetrics, fetchNetworkMetrics }, not raw { data, refetch }.

## Frontend: add a page or route

1. src/consts/paths.ts: camelCase key, snake_case URL, :param for params. PATHS for
   the core app; PDF_PATHS, OUTCOMES_PATHS, USER_PATHS, BUDGET_DISCUSSION_PATHS for
   pillars.
2. src/pages/MyPage.tsx
3. src/App.tsx: where you register decides access. Top level is public, inside the
   Dashboard element route gets connected chrome, wrapped in PublicRoute redirects
   when a wallet is connected.

Route placement controls layout and connected variants, but does not by itself define
all access requirements:

- Public browsing routes are registered at the top level.
- Routes nested under Dashboard receive the connected dashboard layout.
- PublicRoute currently redirects public DRep-directory routes to their
  /connected equivalents when a wallet is connected.
- Wallet-dependent transaction pages, including registration and governance-action
  creation, are currently top-level routes and enforce their requirements through
  their page and wallet logic.
Keep the page thin. It should wire hooks and state to feature components; follow the
surrounding page and organism structure.

## Frontend: add a component

Choose the existing component layer whose responsibilities most closely match the new
component. The atoms, molecules and organisms directories express increasing feature
composition, but the boundaries are not strict: existing atoms and molecules may use
shared hooks or contexts. Match nearby components before introducing a new layering
rule.
For a reusable component, add or update a Storybook story under src/stories, using
.stories.ts or .stories.tsx depending on whether the story contains JSX.
Storybook build and interaction tests run in CI.

## Frontend: add user-facing copy

src/i18n/locales/en.json, read with t("key"), or I18n.t("key") outside components as
in consts/governanceAction/fields.ts. Interpolation is {{name}} with
t("key", { name }). No hardcoded strings in JSX.

## Frontend: add an environment variable

A new React-visible `VITE_` runtime variable is complete only after updating all four
applicable configuration points:

1. .env.example
2. src/config/env.ts, as MY_VAR: getEnv("VITE_MY_VAR")
3. docker-entrypoint.sh, into the window.__ENV__ block. 
4. docker/docker-compose.yaml, passed into the govtool-frontend service

The entrypoint is easy to miss: without runtime injection, a variable may work through
import.meta.env during local development but be undefined in a deployed container.

Container-only variables such as Umami and Nginx proxy configuration do not belong in
src/config/env.ts or window.__ENV__; document and wire those through
docker-entrypoint.sh and Compose instead.

## Frontend: add a feature flag

In src/context/featureFlag.tsx: add to FeatureFlagContextType, the createContext
default, and the useMemo value. A deploy toggle is a VITE_IS_X_ENABLED var, so do the
env-var recipe first, and compare against both the string "true" and boolean true
since runtime injection can give either. A protocol-phase toggle derives from
appContext's isInBootstrapPhase or isFullGovernance as a useCallback returning a
predicate; exemplar areDRepVoteTotalsDisplayed.

## Frontend: build a new transaction or certificate

Shared Cardano serialization, wallet interaction and transaction submission live in
src/context/wallet.tsx. Form-specific validation and selection of the appropriate
builder remain in the calling form hook or page.

Add the appropriate builder beside its siblings:
- buildXCert for certificates
- buildXGovernanceAction for governance-action proposal builders
- A voting builder for votes

Expose the builder through CardanoContextType and the provider value. Pass the
result to buildSignSubmitConwayCertTx through the matching argument:
certBuilder, govActionBuilder or votingBuilder.

buildSignSubmitConwayCertTx owns maintenance checks, UTxO selection, change,
signing, submission and pending-transaction registration. Supply the correct
transaction type and, when applicable, resourceId; it records the submitted hash
so usePendingTransaction can poll /transaction/status/:txId.

Extend an existing builder rather than adding a parallel path, and verify on a
testnet. This code moves real ada and has no unit tests.

## Frontend: write a unit test

Utils go in src/utils/tests/<utility>.test.ts and normally import the
utility from the parent barrel with from "..". Other tests are generally colocated
with the code under test, for example context/featureFlag.test.tsx.

Run a focused test with npx vitest run <path>.Coverage comes from components, consts, context, hooks,
services and utils; pages and models are excluded, so do not chase coverage there.

## Backend: add a REST endpoint

Copy the shape of sql/get-network-total-stake.sql, then Network.networkTotalStake,
then getNetworkTotalStake in API.hs. Six edits, all required:

1. sql/get-my-thing.sql, positional ? placeholders
2. vva-be.cabal, the new file under extra-source-files
3. src/VVA/Types.hs, internal type
4. the matching src/VVA module, domain function. Copy the header of src/VVA/Network.hs
   TemplateHaskell pragma and local sqlFrom. A new module also needs exposed-modules.
5. src/VVA/API/Types.hs, response type with fields prefixed by the type name and
   ToJSON, FromJSON and ToSchema instances. ToSchema is what puts it in Swagger.
6. src/VVA/API.hs, route on VVAApi and handler, in matching positional order

Consequences of getting 2 or 6 wrong are in govtool/backend/AGENTS.md under "rules
that will bite you". Then mirror it on the frontend and add a case to
tests/govtool-backend/test_cases.

## Backend: add caching to an endpoint

Add a field to CacheEnv in src/VVA/Types.hs, initialize it in the cacheEnv
block in `app/Main.hs`, and use it at the handler call site.

Use newCache for the standard configured TTL. Use the DRep-list TTL only when the
endpoint is deliberately intended to share that longer cache duration; rename or add
a dedicated constructor binding when introducing another cache category.

```haskell
CacheEnv {myThingCache} <- asks vvaCache
cacheRequest myThingCache cacheKey $ do ...
```

## Backend: change a db-sync query

Change a SELECT list and you must change the domain function's
case result of [(a, b, c)] pattern in lockstep. Decoding is positional, so the same
arity in the wrong order compiles and fails at runtime. sql/views.sql holds shared
views the other queries build on. There is no local fixture: validate against a
db-sync instance, and say so if you could not.

## Add a new governance action type

docs/operations/HANDLE_NEW_GOVERNANCE_ACTION_TYPE.md covers this but its paths and
line numbers are stale.A genuinely new governance-action type can affect both frontend and backend.

Frontend locations:

1. src/types/governanceAction.ts: add the GovernanceActionType member, field
   types, schema extending SharedGovernanceActionFieldSchema, and the schema union
   member.
2. src/consts/governanceAction/fields.ts: define the fields, meaning components,
   i18n keys and validation rules.
3. src/consts/governanceAction/filters.ts: add the type when it should appear in
   governance-action filters.
4. src/i18n/locales/en.json: add every label, placeholder, tip, error and display
   key used by the new type.
5. src/utils: add any custom validation and update exhaustive mappings such as
   getGovActionVotingThresholdKey.ts.
6. src/context/featureFlag.tsx: define voting and vote-total behavior for bootstrap
   and full-governance phases.
7. src/context/wallet.tsx: add the Cardano serialization builder and expose it
   through the wallet context.
8. src/hooks/forms/useCreateGovernanceActionForm.ts: add the new type to the
   buildTransaction switch and construct the builder arguments.
9. Update details rendering, tests and Storybook fixtures wherever behavior differs
   by governance-action type.

Backend locations:

1. src/VVA/API/Types.hs: add the type to GovernanceActionType. This affects JSON,
   query-parameter parsing and the OpenAPI enum.
2. Review src/VVA/API.hs filtering, response conversion and enacted-details logic
   for type-specific mappings.
3. Update backend API tests and response examples that enumerate or assume the
   existing action types.

Verify creation at /create_governance_action, backend filtering and response
decoding, voting behavior, details rendering and protocol phase visibility.Add
frontend unit tests and backend/E2E coverage where applicable.
Actions must comply with CIP-100 and CIP-108.
Afterward, update the stale operations document rather than preserving conflicting
paths.

## Change metadata validation rules

File layout and the two cross-package constraints are in docs/ai/ARCHITECTURE_MAP.md
under govtool/metadata-validation. Verify with npm run lint && npm test.

## Add E2E coverage

tests/govtool-frontend/playwright, against a deployed environment. The spec goes in
the numbered feature folder that matches; reuse lib/pages page objects over raw
selectors and lib/datafactory for data. Setup and the wallet-bootstrap dependency
chain are in tests/AGENTS.md.

## Finishing any change

Run the checks for what you touched, listed in AGENTS.md under Verification. Frontend
needs lint, tsc and vitest run all green. Add the CHANGELOG.md entry under
"[Unreleased]" with an issue link. Update any doc your change contradicts.
