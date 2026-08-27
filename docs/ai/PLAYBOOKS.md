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

Keep the page thin. It wires hooks to organisms; markup belongs in organisms.

## Frontend: add a component

The only real decision is the layer. atoms for anything with no GovTool dependencies,
molecules for compositions of atoms that do not fetch, organisms for feature blocks
that may use contexts and query hooks. Add a .stories.tsx if it is reusable, since
Storybook builds in CI.

## Frontend: add user-facing copy

src/i18n/locales/en.json, read with t("key"), or I18n.t("key") outside components as
in consts/governanceAction/fields.ts. Interpolation is {{name}} with
t("key", { name }). No hardcoded strings in JSX.

## Frontend: add an environment variable

Real only once it is in all four:

1. .env.example
2. src/config/env.ts, as MY_VAR: getEnv("VITE_MY_VAR")
3. docker-entrypoint.sh, into the window.__ENV__ block. This is the forgotten one:
   skip it and the var is undefined in every container while working locally.
4. docker/docker-compose.yaml, passed into the govtool-frontend service

## Frontend: add a feature flag

In src/context/featureFlag.tsx: add to FeatureFlagContextType, the createContext
default, and the useMemo value. A deploy toggle is a VITE_IS_X_ENABLED var, so do the
env-var recipe first, and compare against both the string "true" and boolean true
since runtime injection can give either. A protocol-phase toggle derives from
appContext's isInBootstrapPhase or isFullGovernance as a useCallback returning a
predicate; exemplar areDRepVoteTotalsDisplayed.

## Frontend: build a new transaction or certificate

All of it lives in src/context/wallet.tsx. Add a buildXCert or
buildXGovernanceAction beside its siblings, hand the certificates to
buildSignSubmitConwayCertTx which owns builder, UTxO selection, change address,
signing and submit, expose it on the context value, and register the result with
pendingTransaction so the UI can poll /transaction/status/:txId.

Extend an existing builder rather than adding a parallel path, and verify on a
testnet. This code moves real ada and has no unit tests.

## Frontend: write a unit test

Utils go in src/utils/tests/util.test.ts importing from the barrel, as from "..".
Everything else is co-located, e.g. context/featureFlag.test.tsx. Iterate with
npx vitest run and a path. Coverage comes from components, consts, context, hooks,
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

Two edits plus the call site: a field on CacheEnv in src/VVA/Types.hs, its
initialisation in the cacheEnv block in app/Main.hs using newCache for the standard
TTL or newDRepListCache for the long one, then in the handler:

```haskell
CacheEnv {myThingCache} <- asks vvaCache
cacheRequest myThingCache cacheKey $ do ...
```

The key must include every parameter affecting the result. cacheRequest needs
Hashable; compound keys use hash or hashWithSalt.

## Backend: change a db-sync query

Change a SELECT list and you must change the domain function's
case result of [(a, b, c)] pattern in lockstep. Decoding is positional, so the same
arity in the wrong order compiles and fails at runtime. sql/views.sql holds shared
views the other queries build on. There is no local fixture: validate against a
db-sync instance, and say so if you could not.

## Add a new governance action type

docs/operations/HANDLE_NEW_GOVERNANCE_ACTION_TYPE.md covers this but its paths and
line numbers are stale. Current locations:

1. src/types/governanceAction.ts: the GovernanceActionType member, any new field type,
   a schema extending SharedGovernanceActionFieldSchema, and that schema added to the
   GovernanceActionFieldSchemas union
2. src/consts/governanceAction/fields.ts: the field declaration, meaning component,
   labelI18nKey, placeholderI18nKey, tipI18nKey and rules. The doc says
   src/constants/governanceActionFields.ts, which does not exist.
3. src/i18n/locales/en.json: every key the schema references
4. Custom validation, if any: a helper in src/utils such as numberValidation.ts or
   isValidFormat.ts, used as validate in rules. The doc's
   src/utils/govActionValidations does not exist.
5. src/context/featureFlag.tsx: how the type behaves in bootstrap versus full
   governance. Skipping this is how a new type ships silently unvotable.
6. src/context/wallet.tsx: a buildXGovernanceAction if it needs a new certificate

Verify at /create_governance_action, then fix the ops doc, which asks you to. The
schema drives both rendering in CreateGovernanceActionForm.tsx and hashing and
validation in useCreateGovernanceActionForm.ts, so getting it right is most of the
work. Actions must comply with CIP-100 and CIP-108.

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
