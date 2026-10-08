# govtool-backend

## Adding a route

Copy src/survey (controller, service, type), the smallest complete example. D164
in docs/api/decisions.md walks one feature from provider to frontend gate.

1. New chain data first: an optional member in
   govtool/govtool-data-providers/src/chain-data/index.ts and a SPEC.md section,
   then rebuild the contract.
2. Controller and service in a folder under src. The controller stays thin; the
   service validates, caches and calls the contract through asHttp, with
   required() for an optional namespace and withMethod() for an optional method
   (src/common/errors.ts).
3. Register the controller and the service in src/app.module.ts, or in a module
   it imports.
4. If the route serves an optional capability: the FeatureId union,
   ProviderSurface and backendFeatures in src/system/capabilities.ts, the surface
   passed in SystemService.getFeatures, its NO_ROUTE entry removed, and every
   line of test/capabilities.spec.ts that expects the feature unavailable (its
   it.each row if it has one, and the LOWERS and committee checks). Then the same
   id in govtool/frontend/src/models/featureSet.ts.
5. A service spec on the typed StubApi and chain() helper in
   test/legacy-shape.spec.ts, and a supertest spec for parsing and status codes,
   as test/governance-actions.routes.spec.ts does. survey.spec.ts casts its stub
   to the contract type instead; do not copy that, since the cast lets a stub
   return an invalid page.
6. The frontend side: govtool/frontend/AGENTS.md, Reading from the backend.
7. If pdf-ui calls it through the forum backend's proxy: add the path to the
   GOVTOOL_PROXY_ALLOWED_PATHS default in
   govtool/govtool-pdf-backend/src/config/config.ts, which no stack overrides,
   and to its copies in that package's .env.example, docker-compose.yml and
   SPEC.md.

Traps:
A controller registered in no module answers 404, and a missing service fails
startup. No test boots AppModule, so npm run verify passes either way; start the
backend and curl the route.
There is no global prefix or versioning. The /api in deployed URLs belongs to
the gateway, which strips it. The /api/v1 surface in docs/api is planned, not
built.
Declare literal routes before :param routes in the same controller.
CORS (src/main.ts) allows GET, HEAD, POST and OPTIONS and the request headers
Authorization and Content-Type; any other method or request header fails the
browser's preflight. Only Retry-After is exposed, so the page cannot read any
other response header.
There is no global ValidationPipe. Validate with src/common/query-enum.ts and
pagination.ts, and throw BadRequestException({ errorType: 'ValidationError',
message }) so the body keeps the legacy shape.
Contract errors map to statuses in src/common/errors.ts, e.g.
CAPABILITY_UNSUPPORTED to 501.
IntegerJsonInterceptor writes the bigint fields. @Res() without passthrough
bypasses it; to set headers use @Res({ passthrough: true }), as the survey
controller does.
CacheService.getOrSet(namespace, key, action, ttl) keys on JSON.stringify(key).
The key must hold every input that changes the result, or one caller's answer
is served to another; normalise inputs and keep property order stable. Each
namespace is its own
LRU, capped by GOVTOOL_CACHE_MAX_ENTRIES; use a new namespace. A route over every
action reuses the warmed snapshot through ProposalService.getActions instead of
paging the provider.
Swagger UI is at /swagger-ui and the document at /swagger.json. Nothing carries
@Api decorators, so it lists routes without response schemas.
Ids at the edge go through src/common/legacy-ids.ts (legacyDRepCandidates,
drepIdToHex, drepIdToCip105, legacyGovActionId).

## Specs that pin behaviour

test/legacy-shape.spec.ts pins the body of every legacy read route with
toEqual, so an added or dropped key fails it; /ipfs/upload
(src/ipfs/ipfs.service.spec.ts) and the newer routes have their own specs.
test/capabilities.spec.ts fails when a declared feature stops matching the code
behind it. Its no-route check greps src for this.chain.x.y, so it cannot see a
call made through required(), withMethod() or destructuring: when a change
starts calling a route that NO_ROUTE lists, update both by hand.

## Adding an environment variable

1. BackendConfig in src/config/config.types.ts.
2. loadConfig in src/config/config.service.ts. A secret reads through
   secretString or requiredSecretString, which add *_FILE and /run/secrets
   support.
3. src/config/config.service.spec.ts.
4. This package's .env.example, and its docker-compose.yml when the value has
   no default.
5. docker/docker-compose.yaml and docker/.env.example.
6. docker/swarm-stack/docker-stack.yml and its .env.example. The stack has no
   env_file, so every variable is listed. A secret also needs the top-level
   secrets block, an entry in the backend's secrets list with target
   govtool_ plus the variable name in lower case, and its export line in
   .env.example.
7. govtool/docker-compose.fixture.yml, govtool/docker-compose.koios.yml and
   tests/devnet/docker-compose.yml, when those stacks need a non-default value.

Specs build ConfigService as a partial cast, so a new field reads as undefined
in them without an error. Limits a reviewer must sign off stay code constants,
not variables (D120, src/metadata/config.ts).

## Adding a governance action type

Rare, since it needs a hard fork, but it spans the contract, every provider, this
backend, the frontend and the forum, and much of it fails silently. The file
list is in docs/docs/developers/operations/handle-new-governance-action-type.md.
