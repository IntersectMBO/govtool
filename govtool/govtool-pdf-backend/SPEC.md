# GovTool PDF backend — Specification

**Version 1 draft · 2026-09-26 · scope: decision D138, amended by D167**

The proposal discussion forum ("PDF", proposal pillar) backend, rebuilt on
NestJS 11, Prisma 6 and Postgres 16. It replaces the Strapi backend of
IntersectMBO/govtool-proposal-pillar and serves the existing `pdf-ui` client
unchanged.

**This document is the decided state.** An implementer builds the backend and
its tests from it without reading the Strapi code. Where the code disagrees
with it, the code is the backlog item. Endpoint index:
[`docs/api/pdf-api.md`](../../docs/api/pdf-api.md).

Deliberate differences from Strapi are marked **Δn** where they apply and are
listed in [§13](#13-deviation-index). Everything unmarked is Strapi v4
(4.19.1) behaviour as pdf-ui observes it.

---

## 1. Scope

In scope: every call pdf-ui makes (`pdf-ui/src/lib/api.js`, 1.0.17/1.0.18-beta,
byte-identical) for the proposal discussion forum, the behaviour the GovTool
Playwright suite relies on through the UI, and the demo data those tests need
([§11.5](#115-demo-data)).

**Budget discussions are not served (D167).** The 2025 budget proposals are a
read-only archive of static files in the frontend (`/budget-proposals-2025/`),
which makes no budget discussion request. This backend has no budget
discussion table or route: `/api/bds`, `/api/bd/versions/:id`, `/api/bd-*` and
`/api/country-lists` answer 404, and comments target proposals only. The
section numbers of the removed parts (§5.4, §8.8–§8.11) and their Δ numbers are
kept, empty, so references elsewhere stay valid.

Not in scope:

- No Strapi admin panel, content manager, GraphQL, generic content-type CRUD,
  users-permissions roles/permissions API, `/documentation`, or upload plugin.
- No moderation: nothing sets `comments_reports.moderation_status`. It is set
  in the database by an operator or by tests.
- No email (Strapi's SES "report limit" mail), no Sentry, no cron.
- Not ported: `/report/generateSnapShootReport`, `/migration/*`,
  `/govtool-proxy?endpoint=`, `POST /proxy/govtool/*`, `wallet-types`,
  `proposal-submitions`, `proposal-update-committee-content`, any budget
  discussion route (above), `GET /users`, `GET /users/:id`, `/auth/local/register` and the password
  flows, `PUT /proposals/:id`, `GET /proposal-contents`, `findOne` routes of the
  governance action types. pdf-ui calls none of them.
- No data migration from a Strapi database. The schema is new; a one-off
  importer, if wanted, is separate work.

Unknown routes answer 404 `NotFoundError` ([§3.6](#36-errors)).

---

## 2. Stack and layout

- NestJS 11 on Express 5, Prisma 6 client, Postgres 16. `docker compose up -d`
  runs `db` (127.0.0.1:5442) and `backend` (127.0.0.1:1337); the backend runs
  `prisma migrate deploy` before listening, so migrations (including the seed
  migration, [§6](#6-seed-data)) apply on start.
- All API routes are under `/api`. `GET /health` (outside `/api`) answers
  `200 {"status":"ok"}` and is not enveloped.
- Crypto and Cardano parsing go through `libcardano` 3.0.6 only
  ([§7.3](#73-signature-verification)).
- Express's query parser is replaced with `qs.parse(raw, {depth: 10,
  arrayLimit: 100, parameterLimit: 1000, allowDots: false})`. Express 5's
  default ("simple") parser does not understand brackets and must not be used.
- JSON bodies only, limit `BODY_LIMIT` (1 MB). `X-Powered-By` is off. Logs never
  contain JWTs, cookies or signatures.

Suggested module split (one Nest module each): `query` (parser, allowlist,
Prisma translation, envelope serializer), `auth`, `users`, `proposals`,
`proposal-votes`, `polls`, `comments`, `lookups`, `proxy`.

---

## 3. Wire conventions

### 3.1 Envelope

- Single: `{"data": {"id": <int>, "attributes": {...}}, "meta": {}}`.
- List: `{"data": [ {id, attributes}, ... ], "meta": {"pagination":
  {"page", "pageSize", "pageCount", "total"}}}`. With `pagination[start]` /
  `pagination[limit]` the pagination object is `{"start", "limit", "total"}`.
  `pageCount = ceil(total / pageSize)` (0 when total is 0).
- Ids are integers. There is no `documentId`.
- The odd shapes are listed per endpoint: `/auth/*`, `/users/*` and `/proxy*`
  are raw (not enveloped); `GET /proposal-votes` returns a single
  object from a list route; `POST /proposals` returns `data` with no `id`.

### 3.2 Attributes

- `attributes` holds every public scalar of the resource ([§5](#5-data-model),
  "wire" column), **including nulls** (pdf-ui compares fields with `===
  null`), plus `createdAt` and `updatedAt`, plus `publishedAt` for the types
  Strapi had as draft-and-publish: governance action types and
  comments-reports. `publishedAt` equals `createdAt`.
- Order within `attributes` is not significant.
- Timestamps are ISO 8601 UTC with milliseconds (`2026-09-26T10:00:00.000Z`).
  `date` fields (`prop_submission_date`) are `YYYY-MM-DD`.
- **Legacy string references.** Strapi stored several references as strings.
  They are integer foreign keys in the database and are serialized as decimal
  strings on the wire: `proposal_id`, `gov_action_type_id`, `user_id`,
  `poll_id`, `comment_parent_id`. `null` stays `null`. Filter values on them are coerced to integers
  ([§4.3](#43-filters)). Exception: `POST /proposals` returns `proposal_id` and
  `proposal_content_id` as numbers.
- Computed attributes (not stored) are named per endpoint:
  `user_govtool_username`, `user_is_validated`, `subcommens_number` (sic).

### 3.3 Relations and components

- A relation appears only when populated ([§4.5](#45-populate)). To-one:
  `{"data": {id, attributes} | null}`. To-many: `{"data": [...]}`. Nested
  relations follow the same rule inside `attributes`.
- Components (repeatable) are inline arrays of plain objects that carry their
  own `id`: `proposal_links` and `proposal_withdrawals`. They are **always**
  included when
  their owner is serialized (Strapi needed a populate). Order is insertion
  order.
- Exceptions inside `/proposals` items: `content` is `{id, attributes}` with no
  `data` wrapper, and `content.attributes.gov_action_type` is `{id,
  attributes}` with no `data` wrapper ([§8.2](#82-proposals)).

### 3.4 User projections

A user row is never serialized whole. Two projections exist:

- **Self** (only to that user: `POST /auth/local`, `GET /users/me`,
  `PUT /users/edit`): `{id, username, provider: "local", confirmed: true,
  blocked, govtool_username, is_validated, createdAt, updatedAt}`. `username` is
  the lowercase hex reward address. **Δ1**: no `email`, `password`,
  `resetPasswordToken`, `confirmationToken` or `role`, ever.
- **Public** (any populated user relation: `creator`, `reporter`):
  `{"data": {"id", "attributes": {"govtool_username"}}}`. A `fields` selection
  on a user relation is intersected with `{govtool_username}`; asking for
  `username` is accepted and yields `attributes: {}` **Δ2** (Strapi returned
  the stake address). pdf-ui sends `fields[0]=username` on `reporter`.

### 3.5 Request bodies

- Every write except `/auth/local`, `/token/refresh`, `/users/edit` and
  `/proxy` takes `{"data": {...}}`. A missing or non-object `data` gives 400
  `ValidationError` `Missing "data" payload in the request body`.
- Only the fields listed per endpoint as **writable** are read; every other key
  in `data` is ignored silently (pdf-ui echoes whole objects back, including
  ids, counters, `createdAt` and `creator`). Fields listed as **forced** are set
  by the server whatever the client sends. **Δ3** (mass assignment: Strapi let
  clients set counters, owners and submission state).
- Type rules for writable fields: strings must be strings (numbers are
  accepted and stringified for varchar fields); booleans must be JSON booleans;
  integer references accept a number or a decimal string. A type error gives
  400 `ValidationError` `<field> is invalid`. Varchar fields are ≤255 chars
  unless stated; over-length gives 400 `ValidationError` `<field> is too long`.
  A string containing U+0000 gives 400 `ValidationError` `<field> is invalid`
  (Postgres cannot store it); in a query filter value it gives V `Invalid value
  for <path>`.

### 3.6 Errors

Every error body is `{"data": null, "error": {"status", "name", "message",
"details"}}`. `details` is `{}` unless stated. The helpers the implementation
exposes, with the abbreviation used in this document:

| Helper | Abbr | Status | name | message | details |
|---|---|---|---|---|---|
| `badRequestDetails(msg)` | **BD** | 400 | BadRequestError | `Bad Request` | `msg` (a string) |
| `badRequest(msg)` | **B** | 400 | BadRequestError | `msg` | `{}` |
| `validationError(msg)` | **V** | 400 | ValidationError | `msg` | `{}` |
| `applicationError(msg)` | **A** | 400 | ApplicationError | `msg` | `{}` |
| `unauthorized(msg)` | **U** | 401 | UnauthorizedError | `msg`, default `Missing or invalid credentials` | `{}` |
| `forbidden(msg)` | **F** | 403 | ForbiddenError | `msg`, default `Forbidden` | `{}` |
| `notFound(msg)` | **N** | 404 | NotFoundError | `msg`, default `Not Found` | `{}` |
| `payloadTooLarge()` | — | 413 | PayloadTooLargeError | `Payload Too Large` | `{}` |
| `internal()` | — | 500 | InternalServerError | `Internal Server Error` | `{}` |

- **BD** is Strapi's `ctx.badRequest(null, msg)`. pdf-ui reads `details` for
  `Proposal not found` and `You can not access draft proposal details.`; keep
  those on BD.
- Unhandled exceptions, Prisma errors and malformed JSON map to `internal()` and
  B `Invalid JSON` respectively; nothing internal is echoed. **Δ4** (Strapi
  controllers answered many 500s with `{error, message}` bodies, echoing
  `err.message`).
- A unique-constraint violation on a user-writable field gives V
  `This attribute must be unique` unless the endpoint names another message.

### 3.7 Auth levels

- **public**: no token needed. A valid Bearer token identifies the caller. An
  invalid or expired token is ignored and the caller is anonymous **Δ5**
  (Strapi answered 401, which breaks every list page for a user holding a
  stale session).
- **authenticated**: no `Authorization` header gives F `Forbidden` (Strapi's
  Public-role outcome). A present but invalid, expired, wrongly signed token,
  or one whose user no longer exists or is blocked, gives U.
- **owner**: authenticated, then the target row must exist (else N `Not
  Found`) and belong to the caller (else F with the message named per
  endpoint) **Δ6** (Strapi's is-owner answered 401, and let a missing row
  through to the controller).

### 3.8 CORS

- Preflight `OPTIONS` answered for every route; methods `GET, POST, PUT,
  DELETE, OPTIONS`; allowed headers `Authorization, Content-Type`;
  `Access-Control-Allow-Credentials: true`; max age 600 s.
- `Access-Control-Allow-Origin` echoes the request `Origin` when it is allowed,
  never `*` (a wildcard breaks pdf-ui's `withCredentials` calls, and so login).
  `CORS_ORIGINS=*` (default) allows every origin; otherwise it is a
  comma-separated list of exact origins and other origins get no CORS headers.
- `Vary: Origin` on every response.
- Startup fails when `REFRESH_COOKIE_SAMESITE=none` and `CORS_ORIGINS=*`: a
  cross-site cookie plus a reflected-any origin would let any site read a fresh
  JWT from `/token/refresh`.

---

## 4. Query subset

Implemented once: a parser from the `qs` object to a query AST, a per-resource
**allowlist**, a translator from the AST to Prisma `where` / `orderBy` /
`include` / `select`, and the envelope serializer. Every list route accepts
it.

### 4.1 Input

- pdf-ui builds query strings by hand and never URL-encodes: brackets and `$`
  arrive literally, search text is interpolated raw. `qs` decoding applies as in
  Strapi: `+` decodes to a space, `%XX` is decoded, malformed `%` sequences are
  left literal. A search text containing `&` or `#` is truncated by the client;
  the backend does not try to repair it.
- Top-level keys accepted: `filters`, `sort`, `populate`, `fields`,
  `pagination`. Any other key gives V `Invalid query parameter: <key>` **Δ7**
  (Strapi ignored or honoured `publicationState`, `locale`, `_q`…).

### 4.2 Allowlist model

Per resource, three sets of wire paths (dot-joined), declared with the
endpoint:

- **filterable**: the resource's public scalars, plus any relation paths
  listed for it. User-relation paths only reach `id` and `govtool_username`.
- **sortable**: the resource's public scalars plus `createdAt`/`updatedAt`,
  plus listed deep paths.
- **populatable**: listed relation paths, with their maximum depth.

A path outside the set gives V `Invalid key <path>` (filters, sort) or V
`Invalid populate <path>` **Δ7**. Strapi's `sanitizeQuery` was skipped by the
bds and proposals controllers, so filters on private user fields worked there.
Paths that pdf-ui sends but that mean nothing (listed per endpoint, e.g.
`comments_reports.maintainer`) are accepted and ignored.

Limits: `$and`/`$or` arrays ≤20 elements, filter nesting ≤4 levels, populate
depth ≤3, `$in`/`$notIn` ≤100 values. Over a limit gives V `Query too complex`.

### 4.3 Filters

- `filters[field]=v` is `$eq`. `filters[field][$op]=v` applies an operator.
  `filters[$and][i][...]` and `filters[$or][i][...]` take arrays (or
  index-keyed objects, as `qs` produces) of filter objects. Sibling keys in one
  object combine with AND.
- Relation paths nest: `filters[comments_reports][hash][$eq]=h`,
  `filters[comments_reports][moderation_status]=true`. A to-many relation
  filter matches when **some** related row matches.
- A scalar value (or operator object) directly on a relation compares the
  related `id`: `filters[<relation>]=5` is `<relation>.id = 5`.
- Operators: `$eq $ne $lt $lte $gt $gte $in $notIn $null $notNull $contains
  $notContains $containsi $notContainsi $startsWith $endsWith`. Anything else
  gives V `Invalid operator <op>`. `$contains*`, `$startsWith`, `$endsWith`
  apply to string fields only, and `$lt $lte $gt $gte` do not apply to
  boolean fields (both give V `Invalid operator <op>`); `$containsi` is
  case-insensitive (`ILIKE`);
  `%`, `_` and `\` in the value match literally. `$containsi` with an empty
  string matches every non-null value. `$null=true` is `IS NULL`,
  `$null=false` is `IS NOT NULL`; `$notNull` is the inverse.
- Value coercion by the target field's type; failure gives V `Invalid value
  for <path>`:
  - integer (ids, legacy string references, counters): decimal digits only;
  - boolean: `true`/`false`/`1`/`0`, case-insensitive;
  - datetime/date: ISO 8601;
  - string: as is.
- `$in`/`$notIn` take an array (`filters[id][$in][0]=1&...[1]=2`) or a
  comma-separated string.

### 4.4 Sort

Forms, all accepted: `sort=createdAt:desc`, `sort=a:asc,b:desc`,
`sort[0]=createdAt:desc`, `sort[createdAt]=desc`,
`sort[proposal][prop_likes]=DESC` (a deep path as nested object), and
`sort[0][createdAt]=asc`. Direction is `asc`/`desc`, **case-insensitive**
(pdf-ui sends `DESC` for lists and `desc`/`asc` for comments); a missing
direction in the string form means `asc`; anything else gives V `Invalid order
direction`. Deep paths join through to-one relations only. `id asc` is
appended as the final tie-breaker. Text columns that list routes sort on (names,
titles, usernames, texts) use the ICU root collation `und-x-icu`: linguistic
and case-insensitive at the first level, as Strapi was on a glibc Postgres and
as pdf-ui-side `localeCompare` checks expect. Nulls follow Postgres defaults (last for
`asc`, first for `desc`). With no `sort`, the order is `id asc` unless the
endpoint says otherwise.

### 4.5 Populate

Forms, all accepted:

- `populate=*`: every populatable relation one level deep.
- `populate=a`, `populate=a,b`, `populate=a.b` (dot path populates each hop).
- `populate[0]=a.b&populate[1]=c`.
- `populate[a]=*` or `populate[a]=true`: `a`, without its relations.
- `populate[a][populate][b]=*`, `populate[a][populate][0]=b`,
  `populate[a][populate]=*`, and recursively `populate[a][populate][b][fields][0]=f`.
- `populate[a][fields][0]=f`: `a` with only `id` and the listed attributes.

Components are not populate targets (always present). Populating a to-one
relation that is null yields `{"data": null}`.

### 4.6 Fields

`fields[0]=a&fields[1]=b` or `fields=a,b` limits the root `attributes` to the
listed public scalars (`id` is always present; populated relations and
computed attributes are unaffected). Unknown field: V `Invalid key <field>`.

### 4.7 Pagination

- `pagination[page]` (default 1) and `pagination[pageSize]` (default 25).
  `pageSize` above `1000` is clamped to 1000. `page < 1` or `pageSize < 1` or a
  non-integer gives V `Invalid pagination`.
- `pagination[start]` (default 0) and `pagination[limit]` (default 25, max
  1000, `-1` means 1000). Mixing the page and start forms gives V `Invalid
  pagination`.
- `pagination[withCount]=false` omits `total` and `pageCount`.

---

## 5. Data model

Prisma models with `@@map` to the snake_case table names given. Every model has
`id Int @id @default(autoincrement())`, `createdAt DateTime @default(now())
@db.Timestamptz(3)` and `updatedAt DateTime @updatedAt @db.Timestamptz(3)`
unless noted; they are not repeated below. Column names are free; the **wire**
name is fixed. `text` is Postgres `text`; `str` is `varchar(255)` unless a
length is given. "FK→X" is an `Int` foreign key; "cascade" is `onDelete:
Cascade`. Lengths are enforced by the API ([§3.5](#35-request-bodies)), not
only by the column.

### 5.1 Users and auth

**User** `users`
- `username` str(58), unique — wire `username`: the lowercase hex reward
  address the user first logged in with.
- `govtoolUsername` str(30)?, unique — wire `govtool_username`.
- `isValidated` Boolean default false — wire `is_validated`.
- `blocked` Boolean default false — wire `blocked`.
- No email, password, provider, role or confirmation columns.

**AuthChallenge** `auth_challenges` (no `updatedAt`)
- `identifier` str(58); `nonce` char(32), unique; `message` text;
  `timestamp` timestamptz; `expiresAt` timestamptz.
- Index `(identifier)`, index `(expiresAt)`. Never serialized.

### 5.2 Proposals

**GovernanceActionType** `governance_action_types` — seeded ids, no
autoincrement use at runtime
- `name` str(80) — wire `gov_action_type_name`. `publishedAt` timestamptz.

**Proposal** `proposals`
- `userId` FK→User — wire `user_id` (string).
- `likes` Int default 0 — `prop_likes`; `dislikes` Int default 0 —
  `prop_dislikes`; `commentsNumber` Int default 0 — `prop_comments_number`.
  All three are server-maintained only.

**ProposalContent** `proposal_contents` — one row per revision
- `proposalId` FK→Proposal cascade — wire `proposal_id` (string); also the
  relation `proposal` (used by deep sort).
- `userId` FK→User — `user_id` (string). Always the proposal's owner.
- `govActionTypeId` FK→GovernanceActionType — `gov_action_type_id` (string).
- `name` str(80) — `prop_name`.
- `abstract` text NOT NULL default `''` — `prop_abstract` (≤2500);
  `motivation` text NOT NULL default `''` — `prop_motivation` (≤12000);
  `rationale` text NOT NULL default `''` — `prop_rationale` (≤12000). `null`
  input is stored as `''` (pdf-ui calls `.length` on all three).
- `revActive` Boolean default false — `prop_rev_active`; `isDraft` Boolean
  default false — `is_draft`; `submitted` Boolean default false —
  `prop_submitted`; `isLocked` Boolean default false — `is_locked`.
- `submissionTxHash` char(64)?, unique — `prop_submission_tx_hash`;
  `submissionDate` date? — `prop_submission_date`.
- `hardForkContentId` FK→ProposalHardForkContent?, unique, `onDelete: SetNull`
  — relation `proposal_hard_fork_content`.
- Relation `proposal_constitution_content` (inverse of
  ProposalConstitutionContent.contentId); components `proposal_links`,
  `proposal_withdrawals`.
- Index `(proposalId, revActive, isDraft)`, index `(userId, isDraft)`, index
  `(govActionTypeId)`.

**ProposalLink** `proposal_links` (component; no timestamps)
- `contentId` FK→ProposalContent cascade; `position` Int; `link` varchar(2048) —
  `prop_link`; `text` str? — `prop_link_text`. Index `(contentId, position)`.

**ProposalWithdrawal** `proposal_withdrawals` (component; no timestamps)
- `contentId` FK→ProposalContent cascade; `position` Int; `receivingAddress`
  varchar(200)? — `prop_receiving_address`; `amount` Float? —
  `prop_amount` (≥0, ADA). Index `(contentId, position)`.

**ProposalConstitutionContent** `proposal_constitution_contents`
- `contentId` FK→ProposalContent cascade, unique (owning side, as in Strapi).
- `constitutionUrl` varchar(2048)? — `prop_constitution_url`;
  `haveGuardrailsScript` Boolean? — `prop_have_guardrails_script`;
  `guardrailsScriptUrl` varchar(2048)? — `prop_guardrails_script_url`;
  `guardrailsScriptHash` str? — `prop_guardrails_script_hash`.

**ProposalHardForkContent** `proposal_hard_fork_contents`
- `previousGaHash` str? — `previous_ga_hash`; `previousGaId` str? —
  `previous_ga_id`; `major` str? — `major`; `minor` str? — `minor`.
- Deleted with its content (explicitly, in the proposal delete transaction)
  **Δ8** (Strapi left them orphaned).

**ProposalVote** `proposal_votes` — likes and dislikes
- `proposalId` FK→Proposal cascade — `proposal_id` (string); `userId`
  FK→User — `user_id` (string); `voteResult` Boolean — `vote_result`
  (true = like).
- Unique `(proposalId, userId)` **Δ9** (Strapi had no duplicate check, so one
  user could like repeatedly).

**Poll** `polls`
- `proposalId` FK→Proposal cascade — `proposal_id` (string); `yes` Int default
  0 — `poll_yes`; `no` Int default 0 — `poll_no`; `startDt` timestamptz? —
  `poll_start_dt`; `isActive` Boolean default false — `is_poll_active`.
- Partial unique index `(proposalId) WHERE is_active` (raw SQL in the
  migration; Prisma cannot declare it). Index `(proposalId, isActive,
  createdAt)`.

**PollVote** `poll_votes`
- `pollId` FK→Poll cascade — `poll_id` (string); `userId` FK→User — `user_id`
  (string); `voteResult` Boolean — `vote_result`. Unique `(pollId, userId)`.

### 5.3 Comments

**Comment** `comments`
- `proposalId` FK→Proposal cascade — `proposal_id` (string), required.
- `parentId` FK→Comment? cascade — `comment_parent_id` (string).
- `userId` FK→User — `user_id` (string); `text` text — `comment_text`
  (1..15000); `drepId` char(56)? — `drep_id`.
- Relation `comments_reports` (to-many).
- Index `(proposalId, parentId, createdAt)`, `(parentId)`.

**CommentsReport** `comments_reports`
- `commentId` FK→Comment cascade — relation `comment`; `reporterId` FK→User —
  relation `reporter`; `moderatorId` FK→User? — relation `moderator` (not
  populatable); `moderationStatus` Boolean? — `moderation_status` (null =
  pending); `hash` char(89), unique — **never serialized** **Δ10** (it is the
  secret in the moderation-review link; Strapi returned it on every public
  comment list); `publishedAt` timestamptz.
- Unique `(commentId, reporterId)`.

### 5.4 Budget discussions

Removed (D167). Migration `20261008000000_remove_budget_discussions` deletes
the budget discussion comments (their replies and reports cascade), drops
`comments.bd_master_id`, makes `comments.proposal_id` NOT NULL, and drops the
`bds`, `bd_*` and `country_lists` tables.

### 5.5 Lookup tables

The only lookup table is **GovernanceActionType** `governance_action_types`
([§5.2](#52-proposals)): seeded, carries `publishedAt`, read-only over the API.

---

## 6. Seed data

One migration (`<ts>_seed_lookups`) inserts these rows with explicit ids, `ON
CONFLICT (id) DO NOTHING`, then `setval`s each sequence to `max(id)`. Names are
exact: Playwright test ids and expected texts derive from them
(`tests/govtool-frontend/playwright/lib/types.ts`), and pdf-ui has magic
values (the type ids below).

**governance_action_types** (ids are semantic in pdf-ui: 2 = withdrawals, 3 =
constitution, 6 = hard fork; 5 is a UI stub and is not seeded):
1 `Info Action` · 2 `Treasury requests` · 3 `Updates to the Constitution` ·
4 `Motion of No Confidence` · 6 `Hard fork`.

---

## 7. Authentication

### 7.1 Identifiers

`identifier` (query or body) is lowercase-normalised hex and takes one of two
forms; any other gives V `Invalid identifier`:

- **Stake login**: a key-hash reward address, 29 bytes = 58 hex chars, header
  byte `e0` (testnet) or `e1` (mainnet). This is pdf-ui's `wallet.stakeKey`.
  Script reward addresses (`f0`/`f1`) are rejected: they cannot sign. The
  expected key hash is `identifier[2:]`. When `CARDANO_NETWORK_ID` is set, the
  header's low nibble must equal it, else V `Identifier network does not
  match`.
- **DRep login**: a DRep key hash, 28 bytes = 56 hex chars (pdf-ui's
  `wallet.dRepID`). The expected key hash is the identifier itself.

The form, not the presence of a Bearer token, decides the flow **Δ13**
(Strapi treated any authenticated login as the DRep flow).

### 7.2 Challenge — `GET /api/auth/challenge?identifier=<hex>` · public

- Missing identifier: B `Missing identifier`. Bad form: V `Invalid identifier`.
- `nonce` = 16 random bytes as 32 lowercase hex (`crypto.randomBytes`).
  `timestamp` = `Date.now()` in ms. `expiresAt` = timestamp +
  `CHALLENGE_TTL_SECONDS` (300).
- `message` is ASCII, exactly (pdf-ui's `utf8ToHex` is per UTF-16 code unit):
  `To proceed, please sign this data to verify your identity. This ensures that the action is secure and confirms your identity.\nNonce: <nonce>\nTimestamp: <timestamp>`
- Insert the row, first deleting every expired challenge (all identifiers)
  **Δ14** (Strapi kept every row until a login succeeded). Live challenges are
  never evicted and there is no per-identifier cap: identifiers are public
  (stake addresses and DRep ids are on chain), so evicting the oldest would let
  anyone cancel a victim's pending login with a few requests, and a cap would
  not bound the table anyway (random identifiers bypass it). The table is
  bounded by request rate × `CHALLENGE_TTL_SECONDS`; rate-limit
  `/api/auth/challenge` at the reverse proxy.
- Response 200, raw: `{"message": "<message>"}`.

### 7.3 Signature verification

Input: `signature` (hex COSE_Sign1) and `key` (hex COSE_Key), as CIP-30
`signData` returns them, and the challenge row. All `libcardano` 3.0.6 root
exports:

1. `CoseSign1.fromBytes(Buffer.from(signature, 'hex'))` and
   `CoseKey.fromBytes(Buffer.from(key, 'hex'))`. Non-hex input or a CBOR/shape
   error fails verification (never a 500).
2. `Ed25519Key.fromCoseKey(coseKey)`: requires `alg` (label 3) = -8 (EdDSA) and
   reads the public key from label -2. Require `coseKey.keyType` = 1 (OKP) and a
   32-byte public key.
3. `await coseSign1.verify(edKey)`: rebuilds `["Signature1", protected, h'',
   payload]` and checks Ed25519. A detached (null) payload fails.
4. `edKey.pkh.toString('hex') === expectedKeyHash` (blake2b-224 of the public
   key).
5. **Payload binding** **Δ15**: with unprotected header `hashed` absent or
   false, `coseSign1.payload` must equal `Buffer.from(challenge.message,
   'utf8')`; with `hashed = true`, it must equal
   `blake.hash28(Buffer.from(challenge.message, 'utf8'))`. Strapi never compared
   the payload, so any old signature by the key replayed against any fresh
   challenge.

Not checked: the protected `address` header (`coseSign1.getAddress()`). The
GovTool Playwright wallet (`libcardano-wallet` `SimpleCip30Wallet`) puts the
wallet's base address there when asked to sign with a hex reward address or
key hash, so binding it to the identifier would break the suite; steps 4 and 5
already bind key and challenge.

Any failure: A `Verification failed`.

### 7.4 Login — `POST /api/auth/local` · public

Body (raw, not `data`): `{identifier, signedMessage: {signature, key,
expectedSignedMessage}}`.

1. Validation, each V: `identifier was not provided` · `signData object was not
   provided` · `Payload was not provided in signData object.` (no
   `expectedSignedMessage`) · `Signature was not provided in signData object.` ·
   `Key was not provided in signData object.` · `Invalid identifier` ([§7.1](#71-identifiers)).
2. Extract `Nonce:\s*([0-9a-f]{32})` and `Timestamp:\s*(\d+)` from
   `expectedSignedMessage`; either missing: V `Invalid expectedSignedMessage
   format`.
3. **Consume** the challenge atomically: `DELETE FROM auth_challenges WHERE
   identifier = $1 AND nonce = $2 RETURNING *`. No row: A `Challenge not found`.
   The challenge is one-shot whatever happens next **Δ16** (Strapi deleted it only
   on success, so a failed attempt could be retried against it).
4. `expiresAt < now`: V `Challenge expired`. `expectedSignedMessage !==
   message`: V `expectedSignedMessage does not match original challenge
   message`.
5. Verify ([§7.3](#73-signature-verification)).
6. **Stake login**: upsert the user by `username = identifier` (created with
   `govtool_username = null`, so first login opens pdf-ui's username modal,
   which test 6I requires). Blocked: A `Your account has been blocked by an
   administrator`. Claims `{id, stakeKey: username}`.
   **DRep login**: requires a valid access token in `Authorization` (U
   otherwise); the user is that token's user. Claims `{id, stakeKey:
   user.username, dRepID: identifier}`. Nothing proves the DRep key and the
   stake key belong to one wallet; that gap is inherited (the wallet holds both
   keys, and pdf-ui cannot supply more).
7. Issue the access JWT and the refresh cookie ([§7.5](#75-tokens)).
8. Response 200, raw: `{"status": "Authenticated", "jwt": "<access>", "user":
   <self projection>}`. **Δ17**: no `refreshToken` in the body (Strapi returned
   it, defeating the httpOnly cookie; pdf-ui never reads it).

### 7.5 Tokens

- **Access JWT**: HS256 with `JWT_SECRET`, expiry `JWT_SECRET_EXPIRES` (`1h`).
  Payload `{id: <int>, stakeKey: <hex reward address>, dRepID?: <56 hex>, iat,
  exp}`. `dRepID` is present only after a DRep login. pdf-ui decodes
  `stakeKey`, `dRepID` and `exp` without verifying. Every authenticated request
  reloads the user by `id`; missing, blocked, or `stakeKey !== username` gives U.
- **Refresh token**: a JWT, HS256 with `REFRESH_SECRET` (must differ from
  `JWT_SECRET`), expiry `REFRESH_TOKEN_EXPIRES` (`7d`), payload `{id, stakeKey,
  dRepID?, typ: "refresh"}`. It is itself tamper-proof, so the cookie is not
  additionally signed. An access token is never accepted as a refresh token, or
  the reverse.
- **Cookie** `refreshToken`: `HttpOnly`, `Path=/`, `Max-Age` = refresh expiry,
  `SameSite` = `REFRESH_COOKIE_SAMESITE` (`lax`), `Secure` =
  `REFRESH_COOKIE_SECURE` (`false`), forced on when SameSite is `none`.
- **Host-mismatch caveat**: `SameSite=Lax` cookies are not sent on a
  cross-site XHR. `localhost:8080` → `127.0.0.1:1337` is cross-site, so
  refresh fails there and the user drops at `exp`. Serve the frontend and the
  backend on the same host name (both `localhost` or both `127.0.0.1`), or in a
  cross-site deployment set `REFRESH_COOKIE_SAMESITE=none` over HTTPS with an
  explicit `CORS_ORIGINS`. Login itself never needs the cookie.

### 7.6 Refresh — `POST /api/token/refresh` · public

- Reads only the `refreshToken` cookie (pdf-ui sends body `{}`, no Bearer).
- No cookie: clear it, B `No Authorization`. Invalid, expired, wrong `typ`:
  clear it, B `Invalid token.` (fixed text **Δ18**; Strapi echoed
  `err.toString()`). User missing or blocked: clear it, B `Invalid token.`.
- Issue a new access JWT and a new refresh cookie with the same claims
  (sliding). No revocation store: logout is client-side only (inherited).
- Response 200, raw: `{"jwt": "<access>"}`.

### 7.7 Users

**`GET /api/users/me`** · authenticated. 200, raw self projection.

**`PUT /api/users/edit`** · authenticated. Body raw `{govtoolUsername}` (not
`data`, not `govtool_username`).
- No body or no `govtoolUsername`: B `Missing parameters for user update.`
- Must match `^(?![._])[a-z0-9._]{1,30}$` (Strapi's schema rule; pdf-ui's
  client rule is stricter): else B `Failed to update user: govtool_username
  must match the following: "^(?![._])[a-z0-9._]{1,30}$"`.
- Unique across users (unique index): else B `Failed to update user: This
  attribute must be unique`.
- Updates the caller only; may change an existing name. 200, raw self
  projection. pdf-ui reads `govtool_username` and shows any error as
  "unavailable".

---

## 8. Endpoints

Notation: **writable** / **forced** fields ([§3.5](#35-request-bodies)); errors
as `<status> <Abbr> "<message>"` ([§3.6](#36-errors)); allowlists as in
[§4.2](#42-allowlist-model). Every counter change runs in the same transaction
as the row change that causes it, as `SET x = x + 1` / `SET x = GREATEST(x - 1,
0)`, never read-modify-write **Δ19** (Strapi lost increments under
concurrency). Every multi-row write is one transaction.

### 8.1 Governance action types

**`GET /api/governance-action-types`** · public · list of all rows, default
sort `id asc`. Query subset over its scalars; no populate.
**Δ20**: no protocol-version filtering; Strapi's filter matched names that no
longer exist, so it returned every row too. pdf-ui reads `data[i].id` and
`attributes.gov_action_type_name` and does not paginate (5 rows < 25).

### 8.2 Proposals

The list and single routes read **proposal-content** rows and wrap each in its
proposal.

**Item shape** (list and single):

```
{ id: <proposal id>,
  attributes: { user_id, prop_likes, prop_dislikes, prop_comments_number,
    createdAt, updatedAt,
    user_govtool_username,            // owner's govtool_username ?? "Anonymous"
    content: { id: <content id>,      // no data wrapper
      attributes: { proposal_id, prop_rev_active, prop_abstract, prop_motivation,
        prop_rationale, gov_action_type_id, prop_name, is_draft, user_id,
        prop_submitted, prop_submission_tx_hash, prop_submission_date, is_locked,
        createdAt, updatedAt,
        proposal_links: [{id, prop_link, prop_link_text}],
        proposal_withdrawals: [{id, prop_receiving_address, prop_amount}],
        proposal_constitution_content: {data: {id, attributes: {prop_constitution_url,
          prop_have_guardrails_script, prop_guardrails_script_url,
          prop_guardrails_script_hash, createdAt, updatedAt}} | null},
        proposal_hard_fork_content: {data: {id, attributes: {previous_ga_hash,
          previous_ga_id, major, minor, createdAt, updatedAt}} | null},
        gov_action_type: {id, attributes: {gov_action_type_name, createdAt,
          updatedAt, publishedAt}} } } } }
```

**Δ21**: the two content relations are `{data}`-wrapped in the list too.
Strapi's list emitted them raw and its single route wrapped them; pdf-ui's
readers (`ConstitutionManager`, `HardForkManager`, the detail page) all handle
the wrapped form.

**`GET /api/proposals`** · public · list.
- Allowlist (paths on proposal-content): filterable = content scalars, virtual
  `prop_id`, `proposal.prop_likes`, `proposal.prop_dislikes`,
  `proposal.prop_comments_number`; sortable = content scalars,
  `proposal.prop_likes`, `proposal.prop_dislikes`,
  `proposal.prop_comments_number`; populate: `proposal_links`,
  `proposal_withdrawals`, `proposal_constitution_content`,
  `proposal_hard_fork_content`, `proposal` are accepted and **ignored** (the
  item shape is fixed); `fields` gives V.
- Rewrites, applied to the top-level keys of `filters` and of each
  `filters.$and` element:
  - `prop_id` present: each `prop_id` condition becomes the same condition on
    `proposal_id` (every revision of that proposal). Otherwise add
    `prop_rev_active = true`.
  - `is_draft` present (any value): anonymous caller gives 400 BD `User is
    required`; otherwise drop every client `user_id` condition and add `user_id
    = caller` **Δ22** (Strapi kept a client `user_id`, exposing other users'
    drafts). The client's `is_draft` condition stays. Otherwise add `is_draft =
    false`.
- Pagination and `total` count content rows.

**`GET /api/proposals/:id`** · public · single.
- `:id` is decimal digits (the proposal id), or 64 hex chars (a
  `prop_submission_tx_hash`, resolved to its proposal). Anything else, or no
  match: 400 BD `Proposal not found` (one message for both paths **Δ23**; pdf-ui
  redirects on it).
- `content` = the proposal's content with `prop_rev_active` and not
  `is_draft`, newest first. If none exists but an active draft does: 400 BD
  `You can not access draft proposal details.` If neither: `content: null`.

**`POST /api/proposals`** · authenticated.
- Writable: `gov_action_type_id` (must be a seeded type, else V
  `gov_action_type_id is invalid`), `prop_name` (required, ≤80), `prop_abstract`,
  `prop_motivation`, `prop_rationale`, `is_draft` (default false),
  `proposal_links` (≤25; `prop_link` ≤2048, `prop_link_text` ≤255; incoming
  component `id`s ignored; entries with an empty `prop_link` are dropped; a
  `prop_link` must parse as a URL with scheme `http:`, `https:` or `ipfs:`,
  drafts included, else 400 V `prop_link is invalid` **Δ47**),
  `proposal_withdrawals` (≤25),
  `proposal_constitution_content` (type 3 only), `proposal_hard_fork_content`
  (type 6 only). A relation object given as `{data: {attributes: {...}}}` (pdf-ui's
  draft-restore echo) is unwrapped first.
- Forced: `user_id` = caller on proposal and content; counters 0;
  `prop_rev_active` true; `prop_submitted` false; `prop_submission_tx_hash`,
  `prop_submission_date` null; `is_locked` false; `proposal_id` = the new id.
- Validation when not a draft:
  - Type 2: withdrawals missing or empty: 400 BD `Withdrawal parametars not
    exist` (sic; Strapi 500'd on a missing field). Each item needs
    `StakeAddress.fromBech32(prop_receiving_address)` to succeed (and, with
    `CARDANO_NETWORK_ID` set, `networkId` to match) and `Number(prop_amount) >
    0`: else 400 BD `Withdrawal addrress or amount parametars not valid` (sic).
  - Type 3: object missing: 400 BD `proposal_constitution_content is required
    for Constitution action`. `prop_constitution_url` must parse as a URL with
    scheme `http:`, `https:` or `ipfs:` **Δ24** (Strapi accepted any scheme,
    including `javascript:`): else 400 BD `prop_constitution_url is required and
    must be a valid URL (IPFS is allowed)`. `prop_have_guardrails_script ===
    true`: `prop_guardrails_script_url` must satisfy the same URL rule (400 BD
    `prop_guardrails_script_url is required and must be a valid URL when
    prop_have_guardrails_script is true`) and `prop_guardrails_script_hash` is
    required (400 BD `prop_guardrails_script_hash is required when
    prop_have_guardrails_script is true`). `false` or `null`: url and hash must
    be empty or absent (pdf-ui sends `''`), else 400 BD
    `prop_guardrails_script_url and prop_guardrails_script_hash must not be
    provided when prop_have_guardrails_script is false or null`. Any other value
    is stored as false.
  - Drafts skip these checks but keep the type and length rules; withdrawal
    amounts that do not parse are stored as null.
- Effects, one transaction: proposal; hard-fork row when type 6 and the object
  has any non-empty field (`previous_ga_id` stringified); content; constitution
  row when type 3 and the object is present; links; withdrawals. Any failure
  rolls back everything **Δ25** (Strapi deleted by hand and could orphan rows).
- Response 200: `{"data": {"attributes": {"proposal_id": <int>,
  "proposal_content_id": <int>}}, "meta": {}}`. No `data.id`; pdf-ui reads
  `data.attributes.proposal_id`.

**`DELETE /api/proposals/:id`** · owner (`proposal.user_id`); F `You can't
access this entry`.
- One transaction deletes the proposal and everything under it: contents,
  links, withdrawals, constitution and hard-fork rows, votes, polls and poll
  votes, comments and their reports. Allowed after submission (Playwright
  cleans up submitted proposals).
- Response 200: `{"data": {"id", "attributes": {proposal scalars}}, "meta":
  {}}` (pdf-ui needs a truthy body).

### 8.3 Proposal contents

**`POST /api/proposal-contents`** · authenticated; the proposal named by
`data.proposal_id` must exist (400 BD `Proposal not found`) and be the caller's
(F `You can't access this entry`) **Δ26** (Strapi let anyone add a revision to
any proposal).
- Missing `proposal_id`: 400 BD `Proposal ID is required`. If the proposal's
  active non-draft content has `prop_submitted`: 400 BD `Proposal can't be
  updated, it has been already submited` (sic) **Δ27**.
- Writable, forced and validation: as `POST /proposals`, the link rule (Δ47)
  included (`publish`,
  `prop_rev_active`, `prop_receiving_address`, `prop_amount` at top level are
  ignored). A missing `proposal_hard_fork_content` is fine (Strapi 500'd);
  the hard-fork row is created only when `previous_ga_id` is non-empty.
- Effects, one transaction: create the content with `prop_rev_active = true`
  and its components/relations. If not a draft, set `prop_rev_active = false`
  on every other content of the proposal. A draft revision leaves the live
  content active (so a proposal can then have one active non-draft and one
  active draft content; the single route prefers the non-draft).
- Response 200: single envelope of the content, scalars only (no relations,
  no components), as Strapi did.

**`PUT /api/proposal-contents/:id`** · owner (`content.user_id`); F `You can't
access this entry`. Submission bookkeeping only **Δ28** (Strapi wrote any field
the owner sent, including `user_id` and counters).
- Writable: `prop_submitted` (must be `true`), `prop_submission_date` (ISO date
  or datetime; stored as the date), `prop_submission_tx_hash` (64 hex, required
  when `prop_submitted`). Other keys ignored.
- The content must be active, non-draft and not yet submitted: else 400 BD
  `Proposal can't be updated, it has been already submited`. Duplicate tx hash:
  V `This attribute must be unique`. The tx is not checked on chain.
- Response 200: single envelope of the content, scalars only. pdf-ui ignores it.

### 8.4 Proposal votes (likes)

**`GET /api/proposal-votes`** · authenticated. Filterable: `proposal_id`.
`user_id` is always forced to the caller; a client `user_id` condition is
dropped **Δ29**. Sort default `createdAt desc`; only the first match is used.
- Response 200: `{"data": {id, attributes: {proposal_id, user_id, vote_result,
  createdAt, updatedAt}}, "meta": {}}` — a **single object from a list route** —
  or `{"data": null, "meta": {}}`.

**`POST /api/proposal-votes`** · authenticated. Writable `proposal_id`,
`vote_result`; forced `user_id`.
- `vote_result` not boolean: 400 BD `Vote result is required`. No `proposal_id`:
  400 BD `Proposal ID is required`. Proposal missing: 400 BD `Proposal not found`.
  Existing vote: 400 BD `Proposal vote for this user already exist`.
- Effect: insert; `prop_likes` or `prop_dislikes` +1.
- Response 200: single envelope of the vote.

**`PUT /api/proposal-votes/:id`** · owner (`vote.user_id`); F `You can't access
this entry`. Writable `vote_result`.
- Not boolean: 400 BD `Vote result is required`. Same as stored: 400 BD
  `Proposal vote already updated`.
- Effect: update; +1 on the new side, −1 on the old side.
- Response 200: single envelope of the vote.

### 8.5 Polls

**`GET /api/polls`** · public · list. Filterable: poll scalars. Sortable: poll
scalars. No populate.

**`POST /api/polls`** · authenticated. Body `{data: {proposal_id, ...}}`
(pdf-ui also sends `poll_start_dt`, `is_poll_active`; both ignored).
- Proposal missing: 400 BD `Proposal not found` (Strapi 500'd). Not the proposal
  owner: F `User is not owner of this proposal`. An active poll exists: 400 BD
  `There is already an active pool for this proposal` (sic).
- Forced: `is_poll_active` true, `poll_yes`/`poll_no` 0, `poll_start_dt` now
  **Δ30** (client-set in Strapi). Earlier polls stay as they are (already
  closed).
- Response 200: single envelope of the poll.

**`PUT /api/polls/:id`** · authenticated; poll missing: 400 BD `Poll not
found`; not the proposal's owner: F `User is not authorized to update this
Poll.`
- Writable `is_poll_active`, which must be `false` (closing); anything else:
  V `Only closing a poll is supported` **Δ31** (reopening could create two
  active polls).
- Response 200: single envelope of the poll.

### 8.6 Poll votes

**`GET /api/poll-votes`** · authenticated · list. Filterable: `poll_id`,
`vote_result`. `user_id` forced to the caller. Sortable: scalars.

**`POST /api/poll-votes`** · authenticated. Writable `poll_id`, `vote_result`;
forced `user_id`.
- 400 BD `Vote result is required` · 400 BD `Poll ID is required` · 400 BD `Poll
  not found` · 400 BD `Poll is not active` **Δ32** · 400 BD `Poll vote for this
  user already exist`.
- Effect: insert; `poll_yes` or `poll_no` +1. Response: single envelope.

**`PUT /api/poll-votes/:id`** · owner; F `You can't access this entry`.
Writable `vote_result`. Errors: `Vote result is required`, `Poll vote already
updated`, `Poll is not active` (all 400 BD). Effect: flip the two counters.
Response: single envelope.

### 8.7 Comments

**Comment attributes**: `proposal_id`, `comment_parent_id`,
`user_id`, `comment_text`, `drep_id`, `createdAt`, `updatedAt`, plus computed
`user_govtool_username` (author's, `?? "Anonymous"`), `user_is_validated`
(author's `is_validated`), `subcommens_number` (count of direct replies).
Populated `comments_reports`: `{data: [{id, attributes: {moderation_status,
createdAt, updatedAt, publishedAt, reporter?: <public user>}}]}`; `hash` never
appears.

**`GET /api/comments`** · public · list.
- Filterable: comment scalars and `comments_reports.hash` (`$eq` only;
  knowing the hash is the capability for the review page),
  `comments_reports.moderation_status`. Sortable: comment scalars. Populate:
  `comments_reports`, `comments_reports.reporter` (max depth 2, `fields`
  allowed); `comments_reports.maintainer` is accepted and ignored (pdf-ui sends
  it; no such relation exists).
- No `filters` is fine **Δ33** (Strapi 500'd). Reported comments are not hidden
  server-side (Strapi's attempt was a no-op); pdf-ui restricts display itself.

**`POST /api/comments`** · authenticated.
- Writable: `comment_text` (1..15000, else 400 BD `Comment text is required`),
  `proposal_id` (missing: 400 BD `Proposal ID is required`; a
  `bd_proposal_id` is ignored like any other unknown key, D167),
  `comment_parent_id` (optional).
- Forced: `user_id` = caller; `drep_id` = the token's `dRepID` or null (the
  client's `drep_id` is ignored; pdf-ui sends `''` or the id).
- Target: `proposal_id` must be an existing proposal, else 400 BD `Proposal
  not found`. `comment_parent_id`, when given, must be a comment on the same
  proposal: else
  400 BD `Parent comment not found` **Δ34** (Strapi stored any string).
- Effect: insert; `prop_comments_number` +1 on the proposal (replies count
  too).
- Response 200: single envelope of the comment (with `comment_parent_id`, which
  pdf-ui reads). Computed attributes are omitted here.

### 8.8 Budget discussions

Removed (D167): 404, as any unknown route.

### 8.9 BD lookups

Removed (D167).

### 8.10 BD drafts

Removed (D167).

### 8.11 BD polls and votes

Removed (D167).

### 8.12 Comment reports

**`POST /api/comments-reports`** · authenticated. Writable `comment` (id);
`reporter` and `moderator` in the body are ignored.
- No `comment`: 400 B `Comment is mandatory.` Unknown comment: 400 B `Comment
  not found`. Already reported by the caller: 400 B `Comment already
  reported`.
- Forced: `reporter` = caller **Δ42** (client-supplied in Strapi), `moderator`
  null, `moderation_status` null, `hash` = 89 characters from `[A-Za-z0-9]` via
  `crypto.randomInt` (Strapi used `Math.random`), `publishedAt` now. No email.
- Response 200: single envelope `{moderation_status, createdAt, updatedAt,
  publishedAt}` (no hash).

**`DELETE /api/comments-reports/:id`** · owner (the reporter) **Δ43** (any
user could delete any report); F `You can't access this entry`. Response:
single envelope.

There is no update route (moderation is out of scope) and no list route;
pdf-ui's id-less `GET`/`PUT /api/comments-reports/` get 404.

---

## 9. Proxy endpoints

### 9.1 `GET /api/proxy/govtool/<path>` · public

- Forwards `GET ${GOVTOOL_API_BASE_URL}/<path>?<query>` when `<path>` is in
  `GOVTOOL_PROXY_ALLOWED_PATHS` (default `proposal/enacted-details`, the only
  path pdf-ui uses, with `?type=HardForkInitiation`). Matching is exact after
  percent-decoding; a path containing `..`, `\`, `//`, or a `%2f`/`%2e`
  sequence is rejected. Not allowed: 404 N `Not Found` **Δ44** (Strapi forwarded
  any path, including `..`).
- The query string is re-serialized from the parsed pairs; no client headers
  are forwarded; `Accept: application/json`, `User-Agent: govtool-pdf-proxy`.
  Timeout `PROXY_TIMEOUT_MS`, size limit `PROXY_MAX_BYTES`, no redirects.
- Response 200: `{"status": <upstream status>, "data": <upstream JSON>}`.
  Upstream non-2xx: that status, `{"error": "<Request failed with status
  code N>", "details": <upstream body or null>}`. Network error, timeout or
  oversize: 502 `{"error": "Upstream request failed", "details": null}`.
  `GOVTOOL_API_BASE_URL` unset: 503 `{"error": "GOVTOOL_API_BASE_URL is not
  configured", "details": null}`. (These bodies are Strapi's proxy shape, not
  the §3.6 envelope.)
- `POST /api/proxy/govtool/*` is not provided.

### 9.2 `POST /api/proxy` · authenticated **Δ45**

A safe fetcher for pdf-ui's constitution-hash step, which only ever sends
`{url, method: 'GET'}`. Strapi's was an open relay (any URL, method, headers,
body; auth:false).

- Body raw `{url, method?}`; `data`, `params` and `headers` are ignored.
  `method` other than absent/`GET`: 400 `{"error": "Only GET is supported",
  "details": null}`.
- `ipfs://<cid>[/path]` is rewritten to `${IPFS_GATEWAY_URL}/<cid>[/path]`
  (default `https://ipfs.io/ipfs`). Then the URL must parse, have scheme
  `http:` or `https:`, and carry no userinfo: else 400 `{"error": "Invalid
  URL", "details": null}`.
- Unless `PDF_ALLOW_PRIVATE_URLS=true` (tests only): resolve the host (all A
  and AAAA records) and reject if any address is loopback, private
  (10/8, 172.16/12, 192.168/16), link-local (169.254/16, fe80::/10), CGNAT
  (100.64/10), unspecified, multicast, broadcast, reserved (0/8, 240/4,
  192.0.0/24, 198.18/15, documentation ranges), unique-local (fc00::/7),
  or an IPv4-mapped/compatible IPv6 form of any of these: 400 `{"error":
  "Destination not allowed", "details": null}`. Connect to the validated
  address (pin it; no second resolution) so DNS rebinding cannot swap it.
- Under `PDF_ALLOW_PRIVATE_URLS=true` only, `PDF_PROXY_HOST_REWRITES` maps a
  URL's `host:port` to another connect target (a container reaching the
  host's `127.0.0.1:3001` through `host.docker.internal:3001`). The URL, its
  Host header and redirect resolution are unchanged; each hop is matched
  again.
- Up to 3 redirects, each re-validated as above. Timeout `PROXY_TIMEOUT_MS`
  (10000). Body limit `PROXY_MAX_BYTES` (5 MiB), enforced while streaming.
  Request headers: `User-Agent: govtool-pdf-proxy`, `Accept: */*` only.
- Response 200: `{"status": <upstream status>, "data": <parsed JSON if the
  content type is JSON, else the body as a UTF-8 string>}`. pdf-ui requires
  `status === 200` and hashes a string as is or `JSON.stringify`s an object.
  Upstream non-2xx: that status with `{"error", "details"}` as in §9.1; network
  error, timeout, oversize: 502 as in §9.1.

---

## 10. Configuration

Read once at startup and validated; an invalid value stops the process with a
message naming the variable. `.env.example` mirrors this list.

`DATABASE_URL`, `JWT_SECRET` and `REFRESH_SECRET` may instead come from a
file: when the variable is unset or blank, it is read from `<NAME>_FILE`,
defaulting to the Swarm secret mount `/run/secrets/<lowercase name>`. A
missing or whitespace-only file counts as unset. The image's command,
`dist/start.js`, loads them before running the migration.

| Variable | Default | Meaning |
|---|---|---|
| `DATABASE_URL` | required | Postgres URL. Local: `postgresql://pdf:pdf@127.0.0.1:5442/pdf?schema=public`. |
| `HOST` | `0.0.0.0` | Listen address. |
| `PORT` | `1337` | Listen port (pdf-ui's historical port). |
| `JWT_SECRET` | required, ≥32 chars | Access-token HMAC key. |
| `JWT_SECRET_EXPIRES` | `1h` | Access-token lifetime (`ms`-style string). |
| `REFRESH_SECRET` | required, ≥32 chars, ≠ `JWT_SECRET` | Refresh-token HMAC key. |
| `REFRESH_TOKEN_EXPIRES` | `7d` | Refresh-token and cookie lifetime. |
| `REFRESH_COOKIE_SAMESITE` | `lax` | `lax`, `strict` or `none` ([§7.5](#75-tokens)). |
| `REFRESH_COOKIE_SECURE` | `false` | Forced true with SameSite `none`. |
| `CHALLENGE_TTL_SECONDS` | `300` | Login challenge lifetime. |
| `CORS_ORIGINS` | `*` | `*` reflects any origin; else a comma list of exact origins. |
| `CARDANO_NETWORK_ID` | unset | `0` or `1`. When set, reward-address identifiers and treasury withdrawal addresses must be on this network. |
| `GOVTOOL_API_BASE_URL` | unset | GovTool backend base URL for §9.1 (e.g. `http://127.0.0.1:9999`). |
| `GOVTOOL_PROXY_ALLOWED_PATHS` | `proposal/enacted-details` | Comma list of forwardable paths. |
| `PROXY_TIMEOUT_MS` | `10000` | Upstream timeout for both proxies. |
| `PROXY_MAX_BYTES` | `5242880` | Upstream body limit for both proxies. |
| `IPFS_GATEWAY_URL` | `https://ipfs.io/ipfs` | Gateway for `ipfs://` in §9.2. |
| `PDF_ALLOW_PRIVATE_URLS` | `false` | Test switch: lets §9.2 reach private and loopback addresses. Never set in a deployment. |
| `PDF_PROXY_HOST_REWRITES` | unset | Test switch: comma list of `host:port=host:port`, where §9.2 connects instead (URL and Host kept). Setting it without `PDF_ALLOW_PRIVATE_URLS=true` stops startup. |
| `BODY_LIMIT` | `1mb` | JSON body limit. |

`docker-compose.yml` sets development values for `JWT_SECRET` and
`REFRESH_SECRET` and says so in its header.

---

## 11. Testing requirements

`npm run verify` (lint, typecheck, unit, build) and `npm run test:e2e` must
pass. No test depends on network access except the proxy tests' local stub
servers.

### 11.1 Unit (jest, `src/**/*.spec.ts`)

- **Query parser**: every sort, populate, fields and pagination form of
  [§4](#4-query-subset); implicit `$eq`; `$and` as array and as index-keyed
  object; relation-with-scalar; each operator's Prisma output; coercion
  failures; the limits; unknown top-level key, path, operator → the exact V
  message; `+` → space; `%`/`_` literal in `$containsi`.
- **Allowlist**: each resource rejects a private user path
  (`filters[comments_reports][reporter][username]`,
  `populate[comments_reports][populate][moderator]`) and accepts every path in [Appendix
  A](#appendix-a--pdf-ui-query-corpus).
- **Serializer**: nulls kept; legacy string references; relation wrapping;
  components inline; the `/proposals` `content`/`gov_action_type` exceptions;
  `fields` projection; user projections.
- **Errors**: each helper's exact body.
- **CIP-8 verifier**, with keys from `Ed25519Key.generate()` and signatures from
  `cip8Sign(addressBytes, key, payload)`: valid; wrong key (hash mismatch);
  payload ≠ challenge; `hashed: true` payload; tampered signature; `alg` ≠ -8;
  non-hex, truncated and non-CBOR inputs fail without throwing.
- **Identifiers**: e0/e1 accepted, f0/f1 and wrong lengths rejected, the network
  check.
- **Proxy guards**: every blocked range for v4 and v6, IPv4-mapped v6, userinfo,
  non-http schemes, `ipfs://` rewrite, redirect re-validation; the govtool path
  matcher against `..`, `%2e%2e`, `//`, `\`.
- **Username rule**: the regex, including leading `.`/`_`, 31 chars, uppercase,
  `-`, `#`, space.

### 11.2 E2E (jest + supertest, `test/**/*.e2e-spec.ts`)

- Against a real Postgres: the compose `db` on 127.0.0.1:5442, database
  `pdf_test` (`DATABASE_URL_TEST`, default
  `postgresql://pdf:pdf@127.0.0.1:5442/pdf_test?schema=public`). The suite
  creates the database if missing, runs `prisma migrate deploy`, and truncates
  every non-lookup table between files. It **refuses to run** unless the
  database name ends in `_test`, so it can never wipe the development database.
- The app boots through the real `AppModule` with test secrets,
  `PDF_ALLOW_PRIVATE_URLS=true` only in the proxy file, and a short
  `CHALLENGE_TTL_SECONDS` in the expiry test.
- **Login helper** (real CIP-8): generate a stake key with `Ed25519Key.generate()`;
  identifier = `e0` + `key.pkh` hex; `GET /api/auth/challenge`; `cip8Sign(<29-byte
  reward address>, key, Buffer.from(message, 'utf8'))`; `POST /api/auth/local`
  with `toCip8Json()`. DRep helper: a second key, identifier = its `pkh` hex,
  with the stake JWT as Bearer. Also sign once with the base address in the
  protected header (the Playwright wallet's behaviour) and expect success.

Required cases:

1. **Anonymous matrix**: every route in [`pdf-api.md`](../../docs/api/pdf-api.md)
   called with no token. Public reads 200 with the right envelope;
   authenticated and owner routes 403 `Forbidden`; `/token/refresh` 400 `No
   Authorization`. Then with a garbage Bearer: public routes still 200,
   authenticated routes 401.
2. **Auth**: first login creates a user with `govtool_username: null` and the
   self projection (no `email`); second login returns the same id; JWT claims;
   refresh cookie attributes; `/token/refresh` with the cookie returns a JWT and
   rotates the cookie; access token as refresh cookie rejected; replaying the
   same signed body gives `Challenge not found`; a signature over a different
   payload gives `Verification failed` and consumes the challenge; expired
   challenge; mismatched `expectedSignedMessage`; DRep login without Bearer 401,
   with Bearer yields `dRepID`; blocked user 401 on `/users/me`;
   `/users/edit` valid, invalid, duplicate (exact messages).
3. **Ownership** (users A and B): B gets F on A's proposal delete,
   proposal-content create and update, poll create and close, vote and poll-vote
   updates, report delete; B's `GET /proposals` with
   `is_draft=true&user_id=<A>` returns only B's drafts; B's `GET
   /proposal-votes?filters[user_id]=<A>` returns B's own vote or null.
4. **Envelope and query compatibility**: every string in [Appendix
   A](#appendix-a--pdf-ui-query-corpus), sent **unencoded** exactly as pdf-ui
   builds it, returns 200 with the fields pdf-ui reads; each list sort option is
   checked for order (the Playwright 8B_2 check); search with
   `$containsi` is case-insensitive; `pageCount`/`total`; `pageSize=1000` and
   `pageSize=5000` (clamped).
5. **Shapes**: `/proposals` item shape including wrapped relations and
   unwrapped `content`/`gov_action_type`; `GET /proposals/:id` by id and by tx
   hash; draft-only proposal gives the BD draft error; `POST /proposals` body
   `{data: {attributes: {proposal_id, proposal_content_id}}}` with no `id`;
   `GET /proposal-votes` single-or-null; no `hash` or `email` anywhere; every
   budget discussion route of D167 answers 404.
6. **Counters**: like, dislike, flip (−1/+1, never negative); poll yes/no and
   flips; comment and reply increments on proposals; 20 concurrent comments
   yield exactly 20; 10 concurrent likes by 10 users yield 10.
7. **Side effects**: proposal create is atomic (a failing link rolls back the
   proposal); non-draft revision deactivates the others, draft revision does
   not; poll create rejects a second active poll and forces its fields; closing
   works, reopening is rejected; proposal delete removes everything including
   hard-fork rows.
8. **Proxies**: govtool proxy against a local stub (allowlisted path, query
   passthrough, `{status, data}`, non-allowlisted 404, `..` rejected, upstream
   500 and timeout); `POST /api/proxy` against a local stub with the switch on
   (JSON, text, redirect, oversize, timeout, non-GET) and with it off
   (`127.0.0.1` and `localhost` rejected).
9. **CORS**: preflight from an arbitrary origin reflects it with credentials
   under `*`; with a list, a foreign origin gets no CORS headers.

### 11.3 Playwright

The GovTool Playwright suite runs against a frontend built with
`VITE_PDF_API_URL` pointing at this backend. It must pass the PDF projects
(`proposal discussion`, `proposal discussion (loggedin)`, `proposal
submission`) with the proposal-discussion
exclusions the suite already uses. Run the frontend and backend on the same
host name ([§7.5](#75-tokens)).

### 11.4 Fixtures

Tests create their data through the API (plus Prisma for
`moderation_status`). Lookup rows come from the seed migration and are never
truncated.

### 11.5 Demo data

`npm run seed:demo` (never run automatically) inserts, idempotently (a marker
row), the data the Playwright specs expect to find already there: two users
with `govtool_username`s, one live proposal of each seeded type with comments,
and one proposal submitted as a GA (with a tx hash).

---

## 12. Known pdf-ui bugs the backend does not fix

For the frontend work (D138: pdf-ui moves into `govtool/frontend`).

1. Query strings are never URL-encoded: search text with `&`, `#` or `+`
   truncates or changes the query.
2. `getProposals` and `getUserProposalVote` return the axios error instead of
   throwing; a failed vote lookup is treated as an existing vote.
3. `SingleGovernanceAction`: the `!polls?.length === 0` guard never fires; the
   inactive-polls query uses `pageSize=1`.
4. `buildHardForkInitiationGovernanceActions` is called but GovTool provides
   `buildHardForkGovernanceAction`: submitting a hard-fork GA throws.
5. `InformationStorageStep` reads `proposal_hard_fork_content.previous_ga_hash`
   etc. without `.data.attributes`, so hard-fork GA fields are `undefined`.
6. Editing a published proposal and choosing "Save Draft" creates a draft
   revision; opening that draft from the drafts list and submitting it creates a
   **new** proposal and then `deleteProposal(selectedDraftId)` deletes the
   original, with its comments, votes and polls.
7. `createProposalContent(data, publish)`: `publish` is never passed.
8. Comment reporting is broken end to end: `handleProceedReport` references an
   undefined `curComment` (and is undefined in `Subcomponent`), the report
   trigger is commented out, `getCommentReportByHash`/`approveCommentReport`/
   `removeComment` call `api/comments-reports/` with no id, the queries
   populate `maintainer` while the body sends `moderator`, and the review page's
   buttons do nothing. With Δ2, the reporter-username check in
   `isCommentRestricted` never matches.
9. `CommentCard` reply: on a falsy response it calls `setRefetchProposal`,
   which the detail pages do not pass.
10. Gone with D167: the frontend's budget discussion pages are a read-only
    archive and `/budget_discussion/propose` redirects to it.
11. `utf8ToHex` encodes per UTF-16 code unit; it only works because the
    challenge is ASCII.
12. The JWT is decoded without verification and 401 is handled nowhere; the
    refresh interval runs only when `govtool_username` is set, so a user who
    never sets one keeps sending an expired token (401 from authenticated
    routes, anonymous on public ones after Δ5). The interval is duplicated in
    `GlobalWrapper` and `UserValidation`.
13. Gone with D167 (the budget discussion edit dialog).
14. Gone with D167 (the budget discussion detail page).
15. The client username rule (`^(?=.*[a-z])[a-z0-9._]{1,30}$`, no leading
    `.`/`_`) differs from the server's; purely numeric names pass the server.
16. `rehype-raw` renders raw HTML from user markdown; sanitisation needs review.
17. pdf-ui couples to GovTool's DOM via
    `document.querySelector('[data-testId="connect-wallet-button"]')`.

Playwright-side (not pdf-ui): every HD-wallet run is a first login, so specs
that do not handle the username modal after `verify-user-link` may stall; 8R's
user-side assertion runs on a never-navigated page and 8P/8R do not await
`goto`.

---

## 13. Deviation index

| Δ | Where | Change | Why |
|---|---|---|---|
| 1 | §3.4 | Self projection has no email or secrets | Privacy |
| 2 | §3.4 | Public user projection is `{id, govtool_username}` | Stake address ↔ name linkage |
| 3 | §3.5 | Writable/forced field lists | Mass assignment |
| 4 | §3.6 | One error body; no internal messages | Consistency, info leak |
| 5 | §3.7 | Invalid token on a public route is anonymous | Stale sessions broke list pages |
| 6 | §3.7 | Ownership 403 / missing 404 | Correct status; is-owner let missing rows through |
| 7 | §4 | Unknown query keys, paths, populates → 400 | Private-field filtering, query abuse |
| 8 | §5.2 | Hard-fork rows deleted with their proposal | Orphans |
| 9 | §5.2 | One like/dislike per user | Counter inflation |
| 10 | §5.3 | Report `hash` never serialized | It is the review-link secret |
| 11 | §5.4 | Removed with budget discussions (D167) | — |
| 12 | §5.4 | Removed with budget discussions (D167) | — |
| 13 | §7.1 | Identifier form picks the login flow | Stale Bearer turned stake logins into DRep logins |
| 14 | §7.2 | Expired challenges purged; live ones never evicted | Unbounded table; eviction let anyone cancel a victim's login |
| 15 | §7.3 | Payload must be the challenge | Signature replay |
| 16 | §7.4 | Challenge consumed on first attempt | Retry against a live challenge |
| 17 | §7.4 | No refresh token in the login body | httpOnly defeated |
| 18 | §7.6 | Fixed refresh error text | Error echo |
| 19 | §8 | Atomic counters in the write transaction | Lost updates |
| 20 | §8.1 | No protocol-version filter on action types | Strapi's filter was dead |
| 21 | §8.2 | Content relations wrapped in lists too | One shape |
| 22 | §8.2 | Draft listing forced to the caller | Draft exposure |
| 23 | §8.2 | One not-found message for id and tx hash | pdf-ui redirect |
| 24 | §8.2 | Constitution URLs http/https/ipfs only | `javascript:` links |
| 25 | §8.2 | Proposal create is one transaction | Orphans |
| 26 | §8.3 | Revisions only by the proposal owner | Anyone could revise any proposal |
| 27 | §8.3 | No revisions after submission | Correctness |
| 28 | §8.3 | Content update limited to submission fields | Mass assignment |
| 29 | §8.4 | Vote lookup always the caller's | Filter override |
| 30 | §8.5 | Poll fields server-set | Client could open pre-counted polls |
| 31 | §8.5 | Polls can only be closed | Two active polls |
| 32 | §8.6 | Votes only on active polls | Votes on closed polls |
| 33 | §8.7 | Comments list without filters works | Strapi 500 |
| 34 | §8.7 | Parent comment must exist on the same target | Dangling replies |
| 35 | §8.8 | Removed with budget discussions (D167) | — |
| 36 | §8.8 | Removed with budget discussions (D167) | — |
| 37 | §8.8 | Removed with budget discussions (D167) | — |
| 38 | §8.8 | Removed with budget discussions (D167) | — |
| 39 | §8.8 | Removed with budget discussions (D167) | — |
| 40 | §8.8 | Removed with budget discussions (D167) | — |
| 41 | §8.10 | Removed with budget discussions (D167) | — |
| 42 | §8.12 | Reporter forced | Spoofed reporter |
| 43 | §8.12 | Only the reporter deletes a report | Anyone could delete |
| 44 | §9.1 | GovTool proxy path allowlist | Path traversal, open forwarding |
| 45 | §9.2 | Safe fetcher, authenticated | Open SSRF relay |
| 46 | §8.8 | Removed with budget discussions (D167) | — |
| 47 | §8.2, §8.3 | Proposal links http/https/ipfs only, blank ones dropped | `javascript:` links |

---

## Appendix A — pdf-ui query corpus

The exact strings pdf-ui sends (1.0.18-beta), with `${…}` the interpolated
values. §11.2 case 4 sends each one unencoded.

**Proposals** (`GET /api/proposals?`):
- `filters[$and][0][gov_action_type_id]=${typeId}&filters[$and][1][prop_name][$containsi]=${search}&pagination[page]=${page}&pagination[pageSize]=25&sort[${fieldId}]=${DIR}&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal`
- the same with `&filters[$and][2][prop_submitted]=${true|false}` after the `$containsi` filter
- `filters[$and][2][is_draft]=true&pagination[page]=${page}&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content`
- `filters[$and][2][is_draft]=true&filters[$and][3][prop_submitted]=${bool}&…` (unreachable in the UI; still accepted)
- `filters[$and][0][is_draft]=true&pagination[page]=1&pagination[pageSize]=1`
- `filters[$and][0][prop_id]=${id}&pagination[page]=1&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals`
- `sort[${fieldId}]=${DIR}` ∈ `sort[createdAt]=DESC|ASC`, `sort[proposal][prop_likes]=DESC|ASC`, `sort[proposal][prop_dislikes]=DESC|ASC`, `sort[proposal][prop_comments_number]=DESC|ASC`, `sort[prop_name]=ASC|DESC`.

**Proposal votes, polls, poll votes**:
- `GET /api/proposal-votes?filters[proposal_id][$eq]=${id}`
- `GET /api/polls?filters[$and][0][proposal_id][$eq]=${id}&filters[$and][1][is_poll_active]=${true|false}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`
- `GET /api/poll-votes?filters[poll_id][$eq]=${pollId}&pagination[page]=1&pagination[pageSize]=1&sort[createdAt]=desc`

**Comments** (`GET /api/comments?`):
- `filters[$and][0][proposal_id]=${id}&filters[$and][1][comment_parent_id][$null]=true&sort[createdAt]=${desc|asc}&pagination[page]=${page}&pagination[pageSize]=25&populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username`
- `filters[comment_parent_id]=${commentId}&pagination[page]=${page}&pagination[pageSize]=3&sort[createdAt]=desc&populate[comments_reports][populate][reporter][fields][0]=username&populate[comments_reports][populate][maintainer][fields][0]=username`
- `filters[comments_reports][hash][$eq]=${hash}&populate[comments_reports][populate][reporter]=*`

**Other**: `GET /api/auth/challenge?identifier=${hex}`;
`GET /api/proxy/govtool/proposal/enacted-details?type=HardForkInitiation`;
`GET /api/governance-action-types`, `GET /api/proposals/${id}` (no query).
