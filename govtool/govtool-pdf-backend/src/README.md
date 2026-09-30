# src map

SPEC.md (package root) is the decided state; this file only says where things are.

## Shared building blocks (do not edit from a resource module)

- `config/config.ts`: every §10 variable, validated at startup. Inject with `@Inject(APP_CONFIG) config: AppConfig`.
- `common/errors.ts`: the §3.6 helpers. `badRequestDetails` (BD, text in `details`), `badRequest` (B), `validationError` (V), `applicationError` (A), `unauthorized`, `forbidden`, `notFound`. Throw them; `ApiExceptionFilter` renders the body. `RawHttpError(status, body)` for the non-enveloped proxy bodies of §9. `isUniqueViolation(e)` for P2002.
- `common/body.ts`: `@DataBody()` unwraps `{data}` or throws V `Missing "data" payload…`; `@RawBody()` for raw bodies.
- `common/fields.ts`: writable-field readers with the §3.5 rules (`readString`, `readText`, `readBool`, `readIntRef`, `readObject`, `readArray`) and `parseRouteId`.
- `auth/auth.guard.ts`: global guard. Routes are **authenticated by default** (no header 403, bad token 401). `@Public()` opts out; there `@CurrentUser()` gives `AuthUser | null`. On authenticated routes `@Caller()` gives `AuthUser` (with `dRepID` from the token). `assertOwner(row, r => r.userId, caller, msg)` is §3.7 owner (missing 404, foreign 403 msg).
- `query/`: the §4 subset.
  - `resource.ts`: descriptors (wire name to Prisma field and type, relations, components, `hidden` filter-only scalars). `col.*` shorthands; `scalarPaths(resource)` is the default filterable/sortable set.
  - `types.ts`: `QueryAllowlist` (filterable, filterOps, virtualFilters, sortable, populatable, ignoredPopulate, fields).
  - `raw-query.ts`: `@RawQuery()` parses `req.originalUrl` with Strapi's qs options. Never use `@Query()`.
  - `parse.ts`: `parseQuery(raw, allowlist)` gives a `ParsedQuery` (AST filters, sort, populate tree, fields, pagination) or throws the exact V. `makeCond` builds a condition for rewrites.
  - `filter-helpers.ts`: top-level rewrites (`hasTopLevel`, `removeTopLevel`, `renameTopLevel`, `andWith`) for the /proposals and caller-forcing rules.
  - `prisma.ts`: `toPrismaWhere`, `toPrismaOrderBy` (adds `id asc`), `toPrismaInclude` (always includes components), `toPrismaPaging`.
  - `serialize.ts`: `serializeEntity(row, resource, {populate, fields, extra})`, `serializeScalars`, `single`, `list`, `paginationMeta`.
  - `list.ts`: `findList` / `listEnvelope(delegate(prisma.model), resource, q, {where, defaultSort, extra})` for a whole list route.
- Descriptors and allowlists already written per module: `users/user.resource.ts` (public projection), `lookups/`, `proposals/proposal.{resources,allowlists}.ts`, `polls/`, `comments/`, `budget/`. The owning module may change its own.
- `seed/`: `lookups.data.ts` (§6 rows), `seed-lookups.ts` (run on container start and by `prisma db seed`), `seed-demo.ts` (§11.5, `npm run seed:demo`; extend it from your module's needs).

## A resource module

Put controllers and services in your folder; the module is already imported in `app.module.ts`.

```ts
@Controller() @Public()
export class PollsController {
  constructor(private readonly prisma: PrismaService) {}
  @Get('polls')
  list(@RawQuery() raw: Record<string, unknown>) {
    return listEnvelope(delegate(this.prisma.poll), PollResource, parseQuery(raw, POLLS_ALLOWLIST));
  }
}
```

`lookups/lookups.controller.ts` is the worked example. Writes: `@DataBody() data`, read only writable fields with `common/fields.ts`, force owner fields from `@Caller()`, counters inside the same `$transaction` as `{ increment: 1 }` (decrements need raw `SET x = GREATEST(x - 1, 0)`; §8 forbids read-modify-write), respond `single(serializeEntity(row, Resource))`.

## Tests

- Unit: `src/**/*.spec.ts` (`npm test`). Query fixtures in `query/__fixtures__/`; the Appendix A corpus in `test/helpers/pdf-ui-corpus.ts` is parsed by `query/corpus.spec.ts`.
- e2e: `test/**/*.e2e-spec.ts` (`npm run test:e2e`) on `pdf_test` (refuses any name not ending `_test`), truncated before each file. Helpers in `test/helpers/`: `createTestApp(env?)` (real AppModule and pipeline; `t.api()` is supertest, `t.prisma` for fixtures such as `submitted_for_vote`), `loginStake(t, {username?})` and `loginDrep(t, stake)` (real CIP-8 via `cip8-signer.ts`), and envelope assertions (`expectList`, `expectSingle`, `expectError`, `expectBadRequestDetails`, `expectForbidden`, `expectNoKeyDeep`).

## Traps

- The partial unique indexes and the comments CHECK are raw SQL at the end of the init migration. `prisma migrate dev` wants to drop them in the next migration it generates; delete those lines from the generated SQL.
- `libcardano` ships `.ts` beside `.d.ts`; `tsconfig.json` `paths` points the compiler at `index.d.ts`, and jest transpiles the ESM-only `@noble/*` it needs (`tsconfig.jest.json`).
- Prisma does not escape `%`/`_` in `contains`; `toPrismaWhere` does. Use it rather than hand-written `contains`.
- Sorted text columns carry `COLLATE "und-x-icu"` (migration `20260926120000_text_collation`); the alpine image's libc collates `en_US.utf8` byte-wise. Prisma cannot declare a collation and `prisma migrate diff` ignores it (checked: empty diff), so generated migrations leave it alone. A new sortable text column needs its own `ALTER COLUMN … COLLATE "und-x-icu"`.
- String readers in `common/fields.ts` and the query parser refuse U+0000 (Postgres would fail the write with a 500). A body string read by hand, not through them, needs `hasNul`/`hasNulDeep`.
