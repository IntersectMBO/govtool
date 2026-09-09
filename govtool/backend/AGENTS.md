# AGENTS.md - govtool/backend

A read-only Haskell REST API over cardano-db-sync. Servant + warp, postgresql-simple,
Conferer config, Data.Cache TTL caching, Sentry. Its only write is pinning to IPFS via
Pinata.

The executable is still vva-be, the historical Voltaire Voting App name. Renaming it
to govtool-backend is a known outstanding chore; see README.md.

## Commands

```bash
direnv allow          # from this directory, after filling .envrc
cabal update && cabal build all
cabal run vva-be -- --config CONFIG.json start-app        # or show-config
pre-commit run --all-files hlint            # <- CI gate
pre-commit run --all-files stylish-haskell  # <- CI gate
```

GHC 9.2.8 and cabal 3.8.1.0, which is what CI uses. Plain cabal outside nix
historically fails on openapi3, hence nix plus direnv. stylish-haskell reads
.stylish-haskell.yaml here and produces the aligned import and record blocks you see
everywhere, so match the surrounding formatting: a reformat-everything diff will fail
review.

CONFIG.json above is a file you create by copying example-config.json. Conferer also accepts
environment variable overrides prefixed with VVA_. Swagger UI is at /swagger-ui, generated
from the VVAApi type, declaring /api and / as server bases.

## Rules that will bite you

1. Every .sql file used with embedFile must be listed in vva-be.cabal under extra-source-files. The
   sqlFrom embedFile call is a compile-time splice, so an unlisted file breaks the
   build and the release tarball even when a local build works.
2. A new module must be added to exposed-modules in the cabal file.
3. The :<|> order in server must match the VVAApi type positionally. A mismatch
   produces a wall of Servant types, so check ordering before anything else.
4. Postgres decoding is positional. Change a SELECT list and you must change the
   case result of [(a, b, c)] pattern in lockstep; the same arity in the wrong order
   compiles and fails at runtime.
5. A cache key must include every parameter that affects the result, or one user's
   response is served to another.
6. A new cache is two edits: a CacheEnv field in Types.hs,initialisation in
   app/Main.hs and use at the relevant handler call site.
7. Throw the right AppError. The HTTP status is derived from the constructor.
8. CORS allows only GET, HEAD, POST and OPTIONS, set in vvaCorsResourcePolicy in
   app/Main.hs. A PUT or DELETE route needs that widened too.
9. Keep API types and internal types separate. Handlers return API/Types.hs types,
   domain modules return Types.hs types. That mapping is what keeps the OpenAPI schema
   decoupled from db-sync's shape.
