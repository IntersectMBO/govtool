# AGENTS.md - govtool/frontend

The gov.tools web app. React 19 + TS + Vite 6, MUI 5 + Emotion, TanStack Query v5,
React Router v8, react-hook-form + yup, i18next, cardano-serialization-lib.

## Commands

```bash
npm ci                  # run this first if anything looks mysteriously missing
npm run dev             # vite, port 5173
npm run lint            # eslint --max-warnings 0      <- CI gate
npm run tsc             # tsc --noEmit --skipLibCheck  <- CI gate
npx vitest run [path]   # one-shot tests               <- CI gate
npm run format          # prettier --write src
npm run storybook       # port 6006
```

npm run test is watch mode and will not exit, so use npx vitest run. Coverage is
enabled in vite.config.ts, so every run emits coverage; that is expected.

Node is pinned to 22.22.0 by .nvmrc. Setup is cp .env.example .env plus VITE_BASE_URL,
which can be any deployed backend, and VITE_METADATA_API_URL. No local backend is
needed for UI work. postinstall runs patch-package against patches, so add a patch
rather than editing node_modules.

## Conventions

Import through the Vite aliases, and add every new file to its folder's index.ts
barrel or it will not resolve through them. Components are arrow functions, one per
file, PascalCase, layered atoms then molecules then organisms by composition; pages
stay thin. Data fetching goes only through hooks/queries, then services/requests, then
services/API.ts: never axios directly, never a fetch inside a component. Copy comes
only from i18n/locales/en.json, env only from the env object in config/env.ts. Style
with MUI sx or Emotion using theme.ts and consts/colors.ts; no new stylesheets.

## Lint

Airbnb plus TS, react-hooks, jest and storybook. Turned off locally:
react-hooks/exhaustive-deps, no-nested-ternary, no-param-reassign, no-plusplus,
import/prefer-default-export. Still errors, and easy to trip:

- no-console: only console.warn and console.error
- @typescript-eslint/no-unused-vars, no-shadow, no-redeclare
- react/jsx-filename-extension: JSX only in .tsx
- react/function-component-definition: arrow functions only, so a function declaration
  for a component is an error
- import/no-extraneous-dependencies: devDependencies only in test and stories files
- semi always, linebreak-style unix

## Traps

- src/context/wallet.tsx is the chain-write boundary: about 1600 lines, no tests, and
  it moves real ada. Extend an existing buildX function and verify on a testnet.
- Recent migrations changed import paths. Use react-router, not react-router-dom, for
  v8, and @tanstack/react-query, not react-query, for v5 with object-syntax useQuery
  only. The old forms do not compile.
- The pillars are npm packages, @intersect.mbo/pdf-ui and
  @intersect.mbo/govtool-outcomes-pillar-ui, mounted as routes. A bug inside them
  cannot be fixed from this repo.
- src/utils/canonizeJSON.ts is duplicated at
  govtool/metadata-validation/src/utils/canonizeJSON.ts and must behave identically,
  or metadata hash checks diverge. Change one, change both.The metadata-validation copy is currently unused. The frontend copy is used for signature verification.
- Stale local node_modules is common. If npm run tsc cannot find a module that is in
  package.json, run npm ci before debugging further.
- Adding a VITE_ variable touches .env.example, src/config/env.ts,
  docker-entrypoint.sh, and docker/docker-compose.yaml. Missing the runtime
  wiring can make it work locally but remain undefined in containers.
