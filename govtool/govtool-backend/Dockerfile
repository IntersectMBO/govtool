# Build context is the `govtool/` directory, NOT this one.
#
# The backend depends on its three sibling packages by path
# (`file:../govtool-data-providers` and friends), so a build context of this
# directory alone cannot resolve them. Build with:
#
#   docker build -f govtool-backend/Dockerfile govtool
#
# `docker-compose.yml` here already sets that context.
FROM node:22-alpine AS build

WORKDIR /src

# Each package is built before the one that depends on it: the contract first,
# then the two implementations, then the backend.
COPY govtool-data-providers ./govtool-data-providers
RUN cd govtool-data-providers && npm ci && npm run build && npm prune --omit=dev

COPY govtool-provider-dbsync ./govtool-provider-dbsync
RUN cd govtool-provider-dbsync && npm ci && npm run build && npm prune --omit=dev

# Every provider is built, because VVA_CHAINDATAPROVIDER picks one at runtime
# rather than at build time — the same image can serve db-sync, Koios or
# Blockfrost.
COPY govtool-provider-koios ./govtool-provider-koios
RUN cd govtool-provider-koios && npm ci && npm run build && npm prune --omit=dev

COPY govtool-provider-blockfrost ./govtool-provider-blockfrost
RUN cd govtool-provider-blockfrost && npm ci && npm run build && npm prune --omit=dev

COPY govtool-pinning-pinata ./govtool-pinning-pinata
RUN cd govtool-pinning-pinata && npm ci && npm run build && npm prune --omit=dev

COPY govtool-backend ./govtool-backend
RUN cd govtool-backend && npm ci && npm run build && npm prune --omit=dev

FROM node:22-alpine AS runtime

ENV NODE_ENV=production

# The whole tree is carried over, because npm links a `file:` dependency as a
# symlink into node_modules — copying only the backend's node_modules would
# leave three dangling links. It also keeps the provider's `sql/` directory
# next to its `dist/`, which is where the SQL loader resolves from.
WORKDIR /app/govtool-backend
COPY --from=build /src /app

EXPOSE 9999

CMD ["node", "dist/main.js"]
