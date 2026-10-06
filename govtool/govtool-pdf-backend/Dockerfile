FROM node:24-alpine AS build
WORKDIR /app
RUN apk add --no-cache openssl
COPY package.json package-lock.json ./
RUN npm ci
COPY prisma ./prisma
COPY prisma.config.ts ./
RUN npm run prisma:generate
COPY tsconfig.json tsconfig.build.json nest-cli.json ./
COPY src ./src
RUN npm run build && npm prune --omit=dev

FROM node:24-alpine
WORKDIR /app
RUN apk add --no-cache openssl
ENV NODE_ENV=production
COPY --from=build /app/node_modules ./node_modules
COPY --from=build /app/dist ./dist
COPY --from=build /app/prisma ./prisma
COPY package.json ./
USER node
EXPOSE 1337
# Migrations and the idempotent lookup seed run on start, so
# `docker compose up -d` yields a ready schema with its lookup rows.
# dist/start.js also reads DATABASE_URL, JWT_SECRET and REFRESH_SECRET from
# Swarm secret files when they are unset. Exec form: node is PID 1 and
# receives SIGTERM, so shutdown hooks run on `docker stop`.
CMD ["node", "dist/start.js"]
