FROM node:24-alpine AS api_base
WORKDIR /app

COPY ./package.json ./yarn.lock ./
# Copy the rest of the application code to the container
RUN yarn install --frozen-lockfile --production

COPY ./prisma ./prisma
COPY ./prisma.config.ts ./prisma.config.ts
RUN DATABASE_URL=postgresql://prisma:prisma@localhost:5432/prisma yarn prisma generate


FROM api_base as builder
RUN yarn install
COPY ./ ./
RUN yarn build
RUN cp ./swagger.yaml ./dist



FROM node:24-alpine

USER node

WORKDIR /api
COPY --from=api_base --chown=node:node /app/node_modules node_modules
COPY --from=builder --chown=node:node /app/prisma prisma
COPY --from=builder --chown=node:node /app/prisma.config.ts prisma.config.ts
COPY --from=builder --chown=node:node /app/dist ./

# Expose the port the app runs on (if applicable)
EXPOSE 3000

# Command to run the application
CMD ["sh", "-c", "./node_modules/.bin/prisma migrate deploy && node /api/index.js"]
