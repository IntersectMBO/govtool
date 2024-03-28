FROM node:19 as prod
USER node
WORKDIR /home/node
COPY --chown=node:node package.json yarn.lock  ./
RUN  yarn install --prod --frozen-lockfile

FROM  prod as builder
RUN yarn install --frozen-lockfile
COPY ./src ./src
COPY ./tsconfig.json .
RUN yarn build

FROM prod
COPY --from=builder /home/node/dist/ ./
ENV PGHOST=postgres \
    PGPORT=5432 \
    PGUSER=postgres \
    NODE_OPTIONS="--max-old-space-size=2048" \
    PORT=8080 \
    NODE_ENV=production
EXPOSE 8080
CMD node index.js
