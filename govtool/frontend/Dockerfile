FROM node:22.22.0-alpine as builder

ENV NODE_OPTIONS="--max-old-space-size=4096"

WORKDIR /src

# Set npm configuration settings using environment variables
RUN npm config set @intersect.mbo:registry "https://registry.npmjs.org/" --location=global


COPY package.json package-lock.json ./
COPY patches ./patches

RUN npm install
COPY . .
RUN npm run build

FROM fholzer/nginx-brotli:v1.28

RUN apk add --no-cache gettext jq

EXPOSE 80

COPY nginx.conf /etc/nginx/conf.d/default.conf

COPY --from=builder /src/maintenance-page/index.html /usr/share/nginx/html/maintenance.html
COPY --from=builder /src/dist /usr/share/nginx/html


COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
