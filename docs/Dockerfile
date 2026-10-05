# GovTool documentation site (Docusaurus), served by an unprivileged nginx.
#
# Build (from this folder):
#   docker build -t ghcr.io/intersectmbo/govtool-docs:dev .
# Build for another domain:
#   docker build --build-arg DOCS_URL=https://docs.dev.gov.tools -t ghcr.io/intersectmbo/govtool-docs:dev .

FROM node:22.22.0-alpine AS builder

WORKDIR /src

COPY package.json package-lock.json ./
RUN npm ci --no-audit --no-fund

COPY . .

# Public URL of the site (used for canonical links, Open Graph tags and the sitemap).
ARG DOCS_URL=https://docs.gov.tools
# Path the site is served under.
ARG DOCS_BASE_URL=/
ENV DOCS_URL=${DOCS_URL} \
    DOCS_BASE_URL=${DOCS_BASE_URL}

RUN npm run build


FROM nginxinc/nginx-unprivileged:1.28-alpine

# The same path the site was built for: pages, assets and the 404 page are
# served under it. The default, /, is docs.gov.tools.
ARG DOCS_BASE_URL=/
ENV DOCS_BASE_URL=${DOCS_BASE_URL}

# Value of the X-Robots-Tag response header. Use "noindex, nofollow" for
# temporary or staging hosts so they are not indexed by search engines.
ENV X_ROBOTS_TAG="all"

COPY nginx.conf.template /etc/nginx/templates/default.conf.template
COPY --from=builder /src/build /usr/share/nginx/docs${DOCS_BASE_URL}

EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
  CMD wget -q --spider http://127.0.0.1:8080/healthz || exit 1
