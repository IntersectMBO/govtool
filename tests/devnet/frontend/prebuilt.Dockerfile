# Serves a frontend built on the host, for machines where the in-Docker build
# (govtool/frontend/Dockerfile) runs out of memory:
#
#   (cd govtool/frontend && npm ci && npm run build)
#   FRONTEND_DOCKERFILE=../../tests/devnet/frontend/prebuilt.Dockerfile ./up.sh
#
# Build context: govtool/frontend. The runtime stage matches
# govtool/frontend/Dockerfile, so runtime config (window.__ENV__) works the
# same way.
ARG NGINX_IMAGE=fholzer/nginx-brotli:v1.28
FROM ${NGINX_IMAGE}

RUN apk add --no-cache gettext jq

EXPOSE 80

COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY maintenance-page/index.html /usr/share/nginx/html/maintenance.html
COPY dist /usr/share/nginx/html

COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
