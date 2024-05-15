version: "3.9"

services:
  traefik:
    image: traefik:v3.0
    command:
      - "--providers.docker=true"
      - "--providers.docker.exposedbydefault=false"
      - "--entrypoints.web.address=:80"
      - "--entrypoints.websecure.address=:443"
      - "--certificatesresolvers.myresolver.acme.httpchallenge=true"
      - "--certificatesresolvers.myresolver.acme.httpchallenge.entrypoint=web"
      - "--certificatesresolvers.myresolver.acme.email=${TRAEFIK_LE_EMAIL}"
      - "--certificatesresolvers.myresolver.acme.storage=/letsencrypt/acme.json"
      - "--log.level=DEBUG"
      - "--entryPoints.metrics.address=:8082"
      - "--metrics.prometheus=true"
      - "--metrics.prometheus.entryPoint=metrics"
      - "--metrics.prometheus.buckets=0.1,0.3,1.2,5.0"
    ports:
      - 80:80
      - 443:443
    volumes:
      - letsencrypt:/letsencrypt
      - "/var/run/docker.sock:/var/run/docker.sock:ro"
    restart: always
    logging: &logging
      driver: "json-file"
      options:
        max-size: "200k"
        max-file: "10"
    labels:
      - "traefik.enable=true"
      - "traefik.http.middlewares.redirect-to-https.redirectscheme.scheme=https"
      - "traefik.http.routers.to-http-catchall.rule=hostregexp(`{host:.+}`)"
      - "traefik.http.routers.to-http-catchall.entrypoints=web"
      - "traefik.http.routers.to-http-catchall.middlewares=redirect-to-https"

  loki:
    image: grafana/loki:2.9.4
    ports:
      - "3100:3100"
    command: -config.file=/etc/loki/loki.yml
    volumes:
      - loki-data:/loki
      - /home/<DOCKER_USER>/config/loki.yml:/etc/loki/loki.yml

  promtail:
    image: grafana/promtail:2.9.4
    volumes:
      - /var/log/deployment.log:/var/log/deployment.log
      - /home/<DOCKER_USER>/config/promtail.yml:/etc/promtail/promtail.yml
    command: -config.file=/etc/promtail/promtail.yml
    depends_on:
      - loki

  prometheus:
    image: prom/prometheus:v2.47.1
    volumes:
      - prometheus-data:/prometheus
      - /home/<DOCKER_USER>/config/prometheus.yml:/etc/prometheus/prometheus.yml
    extra_hosts:
      - "host.docker.internal:host-gateway"
    restart: always
    logging: *logging

  grafana:
    image: grafana/grafana:10.0.8
    volumes:
      - grafana-data:/var/lib/grafana
      - /home/<DOCKER_USER>/config/grafana-provisioning:/etc/grafana/provisioning
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=${GRAFANA_ADMIN_PASSWORD}
      - GF_USERS_ALLOW_SIGN_UP=false
      - GF_INSTALL_PLUGINS=grafana-piechart-panel
      - GF_SERVER_ROOT_URL=https://<DOMAIN>/grafana
      - GF_SERVER_SERVE_FROM_SUB_PATH=true
    restart: always
    logging: *logging
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.to-grafana.rule=Host(`<DOMAIN>`) && PathPrefix(`/grafana`)"
      - "traefik.http.routers.to-grafana.entrypoints=websecure"
      - "traefik.http.routers.to-grafana.tls.certresolver=myresolver"
      - "traefik.http.services.grafana.loadbalancer.server.port=3000"

  postgres:
    image: postgres:15-alpine
    environment:
      - POSTGRES_LOGGING=true
      - POSTGRES_DB_FILE=/run/secrets/postgres_db
      - POSTGRES_PASSWORD_FILE=/run/secrets/postgres_password
      - POSTGRES_USER_FILE=/run/secrets/postgres_user
    secrets:
      - postgres_password
      - postgres_user
      - postgres_db
    volumes:
      - postgres:/var/lib/postgresql/data
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 10s
      timeout: 5s
      retries: 5
    command: ${POSTGRES_ARGS:--c maintenance_work_mem=1GB -c max_parallel_maintenance_workers=4}
    logging: *logging

  cardano-node:
    image: ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_TAG}
    entrypoint:
      - "cardano-node"
      - "run"
      - "--topology"
      - "/configuration/topology.json"
      - "--database-path"
      - "/data/db"
      - "--socket-path"
      - "/ipc/node.socket"
      - "--host-addr"
      - "0.0.0.0"
      - "--port"
      - "3001"
      - "--config"
      - "/configuration/config.json"
      - "+RTS"
      - "-N2"
      - "-I0"
      - "-A16m"
      - "-qg"
      - "-qb"
      - "--disable-delayed-os-memory-return"
      - "-RTS"
    environment:
      - NETWORK=${CARDANO_NETWORK}
    volumes:
      - node-db:/data/db
      - node-ipc:/ipc
      - /home/<DOCKER_USER>/config/cardano-node:/configuration
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "curl -f 127.0.0.1:12788 || exit 1"]
      interval: 60s
      timeout: 10s
      retries: 5
    logging: *logging

  cardano-db-sync:
    image: ghcr.io/intersectmbo/cardano-db-sync:${CARDANO_DB_SYNC_TAG}
    environment:
      - NETWORK=${CARDANO_NETWORK}
      - POSTGRES_HOST=postgres
      - POSTGRES_PORT=5432
      - RESTORE_RECREATE_DB=N
    depends_on:
      cardano-node:
        condition: service_healthy
      postgres:
        condition: service_healthy
    secrets:
      - postgres_password
      - postgres_user
      - postgres_db
    volumes:
      - db-sync-data:/var/lib/cexplorer
      - node-ipc:/node-ipc
    restart: always
    logging: *logging

  status-service:
    image: <REPO_URL>/status-service:${STATUS_SERVICE_TAG}
    environment:
      - GRAFANA_USERNAME=admin
      - GRAFANA_PASSWORD=${GRAFANA_ADMIN_PASSWORD}
    restart: always
    logging: *logging
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.to-status-service.rule=Host(`<DOMAIN>`) && PathPrefix(`/status`)"
      - "traefik.http.routers.to-status-service.entrypoints=websecure"
      - "traefik.http.routers.to-status-service.tls.certresolver=myresolver"
      - "traefik.http.services.status-service.loadbalancer.server.port=8000"

  metadata-validation:
    image: <REPO_URL>/metadata-validation:${METADATA_VALIDATION_TAG}
    environment:
      - PORT=3000
    logging: *logging
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "curl -f 127.0.0.1:3000/health || exit 1"]
      interval: 5s
      timeout: 5s
      retries: 5
    labels:
      - "traefik.enable=true"
      - "traefik.http.middlewares.metadata-validation-stripprefix.stripprefix.prefixes=/metadata-validation"
      - "traefik.http.middlewares.metadata-validation-cors.headers.accesscontrolallowmethods=*"
      - "traefik.http.middlewares.metadata-validation-cors.headers.accesscontrolallowheaders=*"
      - "traefik.http.middlewares.metadata-validation-cors.headers.accesscontrolalloworiginlist=https://<DOMAIN><CSP_ALLOWED_HOSTS>"
      - "traefik.http.middlewares.metadata-validation-cors.headers.accesscontrolmaxage=100"
      - "traefik.http.routers.to-metadata-validation.rule=Host(`<DOMAIN>`) && PathPrefix(`/metadata-validation`)"
      - "traefik.http.routers.to-metadata-validation.middlewares=metadata-validation-stripprefix@docker,metadata-validation-cors@docker"
      - "traefik.http.routers.to-metadata-validation.entrypoints=websecure"
      - "traefik.http.routers.to-metadata-validation.tls.certresolver=myresolver"
      - "traefik.http.services.metadata-validation.loadbalancer.server.port=3000"
      - "traefik.http.services.metadata-validation.loadbalancer.healthcheck.path=/health"
      - "traefik.http.services.metadata-validation.loadbalancer.healthcheck.port=3000"
      - "traefik.http.services.metadata-validation.loadbalancer.healthcheck.interval=10s"
      - "traefik.http.services.metadata-validation.loadbalancer.healthcheck.timeout=5s"

  analytics-dashboard:
    image: <REPO_URL>/analytics-dashboard:${ANALYTICS_DASHBOARD_TAG}
    environment:
      - GA_CLIENT_EMAIL=${GA_CLIENT_EMAIL}
      - GA_PRIVATE_KEY=${GA_PRIVATE_KEY}
      - GOOGLE_APPLICATION_CREDENTIALS=/run/secrets/google-credentials.json
      - NEXT_PUBLIC_API_URL=${NEXT_PUBLIC_API_URL}
      - NEXT_PUBLIC_GA4_PROPERTY_ID=${NEXT_PUBLIC_GA4_PROPERTY_ID}
      - SENTRY_IGNORE_API_RESOLUTION_ERROR=${SENTRY_IGNORE_API_RESOLUTION_ERROR}
    secrets:
      - google-credentials.json
    logging: *logging
    restart: always
    labels:
      - "traefik.enable=true"
      - "traefik.http.routers.to-analytics-dashboard.rule=Host(`participation.sanchogov.tools`)"
      - "traefik.http.routers.to-analytics-dashboard.entrypoints=websecure"
      - "traefik.http.routers.to-analytics-dashboard.tls.certresolver=myresolver"
      - "traefik.http.services.analytics-dashboard.loadbalancer.server.port=3000"

  backend:
    image: <REPO_URL>/backend:${BACKEND_TAG}
    command: /usr/local/bin/vva-be -c /run/secrets/backend-config.json start-app
    depends_on:
      cardano-node:
        condition: service_healthy
      postgres:
        condition: service_healthy
    secrets:
      - backend-config.json
    restart: always
    logging: *logging
    labels:
      - "traefik.enable=true"
      - "traefik.http.middlewares.backend-stripprefix.stripprefix.prefixes=/api"
      - "traefik.http.middlewares.backend-cors.headers.accesscontrolallowmethods=GET,POST,HEAD,OPTIONS"
      - "traefik.http.middlewares.backend-cors.headers.accesscontrolallowheaders=*"
      - "traefik.http.middlewares.backend-cors.headers.accesscontrolalloworiginlist=https://<DOMAIN><CSP_ALLOWED_HOSTS>"
      - "traefik.http.middlewares.backend-cors.headers.accesscontrolmaxage=100"
      - "traefik.http.middlewares.backend-cors.headers.addvaryheader=true"
      - "traefik.http.routers.to-backend.rule=Host(`<DOMAIN>`) && PathPrefix(`/api`)"
      - "traefik.http.routers.to-backend.middlewares=backend-stripprefix@docker,backend-cors@docker"
      - "traefik.http.routers.to-backend.entrypoints=websecure"
      - "traefik.http.routers.to-backend.tls.certresolver=myresolver"
      - "traefik.http.services.backend.loadbalancer.server.port=9876"
      - "traefik.http.services.backend.loadbalancer.healthcheck.path=/epoch/params"
      - "traefik.http.services.backend.loadbalancer.healthcheck.port=9876"
      - "traefik.http.services.backend.loadbalancer.healthcheck.interval=10s"
      - "traefik.http.services.backend.loadbalancer.healthcheck.timeout=5s"

  frontend:
    image: <REPO_URL>/frontend:${FRONTEND_TAG}
    volumes:
      - /home/<DOCKER_USER>/config/nginx/auth.conf:/etc/nginx/conf.d/auth.conf
      - /home/<DOCKER_USER>/config/nginx/govtool.htpasswd:/etc/nginx/conf.d/govtool.htpasswd
    depends_on:
      cardano-node:
        condition: service_healthy
      postgres:
        condition: service_healthy
    restart: always
    logging: *logging
    labels:
      - "traefik.enable=true"
      - "traefik.http.middlewares.frontend-csp.headers.contentSecurityPolicy=default-src 'self'; img-src *.usersnap.com https://www.googletagmanager.com 'self' data:; script-src *.usersnap.com 'self' 'unsafe-inline' https://www.googletagmanager.com https://browser.sentry-cdn.com; style-src *.usersnap.com *.googleapis.com 'self' 'unsafe-inline' https://fonts.googleapis.com; connect-src *.usersnap.com https://s3.eu-central-1.amazonaws.com/upload.usersnap.com 'self' o4506155985141760.ingest.sentry.io *.google-analytics.com; font-src *.usersnap.com *.gstatic.com 'self' 'unsafe-inline' https://fonts.gstatic.com; worker-src blob:"
      - "traefik.http.routers.to-frontend.rule=Host(`<DOMAIN>`)"
      - "traefik.http.routers.to-frontend.entrypoints=websecure"
      - "traefik.http.routers.to-frontend.tls.certresolver=myresolver"
      - "traefik.http.routers.to-frontend.middlewares=frontend-csp@docker"
      - "traefik.http.routers.to-frontend.priority=1"
      - "traefik.http.services.frontend.loadbalancer.server.port=80"

secrets:
  postgres_db:
    file: /home/<DOCKER_USER>/config/dbsync-secrets/postgres_db
  postgres_password:
    file: /home/<DOCKER_USER>/config/dbsync-secrets/postgres_password
  postgres_user:
    file: /home/<DOCKER_USER>/config/dbsync-secrets/postgres_user
  backend-config.json:
    file: /home/<DOCKER_USER>/config/backend-config.json
  google-credentials.json:
    file: /home/<DOCKER_USER>/config/google-credentials.json

volumes:
  letsencrypt:
  db-sync-data:
  grafana-data:
  postgres:
  prometheus-data:
  node-db:
  node-ipc:
  loki-data:
