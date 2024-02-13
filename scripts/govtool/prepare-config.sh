#!/usr/bin/env bash

set -eu pipefail

repo_root_dir="$(git rev-parse --show-toplevel)"
config_dir="$repo_root_dir/scripts/govtool/config"
target_config_dir="$repo_root_dir/scripts/govtool/config/target"
mkdir -p "$target_config_dir"

# cardano node config
cardano_node_config_dir="$target_config_dir/cardano-node"
mkdir -p "$cardano_node_config_dir"
for file in $(curl -s https://book.world.dev.cardano.org/env-"$CARDANO_NETWORK".html | grep -E -o '[a-z-]+\.json' | sort -u); do
	curl -s "https://book.world.dev.cardano.org/environments/$CARDANO_NETWORK/$file" -o "$cardano_node_config_dir/$file"
done
# enable prometheus in node config
sed -i '/"hasPrometheus"/ { N; s/"127\.0\.0\.1"/"0.0.0.0"/ }' "$cardano_node_config_dir/config.json"

# dbsync secret files
dbsync_secrets_dir="$target_config_dir/dbsync-secrets"
mkdir -p "$dbsync_secrets_dir"
echo "$DBSYNC_POSTGRES_USER" > "$dbsync_secrets_dir/postgres_user"
echo "$DBSYNC_POSTGRES_PASSWORD" > "$dbsync_secrets_dir/postgres_password"
echo "$DBSYNC_POSTGRES_DB" > "$dbsync_secrets_dir/postgres_db"

# postgres schema for fake db sync
fakedbsync_init_dir="$target_config_dir/fakedbsync_init.d"
mkdir -p "$fakedbsync_init_dir"
cp "$repo_root_dir/govtool/backend/misc/fakedbsync_users.sql" "$fakedbsync_init_dir/00_fakedbsync_users.sql"
sed -i -e "s/CREATE USER.*$/CREATE USER $FAKEDBSYNC_POSTGRES_USER WITH PASSWORD '$FAKEDBSYNC_POSTGRES_PASSWORD';/g" \
    "$fakedbsync_init_dir/00_fakedbsync_users.sql"
cp "$repo_root_dir/govtool/backend/misc/schema6.sql" "$fakedbsync_init_dir/10_schema.sql"

# backend config file
sed -e "s/FAKEDBSYNC_POSTGRES_DB/$FAKEDBSYNC_POSTGRES_DB/" \
    -e "s/FAKEDBSYNC_POSTGRES_USER/$FAKEDBSYNC_POSTGRES_USER/" \
    -e "s/FAKEDBSYNC_POSTGRES_PASSWORD/$FAKEDBSYNC_POSTGRES_PASSWORD/" \
    -e "s/DBSYNC_POSTGRES_DB/$DBSYNC_POSTGRES_DB/" \
    -e "s/DBSYNC_POSTGRES_USER/$DBSYNC_POSTGRES_USER/" \
    -e "s/DBSYNC_POSTGRES_PASSWORD/$DBSYNC_POSTGRES_PASSWORD/" \
    -e "s|SENTRY_DSN|$SENTRY_DSN_BACKEND|" \
    "$config_dir/secrets/backend-config.json.tpl" \
    > "$target_config_dir/backend-config.json"

# prometheus config file
cat >"$target_config_dir/prometheus.yml" <<_EOF_
global:
  scrape_interval: 15s
  evaluation_interval: 15s
  external_labels:
    monitor: 'govtool'
scrape_configs:
  - job_name: 'traefik'
    scrape_interval: 5s
    static_configs:
      - targets: ['traefik:8082']
  - job_name: 'cardano'
    scrape_interval: 5s
    static_configs:
      - targets: ['cardano-node:12798']
  - job_name: 'cardano_db_sync'
    scrape_interval: 5s
    metrics_path: /
    static_configs:
      - targets: ['cardano-db-sync:8080']
  - job_name: 'host'
    scrape_interval: 5s
    static_configs:
      - targets: ['host.docker.internal:9100']
_EOF_

# grafana provisioning dir
grafana_provisioning_dir="$target_config_dir/grafana-provisioning"
mkdir -p "$grafana_provisioning_dir"
cp -a "$config_dir/grafana-provisioning/"* "$grafana_provisioning_dir"
sed -e "s/GRAFANA_SLACK_RECIPIENT/$GRAFANA_SLACK_RECIPIENT/" \
    -e "s|GRAFANA_SLACK_OAUTH_TOKEN|$GRAFANA_SLACK_OAUTH_TOKEN|" \
    -i "$grafana_provisioning_dir/alerting/alerting.yml"

# nginx config for frontend optional basic auth
nginx_config_dir="$target_config_dir/nginx"
mkdir -p "$nginx_config_dir"
if [[ "$DOMAIN" == *"sanchonet.govtool.byron.network"* ]]; then
  cat >"$nginx_config_dir/auth.conf" <<_EOF_
auth_basic "Restricted";
auth_basic_user_file /etc/nginx/conf.d/govtool.htpasswd;
_EOF_
  cat >"$nginx_config_dir/govtool.htpasswd" <<_EOF_
$NGINX_BASIC_AUTH
_EOF_
else
  # create empty files if no basic auth is needed
  touch "$nginx_config_dir/auth.conf"
  touch "$nginx_config_dir/govtool.htpasswd"
fi
