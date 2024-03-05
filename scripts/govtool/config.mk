common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

# directory paths
config_dir := $(root_dir)/scripts/govtool/config
target_config_dir := $(config_dir)/target
template_config_dir := $(config_dir)/templates
cardano_node_config_dir := $(target_config_dir)/cardano-node
dbsync_secrets_dir := $(target_config_dir)/dbsync-secrets
grafana_provisioning_dir := $(target_config_dir)/grafana-provisioning
nginx_config_dir := $(target_config_dir)/nginx
docker_compose_file := $(target_config_dir)/docker-compose.yml

# metadata
cardano_config_provider := https://book.world.dev.cardano.org

.PHONY: prepare-config
prepare-config: clear generate-docker-compose-file enable-prometheus prepare-dbsync-secrets prepare-backend-config prepare-prometheus-config prepare-grafana-provisioning prepare-nginx-config

.PHONY: clear
clear:
	rm -rf $(target_config_dir)
	mkdir -p $(target_config_dir)

.PHONY: generate-docker-compose-file
generate-docker-compose-file: check-env-defined
	if [[ "$(env)" == "dev" ]]; then CSP_ALLOWED_HOSTS=",http://localhost"; else CSP_ALLOWED_HOSTS=; fi; \
	sed -e "s|<DOMAIN>|$(domain)|g" \
		-e "s|<DOCKER_USER>|$(docker_user)|g" \
		-e "s|<REPO_URL>|$(repo_url)|g" \
		-e "s|<CSP_ALLOWED_HOSTS>|$${CSP_ALLOWED_HOSTS}|g" \
		"$(template_config_dir)/docker-compose.yml.tpl" \
		> "$(target_config_dir)/docker-compose.yml"

.PHONY: fetch-cardano-node-config
fetch-cardano-node-config:
	@:$(call check_defined, cardano_network)
	mkdir -p $(cardano_node_config_dir)
	$(curl) -s "$(cardano_config_provider)/env-$(cardano_network).html" | \
		grep -E -o '[a-z-]+\.json' | \
		sort -u | \
		xargs -I"{}" $(curl) -s "$(cardano_config_provider)/environments/$(cardano_network)/{}" -o "$(cardano_node_config_dir)/{}"

.PHONY: enable-prometheus
enable-prometheus: fetch-cardano-node-config
	sed -i '/"hasPrometheus"/ { N; s/"127\.0\.0\.1"/"0.0.0.0"/ }' "$(cardano_node_config_dir)/config.json"

.PHONY: prepare-dbsync-secrets
prepare-dbsync-secrets:
	mkdir -p $(dbsync_secrets_dir)
	echo "$${DBSYNC_POSTGRES_USER}" > "$(dbsync_secrets_dir)/postgres_user"; \
	echo "$${DBSYNC_POSTGRES_PASSWORD}" > "$(dbsync_secrets_dir)/postgres_password"; \
	echo "$${DBSYNC_POSTGRES_DB}" > "$(dbsync_secrets_dir)/postgres_db"

.PHONY: prepare-backend-config
prepare-backend-config:
	sed -e "s/DBSYNC_POSTGRES_DB/$${DBSYNC_POSTGRES_DB}/" \
		-e "s/DBSYNC_POSTGRES_USER/$${DBSYNC_POSTGRES_USER}/" \
		-e "s/DBSYNC_POSTGRES_PASSWORD/$${DBSYNC_POSTGRES_PASSWORD}/" \
		-e "s|SENTRY_DSN|$${SENTRY_DSN_BACKEND}|" \
		"$(config_dir)/templates/backend-config.json.tpl" \
		> "$(target_config_dir)/backend-config.json"

.PHONY: prepare-prometheus-config
prepare-prometheus-config:
	cp -a "$(template_config_dir)/prometheus.yml" "$(target_config_dir)/prometheus.yml"

.PHONY: prepare-grafana-provisioning
prepare-grafana-provisioning:
	mkdir -p $(grafana_provisioning_dir)
	cp -a $(template_config_dir)/grafana-provisioning/* $(grafana_provisioning_dir)
	sed -e "s/GRAFANA_SLACK_RECIPIENT/$${GRAFANA_SLACK_RECIPIENT}/" \
		-e "s|GRAFANA_SLACK_OAUTH_TOKEN|$${GRAFANA_SLACK_OAUTH_TOKEN}|" \
		-i $(grafana_provisioning_dir)/alerting/alerting.yml

.PHONY: prepare-nginx-config
prepare-nginx-config:
	@:$(call check_defined, domain)
	mkdir -p $(nginx_config_dir)
	touch "$(nginx_config_dir)/auth.conf"
	touch "$(nginx_config_dir)/govtool.htpasswd"
	if [[ "$(domain)" == *"sanchonet.govtool.byron.network"* ]]; then \
	echo "$${NGINX_BASIC_AUTH}" > "$(nginx_config_dir)/govtool.htpasswd"; \
	echo "auth_basic \"Restricted\";" > "$(nginx_config_dir)/auth.conf"; \
	echo "auth_basic_user_file /etc/nginx/conf.d/govtool.htpasswd;" >> "$(nginx_config_dir)/auth.conf"; \
	fi

.PHONY: upload-config
upload-config: check-env-defined prepare-config
	@:$(call check_defined, ssh_url)
	$(rsync) -av -e 'ssh -o StrictHostKeyChecking=no' config/target/. $(ssh_url):config
