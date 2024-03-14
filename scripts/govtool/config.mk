common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

# directory paths
config_dir := $(root_dir)/scripts/govtool/config
template_config_dir := $(config_dir)/templates
target_config_dir := $(config_dir)/target
config_subdirs := cardano-node dbsync-secrets grafana-provisioning nginx
target_config_subdirs := $(target_config_dir) $(addprefix $(target_config_dir)/,$(config_subdirs))

docker_compose_file := $(target_config_dir)/docker-compose.yml

# metadata
cardano_config_provider := https://book.world.dev.cardano.org

.PHONY: prepare-config
prepare-config: clear generate-docker-compose-file enable-prometheus $(target_config_dir)/dbsync-secrets/postgres_user $(target_config_dir)/dbsync-secrets/postgres_db $(target_config_dir)/dbsync-secrets/postgres_password $(target_config_dir)/backend-config.json prepare-grafana-provisioning $(target_config_dir)/prometheus.yml $(target_config_dir)/promtail.yml $(target_config_dir)/loki.yml $(target_config_dir)/nginx/auth.conf $(target_config_dir)/nginx/govtool.htpasswd

.PHONY: clear
clear:
	rm -rf $(target_config_dir)

.SECONDEXPANSION:
$(target_config_subdirs):
	mkdir -p $@

.PHONY: generate-docker-compose-file
generate-docker-compose-file: check-env-defined $(target_config_dir)
	if [[ "$(env)" == "dev" ]]; then CSP_ALLOWED_HOSTS=",http://localhost"; else CSP_ALLOWED_HOSTS=; fi; \
	sed -e "s|<DOMAIN>|$(domain)|g" \
		-e "s|<DOCKER_USER>|$(docker_user)|g" \
		-e "s|<REPO_URL>|$(repo_url)|g" \
		-e "s|<CSP_ALLOWED_HOSTS>|$${CSP_ALLOWED_HOSTS}|g" \
		"$(template_config_dir)/docker-compose.yml.tpl" \
		> "$(target_config_dir)/docker-compose.yml"

.PHONY: fetch-cardano-node-config
fetch-cardano-node-config: $(target_config_dir)/cardano-node
	@:$(call check_defined, cardano_network)
	$(curl) -s "$(cardano_config_provider)/env-$(cardano_network).html" | \
		grep -E -o '[a-z-]+\.json' | \
		sort -u | \
		xargs -I"{}" $(curl) -s "$(cardano_config_provider)/environments/$(cardano_network)/{}" -o "$(target_config_dir)/cardano-node/{}"

.PHONY: enable-prometheus
enable-prometheus: fetch-cardano-node-config $(target_config_dir)/cardano-node
	sed -i '/"hasPrometheus"/ { N; s/"127\.0\.0\.1"/"0.0.0.0"/ }' "$(target_config_dir)/cardano-node/config.json"

$(target_config_dir)/dbsync-secrets/postgres_user: $(target_config_dir)/dbsync-secrets
	echo "$${DBSYNC_POSTGRES_USER}" > $@

$(target_config_dir)/dbsync-secrets/postgres_password: $(target_config_dir)/dbsync-secrets
	echo "$${DBSYNC_POSTGRES_PASSWORD}" > $@

$(target_config_dir)/dbsync-secrets/postgres_db: $(target_config_dir)/dbsync-secrets
	echo "$${DBSYNC_POSTGRES_DB}" > $@

$(target_config_dir)/backend-config.json: $(target_config_dir)
	sed -e "s|<DBSYNC_POSTGRES_DB>|$${DBSYNC_POSTGRES_DB}|" \
		-e "s|<DBSYNC_POSTGRES_USER>|$${DBSYNC_POSTGRES_USER}|" \
		-e "s|<DBSYNC_POSTGRES_PASSWORD>|$${DBSYNC_POSTGRES_PASSWORD}|" \
		-e "s|<SENTRY_DSN>|$${SENTRY_DSN_BACKEND}|" \
		"$(config_dir)/templates/backend-config.json.tpl" \
		> $@

$(target_config_dir)/prometheus.yml: $(target_config_dir)
	cp -a "$(template_config_dir)/prometheus.yml" $@

$(target_config_dir)/promtail.yml: $(target_config_dir)
	cp -a "$(template_config_dir)/promtail.yml" $@

$(target_config_dir)/loki.yml: $(target_config_dir)
	cp -a "$(template_config_dir)/loki.yml" $@

.PHONY: prepare-grafana-provisioning
prepare-grafana-provisioning: $(target_config_dir)/grafana-provisioning
	cp -a $(template_config_dir)/grafana-provisioning/* $(target_config_dir)/grafana-provisioning
	sed -e "s|<GRAFANA_SLACK_RECIPIENT>|$${GRAFANA_SLACK_RECIPIENT}|" \
		-e "s|<GRAFANA_SLACK_OAUTH_TOKEN>|$${GRAFANA_SLACK_OAUTH_TOKEN}|" \
		-i $(target_config_dir)/grafana-provisioning/alerting/alerting.yml

$(target_config_dir)/nginx/auth.conf: $(target_config_dir)/nginx
	@:$(call check_defined, domain)
	if [[ "$(domain)" == *"sanchonet.govtool.byron.network"* ]]; then \
	  echo "auth_basic \"Restricted\";" > $@; \
	  echo "auth_basic_user_file /etc/nginx/conf.d/govtool.htpasswd;" >> $@; \
	else \
	  echo > $@; \
	fi

$(target_config_dir)/nginx/govtool.htpasswd: $(target_config_dir)/nginx
	@:$(call check_defined, domain)
	if [[ "$(domain)" == *"sanchonet.govtool.byron.network"* ]]; then \
	  echo "$${NGINX_BASIC_AUTH}" > $@; \
	else \
	  echo > $@; \
	fi

.PHONY: upload-config
upload-config: check-env-defined prepare-config
	@:$(call check_defined, ssh_url)
	$(rsync) -av -e 'ssh -o StrictHostKeyChecking=no' config/target/. $(ssh_url):config
