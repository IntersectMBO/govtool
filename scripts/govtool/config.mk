common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

# metadata
cardano_config_provider := https://book.world.dev.cardano.org

# directories
config_dir := $(root_dir)/scripts/govtool/config
template_config_dir := $(config_dir)/templates
target_config_dir := $(config_dir)/target

# files
docker_compose_file := $(target_config_dir)/docker-compose.yml
cardano_configs := alonzo-genesis byron-genesis conway-genesis db-sync-config shelley-genesis submit-api-config topology
cardano_config_files := $(addprefix $(target_config_dir)/cardano-node/,$(addsuffix .json,$(cardano_configs)))
outputs := cardano-node/config.json \
  dbsync-secrets/postgres_user \
  dbsync-secrets/postgres_db \
  dbsync-secrets/postgres_password \
  backend-config.json prometheus.yml \
  promtail.yml \
  loki.yml \
  nginx/auth.conf \
  nginx/govtool.htpasswd \
  grafana-provisioning/alerting/alerting.yml \
  grafana-provisioning/datasources/datasource.yml \
  grafana-provisioning/dashboards/dashboard.yml \
  grafana-provisioning/dashboards/govtool.json \
  grafana-provisioning/dashboards/traefik_rev4.json

output_files := $(addprefix $(target_config_dir)/,$(outputs)) $(cardano_config_files) $(docker_compose_file)
output_dirs := $(sort $(foreach file,$(output_files),$(dir $(file))))

.PHONY: prepare-config
prepare-config: clear $(output_files)

.PHONY: upload-config
upload-config: check-env-defined prepare-config
	@:$(call check_defined, ssh_url)
	$(rsync) -av -e 'ssh -o StrictHostKeyChecking=no' config/target/. $(ssh_url):config

.PHONY: clear
clear:
	rm -rf $(target_config_dir)

.SECONDEXPANSION:
$(output_dirs):
	mkdir -p $@

$(docker_compose_file): $(template_config_dir)/docker-compose.yml.tpl $(target_config_dir)/
	if [[ "$(env)" == "dev" ]]; then CSP_ALLOWED_HOSTS=",http://localhost,http://localhost:5173"; else CSP_ALLOWED_HOSTS=; fi; \
	sed -e "s|<DOMAIN>|$(domain)|g" \
		-e "s|<DOCKER_USER>|$(docker_user)|g" \
		-e "s|<REPO_URL>|$(repo_url)|g" \
		-e "s|<CSP_ALLOWED_HOSTS>|$${CSP_ALLOWED_HOSTS}|g" \
		$< > $@

$(cardano_config_files): $(target_config_dir)/cardano-node/
	@:$(call check_defined, cardano_network)
	$(curl) -s "$(cardano_config_provider)/environments/$(cardano_network)/$(notdir $@)" -o $@

$(target_config_dir)/cardano-node/config.json: $(target_config_dir)/cardano-node/
	$(curl) -s "$(cardano_config_provider)/environments/$(cardano_network)/$(notdir $@)" -o $@
	sed -i '/"hasPrometheus"/ { N; s/"127\.0\.0\.1"/"0.0.0.0"/ }' "$(target_config_dir)/cardano-node/config.json"

$(target_config_dir)/dbsync-secrets/postgres_user: $(target_config_dir)/dbsync-secrets/
	echo "$${DBSYNC_POSTGRES_USER}" > $@

$(target_config_dir)/dbsync-secrets/postgres_password: $(target_config_dir)/dbsync-secrets/
	echo "$${DBSYNC_POSTGRES_PASSWORD}" > $@

$(target_config_dir)/dbsync-secrets/postgres_db: $(target_config_dir)/dbsync-secrets/
	echo "$${DBSYNC_POSTGRES_DB}" > $@

$(target_config_dir)/backend-config.json: $(config_dir)/templates/backend-config.json.tpl $(target_config_dir)/
	sed -e "s|<DBSYNC_POSTGRES_DB>|$${DBSYNC_POSTGRES_DB}|" \
		-e "s|<DBSYNC_POSTGRES_USER>|$${DBSYNC_POSTGRES_USER}|" \
		-e "s|<DBSYNC_POSTGRES_PASSWORD>|$${DBSYNC_POSTGRES_PASSWORD}|" \
		-e "s|<SENTRY_DSN>|$${SENTRY_DSN_BACKEND}|" \
		$< > $@

$(target_config_dir)/%.yml: $(template_config_dir)/%.yml $(target_config_dir)/
	cp -a $< $@

$(target_config_dir)/grafana-provisioning/datasources/%: $(template_config_dir)/grafana-provisioning/datasources/% $(target_config_dir)/grafana-provisioning/datasources/
	cp -a $< $@

$(target_config_dir)/grafana-provisioning/dashboards/%: $(template_config_dir)/grafana-provisioning/dashboards/% $(target_config_dir)/grafana-provisioning/dashboards/
	cp -a $< $@

$(target_config_dir)/grafana-provisioning/alerting/alerting.yml: $(template_config_dir)/grafana-provisioning/alerting/alerting.yml $(target_config_dir)/grafana-provisioning/alerting/
	cp -a $< $@
	sed -e "s|<GRAFANA_SLACK_RECIPIENT>|$${GRAFANA_SLACK_RECIPIENT}|" \
		-e "s|<GRAFANA_SLACK_OAUTH_TOKEN>|$${GRAFANA_SLACK_OAUTH_TOKEN}|" \
		-i $@

$(target_config_dir)/nginx/auth.conf: $(target_config_dir)/nginx/
	@:$(call check_defined, domain)
	if [[ "$(domain)" == *"sanchonet.govtool.byron.network"* ]]; then \
	  echo 'map $$http_x_forwarded_for $$auth {' > $@; \
	  echo "  default \"Restricted\";" >> $@; \
	  echo "  $${IP_ADDRESS_BYPASSING_BASIC_AUTH1} \"off\";" >> $@; \
	  echo "  $${IP_ADDRESS_BYPASSING_BASIC_AUTH2} \"off\";" >> $@; \
	  echo "}" >> $@; \
	  echo 'auth_basic $$auth;' >> $@; \
	  echo "auth_basic_user_file /etc/nginx/conf.d/govtool.htpasswd;" >> $@; \
	else \
	  echo > $@; \
	fi

$(target_config_dir)/nginx/govtool.htpasswd: $(target_config_dir)/nginx/
	@:$(call check_defined, domain)
	if [[ "$(domain)" == *"sanchonet.govtool.byron.network"* ]]; then \
	  echo "$${NGINX_BASIC_AUTH}" > $@; \
	else \
	  echo > $@; \
	fi
