include ../../govtool/backend/Makefile
include ../../govtool/frontend/Makefile
include utils.mk
include info.mk

.DEFAULT_GOAL := info

# tools
curl ?= curl
rsync ?= rsync
ssh-keyscan ?= ssh-keyscan

# environment variables
grafana_admin_password := $(shell echo $${GRAFANA_ADMIN_PASSWORD})
grafana_slack_recipient := $(shell echo $${GRAFANA_SLACK_RECIPIENT})
grafana_slack_oauth_token := $(shell echo $${GRAFANA_SLACK_OAUTH_TOKEN})

# target addresses
docker_host := $(domain)
docker_user := ubuntu
ssh_url := $(docker_user)@$(docker_host)

# stack configuration
docker_compose_file := docker-compose.$(env).yml

# image tags
cardano_node_image_tag := 8.8.0-pre
cardano_db_sync_image_tag := sancho-4-0-0-fix-config

.PHONY: all
all: deploy-stack info #notify

.PHONY: prepare-config
prepare-config:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, grafana_slack_recipient)
	@:$(call check_defined, grafana_slack_oauth_token)
	export CARDANO_NETWORK=$(cardano_network); \
	export GRAFANA_SLACK_RECIPIENT=$(grafana_slack_recipient); \
	export GRAFANA_SLACK_OAUTH_TOKEN=$(grafana_slack_oauth_token); \
	./prepare-config.sh

.PHONY: upload-config
upload-config: check-env-defined prepare-config
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(rsync) -av -e 'ssh -o StrictHostKeyChecking=no' config/target/. $(ssh_url):config

.PHONY: deploy-stack
deploy-stack: upload-config push-backend push-frontend
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@:$(call check_defined, grafana_admin_password)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export GRAFANA_ADMIN_PASSWORD=$(grafana_admin_password); \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	$(ssh-keyscan) $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) pull; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) up -d

.PHONY: destroy-cardano-node-and-dbsync
destroy-cardano-node-and-dbsync: check-env-defined
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	$(ssh-keyscan) $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	containers="$$(docker container ls --no-trunc --format '{{.Names}}' -f name=cardano-node -f name=cardano-db-sync -f name=postgres)"; \
	volumes="$$(docker volume ls --format '{{.Name}}' -f name=db-sync-data -f name=node-db -f name=node-ipc -f name=postgres)"; \
	$(docker) container stop $${containers}; \
	$(docker) container rm $${containers}; \
	$(docker) volume rm $${volumes}

.PHONY: toggle-maintenance
toggle-maintenance: check-env-defined
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@:$(call check_defined, maintenance)
	export ENVIRONMENT=$(env); \
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	$(ssh-keyscan) $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	if [[ "$(maintenance)" = "enable" ]]; then $(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) exec frontend touch /var/run/maintenance_enabled; \
	else $(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) exec frontend rm /var/run/maintenance_enabled; fi
