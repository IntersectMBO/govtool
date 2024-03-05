include ../../govtool/backend/Makefile
include ../../govtool/frontend/Makefile
include utils.mk
include info.mk
include config.mk

.DEFAULT_GOAL := info

# image tags
cardano_node_image_tag := 8.8.0-pre
cardano_db_sync_image_tag := sancho-4-0-0-fix-config

.PHONY: all
all: deploy-stack info notify

.PHONY: deploy-stack
deploy-stack: upload-config push-backend push-frontend
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export GRAFANA_ADMIN_PASSWORD=$${GRAFANA_ADMIN_PASSWORD}; \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	$(ssh-keyscan) $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) pull; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) up -d

.PHONY: destroy-cardano-node-and-dbsync
destroy-cardano-node-and-dbsync: prepare-config
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
toggle-maintenance: prepare-config
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
