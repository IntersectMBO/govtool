.PHONY: ssh
ssh:
	export TERM=xterm-256color; \
	ssh $(ssh_url)

.PHONY: docker-compose
docker-compose:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@:$(call check_defined, cmd)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export GRAFANA_ADMIN_PASSWORD=$${GRAFANA_ADMIN_PASSWORD}; \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export STATUS_SERVICE_TAG=$(status_service_image_tag); \
	export METADATA_VALIDATION_TAG=$(metadata_validation_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	$(ssh-keyscan) $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) $(cmd)

.PHONY: reload-grafana
reload-grafana:
	@:$(call check_defined, domain)
	curl -X POST -u "admin:$${GRAFANA_ADMIN_PASSWORD}" https://$(domain)/grafana/api/admin/provisioning/alerting/reload
	curl -X POST -u "admin:$${GRAFANA_ADMIN_PASSWORD}" https://$(domain)/grafana/api/admin/provisioning/dashboards/reload
	curl -X POST -u "admin:$${GRAFANA_ADMIN_PASSWORD}" https://$(domain)/grafana/api/admin/provisioning/notifications/reload
