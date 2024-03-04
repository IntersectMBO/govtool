# pipeline information
pipeline_url := $(shell echo $${PIPELINE_URL:-})
ifeq ($(PIPELINE_URL),)
  pipeline_info := _Deployed from local machine._
else
  pipeline_info := _Deployed from <$(pipeline_url)|GitHub>._
endif

.PHONY: info
info:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@echo "+-----------"
	@echo "|  TIME      $(shell date +'%Y-%m-%d %H:%M:%S%z')"
	@echo "|  BRANCH    $(branch) [$(commit)]"
	@echo "|  ENV       $(env)"
	@echo "I  NETWORK   $(cardano_network)"
	@echo "N  BACKEND   $(repo_url)/backend:$(backend_image_tag)"
	@echo "F  FRONTEND  $(repo_url)/frontend:$(frontend_image_tag)"
	@echo "O  NODE      ghcr.io/intersectmbo/cardano-node:$(cardano_node_image_tag)"
	@echo "|  DBSYNC    ghcr.io/intersectmbo/cardano-db-sync:$(cardano_db_sync_image_tag)"
	@echo "|  SSH       $(ssh_url)"
	@echo "|  URL       https://$(docker_host)"
	@echo "+-----------"

.PHONY: notify
notify:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(curl) -X POST https://slack.com/api/chat.postMessage\
		-H "Authorization: Bearer $(grafana_slack_oauth_token)" \
		-H "Content-Type: application/json; charset=utf-8" \
		--data "{ \"channel\":\"$${GRAFANA_SLACK_RECIPIENT}\", \"text\":\":rocket: *Deploy performed on \`$(env)\`*\n- from *branch* \`$(branch)\` (\`$(commit)\`),\n- using *Cardano Node* version \`$(cardano_node_image_tag)\`,\n- using *Cardano DB Sync* version \`$(cardano_db_sync_image_tag)\`,\n- using *GovTool backend* version \`$(backend_image_tag)\`,\n- using *Govtool frontend* version \`$(frontend_image_tag)\`.\n$(pipeline_info)\" }"

