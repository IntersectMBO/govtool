SHELL := bash
ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

cardano_network := $(shell echo $${CARDANO_NETWORK})
env := $(shell echo $${ENVIRONMENT})
grafana_admin_password := $(shell echo $${GRAFANA_ADMIN_PASSWORD})
grafana_slack_recipient := $(shell echo $${GRAFANA_SLACK_RECIPIENT})
grafana_slack_oauth_token := $(shell echo $${GRAFANA_SLACK_OAUTH_TOKEN})
gtm_id := $(shell echo $${GTM_ID})
sentry_dsn := $(shell echo $${SENTRY_DSN})

commit := $(shell git rev-parse HEAD)
branch := $(shell git rev-parse --abbrev-ref HEAD)
docker ?= docker
domain := $(shell echo $${DOMAIN:-$(env)-$(cardano_network).govtool.byron.network})
repo_url ?= 733019650473.dkr.ecr.eu-west-1.amazonaws.com

docker_host := $(domain)
docker_user := ubuntu
ssh_url := $(docker_user)@$(docker_host)
docker_compose_file := docker-compose.$(env).yml
compose_stack_name := govtool-$(env)-$(cardano_network)
pipeline_url := $(shell echo $${PIPELINE_URL:-})
ifeq ($(PIPELINE_URL),)
  pipeline_info := _Deployed from local machine._
else
  pipeline_info := _Deployed from <$(pipeline_url)|GitHub>._
endif

cardano_node_image_tag := 8.8.0-pre
cardano_db_sync_image_tag := sancho-4-0-0-fix-config
base_backend_image_tag := $(shell ./utils/backend-base-version)
backend_image_tag := $(shell ./utils/backend-version)
frontend_image_tag := $(shell ./utils/frontend-version)

# helper function for checking undefined variables
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error Undefined $1$(if $2, ($2))))

# helper function for checking if image exists on ECR
check_image_on_ecr = \
	$(docker) manifest inspect "$(repo_url)/$1:$2" > /dev/null 2>&1

.PHONY: all
all: docker-login prepare-config upload-config build-backend push-backend build-frontend push-frontend deploy-stack info notify

.PHONY: check-env-defined
check-env-defined:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@grep -q "module \"$(compose_stack_name)\"" ../../infra/terraform/main.tf && \
	echo "Environment $(env) for network $(cardano_network) is defined in Terraform" || \
	{ echo "Environment $(env) for network $(cardano_network) is NOT defined in Terraform, cannot deploy there"; exit 1; }

.PHONY: docker-login
docker-login:
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/backend
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/frontend

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
upload-config: prepare-config
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	rsync -av -e 'ssh -o StrictHostKeyChecking=no' config/target/. $(ssh_url):config

.PHONY: build-backend
build-backend: docker-login build-backend-base
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(call check_image_on_ecr,backend,$(backend_image_tag)) || \
	$(docker) build --build-arg BASE_IMAGE_TAG=$(base_backend_image_tag) --tag "$(repo_url)/backend:$(backend_image_tag)" ../../govtool/backend

.PHONY: build-backend-base
build-backend-base: docker-login
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(call check_image_on_ecr,backend-base,$(base_backend_image_tag)) || \
	$(docker) build --file ../../govtool/backend/Dockerfile.base --tag "$(repo_url)/backend-base:$(base_backend_image_tag)" ../../govtool/backend
	@echo "Using backend-base image: $(repo_url)/backend-base:$(base_backend_image_tag)"

.PHONY: push-backend
push-backend: docker-login push-backend-base build-backend
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(call check_image_on_ecr,backend,$(backend_image_tag)) || \
	$(docker) push $(repo_url)/backend:$(backend_image_tag)

.PHONY: push-backend-base
push-backend-base: docker-login build-backend-base
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(call check_image_on_ecr,backend-base,$(base_backend_image_tag)) || \
	$(docker) push $(repo_url)/backend-base:$(base_backend_image_tag)

.PHONY: build-frontend
build-frontend: docker-login
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@:$(call check_defined, gtm_id)
	@:$(call check_defined, sentry_dsn)
	if [[ "$(cardano_network)" = "mainnet" ]]; then NETWORK_FLAG=1; else NETWORK_FLAG=0; fi; \
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) build --tag "$(repo_url)/frontend:$(frontend_image_tag)" \
		--build-arg VITE_BASE_URL="https://$(domain)/api" \
		--build-arg VITE_GTM_ID="$(gtm_id)" \
		--build-arg VITE_NETWORK_FLAG="$$NETWORK_FLAG" \
		--build-arg VITE_SENTRY_DSN="$(sentry_dsn)" \
		../../govtool/frontend

.PHONY: push-frontend
push-frontend: docker-login build-frontend
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) push $(repo_url)/frontend:$(frontend_image_tag)

.PHONY: deploy-stack
deploy-stack: docker-login
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
	ssh-keyscan $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) pull; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) up -d

.PHONY: destroy-cardano-node-and-dbsync
destroy-cardano-node-and-dbsync:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	ssh-keyscan $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	containers="$$(docker container ls --no-trunc --format '{{.Names}}' -f name=cardano-node -f name=cardano-db-sync -f name=postgres)"; \
	volumes="$$(docker volume ls --format '{{.Name}}' -f name=db-sync-data -f name=node-db -f name=node-ipc -f name=postgres)"; \
	$(docker) container stop $${containers}; \
	$(docker) container rm $${containers}; \
	$(docker) volume rm $${volumes}

.PHONY: toggle-maintenance
toggle-maintenance:
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
	ssh-keyscan $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	if [[ "$(maintenance)" = "enable" ]]; then $(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) exec frontend touch /var/run/maintenance_enabled; \
	else $(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) exec frontend rm /var/run/maintenance_enabled; fi

.DEFAULT_GOAL := info
.PHONY: info
info:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@echo "+-----------"
	@echo "|  TIME      $(shell date +'%Y-%m-%d %H:%M:%S%z')"
	@echo "|  BRANCH    $(branch) [$(shell git rev-parse HEAD)]"
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
	curl -X POST https://slack.com/api/chat.postMessage\
		-H "Authorization: Bearer $(grafana_slack_oauth_token)" \
		-H "Content-Type: application/json; charset=utf-8" \
		--data "{ \"channel\":\"$(grafana_slack_recipient)\", \"text\":\":rocket: *Deploy performed on \`$(env)\`*\n- from *branch* \`$(branch)\` (\`$(commit)\`),\n- using *Cardano Node* version \`$(cardano_node_image_tag)\`,\n- using *Cardano DB Sync* version \`$(cardano_db_sync_image_tag)\`,\n- using *GovTool backend* version \`$(backend_image_tag)\`,\n- using *Govtool frontend* version \`$(frontend_image_tag)\`.\n$(pipeline_info)\" }"

.PHONY: ssh
ssh:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	export TERM=xterm-256color; \
	ssh $(ssh_url)

.PHONY: docker
docker:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@:$(call check_defined, grafana_admin_password)
	@:$(call check_defined, cmd)
	export CARDANO_NETWORK=$(cardano_network); \
	export DOCKER_HOST=ssh://$(ssh_url); \
	export ENVIRONMENT=$(env); \
	export GRAFANA_ADMIN_PASSWORD=$(grafana_admin_password); \
	export BACKEND_TAG=$(backend_image_tag); \
	export FRONTEND_TAG=$(frontend_image_tag); \
	export CARDANO_NODE_TAG=$(cardano_node_image_tag); \
	export CARDANO_DB_SYNC_TAG=$(cardano_db_sync_image_tag); \
	ssh-keyscan $(docker_host) 2>/dev/null >> ~/.ssh/known_hosts; \
	$(docker) compose -f $(docker_compose_file) -p $(compose_stack_name) $(cmd)
