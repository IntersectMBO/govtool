SHELL := bash
ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

# tools
docker ?= docker
curl ?= curl
rsync ?= rsync
ssh-keyscan ?= ssh-keyscan

# environment variables
env := $(shell echo $${ENVIRONMENT})
cardano_network := $(shell echo $${CARDANO_NETWORK})

# git state
commit := $(shell git rev-parse HEAD)
branch := $(shell git rev-parse --abbrev-ref HEAD)
root_dir := $(shell git rev-parse --show-toplevel)

# target addresses
domain := $(shell echo $${DOMAIN})
repo_url ?= 733019650473.dkr.ecr.eu-west-1.amazonaws.com
docker_host := $(domain)
docker_user := ubuntu
ssh_url := $(docker_user)@$(docker_host)

# stack configuration
compose_stack_name := govtool-$(env)-$(cardano_network)

# helper function for checking undefined variables
check_defined = \
    $(strip $(foreach 1,$1, \
        $(call __check_defined,$1,$(strip $(value 2)))))
__check_defined = \
    $(if $(value $1),, \
      $(error Undefined $1$(if $2, ($2))))
			
force_rebuild := $(shell echo $${FORCE_REBUILD:-false})
# helper function for checking if image exists on ECR
check_image_on_ecr = \
  if [ "$(force_rebuild)" = "true" ]; then \
    false; \
  else \
    $(docker) manifest inspect "$(repo_url)/$1:$2" > /dev/null 2>&1; \
  fi

.PHONY: check-env-defined
check-env-defined:
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, env)
	@grep -q "module \"$(compose_stack_name)\"" ../../infra/terraform/main.tf && \
	echo "Environment $(env) for network $(cardano_network) is defined in Terraform" || \
	{ echo "Environment $(env) for network $(cardano_network) is NOT defined in Terraform, cannot deploy there"; exit 1; }

.PHONY: ssh-client-setup
ssh-client-setup:
	if [[ "$${CI}" == "true" ]]; then mkdir -p ~/.ssh; sed -e "s/DOCKER_HOST/$(docker_host)/g" ./config/templates/ssh-client-config > ~/.ssh/config; fi

.PHONY: docker-login
docker-login: ssh-client-setup
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/analytics-dashboard
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/backend
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/frontend
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/metadata-validation
	aws ecr get-login-password --region eu-west-1 | $(docker) login --username AWS --password-stdin $(repo_url)/status-service
