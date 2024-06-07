common_mk := common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-frontend

# image tags
frontend_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/frontend)-$(env)

.PHONY: build-frontend
build-frontend: docker-login
	@:$(call check_defined, cardano_network)
	if [[ "$(cardano_network)" = "mainnet" ]]; then NETWORK_FLAG=1; else NETWORK_FLAG=0; fi; \
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) build --tag "$(repo_url)/frontend:$(frontend_image_tag)" \
		--build-arg VITE_BASE_URL="https://$(domain)/api" \
		--build-arg VITE_GTM_ID="$${GTM_ID}" \
		--build-arg VITE_NETWORK_FLAG="$$NETWORK_FLAG" \
		--build-arg VITE_SENTRY_DSN="$${SENTRY_DSN}" \
		--build-arg VITE_USERSNAP_SPACE_API_KEY="$${USERSNAP_SPACE_API_KEY}" \
		--build-arg VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED="$${IS_PROPOSAL_DISCUSSION_FORUM_ENABLED}" \
		--build-arg NPMRC_TOKEN="$${NPMRC_TOKEN}" \
		$(root_dir)/govtool/frontend

.PHONY: push-frontend
push-frontend: build-frontend
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) push $(repo_url)/frontend:$(frontend_image_tag)
