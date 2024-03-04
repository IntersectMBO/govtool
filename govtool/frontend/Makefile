common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-frontend

# environment variables
gtm_id := $(shell echo $${GTM_ID})
sentry_dsn := $(shell echo $${SENTRY_DSN})

# image tags
frontend_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/frontend)

.PHONY: build-frontend
build-frontend: docker-login
	@:$(call check_defined, cardano_network)
	@:$(call check_defined, gtm_id)
	@:$(call check_defined, sentry_dsn)
	if [[ "$(cardano_network)" = "mainnet" ]]; then NETWORK_FLAG=1; else NETWORK_FLAG=0; fi; \
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) build --tag "$(repo_url)/frontend:$(frontend_image_tag)" \
		--build-arg VITE_BASE_URL="https://$(domain)/api" \
		--build-arg VITE_GTM_ID="$(gtm_id)" \
		--build-arg VITE_NETWORK_FLAG="$$NETWORK_FLAG" \
		--build-arg VITE_SENTRY_DSN="$(sentry_dsn)" \
		$(root_dir)/govtool/frontend

.PHONY: push-frontend
push-frontend: build-frontend
	$(call check_image_on_ecr,frontend,$(frontend_image_tag)) || \
	$(docker) push $(repo_url)/frontend:$(frontend_image_tag)
