common_mk := common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-status-service

# image tags
status_service_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/status-service)

.PHONY: build-status-service
build-status-service: docker-login
	$(call check_image_on_ecr,status-service,$(status_service_image_tag)) || \
	$(docker) build --tag "$(repo_url)/status-service:$(status_service_image_tag)" \
		$(root_dir)/govtool/status-service

.PHONY: push-status-service
push-status-service: build-status-service
	$(call check_image_on_ecr,status-service,$(status_service_image_tag)) || \
	$(docker) push $(repo_url)/status-service:$(status_service_image_tag)
