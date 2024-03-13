common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-backend

# image tags
base_backend_image_tag := $(shell git hash-object $(root_dir)/govtool/backend/vva-be.cabal)
backend_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/backend)

.PHONY: build-backend-base
build-backend-base: docker-login
	$(call check_image_on_ecr,backend-base,$(base_backend_image_tag)) || \
	$(docker) build --file $(root_dir)/govtool/backend/Dockerfile.base --tag "$(repo_url)/backend-base:$(base_backend_image_tag)" $(root_dir)/govtool/backend

.PHONY: push-backend-base
push-backend-base: build-backend-base
	$(call check_image_on_ecr,backend-base,$(base_backend_image_tag)) || \
	$(docker) push $(repo_url)/backend-base:$(base_backend_image_tag)

.PHONY: build-backend
build-backend: build-backend-base
	$(call check_image_on_ecr,backend,$(backend_image_tag)) || \
	$(docker) build --build-arg BASE_IMAGE_TAG=$(base_backend_image_tag) --tag "$(repo_url)/backend:$(backend_image_tag)" $(root_dir)/govtool/backend

.PHONY: push-backend
push-backend: push-backend-base build-backend
	$(call check_image_on_ecr,backend,$(backend_image_tag)) || \
	$(docker) push $(repo_url)/backend:$(backend_image_tag)
