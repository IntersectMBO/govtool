common_mk := ../../scripts/govtool/common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-metadata-validation

# image tags
metadata_validation_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/metadata-validation)

.PHONY: build-metadata-validation
build-metadata-validation: docker-login
	$(call check_image_on_ecr,metadata-validation,$(metadata_validation_image_tag)) || \
	$(docker) build --tag "$(repo_url)/metadata-validation:$(metadata_validation_image_tag)" \
		$(root_dir)/govtool/metadata-validation

.PHONY: push-metadata-validation
push-metadata-validation: build-metadata-validation
	$(call check_image_on_ecr,metadata-validation,$(metadata_validation_image_tag)) || \
	$(docker) push $(repo_url)/metadata-validation:$(metadata_validation_image_tag)
