common_mk := common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-custom-cardano-db-sync

# image tags
custom-cardano-db-sync_image_tag := latest

.PHONY: build-custom-cardano-db-sync
build-custom-cardano-db-sync: docker-login
	@:$(call check_defined, cardano_network)
	$(call check_image_on_ecr,custom-cardano-db-sync,$(custom-cardano-db-sync_image_tag)) || \
	$(docker) build --tag "$(repo_url)/custom-cardano-db-sync:$(custom-cardano-db-sync_image_tag)" --file "$(root_dir)/scripts/govtool/custom-cardano-db-sync.Dockerfile"\
		$(root_dir)/scripts/govtool

.PHONY: push-custom-cardano-db-sync
push-custom-cardano-db-sync: build-custom-cardano-db-sync
	$(call check_image_on_ecr,custom-cardano-db-sync,$(custom-cardano-db-sync_image_tag)) || \
	$(docker) push $(repo_url)/custom-cardano-db-sync:$(custom-cardano-db-sync_image_tag)
