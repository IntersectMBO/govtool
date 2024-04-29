common_mk := common.mk
ifeq ($(origin $(common_mk)), undefined)
  $(eval $(common_mk) := included)
  include $(common_mk)
endif

.DEFAULT_GOAL := push-analytics-dashboard

# image tags
analytics_dashboard_image_tag := $(shell git log -n 1 --format="%H" -- $(root_dir)/govtool/analytics-dashboard)

.PHONY: build-analytics-dashboard
build-analytics-dashboard:
	$(call check_image_on_ecr,analytics-dashboard,$(analytics_dashboard_image_tag)) || \
	$(docker) build --tag "$(repo_url)/analytics-dashboard:$(analytics_dashboard_image_tag)" \
		--build-arg NEXT_PUBLIC_API_URL="$${NEXT_PUBLIC_API_URL}" \
		$(root_dir)/govtool/analytics-dashboard

.PHONY: push-analytics-dashboard
push-analytics-dashboard: build-analytics-dashboard
	$(call check_image_on_ecr,analytics-dashboard,$(analytics_dashboard_image_tag)) || \
	$(docker) push $(repo_url)/analytics-dashboard:$(analytics_dashboard_image_tag)
