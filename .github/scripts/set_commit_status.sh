#!/bin/bash

# Ensure required environment variables are set
if [ -z "$GITHUB_REPOSITORY" ] || [ -z "$GITHUB_SHA" ] || [ -z "$GITHUB_TOKEN" ] || [ -z "$GITHUB_RUN_ID" ]; then
  echo "Missing required environment variables!"
  exit 1
fi

# Retrieve necessary variables from workflow
START_TIME=${START_TIME:-$(date +%s)}
TEST_STATUS=${TEST_STATUS:-"failure"}
REPORT_NUMBER=${REPORT_NUMBER:-1}
REPORT_NAME=${REPORT_NAME:-"govtool-frontend"}
HOST_URL=${HOST_URL:-"https://govtool.cardanoapi.io"}
CONTEXT="Playwright Tests : $HOST_URL"

if [[ "$REPORT_NAME" == "govtool-backend" ]]; then
  CONTEXT="Backend Tests : $BASE_URL"
fi

# Parse Allure JSON results
if [ -z "$(ls -A allure-results/*.json 2>/dev/null)" ]; then
  echo "No Allure JSON results found!"
  PASSED=0
  FAILED=0
  TOTAL=0
else
  PASSED=$(jq -s '[.[] | select(.status == "passed")] | length' allure-results/*.json)
  FAILED=$(jq -s '[.[] | select(.status == "failed")] | length' allure-results/*.json)
  TOTAL=$((PASSED + FAILED))
fi



# Calculate test duration
CURRENT_TIME=$(date +%s)
TEST_DURATION_SECONDS=$((CURRENT_TIME - START_TIME))
TEST_DURATION_MINUTES=$((TEST_DURATION_SECONDS / 60))
TEST_DURATION_HOURS=$((TEST_DURATION_MINUTES / 60))

if [ "$TEST_DURATION_HOURS" -gt 0 ]; then
  TEST_DURATION="${TEST_DURATION_HOURS} hour$( [ "$TEST_DURATION_HOURS" -ne 1 ] && echo "s"), $((TEST_DURATION_MINUTES % 60)) minute$( [ "$((TEST_DURATION_MINUTES % 60))" -ne 1 ] && echo "s"), and $((TEST_DURATION_SECONDS % 60)) second$( [ "$((TEST_DURATION_SECONDS % 60))" -ne 1 ] && echo "s")"
elif [ "$TEST_DURATION_MINUTES" -gt 0 ]; then
  TEST_DURATION="${TEST_DURATION_MINUTES} minute$( [ "$TEST_DURATION_MINUTES" -ne 1 ] && echo "s") and $((TEST_DURATION_SECONDS % 60)) second$( [ "$((TEST_DURATION_SECONDS % 60))" -ne 1 ] && echo "s")"
else
  TEST_DURATION="${TEST_DURATION_SECONDS} second$( [ "$TEST_DURATION_SECONDS" -ne 1 ] && echo "s")"
fi

# Determine target URL based on environment
case "$GH_PAGES" in
  "IntersectMBO/govtool-test-reports") TARGET_URL="https://intersectmbo.github.io/govtool-test-reports/${REPORT_NAME}/${REPORT_NUMBER}" ;;
  "cardanoapi/govtool-test-reports") TARGET_URL="https://cardanoapi.github.io/govtool-test-reports/${REPORT_NAME}/${REPORT_NUMBER}" ;;
  *) TARGET_URL="https://intersectmbo.github.io/govtool-test-reports/${REPORT_NAME}/${REPORT_NUMBER}" ;;
esac

# Determine test result message
if [[ "$TEST_STATUS" == "success" ]]; then
  DESCRIPTION="Tests passed in ${TEST_DURATION}"
elif [[ "$TEST_STATUS" == "failure" && "$TOTAL" -ne 0 ]]; then
  DESCRIPTION="Tests failed in ${TEST_DURATION}: Passed ${PASSED}, Failed ${FAILED} out of ${TOTAL}"
else
  DESCRIPTION="⚠️ Tests execution failed :$TEST_STATUS"
  TEST_STATUS="error"
  TARGET_URL="https://github.com/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"
fi



# Send commit status update to GitHub
curl -X POST -H "Authorization: Bearer ${GITHUB_TOKEN}" \
  -H "Accept: application/vnd.github+json" \
  https://api.github.com/repos/${GITHUB_REPOSITORY}/statuses/${GITHUB_SHA} \
  -d "{\"state\": \"${TEST_STATUS}\", \"context\": \"${CONTEXT}\", \"description\": \"${DESCRIPTION}\", \"target_url\": \"${TARGET_URL}\"}"

echo "Commit status updated successfully!"
