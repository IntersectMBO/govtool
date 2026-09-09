#!/bin/sh
set -eu

json_escape() {
  jq -Rn --arg value "${1:-}" '$value'
}

ENV_SCRIPT="<script>
window.__ENV__ = {
  VITE_IS_DEV: $(json_escape "${VITE_IS_DEV:-}"),
  VITE_APP_ENV: $(json_escape "${VITE_APP_ENV:-}"),
  VITE_BASE_URL: $(json_escape "${VITE_BASE_URL:-}"),
  VITE_METADATA_API_URL: $(json_escape "${VITE_METADATA_API_URL:-}"),
  VITE_GTM_ID: $(json_escape "${VITE_GTM_ID:-}"),
  VITE_NETWORK_FLAG: $(json_escape "${VITE_NETWORK_FLAG:-}"),
  VITE_SENTRY_DSN: $(json_escape "${VITE_SENTRY_DSN:-}"),
  VITE_USERSNAP_SPACE_API_KEY: $(json_escape "${VITE_USERSNAP_SPACE_API_KEY:-}"),
  VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: $(json_escape "${VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED:-}"),
  VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: $(json_escape "${VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED:-}"),
  VITE_IS_CIP179_ENABLED: $(json_escape "${VITE_IS_CIP179_ENABLED:-false}"),
  VITE_PDF_API_URL: $(json_escape "${VITE_PDF_API_URL:-}"),
  VITE_OUTCOMES_API_URL: $(json_escape "${VITE_OUTCOMES_API_URL:-}"),
  VITE_IPFS_GATEWAY: $(json_escape "${VITE_IPFS_GATEWAY:-}"),
  VITE_IPFS_PROJECT_ID: $(json_escape "${VITE_IPFS_PROJECT_ID:-}")
};
</script>"

awk -v env_script="$ENV_SCRIPT" '
  /<\/head>/ {
    print env_script
  }
  { print }
' /usr/share/nginx/html/index.html > /tmp/index.html

mv /tmp/index.html /usr/share/nginx/html/index.html

rm -f /usr/share/nginx/html/index.html.br /usr/share/nginx/html/index.html.gz

exec nginx -g "daemon off;"