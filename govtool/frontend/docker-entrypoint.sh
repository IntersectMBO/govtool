#!/bin/sh
set -eu

json_escape() {
  jq -Rn --arg value "${1:-}" '$value'
}

html_escape() {
  jq -Rnr --arg value "${1:-}" '$value | @html'
}

TRUSTED_PROXY_CIDRS_VALUE="${TRUSTED_PROXY_CIDRS:-}"
REAL_IP_HEADER_VALUE="${REAL_IP_HEADER:-X-Forwarded-For}"
REAL_IP_CONFIG=""

# Real IP resolution applies only to the Umami proxy location, so
# $remote_addr stays the direct peer everywhere else. Unset means trust
# the private ranges reverse proxies normally connect from; internet
# clients cannot spoof these as a source address. Set
# TRUSTED_PROXY_CIDRS=none to disable real IP resolution entirely.
if [ -z "$TRUSTED_PROXY_CIDRS_VALUE" ]; then
  TRUSTED_PROXY_CIDRS_VALUE="10.0.0.0/8 172.16.0.0/12 192.168.0.0/16 fc00::/7"
elif [ "$(printf '%s' "$TRUSTED_PROXY_CIDRS_VALUE" | tr '[:upper:]' '[:lower:]')" = "none" ]; then
  TRUSTED_PROXY_CIDRS_VALUE=""
fi

if [ -n "$TRUSTED_PROXY_CIDRS_VALUE" ]; then
  if ! printf '%s' "$REAL_IP_HEADER_VALUE" | grep -Eq '^[A-Za-z0-9-]+$'; then
    echo "WARNING: Real IP resolution is disabled because REAL_IP_HEADER contains unsupported characters." >&2
  else
    SET_REAL_IP_FROM_DIRECTIVES=""
    for TRUSTED_PROXY_CIDR in $(printf '%s' "$TRUSTED_PROXY_CIDRS_VALUE" | tr ',' ' '); do
      if printf '%s' "$TRUSTED_PROXY_CIDR" | grep -Eq '^[0-9A-Fa-f:.]+(/[0-9]{1,3})?$'; then
        SET_REAL_IP_FROM_DIRECTIVES="${SET_REAL_IP_FROM_DIRECTIVES}    set_real_ip_from ${TRUSTED_PROXY_CIDR};
"
      else
        echo "WARNING: Ignoring TRUSTED_PROXY_CIDRS entry with unsupported characters: ${TRUSTED_PROXY_CIDR}" >&2
      fi
    done

    if [ -n "$SET_REAL_IP_FROM_DIRECTIVES" ]; then
      REAL_IP_CONFIG="${SET_REAL_IP_FROM_DIRECTIVES}    real_ip_header ${REAL_IP_HEADER_VALUE};
    real_ip_recursive on;"
    fi
  fi
fi

UMAMI_URL_VALUE="${UMAMI_URL:-}"
UMAMI_WEBSITE_ID_VALUE="${UMAMI_WEBSITE_ID:-}"
UMAMI_SSL_VERIFY_VALUE="$(printf '%s' "${UMAMI_SSL_VERIFY:-true}" | tr '[:upper:]' '[:lower:]')"
UMAMI_SCRIPT=""
UMAMI_UPSTREAM_CONFIG=""
UMAMI_PROXY_CONFIG=""

case "$UMAMI_SSL_VERIFY_VALUE" in
  true|yes|1) UMAMI_SSL_VERIFY_ENABLED="on" ;;
  false|no|0) UMAMI_SSL_VERIFY_ENABLED="off" ;;
  *)
    echo "WARNING: UMAMI_SSL_VERIFY must be true/false, yes/no, or 1/0; defaulting to true." >&2
    UMAMI_SSL_VERIFY_ENABLED="on"
    ;;
esac

if [ -n "$UMAMI_URL_VALUE" ] && [ -n "$UMAMI_WEBSITE_ID_VALUE" ]; then
  case "$UMAMI_URL_VALUE" in
    http://[!/]*|https://[!/]*) ;;
    *)
      echo "WARNING: Umami analytics is disabled because UMAMI_URL must be a valid http:// or https:// URL." >&2
      UMAMI_URL_VALUE=""
      ;;
  esac

  if [ -n "$UMAMI_URL_VALUE" ] && printf '%s' "$UMAMI_URL_VALUE" | grep -q '[[:space:];{}@?#]'; then
    echo "WARNING: Umami analytics is disabled because UMAMI_URL contains unsupported characters." >&2
    UMAMI_URL_VALUE=""
  fi

  if [ -n "$UMAMI_URL_VALUE" ]; then
    while [ "${UMAMI_URL_VALUE%/}" != "$UMAMI_URL_VALUE" ]; do
      UMAMI_URL_VALUE="${UMAMI_URL_VALUE%/}"
    done

    UMAMI_SCRIPT="<script defer src=\"/x/script.js\" data-website-id=\"$(html_escape "$UMAMI_WEBSITE_ID_VALUE")\"></script>"

    UMAMI_SCHEME="${UMAMI_URL_VALUE%%://*}"
    UMAMI_URL_REMAINDER="${UMAMI_URL_VALUE#*://}"
    UMAMI_AUTHORITY="${UMAMI_URL_REMAINDER%%/*}"
    if [ "$UMAMI_AUTHORITY" = "$UMAMI_URL_REMAINDER" ]; then
      UMAMI_BASE_PATH=""
    else
      UMAMI_BASE_PATH="/${UMAMI_URL_REMAINDER#*/}"
    fi

    DNS_RESOLVERS="$(awk '
      /^nameserver[[:space:]]+/ {
        if ($2 ~ /:/) {
          printf "[%s] ", $2
        } else {
          printf "%s ", $2
        }
      }
    ' /etc/resolv.conf)"

    if [ -z "$DNS_RESOLVERS" ]; then
      echo "WARNING: Umami analytics is disabled because no DNS resolver is configured in the container." >&2
      UMAMI_SCRIPT=""
    else
      UMAMI_UPSTREAM_CONFIG="resolver ${DNS_RESOLVERS}valid=30s;

upstream umami_backend {
  zone umami_backend 64k;
  server ${UMAMI_AUTHORITY} resolve;
}"

      PROXY_SSL_CONFIG=""
      case "$UMAMI_SCHEME" in
        https)
          case "$UMAMI_AUTHORITY" in
            \[*\]*)
              UMAMI_TLS_NAME="${UMAMI_AUTHORITY#\[}"
              UMAMI_TLS_NAME="${UMAMI_TLS_NAME%%\]*}"
              ;;
            *:*) UMAMI_TLS_NAME="${UMAMI_AUTHORITY%%:*}" ;;
            *) UMAMI_TLS_NAME="$UMAMI_AUTHORITY" ;;
          esac

          PROXY_SSL_CONFIG="    proxy_ssl_server_name on;
    proxy_ssl_name ${UMAMI_TLS_NAME};
    proxy_ssl_verify ${UMAMI_SSL_VERIFY_ENABLED};"
          if [ "$UMAMI_SSL_VERIFY_ENABLED" = "on" ]; then
            PROXY_SSL_CONFIG="${PROXY_SSL_CONFIG}
    proxy_ssl_trusted_certificate /etc/ssl/certs/ca-certificates.crt;"
          fi
          ;;
      esac

      UMAMI_PROXY_CONFIG="  location /x/ {
${REAL_IP_CONFIG}
    proxy_pass ${UMAMI_SCHEME}://umami_backend${UMAMI_BASE_PATH}/;
    proxy_set_header Host ${UMAMI_AUTHORITY};
    proxy_set_header X-Real-IP \$remote_addr;
    proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
    proxy_set_header X-Forwarded-Proto \$scheme;
${PROXY_SSL_CONFIG}
  }"
    fi
  fi
elif [ -n "$UMAMI_URL_VALUE" ] || [ -n "$UMAMI_WEBSITE_ID_VALUE" ]; then
  echo "WARNING: Umami analytics is disabled because both UMAMI_URL and UMAMI_WEBSITE_ID must be set." >&2
fi

ENV_SCRIPT="<script>
window.__ENV__ = {
  VITE_IS_DEV: $(json_escape "${VITE_IS_DEV:-}"),
  VITE_APP_ENV: $(json_escape "${VITE_APP_ENV:-}"),
  VITE_BASE_URL: $(json_escape "${VITE_BASE_URL:-}"),
  VITE_METADATA_API_URL: $(json_escape "${VITE_METADATA_API_URL:-}"),
  VITE_NETWORK_FLAG: $(json_escape "${VITE_NETWORK_FLAG:-}"),
  VITE_SENTRY_DSN: $(json_escape "${VITE_SENTRY_DSN:-}"),
  VITE_CHATWOOT_URL: $(json_escape "${VITE_CHATWOOT_URL:-}"),
  VITE_CHATWOOT_WEBSITE_TOKEN: $(json_escape "${VITE_CHATWOOT_WEBSITE_TOKEN:-}"),
  VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: $(json_escape "${VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED:-}"),
  VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: $(json_escape "${VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED:-}"),
  VITE_PDF_API_URL: $(json_escape "${VITE_PDF_API_URL:-}"),
  VITE_OUTCOMES_API_URL: $(json_escape "${VITE_OUTCOMES_API_URL:-}"),
  VITE_IPFS_GATEWAY: $(json_escape "${VITE_IPFS_GATEWAY:-}"),
  VITE_IPFS_PROJECT_ID: $(json_escape "${VITE_IPFS_PROJECT_ID:-}")
};
</script>
${UMAMI_SCRIPT}"

awk -v env_script="$ENV_SCRIPT" '
  /<\/head>/ {
    print env_script
  }
  { print }
' /usr/share/nginx/html/index.html > /tmp/index.html

mv /tmp/index.html /usr/share/nginx/html/index.html

rm -f /usr/share/nginx/html/index.html.br /usr/share/nginx/html/index.html.gz

awk -v umami_upstream="$UMAMI_UPSTREAM_CONFIG" -v umami_proxy="$UMAMI_PROXY_CONFIG" '
  /# UMAMI_UPSTREAM_CONFIG/ {
    if (umami_upstream != "") {
      print umami_upstream
    }
    next
  }
  /# UMAMI_PROXY_CONFIG/ {
    if (umami_proxy != "") {
      print umami_proxy
    }
    next
  }
  { print }
' /etc/nginx/conf.d/default.conf > /tmp/default.conf

mv /tmp/default.conf /etc/nginx/conf.d/default.conf

nginx -t

exec nginx -g "daemon off;"
