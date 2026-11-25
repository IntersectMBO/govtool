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

# The container's DNS servers, for upstreams resolved at run time (Umami and
# the SEO backend), so nginx starts even before they resolve.
DNS_RESOLVERS="$(awk '
  /^nameserver[[:space:]]+/ {
    if ($2 ~ /:/) {
      printf "[%s] ", $2
    } else {
      printf "%s ", $2
    }
  }
' /etc/resolv.conf)"

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

    if [ -z "$DNS_RESOLVERS" ]; then
      echo "WARNING: Umami analytics is disabled because no DNS resolver is configured in the container." >&2
      UMAMI_SCRIPT=""
    else
      UMAMI_UPSTREAM_CONFIG="upstream umami_backend {
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

# SEO: nginx fills index.html's SSI includes from govtool-backend's /seo
# routes, and proxies /sitemap.xml and the /og/ cards to it. SEO_BACKEND_URL
# is the backend's internal origin (http://backend:9999); SITE_URL the public
# origin (https://gov.tools); SEO_NOINDEX=true keeps a test site out of
# search results. Without a backend the page keeps index.html's defaults.
ORIGIN_PATTERN='^https?://[A-Za-z0-9.-]+(:[0-9]{1,5})?$'
SEO_BACKEND_URL_VALUE="${SEO_BACKEND_URL:-}"
SEO_BACKEND_URL_VALUE="${SEO_BACKEND_URL_VALUE%/}"
SITE_URL_VALUE="${SITE_URL:-}"
SITE_URL_VALUE="${SITE_URL_VALUE%/}"
SEO_NOINDEX_VALUE="$(printf '%s' "${SEO_NOINDEX:-}" | tr '[:upper:]' '[:lower:]')"
SEO_UPSTREAM_CONFIG=""
SEO_SERVER_CONFIG=""

if [ -n "$SITE_URL_VALUE" ] && ! printf '%s' "$SITE_URL_VALUE" | grep -Eq "$ORIGIN_PATTERN"; then
  echo "WARNING: SITE_URL is ignored because it must be an http(s) origin such as https://gov.tools." >&2
  SITE_URL_VALUE=""
fi

if [ -n "$SEO_BACKEND_URL_VALUE" ] && ! printf '%s' "$SEO_BACKEND_URL_VALUE" | grep -Eq "$ORIGIN_PATTERN"; then
  echo "WARNING: SEO tags are disabled because SEO_BACKEND_URL must be an http(s) origin such as http://backend:9999." >&2
  SEO_BACKEND_URL_VALUE=""
fi
if [ -n "$SEO_BACKEND_URL_VALUE" ] && [ -z "$DNS_RESOLVERS" ]; then
  echo "WARNING: SEO tags are disabled because no DNS resolver is configured in the container." >&2
  SEO_BACKEND_URL_VALUE=""
fi

case "$SEO_NOINDEX_VALUE" in
  true|yes|1) SEO_SERVER_CONFIG='  add_header X-Robots-Tag "noindex" always;' ;;
  ""|false|no|0) ;;
  *) echo "WARNING: SEO_NOINDEX must be true/false, yes/no, or 1/0; treating it as false." >&2 ;;
esac

if [ -n "$SEO_BACKEND_URL_VALUE" ]; then
  SEO_SCHEME="${SEO_BACKEND_URL_VALUE%%://*}"
  SEO_AUTHORITY="${SEO_BACKEND_URL_VALUE#*://}"
  SEO_SSL_CONFIG=""
  if [ "$SEO_SCHEME" = "https" ]; then
    SEO_SSL_CONFIG="    proxy_ssl_server_name on;
    proxy_ssl_name ${SEO_AUTHORITY%%:*};"
  fi
  # The backend answers only direct calls (src/seo/direct-only.guard.ts), so
  # the gateway's proxy headers, which a page request carries, are dropped.
  SEO_PROXY_COMMON="    proxy_http_version 1.1;
    proxy_set_header Host ${SEO_AUTHORITY};
    proxy_set_header Cookie \"\";
    proxy_set_header Authorization \"\";
    proxy_set_header X-Forwarded-For \"\";
    proxy_set_header X-Real-IP \"\";
    proxy_set_header Forwarded \"\";
    proxy_set_header X-Forwarded-Host \"\";
    proxy_set_header X-Forwarded-Proto \"\";
    proxy_set_header X-Forwarded-Port \"\";
${SEO_SSL_CONFIG}"

  SEO_UPSTREAM_CONFIG="upstream seo_backend {
  zone seo_backend 64k;
  server ${SEO_AUTHORITY} resolve;
}

proxy_cache_path /var/cache/nginx/seo_og levels=1:2 keys_zone=seo_og_cache:10m max_size=200m inactive=365d use_temp_path=off;
limit_req_zone \$binary_remote_addr zone=seo_og_rate:10m rate=10r/s;"

  # /__seo/ is reached only through the SSI includes. A failure or a slow
  # answer becomes an empty body, which the include replaces with its stub.
  SEO_LOCATIONS_CONFIG="  location /__seo/ {
    internal;
    proxy_pass ${SEO_SCHEME}://seo_backend/seo/;
${SEO_PROXY_COMMON}
    proxy_set_header X-Seo-Path \$request_uri;
    proxy_set_header Accept-Encoding \"\";
    proxy_connect_timeout 500ms;
    proxy_send_timeout 500ms;
    proxy_read_timeout 500ms;
    proxy_intercept_errors on;
    error_page 400 404 500 502 503 504 = @seo_empty;
  }

  # Asked for /__seo/ directly, the internal check's 404 lands here too;
  # in an SSI subrequest \$request_uri is still the page's own URI.
  location @seo_empty {
    if (\$request_uri ~ \"^/__seo/\") {
      return 404;
    }
    return 204;
  }

  location = /sitemap.xml {
    proxy_pass ${SEO_SCHEME}://seo_backend/seo/sitemap.xml;
${SEO_PROXY_COMMON}
    proxy_read_timeout 30s;
  }

  # Cards are addressed by a hash of what they show, so a cached one never
  # goes stale; limit_req keeps a loop over every id off the renderer.
  location /og/ {
${REAL_IP_CONFIG}
    limit_req zone=seo_og_rate burst=30 nodelay;
    limit_req_status 429;
    proxy_cache seo_og_cache;
    proxy_cache_key \$uri;
    # Only cards: a redirect to the current hash must never be cached.
    proxy_cache_valid 200 365d;
    proxy_cache_valid 404 1m;
    proxy_cache_lock on;
    proxy_ignore_headers Set-Cookie;
    add_header X-Cache-Status \$upstream_cache_status always;
    proxy_pass ${SEO_SCHEME}://seo_backend/seo/image/;
${SEO_PROXY_COMMON}
    proxy_read_timeout 15s;
  }"
else
  SEO_LOCATIONS_CONFIG="  location /__seo/ {
    internal;
    return 204;
  }

  location = /sitemap.xml {
    return 404;
  }

  location /og/ {
    return 404;
  }"
fi

RESOLVER_CONFIG=""
if [ -n "$UMAMI_UPSTREAM_CONFIG" ] || [ -n "$SEO_UPSTREAM_CONFIG" ]; then
  RESOLVER_CONFIG="resolver ${DNS_RESOLVERS}valid=30s;"
fi

ENV_SCRIPT="<script>
window.__ENV__ = {
  VITE_IS_DEV: $(json_escape "${VITE_IS_DEV:-}"),
  VITE_APP_ENV: $(json_escape "${VITE_APP_ENV:-}"),
  VITE_BASE_URL: $(json_escape "${VITE_BASE_URL:-}"),
  VITE_NETWORK_FLAG: $(json_escape "${VITE_NETWORK_FLAG:-}"),
  VITE_SENTRY_DSN: $(json_escape "${VITE_SENTRY_DSN:-}"),
  VITE_CHATWOOT_URL: $(json_escape "${VITE_CHATWOOT_URL:-}"),
  VITE_CHATWOOT_WEBSITE_TOKEN: $(json_escape "${VITE_CHATWOOT_WEBSITE_TOKEN:-}"),
  VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: $(json_escape "${VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED:-}"),
  VITE_IS_CIP179_ENABLED: $(json_escape "${VITE_IS_CIP179_ENABLED:-true}"),
  VITE_PDF_API_URL: $(json_escape "${VITE_PDF_API_URL:-}"),
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

# The default og:image needs an absolute URL; without SITE_URL the line goes.
if [ -n "$SITE_URL_VALUE" ]; then
  sed -i "s#__SITE_URL__#${SITE_URL_VALUE}#g" /usr/share/nginx/html/index.html
else
  sed -i '/__SITE_URL__/d' /usr/share/nginx/html/index.html
fi

# robots.txt allows everything: a page blocked here could never show the
# crawler its noindex. The sitemap is named only when the backend serves it.
{
  printf 'User-agent: *\nAllow: /\n'
  if [ -n "$SITE_URL_VALUE" ] && [ -n "$SEO_BACKEND_URL_VALUE" ]; then
    printf '\nSitemap: %s/sitemap.xml\n' "$SITE_URL_VALUE"
  fi
} > /usr/share/nginx/html/robots.txt

awk -v resolver="$RESOLVER_CONFIG" -v umami_upstream="$UMAMI_UPSTREAM_CONFIG" -v umami_proxy="$UMAMI_PROXY_CONFIG" -v seo_upstream="$SEO_UPSTREAM_CONFIG" -v seo_server="$SEO_SERVER_CONFIG" -v seo_locations="$SEO_LOCATIONS_CONFIG" '
  /# RESOLVER_CONFIG/ {
    if (resolver != "") {
      print resolver
    }
    next
  }
  /# SEO_UPSTREAM_CONFIG/ {
    if (seo_upstream != "") {
      print seo_upstream
    }
    next
  }
  /# SEO_SERVER_CONFIG/ {
    if (seo_server != "") {
      print seo_server
    }
    next
  }
  /# SEO_LOCATIONS_CONFIG/ {
    print seo_locations
    next
  }
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
