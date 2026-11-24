#!/usr/bin/env bash
# Checks the frontend image's SEO wiring in Docker: nginx fills index.html's
# SSI includes from the backend's /seo routes (here scripts/seo-stub-backend.mjs),
# falls back to the defaults when the backend fails, serves robots.txt, the
# sitemap and cached cards, and sends X-Robots-Tag only with SEO_NOINDEX.
#
# Runs the runtime stage of the Dockerfile over a local `vite build`, which
# avoids the full image build. Set SKIP_BUILD=1 to reuse an existing dist/.
#
#   scripts/check-seo-nginx.sh
set -euo pipefail

HERE="$(cd "$(dirname "$0")/.." && pwd)"
WORK="$(mktemp -d)"
RUN_ID="seo-check-$$"
NET="$RUN_ID"
IMAGE="govtool-frontend:$RUN_ID"
FAILED=0

# Every container is named after RUN_ID, so cleanup finds them all, including
# those started inside $(start ...), whose subshell cannot record them.
cleanup() {
  docker ps -aq --filter "name=$RUN_ID-" | xargs docker rm -f >/dev/null 2>&1 || true
  docker network rm "$NET" >/dev/null 2>&1 || true
  docker rmi "$IMAGE" >/dev/null 2>&1 || true
  rm -rf "$WORK"
}
trap cleanup EXIT

pass() { printf '  ok    %s\n' "$1"; }
fail() { printf '  FAIL  %s\n' "$1"; FAILED=1; }
check() { if eval "$2"; then pass "$1"; else fail "$1"; fi; }

if [ -z "${SKIP_BUILD:-}" ]; then
  (cd "$HERE" && npx vite build >/dev/null)
fi

# The runtime stage of the Dockerfile, over the local dist.
mkdir -p "$WORK/ctx"
cp -R "$HERE/dist" "$WORK/ctx/dist"
cp "$HERE/nginx.conf" "$HERE/docker-entrypoint.sh" "$WORK/ctx/"
cp "$HERE/maintenance-page/index.html" "$WORK/ctx/maintenance.html"
cat > "$WORK/ctx/Dockerfile" <<'EOF'
FROM fholzer/nginx-brotli:v1.30
RUN apk add --no-cache gettext jq
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY maintenance.html /usr/share/nginx/html/maintenance.html
COPY dist /usr/share/nginx/html
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh
ENTRYPOINT ["/docker-entrypoint.sh"]
EOF
docker build -q -t "$IMAGE" "$WORK/ctx" >/dev/null
docker network create "$NET" >/dev/null

docker run -d --name "$RUN_ID-stub" --network "$NET" --network-alias seo-stub \
  -v "$HERE/scripts/seo-stub-backend.mjs:/stub.mjs:ro" node:22-alpine node /stub.mjs >/dev/null

# Starts the frontend with the given env; prints its base URL.
start() {
  local name="$RUN_ID-$1"; shift
  local args=()
  for kv in "$@"; do args+=(-e "$kv"); done
  docker run -d --name "$name" --network "$NET" -p 127.0.0.1::80 "${args[@]+"${args[@]}"}" "$IMAGE" >/dev/null
  # Docker Desktop can take a few seconds to start a container, and its
  # port is known only once it has.
  local port="" tries=0
  while [ -z "$port" ] && [ "$tries" -lt 60 ]; do
    port="$(docker port "$name" 80/tcp 2>/dev/null | head -1 | sed 's/.*://')"
    [ -n "$port" ] || { tries=$((tries + 1)); sleep 1; }
  done
  [ -n "$port" ] || { docker logs "$name" >&2; return 1; }
  # Docker Desktop's port forwarder resets connections until nginx listens,
  # which plain --retry does not retry.
  curl -s --retry 30 --retry-all-errors --retry-delay 1 -o /dev/null "http://127.0.0.1:$port/" \
    || { docker logs "$name" >&2; return 1; }
  echo "http://127.0.0.1:$port"
}

page() { curl -s "$@"; }
headers() { curl -s -o /dev/null -D - "$@" | tr -d '\r'; }

echo "With a backend and SITE_URL"
A="$(start a SEO_BACKEND_URL=http://seo-stub:9999 SITE_URL=https://example.test)"

body="$(page "$A/actions/x?q=1")"
check "head comes from the backend, with the page's full request URI" \
  '[[ "$body" == *"<title data-seo>Stub /actions/x?q=1</title>"* ]]'
GATEWAY=(-H "X-Forwarded-For: 203.0.113.7" -H "X-Real-IP: 203.0.113.7" -H "X-Forwarded-Proto: https" -H "X-Forwarded-Host: gov.tools")
check "the gateway's proxy headers never reach the backend" \
  '[[ "$(page "${GATEWAY[@]}" "$A/actions/x")" == *"<title data-seo>Stub /actions/x</title>"* ]] && [ "$(page "${GATEWAY[@]}" "$A/sitemap.xml")" = "<urlset>stub</urlset>" ] && [ "$(curl -s -o /dev/null -w "%{http_code}" "${GATEWAY[@]}" "$A/og/action/b/0123456789ab.png")" = 200 ]'
check "exactly one <title>" '[ "$(grep -o "<title" <<<"$body" | wc -l | tr -d " ")" = 1 ]'
check "body summary inside #root" \
  '[[ "$body" == *"<div id=\"root\"><main data-seo><h1>Stub body</h1></main></div>"* ]]'
check "the page is sent with Cache-Control: no-cache" \
  'headers "$A/actions/x" | grep -qi "^cache-control: no-cache"'
asset="$(grep -o '/assets/index-[^"]*\.js' <<<"$body" | head -1)"
check "hashed assets do not get no-cache" \
  '! headers "$A$asset" | grep -qi "^cache-control: no-cache"'

for mode in error empty slow; do
  start_s=$(date +%s)
  body="$(page -H "X-Stub-Mode: $mode" "$A/actions/x")"
  took=$(( $(date +%s) - start_s ))
  check "backend $mode: default title" '[[ "$body" == *"<title data-seo>Cardano GovTool</title>"* ]]'
  check "backend $mode: default og:image on SITE_URL" \
    '[[ "$body" == *"content=\"https://example.test/images/og-default.png\""* ]]'
  check "backend $mode: nothing from the backend leaks in" '[[ "$body" != *"STUB ERROR"* ]]'
  check "backend $mode: answered within 1 s of the timeout" '[ "$took" -le 2 ]'
done

check "gzip compresses the filled page" \
  '[[ "$(curl -s --compressed -H "Accept-Encoding: gzip" "$A/actions/x")" == *"<title data-seo>Stub /actions/x</title>"* ]] && headers -H "Accept-Encoding: gzip" "$A/actions/x" | grep -qi "^content-encoding: gzip"'
check "brotli compresses the filled page" \
  'headers -H "Accept-Encoding: br" "$A/actions/x" | grep -qi "^content-encoding: br"'
check "no precompressed index.html bypasses SSI" \
  '! docker exec "$RUN_ID-a" sh -c "ls /usr/share/nginx/html/index.html.br /usr/share/nginx/html/index.html.gz" >/dev/null 2>&1'
check "/__seo/head is not reachable from outside" \
  '[ "$(curl -s -o /dev/null -w "%{http_code}" "$A/__seo/head")" = 404 ]'
robots="$(page "$A/robots.txt")"
check "robots.txt allows everything" '[[ "$robots" == *"Allow: /"* && "$robots" != *"Disallow"* ]]'
check "robots.txt names the sitemap" '[[ "$robots" == *"Sitemap: https://example.test/sitemap.xml"* ]]'
check "no X-Robots-Tag without SEO_NOINDEX" '! headers "$A/" | grep -qi "^x-robots-tag"'
check "/sitemap.xml comes from the backend" '[ "$(page "$A/sitemap.xml")" = "<urlset>stub</urlset>" ]'
page -o /dev/null "$A/og/action/a/0123456789ab.png"
check "a card is cached after the first request" \
  'headers "$A/og/action/a/0123456789ab.png" | grep -qi "^x-cache-status: hit"'
check "a card keeps the backend's immutable Cache-Control" \
  'headers "$A/og/action/a/0123456789ab.png" | grep -qi "^cache-control: public, max-age=31536000, immutable"'

page -o /dev/null "$A/og/action/a/000000000000.png"
check "a card redirect is never cached" \
  '! headers "$A/og/action/a/000000000000.png" | grep -qi "^x-cache-status: hit"'

# Links from before the /actions URLs move permanently; the app maps the rest.
location_of() { headers "$1" | sed -n 's/^location: //Ip'; }
redirects_to() {
  [ "$(curl -s -o /dev/null -w "%{http_code}" "$A$1")" = 301 ] && [ "$(location_of "$A$1")" = "$2" ]
}
for pair in \
  "/governance_actions/gov_action1x?q=1 /actions/gov_action1x?q=1" \
  "/governance_actions /actions" \
  "/GOVERNANCE_ACTIONS/history /actions/history" \
  "/drep_directory/drep1x /dreps/drep1x" \
  "/connected/governance_actions/gov_action1x /governance_actions/gov_action1x" \
  "/proposal_discussion/propose /proposals/new" \
  "/proposal_discussion/42 /proposals/42" \
  "/budget_discussion/7 /budget/7" \
  "/create_governance_action /actions/new" \
  "/edit_drep /my/drep/edit" \
  "/register_drep /my/drep/register" \
  "/retire_drep /my/drep/retire" \
  "/register_direct_voter /my/direct-voter/register" \
  "/retire_direct_voter /my/direct-voter/retire" \
  "/stake_keys /my/stake-key" \
  "/my/votes_and_favorites /my/votes"; do
  set -- $pair
  check "$1 redirects to $2" "redirects_to '$1' '$2'"
done
check "the budget archive's files are not redirected" \
  '[ "$(curl -s -o /dev/null -w "%{http_code}" "$A/budget-proposals-2025/missing.json")" = 404 ]'
check "current URLs are not redirected" \
  '[ "$(curl -s -o /dev/null -w "%{http_code}" "$A/actions/x")" = 200 ]'

echo "With SEO_NOINDEX=true"
B="$(start b SEO_BACKEND_URL=http://seo-stub:9999 SITE_URL=https://example.test SEO_NOINDEX=true)"
check "X-Robots-Tag: noindex on pages" 'headers "$B/" | grep -qi "^x-robots-tag: noindex"'
check "X-Robots-Tag: noindex on robots.txt" 'headers "$B/robots.txt" | grep -qi "^x-robots-tag: noindex"'
check "robots.txt still allows everything" '[[ "$(page "$B/robots.txt")" != *"Disallow"* ]]'

echo "Without a backend or SITE_URL"
C="$(start c)"
body="$(page "$C/actions/x")"
check "default title" '[[ "$body" == *"<title data-seo>Cardano GovTool</title>"* ]]'
check "no og:image without SITE_URL, and no placeholder left" \
  '[[ "$body" != *"og:image\" content"* && "$body" != *"__SITE_URL__"* ]]'
check "/sitemap.xml is 404" '[ "$(curl -s -o /dev/null -w "%{http_code}" "$C/sitemap.xml")" = 404 ]'
check "/og/ is 404" '[ "$(curl -s -o /dev/null -w "%{http_code}" "$C/og/action/a/b.png")" = 404 ]'
check "robots.txt has no Sitemap line" '[[ "$(page "$C/robots.txt")" != *"Sitemap:"* ]]'

echo "With a backend host that does not resolve"
D="$(start d SEO_BACKEND_URL=http://nowhere.invalid:9999 SITE_URL=https://example.test)"
check "nginx starts and serves the defaults" \
  '[[ "$(page "$D/actions/x")" == *"<title data-seo>Cardano GovTool</title>"* ]]'

if [ "$FAILED" -ne 0 ]; then
  echo "SEO nginx check FAILED"
  exit 1
fi
echo "SEO nginx check passed"
