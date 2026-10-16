#!/usr/bin/env bash
# Starts a built govtool-docs image and checks that nginx serves it: the
# health check, the home page, an image, a hashed asset with the security and
# cache headers, the 404 page, and, with a base path other than /, the
# redirect from / to it. Catches a broken nginx.conf.template, which no build
# step runs.
#
# Usage: scripts/smoke-test-image.sh <image> [base path, default /]
set -euo pipefail

image="$1"
base="${2:-/}"
port="${SMOKE_TEST_PORT:-18080}"
name="docs-smoke-test-$$"
url="http://127.0.0.1:${port}"

cleanup() { docker rm -f "$name" >/dev/null 2>&1 || true; }
trap cleanup EXIT

docker run -d --name "$name" -p "127.0.0.1:${port}:8080" "$image" >/dev/null

# nginx fails at startup on a bad template, so wait for it or for an exit.
for _ in $(seq 1 30); do
  if curl -fsS "${url}/healthz" >/dev/null 2>&1; then break; fi
  if [ "$(docker inspect -f '{{.State.Running}}' "$name")" != "true" ]; then
    echo "container exited:" >&2
    docker logs "$name" >&2
    exit 1
  fi
  sleep 1
done

failures=0
expect() { # expect <description> <expected> <actual>
  if [ "$2" = "$3" ]; then
    echo "ok   $1"
  else
    echo "FAIL $1: expected '$2', got '$3'" >&2
    failures=$((failures + 1))
  fi
}
status() { curl -s -o /dev/null -w '%{http_code}' "$1"; }
header() { curl -sI "$1" | tr -d '\r' | awk -v h="$2" 'BEGIN{IGNORECASE=1} tolower($0) ~ "^" tolower(h) ":" {sub(/^[^:]*: */, ""); print; exit}'; }

expect "healthz" 200 "$(status "${url}/healthz")"
expect "home page" 200 "$(status "${url}${base}")"
expect "image" 200 "$(status "${url}${base}img/logo.svg")"
expect "unknown page is 404" 404 "$(status "${url}${base}this-page-does-not-exist")"
expect "404 page is the site's" 1 "$(curl -s "${url}${base}this-page-does-not-exist" | grep -c 'data-rh=' | awk '{print ($1 > 0) ? 1 : 0}')"
expect "security header on pages" nosniff "$(header "${url}${base}" X-Content-Type-Options)"

asset="$(curl -s "${url}${base}" | grep -oE "${base}assets/js/[^\"]+\.js" | head -n 1)"
expect "home page links a hashed asset" 1 "$([ -n "$asset" ] && echo 1 || echo 0)"
if [ -n "$asset" ]; then
  expect "asset" 200 "$(status "${url}${asset}")"
  expect "asset cache header" "public, max-age=31536000, immutable" "$(header "${url}${asset}" Cache-Control)"
  expect "security header on assets" nosniff "$(header "${url}${asset}" X-Content-Type-Options)"
fi

if [ "$base" != "/" ]; then
  expect "/ redirects" 302 "$(status "${url}/")"
  expect "/ redirects to the base path" "$base" "$(header "${url}/" Location)"
fi

if [ "$failures" -gt 0 ]; then
  docker logs "$name" >&2
  exit 1
fi
echo "image ${image} serves the site under ${base}"
