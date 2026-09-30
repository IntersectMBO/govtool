#!/usr/bin/env bash
# Runs a test suite against a devnet started by up.sh.
#
#   ./run-tests.sh pytest     [pytest args]
#   ./run-tests.sh playwright [playwright args]
#
# Playwright runs the projects listed below and skips the specs listed in
# README.md ("Excluded"). DEVNET_PLAYWRIGHT_PROJECTS, DEVNET_PLAYWRIGHT_FILES
# and DEVNET_PLAYWRIGHT_GREP_INVERT change that. To re-run only failures,
# pass --last-failed. TEST_WALLET_MNEMONIC comes from .state/playwright.env
# (one per devnet).
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"

suite="${1:-}"
[ -n "$suite" ] || die "usage: $0 pytest|playwright [args]"
shift

case "$suite" in
  pytest)
    [ -f "$DEVNET_STATE_DIR/pytest.env" ] || die "run ./up.sh first"
    set -a; . "$DEVNET_STATE_DIR/pytest.env"; set +a
    cd "$REPO_ROOT/tests/govtool-backend"
    # PYTHON: an interpreter with requirements.txt installed (e.g. a venv).
    exec "${PYTHON:-python3}" -m pytest "$@"
    ;;
  playwright)
    [ -f "$DEVNET_STATE_DIR/playwright.env" ] || die "run ./up.sh first"
    set -a; . "$DEVNET_STATE_DIR/playwright.env"; set +a
    cd "$REPO_ROOT/tests/govtool-frontend/playwright"
    # Every project runs by default, forum and mobile included; only
    # chatwoot.spec.ts is left out (Chatwoot is disabled on the devnet).
    # DEVNET_PLAYWRIGHT_PROJECTS (comma-separated) narrows the projects.
    # Playwright ORs file filters, so a targeted run replaces this one:
    # DEVNET_PLAYWRIGHT_FILES=walletConnect.loggedin.spec.ts:10
    file_filter="${DEVNET_PLAYWRIGHT_FILES:-^(?!.*chatwoot\.spec\.ts).*}"
    grep_invert="${DEVNET_PLAYWRIGHT_GREP_INVERT:-}"
    args=()
    if [ -n "${DEVNET_PLAYWRIGHT_PROJECTS:-}" ]; then
      IFS=',' read -r -a names <<< "$DEVNET_PLAYWRIGHT_PROJECTS"
      for name in "${names[@]}"; do args+=("--project=$name"); done
    fi
    [ -z "$grep_invert" ] || args+=(--grep-invert "$grep_invert")
    # The config traces only on retry and retries are 0, so keep a trace of
    # every failure (network, console, DOM) in test-results/, which CI
    # uploads. DEVNET_PLAYWRIGHT_TRACE=off drops it.
    args+=(--trace "${DEVNET_PLAYWRIGHT_TRACE:-retain-on-failure}")
    exec npx playwright test ${args[@]+"${args[@]}"} "$file_filter" "$@"
    ;;
  *)
    die "unknown suite: $suite (pytest|playwright)"
    ;;
esac
