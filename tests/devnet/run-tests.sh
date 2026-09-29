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
    projects="${DEVNET_PLAYWRIGHT_PROJECTS:-dRep setup,loggedin (desktop),dRep,delegation,wallet,independent (desktop)}"
    # Left out by default (README.md, "Excluded"): the proposal discussion
    # forum specs (7, 8, 11, 12; their .pd/.pb/.ga projects are not in the
    # list above, this drops their plain specs), chatwoot (disabled), and
    # the pdf username tests 6I-6L.
    # Playwright ORs file filters, so a targeted run replaces this one:
    # DEVNET_PLAYWRIGHT_FILES=walletConnect.loggedin.spec.ts:10
    file_filter="${DEVNET_PLAYWRIGHT_FILES:-^(?!.*/(7-proposal-submission|8-proposal-discussion|11-proposal-budget|12-proposal-budget-submission)/)(?!.*chatwoot\.spec\.ts).*}"
    grep_invert="${DEVNET_PLAYWRIGHT_GREP_INVERT:-^6[IJKL]\. |\s6[IJKL]\. }"
    project_args=()
    IFS=',' read -r -a names <<< "$projects"
    for name in "${names[@]}"; do project_args+=("--project=$name"); done
    exec npx playwright test "${project_args[@]}" --grep-invert "$grep_invert" "$file_filter" "$@"
    ;;
  *)
    die "unknown suite: $suite (pytest|playwright)"
    ;;
esac
