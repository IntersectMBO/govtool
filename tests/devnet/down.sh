#!/usr/bin/env bash
# Tears the environment down: the app stack with its volumes (databases,
# bucket), then the devnet (adaup removes its containers, volumes and
# network) unless SKIP_CHAIN=1.
#   ./down.sh                 everything
#   ./down.sh --keep-volumes  keep the app volumes (the devnet is always new)
set -euo pipefail
source "$(dirname "${BASH_SOURCE[0]}")/lib.sh"
load_env

args=(down --remove-orphans)
[ "${1:-}" = "--keep-volumes" ] || args+=(--volumes)
# compose needs the external network and volume only for `up`.
compose "${args[@]}" || log "warning: compose down failed"

if [ "${SKIP_CHAIN:-0}" != "1" ]; then
  if command -v "$ADAUP_CARDANO" >/dev/null 2>&1; then
    log "removing the devnet"
    "$ADAUP_CARDANO" devnet down --docker || log "warning: adaup devnet down failed"
  else
    log "warning: adaup CLI '$ADAUP_CARDANO' not found; devnet left running"
  fi
fi
