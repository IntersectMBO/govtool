# Shared by up.sh, down.sh and seed.sh. Sourced, not executed.

DEVNET_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$DEVNET_DIR/../.." && pwd)"
export DEVNET_STATE_DIR="${DEVNET_STATE_DIR:-$DEVNET_DIR/.state}"

# Exports .env.devnet.local, then .env.devnet. Variables already set in the
# environment win over both files, and .env.devnet.local wins over
# .env.devnet; since the local file is read first, values in .env.devnet that
# are built from other variables (PUBLIC_FRONTEND_URL from FRONTEND_PORT, ...)
# follow its overrides. A local value may use ${HOME} and environment
# variables, not variables defined only in .env.devnet. (bash 3.2
# compatible: no associative arrays, since macOS ships that bash.)
load_env() {
  local preset file line key
  preset=" $(compgen -e | tr '\n' ' ') "
  for file in "$DEVNET_DIR/.env.devnet.local" "$DEVNET_DIR/.env.devnet"; do
    [ -f "$file" ] || continue
    while IFS= read -r line || [ -n "$line" ]; do
      case "$line" in ''|'#'*) continue ;; esac
      key="${line%%=*}"
      [[ "$key" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]] || continue
      case "$preset" in *" $key "*) continue ;; esac
      eval "export $line"
      preset="$preset$key "
    done < "$file"
  done
}

compose() {
  local files=(-f "$DEVNET_DIR/docker-compose.yml")
  if [ "$(uname -s)" = "Darwin" ]; then
    files+=(-f "$DEVNET_DIR/docker-compose.ipv6.yml")
  fi
  docker compose --project-directory "$DEVNET_DIR" "${files[@]}" "$@"
}

log() { printf '\033[1m[devnet]\033[0m %s\n' "$*" >&2; }
die() { log "error: $*"; exit 1; }

# wait_for <description> <timeout seconds> <command...>
wait_for() {
  local what="$1" timeout="$2"; shift 2
  local deadline=$((SECONDS + timeout))
  until "$@" >/dev/null 2>&1; do
    if [ "$SECONDS" -ge "$deadline" ]; then
      die "timed out after ${timeout}s waiting for $what"
    fi
    sleep 3
  done
  log "$what: ready"
}

http_ok() { curl -fsS -m 5 -o /dev/null "$1"; }

# step_start; ...; step_end <name>: logs the step's duration and appends it
# to $DEVNET_STATE_DIR/timings.txt.
step_start() { STEP_STARTED=$SECONDS; }
step_end() {
  local took=$((SECONDS - ${STEP_STARTED:-$SECONDS}))
  log "$1: ${took}s"
  echo "$1=${took}s" >> "$DEVNET_STATE_DIR/timings.txt"
}
