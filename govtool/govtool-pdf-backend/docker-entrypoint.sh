#!/bin/sh
# Resolves the secrets, then migrates, seeds the lookups and serves.
#
# Each secret is read from its environment variable first; when that is
# missing or blank, from the file named by <NAME>_FILE, defaulting to the
# Swarm mount /run/secrets/<lowercase name>. A missing or whitespace-only
# file counts as unset. Exporting here covers all three processes below
# (the Prisma CLI, the seed and the app), which read only the environment.
set -eu

load_secret() {
  name=$1
  eval "current=\${$name:-}"
  [ -n "$(printf %s "$current" | tr -d '[:space:]')" ] && return 0
  lower=$(printf %s "$name" | tr '[:upper:]' '[:lower:]')
  eval "file=\${${name}_FILE:-/run/secrets/$lower}"
  [ -r "$file" ] || return 0
  value=$(cat "$file")
  [ -n "$(printf %s "$value" | tr -d '[:space:]')" ] || return 0
  export "$name=$value"
}

for secret in DATABASE_URL JWT_SECRET REFRESH_SECRET; do
  load_secret "$secret"
done

if [ "$#" -gt 0 ]; then
  exec "$@"
fi

npx prisma migrate deploy
node dist/seed/main.js
exec node dist/main
