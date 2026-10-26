#!/bin/sh
# Imports the proposal discussion forum from a Strapi dump into the stack's
# pdf database, with import-strapi.sql. Run on the node where the stack's
# Postgres runs, from the folder holding this script and import-strapi.sql.
#
#   STACK_NAME=mainnet-govtool ./import-forum.sh prepare strapi.dump
#   STACK_NAME=mainnet-govtool ./import-forum.sh dry-run
#   STACK_NAME=mainnet-govtool ./import-forum.sh import
#   STACK_NAME=mainnet-govtool ./import-forum.sh cleanup
#
# prepare  restores the dump and copies its tables into a `strapi` schema in pdf
# dry-run  runs the import and rolls it back: prints the report, writes nothing
# import   backs pdf up to ./pdf-before-import-<time>.dump, without the strapi
#          schema and the emails and password hashes it holds, then imports
# cleanup  drops the `strapi` schema
set -eu

cd "$(dirname "$0")"
SQL=import-strapi.sql
[ -f "$SQL" ] || { echo "$SQL not found next to this script" >&2; exit 1; }

C="${PG_CONTAINER:-$(docker ps -q -f "name=${STACK_NAME:?set STACK_NAME, e.g. mainnet-govtool}_postgres" | head -n 1)}"
[ -n "$C" ] || { echo "no running ${STACK_NAME}_postgres container on this node" >&2; exit 1; }

psql_pdf() { docker exec -i "$C" psql -v ON_ERROR_STOP=1 -U postgres -d pdf "$@"; }
has_strapi() { [ "$(psql_pdf -At -c "SELECT to_regnamespace('strapi') IS NOT NULL")" = t ]; }

case "${1:-}" in
  prepare)
    dump="${2:?usage: $0 prepare <strapi dump>}"
    [ -f "$dump" ] || { echo "$dump not found" >&2; exit 1; }
    if has_strapi; then echo "pdf already has a strapi schema; run cleanup first" >&2; exit 1; fi
    docker exec "$C" dropdb -U postgres --if-exists strapi_import
    docker exec "$C" createdb -U postgres strapi_import
    echo "restoring $dump ..."
    docker exec -i "$C" pg_restore -U postgres -d strapi_import --no-owner --no-privileges < "$dump"
    docker exec "$C" psql -q -v ON_ERROR_STOP=1 -U postgres -d strapi_import -c "ALTER SCHEMA public RENAME TO strapi"
    docker exec "$C" sh -c 'pg_dump -U postgres -d strapi_import -n strapi --no-owner --no-privileges | psql -q -o /dev/null -v ON_ERROR_STOP=1 -U postgres -d pdf'
    docker exec "$C" dropdb -U postgres strapi_import
    psql_pdf -c "SELECT (SELECT count(*) FROM strapi.up_users) AS users, (SELECT count(*) FROM strapi.proposals) AS proposals, (SELECT count(*) FROM strapi.comments) AS comments"
    echo "prepared; next: dry-run"
    ;;
  dry-run)
    has_strapi || { echo "no strapi schema in pdf; run prepare first" >&2; exit 1; }
    [ "$(grep -c '^COMMIT;$' "$SQL")" = 1 ] || { echo "$SQL must end with exactly one COMMIT;" >&2; exit 1; }
    sed 's/^COMMIT;$/ROLLBACK;/' "$SQL" | psql_pdf -q -f -
    echo "dry run rolled back; nothing was written"
    ;;
  import)
    has_strapi || { echo "no strapi schema in pdf; run prepare first" >&2; exit 1; }
    backup="pdf-before-import-$(date +%Y%m%d-%H%M%S).dump"
    docker exec "$C" pg_dump -U postgres -Fc -N strapi pdf > "$backup"
    echo "backup: $backup"
    psql_pdf -q -f - < "$SQL"
    echo "imported; next: cleanup"
    ;;
  cleanup)
    psql_pdf -q -c "DROP SCHEMA IF EXISTS strapi CASCADE" 2>&1 | grep -v -e '^NOTICE' -e '^DETAIL' -e '^drop cascades' || true
    docker exec "$C" dropdb -U postgres --if-exists strapi_import
    echo "cleaned up"
    ;;
  *)
    sed -n '2,15p' "$0" >&2
    exit 1
    ;;
esac
