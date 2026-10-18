#!/bin/sh
# Runs once, when the postgres service starts on an empty data volume: one
# login role and database per app, each owning its database. The passwords
# come from the Swarm secrets mounted into the postgres service; nothing
# secret is in this file.
set -eu

for app in metadata pdf; do
  password=$(cat "/run/secrets/${app}_db_password")
  # Fed on stdin so the password is not on a command line.
  psql -v ON_ERROR_STOP=1 -q -U "$POSTGRES_USER" -d postgres <<SQL
\set pw '$password'
CREATE ROLE $app LOGIN PASSWORD :'pw';
CREATE DATABASE $app OWNER $app;
SQL
done
