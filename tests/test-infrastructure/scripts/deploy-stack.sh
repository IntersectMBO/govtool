#!/bin/bash
## Docker swarm doesn't read .env file.
## This script reads env file and variables
## and apply them to compose file and
## then execute `docker stack deploy`

set -eo pipefail

function load_env(){
  set -a
  . ./.env
  set +a
}

function deploy-stack(){
      echo "++ deploy-stack" "$@"
      ## apply the environment to compose file
      ## deploy the govtool test infrastructure stack
      ## first argument is stack name and 2nd argument is the file name
      STACK_NAME=$1
      COMPOSE_FILE=$2
      FILENAME=$(basename -- "$COMPOSE_FILE")
      EXTENSION="${FILENAME##*.}"
      FILENAME_WITHOUT_EXT="${FILENAME%.*}"
      RENDERED_FILENAME="${FILENAME_WITHOUT_EXT}-rendered.${EXTENSION}"
      envsubst < "$COMPOSE_FILE" > "$RENDERED_FILENAME"
      docker stack deploy -c "$RENDERED_FILENAME" ${STACK_NAME}
}