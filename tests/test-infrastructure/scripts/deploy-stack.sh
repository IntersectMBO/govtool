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

function help_deploy(){
      echo "Something is wrong with the command"
      echo
      echo "  Usage:"
      echo "    $0 [stack-name] [filename]"
      echo
}

function deploy-stack(){
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
      echo docker stack deploy -c "$RENDERED_FILENAME" ${STACK_NAME}
}


if [ "$#" -eq 0 ]; then
  exit 0
elif [ "$#" -ne 2 ];
then
  help_deploy
  exit 1
else
  load_env
  deploy-stack "$1" "$2"
fi
