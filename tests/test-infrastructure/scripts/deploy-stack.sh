#!/usr/bin/env bash
## Docker swarm doesn't read .env file.
## This script reads env file and variables
## and apply them to compose file and
## then execute `docker stack deploy`

set -eo pipefail

function load_env(){
  if  [[ -f ./.env ]]
  then
    set -a
    . ./.env
    set +a
  fi
  check_env
}


function check_env(){

    # Path to the .env.example file
    EXAMPLE_FILE=".env.example"

    unset_keys=()

    # Read each line of the .env.example file
    while IFS= read -r line || [ -n "$line" ]; do
        # Skip empty lines
        if [ -z "$line" ]; then
            continue
        fi

        line=$(echo "$line" | sed -e 's/^[[:space:]]*//')

        # Extract the key from each line
        key=$(echo "$line" | cut -d'=' -f1)

        if [ -z "${!key}" ]; then
            unset_keys+=("$key")
        fi
    done < "$EXAMPLE_FILE"

    # Print error message for unset keys
    if [ ${#unset_keys[@]} -gt 0 ]; then
        echo "The following keys are not set in the environment:"
        for key in "${unset_keys[@]}"; do
            echo "- $key"
        done
        echo  " Exiting due to missing env variables"
        exit 2
    fi
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