#!/bin/bash
####### Script for generating docker secret files and configs.
####### If the docker is in swarm mode, it will also generate the docker swarm secrets.
####### 
if ! [ -f ./.env ] 
then 
    echo ".env file is missing"
    exit 1
fi
set -a 
. ./.env
set +a
# Function to generate a random secret in base64 format without padding and '+'
function generate_secret() {
    openssl rand -base64 16 | tr -d '=+/' 
}

# Generate random secrets
export POSTGRES_USER=postgres
export POSTGRES_PASSWORD=$(generate_secret)
metrics_api_secret=$(generate_secret)


if [ "$1" == "clean" ]; then
    set -x
    rm -rf ./configs;
    rm -rf ./secrets;

    set +x

    docker info | grep 'Swarm: active' > /dev/null 2>/dev/null || exit 0
    for CONFIG_FILE in $(ls ./configs_template)
    do
        echo -n "Removing Config : "
       docker config rm "${STACK_NAME}_${CONFIG_FILE}" || true
    done

    for SECRET_FILE in "$(ls ./secrets_template)" "postgres_user" "postgres_password" "metrics_api_secret"
    do
        echo -n "Removing Secret : "
        docker secret rm "${STACK_NAME}_${SECRET_FILE}"   ||true
    done
    exit 0
fi

## Check if one fo the secrets already exists
if [[ -f ./secrets/govtool_postgres_user ]]
then
    echo "File  ./secrets/govtool_postgres_user already exists."
    echo "Assuming that the secrets were already generated"
    echo "   Use:"
    echo "      > ./gen-configs.sh clean"
    echo "   To clean up the configs and secrets"
    exit 0
fi

## create dir if not present.
mkdir -p ./configs;
mkdir -p ./secrets;


## save secrets to secrets folder
echo -n $POSTGRES_USER > ./secrets/govtool_postgres_user
echo -n $POSTGRES_PASSWORD > ./secrets/govtool_postgres_password
echo -n $metrics_api_secret > ./secrets/govtool_metrics_api_secret


## loop over templates and updaete them.

for CONFIG_FILE in $(ls ./configs_template)
do
    echo -n "Config ${STACK_NAME}_${CONFIG_FILE}: "
    envsubst < "./configs_template/$CONFIG_FILE"   > "./configs/${STACK_NAME}_${CONFIG_FILE}"
done

for SECRET_FILE in $(ls ./secrets_template)
do
    echo -n "Secret ${STACK_NAME}_${SECRET_FILE}: "
    envsubst < "./secrets_template/$SECRET_FILE" > "./secrets/${STACK_NAME}_${SECRET_FILE}"
done



################################################################################
################ Create secret/config for swarm  ###############################
################################################################################

docker info | grep 'Swarm: active' > /dev/null 2>/dev/null || exit 0

echo "Creating Secret: ${STACK_NAME}_postgres_user"
echo "$POSTGRES_USER" | (docker secret create "${STACK_NAME}_postgres_user" - )  || true

echo "Generating Secret: ${STACK_NAME}_postgres_password"
echo "$POSTGRES_PASSWORD" | (docker secret create "${STACK_NAME}_postgres_password" -  ) || true

echo "Generating Secret: ${STACK_NAME}_metrics_api_secret"
echo "$metrics_api_secret" | (docker secret create "${STACK_NAME}_metrics_api_secret" - )|| true



for CONFIG_FILE in $(ls ./configs_template)
do
    echo -n "Creating Config: ${STACK_NAME}_${CONFIG_FILE} "
    cat "./configs/${STACK_NAME}_${CONFIG_FILE}" | docker config create "${STACK_NAME}_${CONFIG_FILE}" - || true
done

for SECRET_FILE in $(ls ./secrets_template)
do
    echo -n "Creating Secret: ${STACK_NAME}_${SECRET_FILE} "
    cat "./secrets/${STACK_NAME}_${SECRET_FILE}" | docker secret create "${STACK_NAME}_${SECRET_FILE}" -  ||true
done
