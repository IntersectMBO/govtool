#!/bin/bash
####### Script for generating docker secret files and configs.
####### If the docker is in swarm mode, it will also generate the docker swarm secrets.
#######
set -e
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
    local filename=$2
    local var_name=$1
    if [ -s "$filename" ]; then
        export "$var_name"=$(<"$filename")
    else
        local secret=$(openssl rand -base64 16 | tr -d '=+/')
        echo -n "$secret" > "$filename"
        export "$var_name"="$secret"
    fi
}

if [ "$1" == "clean" ]; then

    # Create secrets from files
    for SECRET_FILE in $(ls ./secrets)
    do
        SECRET_NAME="$(basename $SECRET_FILE)"
        echo -n "Removing secret: ${PROJECT_NAME}_${SECRET_NAME}"
        docker secret rm "${PROJECT_NAME}_${SECRET_NAME}" || true
    done

    # Create configs from files
    for CONFIG_FILE in $(ls ./configs)
    do
        CONFIG_NAME=$(basename $CONFIG_FILE)
        echo -n "Removing config: ${PROJECT_NAME}_${CONFIG_NAME}"
        docker config rm "${PROJECT_NAME}_${CONFIG_NAME}" || true
    done

    set -x
    rm -rf ./configs;
    rm -rf ./secrets;

    set +x;
    exit 0
fi

## create dir if not present.
mkdir -p ./configs;
mkdir -p ./secrets;


# Generate random secrets
export POSTGRES_USER=postgres
export DBSYNC_DATABASE="${PROJECT_NAME}_dbsync"

# Save secrets to files
echo -n $POSTGRES_USER > ./secrets/postgres_user
echo -n "$DBSYNC_DATABASE" > ./secrets/dbsync_database

# generate or load the secret
generate_secret "POSTGRES_PASSWORD" "./secrets/postgres_password"

## loop over templates and update them.
for CONFIG_FILE in $(ls ./configs_template)
do
    echo -n "Config ${PROJECT_NAME}_${CONFIG_FILE}: "
    ./scripts/envsubst.py < "./configs_template/$CONFIG_FILE"   > "./configs/${CONFIG_FILE}"
done

for SECRET_FILE in $(ls ./secrets_template)
do
    echo -n "Secret ${PROJECT_NAME}_${SECRET_FILE}: "
    ./scripts/envsubst.py < "./secrets_template/$SECRET_FILE" > "./secrets/${SECRET_FILE}"
done

################################################################################
################ Create secret/config for swarm  ###############################
################################################################################

docker info | grep 'Swarm: active' > /dev/null 2>/dev/null || exit 0

# Create secrets from files
ls ./secrets | while IFS= read -r SECRET_FILE; do
    SECRET_NAME=$(basename "$SECRET_FILE")
    echo -n "Secret: ${PROJECT_NAME}_${SECRET_NAME}: "
    cat "./secrets/$SECRET_NAME" | (docker secret create "${PROJECT_NAME}_${SECRET_NAME}" -) || true
done


# Create configs from files
for CONFIG_FILE in $(ls ./configs)
do
    CONFIG_NAME=$(basename $CONFIG_FILE)
    echo -n "Config: ${PROJECT_NAME}_${CONFIG_NAME}: "
    cat "./configs/$CONFIG_NAME" | (docker config create "${PROJECT_NAME}_${CONFIG_NAME}" -) || true
done