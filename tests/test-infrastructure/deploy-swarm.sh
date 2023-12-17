#!/bin/bash
## Load environment variables and deploy to the docker swarm.
##
## Usages:
##   ./deploy-swarm prepare
##
set -eo pipefail
set -a
. ./.env
set +a 

if [ "$1" == "destroy" ]
then
    echo "This will remove everything in your stack including volumes"
    echo "Are you Sure? (Y/N)"
    read user_input
    if ! ( [ "$user_input" = "y" ] || [ "$user_input" = "Y" ])
    then 
        exit 1
    fi
    echo "Proceeding..."    # Delete the Docker stack if "destroy" argument is provided
    docker stack rm "${STACK_NAME}-services" || echo  "${STACK_NAME}-services doesn't exist"
    docker stack rm ${STACK_NAME} || echo  "${STACK_NAME} doesn't exist"
    ./gen-configs.sh clean

    for VOLUME in $(docker volume ls --filter "label=com.docker.stack.namespace=${STACK_NAME}" -q) "${STACK_NAME}-services_postgres"
    do
        echo -n "Removing Volume : "
        docker volume rm "$VOLUME"
    done

elif [ "$1" == "prepare" ]
then
    ## apply the enviroment to services compose file 
    ## and deploy the stack
    envsubst < ./docker-compose-services.yml  > ./docker-compose-services-rendered.yml
    docker stack deploy -c './docker-compose-services-rendered.yml' ${STACK_NAME}-services

elif [ "$1" == "finalize" ]
then
    ## apply the environment to compose file
    ## deploy the vva test infrastructure stack
    envsubst < ./docker-compose.yml  > ./docker-compose-rendered.yml
    docker stack deploy -c './docker-compose-rendered.yml' ${STACK_NAME}
else
    echo "Something is wrong with the command"
    echo
    echo "  Usage:"
    echo "    $0 (prepare | destroy | finalize)"
    echo ''
    echo "  Options:"
    echo "    prepare  -> deploys the services required by the test stack. i.e 'postgres' and 'reverse-proxy'"
    echo "    finalize -> deploys the test infrastructure services"
    echo "    destroy  -> teardown everything except the volumes"
fi


