#!/bin/bash
## Load environment variables and deploy to the docker swarm.
##
## Usages:
##   ./deploy-swarm prepare
##
set -eo pipefail
. ./scripts/deploy-stack.sh
load_env

DOCKER_STACKS=("basic-services" "cardano" "govaction-loader" "govtool" "test")

if [ "$1" == "destroy" ]
then
    echo "This will remove everything in your stack except volumes, configs and secrets"
    echo "Are you Sure? (Y/N)"
    read user_input
    if ! ( [ "$user_input" = "y" ] || [ "$user_input" = "Y" ])
    then
        exit 1
    fi
    echo "Proceeding..."    # Delete the Docker stack if "destroy" argument is provided

    REVERSE_STACKS=()
    for ((i=${#STACKS[@]}-1; i>=0; i--)); do
        REVERSE_STACKS+=("${STACKS[i]}")
    done

    for CUR_STACK in "${REVERSE_STACKS[@]}"; do
        docker stack rm  "$CUR_STACK"
        sleep 6 # wait 6 seconds for each stack cleanup.
    done

#    ./gen-configs.sh clean

#    for VOLUME in $(docker volume ls --filter "label=com.docker.stack.namespace=${STACK_NAME}" -q) "${STACK_NAME}-services_postgres"
#    do
#        echo -n "Removing Volume : "
#        docker volume rm "$VOLUME"
#    done
elif [ "$1" == 'prepare' ]
then

    # Get the number of nodes in the swarm
    NODES=$(docker node ls --format "{{.ID}}" | wc -l)

    # If there is only one node, set the labels
    if [ "$NODES" -eq 1 ]; then
        NODE_ID=$(docker node ls --format "{{.ID}}")

        docker node update --label-add govtool-test-stack=true \
                           --label-add blockchain=true \
                           --label-add gateway=true \
                           --label-add govtool=true \
                           --label-add gov-action-loader=true \
                           "$NODE_ID"

        echo "Labels set on node: $NODE_ID"
    else
        echo "There  are multiple nodes in the docker swarm."
        echo "Please set the following labels to correct nodes manually."
        echo "  - govtool-test-stack "
        echo "  - blockchain"
        echo "  - gateway"
        echo "  - govtool"
        echo "  - gov-action-loader"
        echo ""
        echo " e.g.  $ docker node update xxxx --label-add gateway=true"

        exit 1
    fi

elif [ "$1" == 'stack' ]
then
   if [ "$#" -ne 2 ]
   then
     echo "stack requires the stack name".
     echo "Usage :"
     echo "   > $0 stack [stack-name]".
     echo ""
     echo "  stack-name : One of the following"ß
     echo "               $DOCKER_STACKS"
  else
     case "$2" in
       all)

      for DEPLOY_STACK in "${DOCKER_STACKS[@]}"; do
        deploy-stack "$DEPLOY_STACK" "docker-compose-$DEPLOY_STACK.yml"
      done

         ;;
       *)
         if [[ ! -f ./"docker-compose-$2.yml" ]]
         then
          echo "Invalid stack name. $2"
         else
          deploy-stack $2  "docker-compose-$2.yml"
         fi
         ;;
     esac
  fi
else
    echo "Something is wrong with the command"
    echo
    echo "  Usage:"
    echo "    $0 (prepare | destroy | deploy)"
    echo ''
    echo "  Options:"
    echo "    prepare  -> set required labels to docker swarm node."
    echo "    destroy  -> teardown everything except the volumes"
    echo "    deploy  [stack_name] -> Deploy the stack."
fi
