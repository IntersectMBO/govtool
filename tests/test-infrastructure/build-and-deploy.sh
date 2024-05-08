#!/usr/bin/env bash
set -vx;
export BASE_IMAGE_NAME=govtool
export GOVTOOL_TAG="$(git rev-parse HEAD)"
export PROJECT_NAME=govtool
export CARDANO_NETWORK=sanchonet
export BASE_DOMAIN=govtool.cardanoapi.io

. ./scripts/deploy-stack.sh

check_env

# Build images
./build-images.sh
function update-service(){
  docker service update --image "$2" "$1"
}

if [[ "$1" == "update-images" ]]

  update-service govtool_backend  "$BASE_IMAGE_NAME"/backend:${GOVTOOL_TAG}
  update-service govtool_frontend "$BASE_IMAGE_NAME"/frontend:${GOVTOOL_TAG}
  update-service govtool_metadata-validation "$BASE_IMAGE_NAME"/metadata-validation:${GOVTOOL_TAG}

  update-service govaction-loader_backend "$BASE_IMAGE_NAME"/gov-action-loader-frontend:${GOVTOOL_TAG}
  update-service govaction-loader_frontend "$BASE_IMAGE_NAME"/gov-action-loader-backend:${GOVTOOL_TAG}

elif  [[ $1 == "full" ]]
  ./deploy stack all
fi
