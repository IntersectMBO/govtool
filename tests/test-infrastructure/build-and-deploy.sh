#!/usr/bin/env bash
export BASE_IMAGE_NAME=govtool
export PROJECT_NAME=govtool
export CARDANO_NETWORK=sanchonet
export BASE_DOMAIN=govtool.cardanoapi.io

if [ -z "$GOVTOOL_TAG" ]; then
    GOVTOOL_TAG="$(git rev-parse HEAD)"
fi
export GOVTOOL_TAG

. ./scripts/deploy-stack.sh

check_env

# Build images
./build-images.sh
function update-service(){
  docker service update --image "$2" "$1"
}

if [[ "$1" == "update-images" ]]
then
  update-service govtool_backend  "$BASE_IMAGE_NAME"/backend:${GOVTOOL_TAG}
  update-service govtool_frontend "$BASE_IMAGE_NAME"/frontend:${GOVTOOL_TAG}
  update-service govtool_metadata-validation "$BASE_IMAGE_NAME"/metadata-validation:${GOVTOOL_TAG}

  update-service govaction-loader_backend "$BASE_IMAGE_NAME"/gov-action-loader-frontend:${GOVTOOL_TAG}
  update-service govaction-loader_frontend "$BASE_IMAGE_NAME"/gov-action-loader-backend:${GOVTOOL_TAG}

  # test metadata API
  update-service test_metadata-api "$BASE_IMAGE_NAME"/gov-action-loader-backend:${GOVTOOL_TAG}

elif  [[ $1 == "full" ]]
then
  ./deploy.sh stack all
fi
