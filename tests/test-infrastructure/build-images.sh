#!/usr/bin/env bash
set -e

# Define a function to log and execute Docker commands
docker_() {
  local cmd="$*"
  echo docker "$cmd"
  docker $cmd 
}

export BASE_IMAGE_NAME="govtool"

BASE_IMAGE_EXISTS=$(docker images -q "$BASE_IMAGE_NAME"/backend-base)

if [ -z "$BASE_IMAGE_EXISTS" ]; then
  echo "Building the base image..."
  docker_ build -t "$BASE_IMAGE_NAME"/backend-base -f ../../govtool/backend/Dockerfile.base ../../govtool/backend
else
  echo "Base image already exists. Skipping build."
fi

docker_ compose -f ./docker-compose-govtool.yml build
docker_ compose -f ./docker-compose-govaction-loader.yml build
docker_ compose -f ./docker-compose-test.yml build
