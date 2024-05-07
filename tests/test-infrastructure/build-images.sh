#!/bin/bash

export BASE_IMAGE_NAME="govtool"
# build the base image
docker build  -t "$BASE_IMAGE_NAME"/backend-base -f ../../govtool/backend/Dockerfile.base ../../govtool/backend
docker compose -f ./docker-compose-govtool.yml build
docker compose -f ./docker-compose.yml build
