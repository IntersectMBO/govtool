ARG BASE_IMAGE_TAG=latest
ARG BASE_IMAGE_REPO=733019650473.dkr.ecr.eu-west-1.amazonaws.com/backend-base
FROM $BASE_IMAGE_REPO:$BASE_IMAGE_TAG
WORKDIR /src
COPY . .
RUN cabal build
RUN cp dist-newstyle/build/x86_64-linux/ghc-9.2.7/vva-be-1.0.25/x/vva-be/build/vva-be/vva-be /usr/local/bin
