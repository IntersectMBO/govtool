ARG BASE_IMAGE_TAG=latest
ARG BASE_IMAGE_REPO=ghcr.io/intersectmbo/backend-base
FROM $BASE_IMAGE_REPO:$BASE_IMAGE_TAG
WORKDIR /src
COPY . .
RUN cabal build
RUN cp dist-newstyle/build/x86_64-linux/ghc-9.2.7/vva-be-2.0.29/x/vva-be/build/vva-be/vva-be /usr/local/bin