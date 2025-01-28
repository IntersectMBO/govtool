FROM node:lts-hydrogen

ARG IPFS_GATEWAY
ARG IPFS_PROJECT_ID

WORKDIR /dist

COPY package*.json ./

RUN yarn

COPY . /dist

RUN yarn build

ENV IPFS_GATEWAY=$IPFS_GATEWAY
ENV IPFS_PROJECT_ID=$IPFS_PROJECT_ID

ENTRYPOINT ["/bin/sh", "-c", "yarn start:prod"]