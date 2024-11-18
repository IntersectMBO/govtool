FROM node:lts-hydrogen

ARG IPFS_GATEWAY
ARG IPFS_PROJECT_ID

WORKDIR /dist

COPY package*.json ./

RUN yarn

COPY . /dist

RUN yarn build

ENTRYPOINT ["/bin/sh", "-c", "yarn start:prod"]