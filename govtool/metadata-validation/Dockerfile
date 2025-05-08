FROM node:lts-hydrogen

ARG IPFS_GATEWAY
ARG IPFS_PROJECT_ID

WORKDIR /dist

COPY package*.json ./

RUN npm install

COPY . /dist

RUN npm run build

ENV IPFS_GATEWAY=$IPFS_GATEWAY
ENV IPFS_PROJECT_ID=$IPFS_PROJECT_ID

ENTRYPOINT ["/bin/sh", "-c", "npm run start:prod"]