FROM node:lts-hydrogen

WORKDIR /dist

COPY package*.json ./

RUN yarn

COPY . /dist

RUN yarn build

ENTRYPOINT ["/bin/sh", "-c", "yarn start:prod"]