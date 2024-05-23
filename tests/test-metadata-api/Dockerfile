FROM node:18-alpine
WORKDIR /src
COPY package.json yarn.lock ./
RUN yarn install
COPY . .
VOLUME /data
ENV DATA_DIR=/data
EXPOSE 3000
CMD [ "yarn", "start"]