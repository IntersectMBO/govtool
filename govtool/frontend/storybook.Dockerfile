FROM node:18-alpine as deps
ARG NPMRC_TOKEN

WORKDIR /src

# Set npm configuration settings using environment variables
RUN npm config set @intersect.mbo:registry "https://registry.npmjs.org/" --location=global \
    && npm config set //registry.npmjs.org/:_authToken ${NPMRC_TOKEN} --location=global

COPY package.json package-lock.json ./
COPY patches ./patches

RUN npm install

FROM node:18-alpine as builder
ARG NPMRC_TOKEN
WORKDIR /src

ENV NODE_OPTIONS="--max-old-space-size=4096"

COPY --from=deps /src/node_modules ./node_modules
COPY . .

RUN npm run build:storybook --quiet

FROM nginx:stable-alpine
EXPOSE 80

COPY --from=builder /src/storybook-static /usr/share/nginx/html

CMD ["nginx", "-g", "daemon off;"]
