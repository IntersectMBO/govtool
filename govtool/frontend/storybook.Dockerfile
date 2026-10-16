FROM node:18-alpine as deps

WORKDIR /src

COPY package.json package-lock.json ./
COPY patches ./patches

RUN npm install

FROM node:18-alpine as builder
WORKDIR /src

ENV NODE_OPTIONS="--max-old-space-size=4096"

COPY --from=deps /src/node_modules ./node_modules
COPY . .

RUN npm run build:storybook --quiet

FROM nginx:stable-alpine
EXPOSE 80

COPY --from=builder /src/storybook-static /usr/share/nginx/html

CMD ["nginx", "-g", "daemon off;"]
