FROM node:18-alpine as builder
WORKDIR /src
COPY package.json yarn.lock ./
RUN yarn install
COPY . .
RUN VITE_DATA_LOADER_API='/' VITE_VVA_WEBAPP_URL='https://vva-be.cardanoapi.io' yarn build

FROM nginx:stable-alpine
RUN echo -e "server {\n    listen       80;\n    location / {\n        root   /usr/share/nginx/html;\n        index  index.html;\n\ttry_files \$uri \$uri /index.html;\n    }\n}" | tee /etc/nginx/conf.d/nginx.conf
COPY --from=builder /src/dist /usr/share/nginx/html
