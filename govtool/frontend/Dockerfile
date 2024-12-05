FROM node:20.18.1-alpine as builder

ARG VITE_APP_ENV='beta'
ARG VITE_BASE_URL
ARG VITE_METADATA_API_URL
ARG VITE_GTM_ID
ARG VITE_NETWORK_FLAG=0
ARG VITE_SENTRY_DSN
ARG NPMRC_TOKEN
ARG VITE_USERSNAP_SPACE_API_KEY
ARG VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED='true'
ARG VITE_PDF_API_URL
ARG VITE_IPFS_GATEWAY
ARG VITE_IPFS_PROJECT_ID

# Ensure all required build arguments are set
RUN \
  : "${VITE_APP_ENV:?Build argument VITE_APP_ENV is not set}" && \
  : "${VITE_BASE_URL:?Build argument VITE_BASE_URL is not set}" && \
  : "${VITE_METADATA_API_URL:?Build argument VITE_METADATA_API_URL is not set}" && \
  : "${VITE_GTM_ID:?Build argument VITE_GTM_ID is not set}" && \
  : "${VITE_NETWORK_FLAG:?Build argument VITE_NETWORK_FLAG is not set}" && \
  : "${VITE_SENTRY_DSN:?Build argument VITE_SENTRY_DSN is not set}" && \
  : "${NPMRC_TOKEN:?Build argument NPMRC_TOKEN is not set}" && \
  : "${VITE_USERSNAP_SPACE_API_KEY:?Build argument VITE_USERSNAP_SPACE_API_KEY is not set}" && \
  : "${VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED:?Build argument VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED is not set}" && \
  : "${VITE_IPFS_GATEWAY:?Build argument VITE_IPFS_GATEWAY is not set}" && \
  : "${VITE_IPFS_PROJECT_ID:?Build argument VITE_IPFS_PROJECT_ID is not set}"

ENV NODE_OPTIONS=--max_old_space_size=8192

WORKDIR /src

# Set npm configuration settings using environment variables
RUN npm config set @intersect.mbo:registry "https://registry.npmjs.org/" --location=global
RUN npm config set //registry.npmjs.org/:_authToken ${NPMRC_TOKEN} --location=global

COPY package.json package-lock.json ./
RUN npm install
COPY . .
RUN npm run build

FROM nginx:stable-alpine
EXPOSE 80
COPY nginx.conf /etc/nginx/conf.d/default.conf
COPY --from=builder /src/maintenance-page/index.html /usr/share/nginx/html/maintenance.html
COPY --from=builder /src/dist /usr/share/nginx/html
