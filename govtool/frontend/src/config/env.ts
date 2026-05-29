const getEnv = (key: string) => {
  const value = runtimeEnv[key] || import.meta.env[key];

  if (!value) return undefined;
  if (typeof value !== "string") return value;
  if (value.trim() === "") return undefined;
  if (value.startsWith("$VITE_")) return undefined;

  return value;
};
declare global {
  interface Window {
    __ENV__?: Record<string, unknown>;
  }
}

/* eslint-disable-next-line dot-notation */
const runtimeEnv = window['__ENV__'] || {};

export const env = {
  VITE_APP_ENV: getEnv("VITE_APP_ENV"),
  VITE_BASE_URL: getEnv("VITE_BASE_URL"),
  VITE_METADATA_API_URL: getEnv("VITE_METADATA_API_URL"),
  VITE_PDF_API_URL: getEnv("VITE_PDF_API_URL"),
  VITE_OUTCOMES_API_URL: getEnv("VITE_OUTCOMES_API_URL"),
  VITE_IPFS_GATEWAY: getEnv("VITE_IPFS_GATEWAY"),
  VITE_IPFS_PROJECT_ID: getEnv("VITE_IPFS_PROJECT_ID"),
  VITE_IPFS_UPLOAD_API_KEY: getEnv("VITE_IPFS_UPLOAD_API_KEY"),

  VITE_GTM_ID: getEnv("VITE_GTM_ID"),
  VITE_SENTRY_DSN: getEnv("VITE_SENTRY_DSN"),
  VITE_USERSNAP_SPACE_API_KEY: getEnv("VITE_USERSNAP_SPACE_API_KEY"),

  VITE_NETWORK_FLAG: getEnv("VITE_NETWORK_FLAG"),
  VITE_IS_DEV: getEnv("VITE_IS_DEV"),
  VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED: getEnv(
    "VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED",
  ),
  VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED: getEnv(
    "VITE_IS_GOVERNANCE_OUTCOMES_PILLAR_ENABLED",
  ),
};
