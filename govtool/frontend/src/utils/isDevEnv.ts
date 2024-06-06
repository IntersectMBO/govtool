export const isDevEnv = import.meta?.env?.VITE_BASE_URL?.includes(
  "dev-sanchonet",
);
