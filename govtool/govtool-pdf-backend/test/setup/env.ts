// Per test file, before any module loads: point the app at pdf_test and give
// it test secrets. createTestApp can override individual variables.

import { TEST_DATABASE_URL, assertTestDatabase } from './test-db';

assertTestDatabase();

export const TEST_ENV: Record<string, string> = {
  NODE_ENV: 'test',
  DATABASE_URL: TEST_DATABASE_URL,
  JWT_SECRET: 'e2e-access-secret-0123456789abcdef0123',
  REFRESH_SECRET: 'e2e-refresh-secret-0123456789abcdef012',
};

const CLEARED = [
  'HOST',
  'PORT',
  'JWT_SECRET_EXPIRES',
  'REFRESH_TOKEN_EXPIRES',
  'REFRESH_COOKIE_SAMESITE',
  'REFRESH_COOKIE_SECURE',
  'CHALLENGE_TTL_SECONDS',
  'CORS_ORIGINS',
  'CARDANO_NETWORK_ID',
  'GOVTOOL_API_BASE_URL',
  'GOVTOOL_PROXY_ALLOWED_PATHS',
  'PROXY_TIMEOUT_MS',
  'PROXY_MAX_BYTES',
  'IPFS_GATEWAY_URL',
  'PDF_ALLOW_PRIVATE_URLS',
  'PDF_PROXY_HOST_REWRITES',
  'BODY_LIMIT',
];

for (const k of CLEARED) delete process.env[k];
Object.assign(process.env, TEST_ENV);
