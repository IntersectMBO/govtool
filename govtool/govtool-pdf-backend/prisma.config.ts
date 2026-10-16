// The Prisma CLI's config for local use and the image build (`prisma generate`).
// The runtime image does not ship it: dist/start.js resolves secrets and runs
// the migration itself. With this file present Prisma no longer reads .env, so
// it is loaded here when there is one.
import { defineConfig } from 'prisma/config';

try {
  process.loadEnvFile();
} catch {
  // No .env: the environment is used as it is.
}

export default defineConfig({
  schema: 'prisma/schema.prisma',
  migrations: {
    path: 'prisma/migrations',
    seed: 'ts-node --transpile-only src/seed/main.ts',
  },
});
