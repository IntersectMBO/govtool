// The Prisma CLI's config. Mirrors src/config/secrets.ts: DATABASE_URL may
// come from a file (`DATABASE_URL_FILE`, defaulting to the Swarm secret mount
// /run/secrets/database_url), so `prisma migrate deploy` works in the
// container with no shell step. With this file present Prisma no longer reads
// .env itself, so it is loaded here when there is one.
import { readFileSync } from 'node:fs';
import { defineConfig } from 'prisma/config';

try {
  process.loadEnvFile();
} catch {
  // No .env: the environment is used as it is.
}

const direct = process.env.DATABASE_URL;
if (direct === undefined || direct.trim() === '') {
  try {
    const value = readFileSync(
      process.env.DATABASE_URL_FILE ?? '/run/secrets/database_url',
      'utf8',
    ).replace(/\r?\n$/, '');
    if (value.trim() !== '') process.env.DATABASE_URL = value;
  } catch {
    // Unset: the Prisma CLI reports the missing variable.
  }
}

export default defineConfig({
  schema: 'prisma/schema.prisma',
  migrations: {
    path: 'prisma/migrations',
    seed: 'ts-node --transpile-only src/seed/main.ts',
  },
});
