// The e2e database: `DATABASE_URL_TEST`, default pdf_test on the compose
// Postgres. Every function here refuses a database whose name does not end
// in `_test`, so the suite can never wipe the development database.

import { PrismaClient } from '@prisma/client';

export const TEST_DATABASE_URL =
  process.env.DATABASE_URL_TEST ?? 'postgresql://pdf:pdf@127.0.0.1:5442/pdf_test?schema=public';

export function databaseName(url: string): string {
  return decodeURIComponent(new URL(url).pathname.replace(/^\//, ''));
}

export function assertTestDatabase(url: string = TEST_DATABASE_URL): void {
  const name = databaseName(url);
  if (!/^[A-Za-z0-9_]+_test$/.test(name)) {
    throw new Error(`Refusing to run e2e tests against database "${name}": its name must end in _test.`);
  }
}

/** Same server, the `postgres` maintenance database (for CREATE DATABASE). */
export function maintenanceUrl(url: string = TEST_DATABASE_URL): string {
  const u = new URL(url);
  u.pathname = '/postgres';
  return u.toString();
}

/** Every table the seed does not own; truncated between test files. */
export const LOOKUP_TABLES = new Set(['governance_action_types', '_prisma_migrations']);

export async function truncateAll(prisma?: PrismaClient): Promise<void> {
  assertTestDatabase();
  const client = prisma ?? new PrismaClient({ datasourceUrl: TEST_DATABASE_URL });
  try {
    const rows = await client.$queryRaw<Array<{ tablename: string }>>`
      SELECT tablename FROM pg_tables WHERE schemaname = 'public'`;
    const tables = rows
      .map((r) => r.tablename)
      .filter((t) => !LOOKUP_TABLES.has(t) && /^[a-z0-9_]+$/.test(t));
    if (tables.length) {
      await client.$executeRawUnsafe(
        `TRUNCATE TABLE ${tables.map((t) => `"${t}"`).join(', ')} RESTART IDENTITY CASCADE`,
      );
    }
  } finally {
    if (!prisma) await client.$disconnect();
  }
}
