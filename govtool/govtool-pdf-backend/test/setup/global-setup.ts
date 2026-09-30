// Once per e2e run: create pdf_test if missing, migrate it, seed lookups.

import { execFileSync } from 'node:child_process';
import * as path from 'node:path';
import { PrismaClient } from '@prisma/client';
import { seedLookups } from '../../src/seed/seed-lookups';
import { TEST_DATABASE_URL, assertTestDatabase, databaseName, maintenanceUrl } from './test-db';

export default async function globalSetup(): Promise<void> {
  assertTestDatabase();
  const name = databaseName(TEST_DATABASE_URL);

  const admin = new PrismaClient({ datasourceUrl: maintenanceUrl() });
  try {
    const found = await admin.$queryRaw<unknown[]>`SELECT 1 FROM pg_database WHERE datname = ${name}`;
    if (found.length === 0) {
      // The name matched ^[A-Za-z0-9_]+_test$ above, so quoting is safe.
      await admin.$executeRawUnsafe(`CREATE DATABASE "${name}"`);
    }
  } finally {
    await admin.$disconnect();
  }

  const root = path.resolve(__dirname, '..', '..');
  execFileSync(path.join(root, 'node_modules', '.bin', 'prisma'), ['migrate', 'deploy'], {
    cwd: root,
    env: { ...process.env, DATABASE_URL: TEST_DATABASE_URL },
    stdio: 'pipe',
  });

  const prisma = new PrismaClient({ datasourceUrl: TEST_DATABASE_URL });
  try {
    await seedLookups(prisma);
  } finally {
    await prisma.$disconnect();
  }
}
