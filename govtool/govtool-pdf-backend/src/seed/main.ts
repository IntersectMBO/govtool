// `prisma db seed`, and the container start (dist/start.js) after the migration.
import { PrismaClient } from '@prisma/client';
import { loadSecretsIntoEnv } from '../config/secrets';
import { seedLookups } from './seed-lookups';

export async function main(): Promise<void> {
  loadSecretsIntoEnv();
  const prisma = new PrismaClient();
  try {
    await seedLookups(prisma);
    console.log('Seeded lookup tables.');
  } finally {
    await prisma.$disconnect();
  }
}

if (require.main === module) {
  main().catch((e: unknown) => {
    console.error(e instanceof Error ? e.message : e);
    process.exit(1);
  });
}
