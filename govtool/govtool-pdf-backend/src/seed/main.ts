// `prisma db seed` and the container start (after `prisma migrate deploy`).
import { PrismaClient } from '@prisma/client';
import { loadSecretsIntoEnv } from '../config/secrets';
import { seedLookups } from './seed-lookups';

async function main() {
  loadSecretsIntoEnv();
  const prisma = new PrismaClient();
  try {
    await seedLookups(prisma);
    console.log('Seeded lookup tables.');
  } finally {
    await prisma.$disconnect();
  }
}

main().catch((e: unknown) => {
  console.error(e instanceof Error ? e.message : e);
  process.exit(1);
});
