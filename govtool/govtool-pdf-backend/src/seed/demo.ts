// `npm run seed:demo` (host) or `node dist/seed/demo.js` (container).
import { PrismaClient } from '@prisma/client';
import { loadSecretsIntoEnv } from '../config/secrets';
import { seedLookups } from './seed-lookups';
import { seedDemo } from './seed-demo';

async function main() {
  loadSecretsIntoEnv();
  const prisma = new PrismaClient();
  try {
    await seedLookups(prisma);
    const wrote = await seedDemo(prisma);
    console.log(wrote ? 'Demo data inserted.' : 'Demo data already present; nothing to do.');
  } finally {
    await prisma.$disconnect();
  }
}

main().catch((e: unknown) => {
  console.error(e instanceof Error ? e.message : e);
  process.exit(1);
});
