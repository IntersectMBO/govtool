// The container's command: resolve file-backed secrets once, migrate, seed the
// lookup tables, then serve, with node as PID 1 so SIGTERM reaches the app.
// Prisma has no programmatic migrate API, so only the migration is a child
// process; it inherits the resolved DATABASE_URL.
import { spawnSync } from 'node:child_process';
import { loadSecretsIntoEnv } from './config/secrets';
import { bootstrap } from './main';
import { main as seedLookupTables } from './seed/main';

async function start(): Promise<void> {
  loadSecretsIntoEnv();
  const migrate = spawnSync(
    process.execPath,
    [require.resolve('prisma/build/index.js'), 'migrate', 'deploy', '--schema', 'prisma/schema.prisma'],
    { stdio: 'inherit' },
  );
  if (migrate.status !== 0) {
    process.exit(migrate.status ?? 1);
  }
  await seedLookupTables();
  await bootstrap();
}

start().catch((e: unknown) => {
  console.error(e instanceof Error ? e.message : e);
  process.exit(1);
});
