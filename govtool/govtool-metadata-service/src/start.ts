// The container's command: resolve DATABASE_URL from its secret file, apply
// migrations, then serve, with node as PID 1 so SIGTERM reaches the app.
// Prisma has no programmatic migrate API, so the migration is a child
// process; it inherits the resolved DATABASE_URL and reads the compiled
// prisma.config.mjs, so no TypeScript is loaded at runtime.
import { spawnSync } from 'child_process';
import path from 'path';
import { resolveSecret } from './config/secrets';

const databaseUrl = resolveSecret('DATABASE_URL');
if (databaseUrl !== undefined) process.env.DATABASE_URL = databaseUrl;

const migrate = spawnSync(
  process.execPath,
  [
    require.resolve('prisma/build/index.js'),
    'migrate',
    'deploy',
    '--config',
    path.join(__dirname, 'prisma.config.mjs'),
  ],
  { stdio: 'inherit' },
);
if (migrate.status !== 0) {
  process.exit(migrate.status ?? 1);
}

void import('./index');
