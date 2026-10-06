// Secrets that may come from a file instead of the environment (SPEC §10),
// as govtool-backend and govtool-metadata-service read theirs.
import { readFileSync } from 'node:fs';

export const SECRET_VARIABLES = ['DATABASE_URL', 'JWT_SECRET', 'REFRESH_SECRET'] as const;

type Env = Record<string, string | undefined>;

/**
 * `env[name]` wins; when it is missing or blank, the file at `env[name_FILE]`,
 * defaulting to the Swarm secret mount `/run/secrets/<lowercase name>`. A
 * missing or whitespace-only file counts as unset; one trailing newline is
 * dropped.
 */
export function resolveSecret(name: string, env: Env = process.env): string | undefined {
  const direct = env[name];
  if (direct !== undefined && direct.trim() !== '') return direct;
  const file = env[`${name}_FILE`] ?? `/run/secrets/${name.toLowerCase()}`;
  let content: string;
  try {
    content = readFileSync(file, 'utf8');
  } catch {
    return undefined;
  }
  const value = content.replace(/\r?\n$/, '');
  return value.trim() === '' ? undefined : value;
}

/**
 * Writes each file-backed secret into `env`, so Prisma, which reads only the
 * environment, and child processes see it. Idempotent.
 */
export function loadSecretsIntoEnv(env: Env = process.env): void {
  for (const name of SECRET_VARIABLES) {
    const value = resolveSecret(name, env);
    if (value !== undefined) env[name] = value;
  }
}
