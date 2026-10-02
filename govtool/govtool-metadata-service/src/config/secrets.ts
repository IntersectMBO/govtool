import { readFileSync } from 'fs';

/**
 * Read a secret: `process.env[name]` wins; when it is missing or blank,
 * read the file at `process.env[name_FILE]`, defaulting to the Swarm
 * secret mount `/run/secrets/<lowercase name>`. A missing file counts as
 * unset, and a whitespace-only file counts as unset, so an optional secret
 * can be created as `" "`.
 */
export function resolveSecret(name: string): string | undefined {
  const direct = process.env[name];
  if (direct !== undefined && direct.trim() !== '') {
    return direct;
  }
  const file =
    process.env[`${name}_FILE`] ?? `/run/secrets/${name.toLowerCase()}`;
  let content: string;
  try {
    content = readFileSync(file, 'utf8');
  } catch {
    return undefined;
  }
  const value = content.replace(/\r?\n$/, '');
  return value.trim() === '' ? undefined : value;
}
