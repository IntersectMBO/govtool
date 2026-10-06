import { mkdtempSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { loadSecretsIntoEnv, resolveSecret } from './secrets';

describe('resolveSecret', () => {
  const dir = mkdtempSync(join(tmpdir(), 'pdf-secrets-'));
  const file = (name: string, content: string) => {
    const path = join(dir, name);
    writeFileSync(path, content);
    return path;
  };

  it('prefers the environment variable', () => {
    const env = { JWT_SECRET: 'from-env', JWT_SECRET_FILE: file('a', 'from-file') };
    expect(resolveSecret('JWT_SECRET', env)).toBe('from-env');
  });

  it('falls back to <NAME>_FILE when the variable is blank, dropping one trailing newline', () => {
    const env = { JWT_SECRET: '  ', JWT_SECRET_FILE: file('b', 'from-file\n') };
    expect(resolveSecret('JWT_SECRET', env)).toBe('from-file');
  });

  it('treats a missing or whitespace-only file as unset', () => {
    expect(resolveSecret('JWT_SECRET', { JWT_SECRET_FILE: join(dir, 'missing') })).toBeUndefined();
    expect(resolveSecret('JWT_SECRET', { JWT_SECRET_FILE: file('c', ' \n') })).toBeUndefined();
  });

  it('defaults to the Swarm mount /run/secrets/<lowercase name>', () => {
    // Nothing is mounted there in a test run, so the default path reads as unset.
    expect(resolveSecret('REFRESH_SECRET', {})).toBeUndefined();
  });
});

describe('loadSecretsIntoEnv', () => {
  it('writes file-backed secrets into the environment and leaves the rest alone', () => {
    const dir = mkdtempSync(join(tmpdir(), 'pdf-secrets-'));
    const urlFile = join(dir, 'database_url');
    writeFileSync(urlFile, 'postgresql://pdf:x@postgres:5432/pdf\n');
    const env: Record<string, string | undefined> = {
      DATABASE_URL_FILE: urlFile,
      JWT_SECRET: 'kept',
      REFRESH_SECRET_FILE: join(dir, 'missing'),
    };
    loadSecretsIntoEnv(env);
    expect(env.DATABASE_URL).toBe('postgresql://pdf:x@postgres:5432/pdf');
    expect(env.JWT_SECRET).toBe('kept');
    expect(env.REFRESH_SECRET).toBeUndefined();
  });
});
