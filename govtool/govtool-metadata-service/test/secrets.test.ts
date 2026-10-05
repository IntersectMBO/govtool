import { describe, it, expect, afterEach, afterAll } from 'vitest';
import { mkdtempSync, writeFileSync, rmSync } from 'fs';
import { tmpdir } from 'os';
import { join } from 'path';
import { resolveSecret } from '../src/config/secrets';

describe('resolveSecret', () => {
  const saved = { ...process.env };
  const dir = mkdtempSync(join(tmpdir(), 'govtool-secrets-'));

  const secretFile = (name: string, content: string): string => {
    const file = join(dir, name);
    writeFileSync(file, content);
    return file;
  };

  afterEach(() => {
    process.env = { ...saved };
  });

  afterAll(() => {
    rmSync(dir, { recursive: true, force: true });
  });

  it('reads the env var when set', () => {
    process.env.DATABASE_URL = 'postgresql://env/db';
    process.env.DATABASE_URL_FILE = secretFile('url', 'postgresql://file/db');
    expect(resolveSecret('DATABASE_URL')).toBe('postgresql://env/db');
  });

  it('reads the file when the env var is missing', () => {
    delete process.env.DATABASE_URL;
    process.env.DATABASE_URL_FILE = secretFile(
      'url',
      'postgresql://file/db\n',
    );
    expect(resolveSecret('DATABASE_URL')).toBe('postgresql://file/db');
  });

  it('treats a whitespace-only file as unset', () => {
    delete process.env.DATABASE_URL;
    process.env.DATABASE_URL_FILE = secretFile('url', '   ');
    expect(resolveSecret('DATABASE_URL')).toBeUndefined();
  });

  it('treats a missing file as unset', () => {
    delete process.env.DATABASE_URL;
    process.env.DATABASE_URL_FILE = join(dir, 'does-not-exist');
    expect(resolveSecret('DATABASE_URL')).toBeUndefined();
  });
});
