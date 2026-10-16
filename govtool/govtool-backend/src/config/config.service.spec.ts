import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';

import { ConfigService } from './config.service';

describe('ConfigService: db-sync network', () => {
  const saved = { ...process.env };

  beforeEach(() => {
    process.env = {
      ...saved,
      GOVTOOL_CHAIN_DATA_PROVIDER: 'dbsync',
      GOVTOOL_DBSYNC_HOST: 'localhost',
      GOVTOOL_DBSYNC_DATABASE: 'cexplorer',
      GOVTOOL_DBSYNC_USER: 'postgres',
      GOVTOOL_DBSYNC_PASSWORD: 'postgres',
    };
    delete process.env.GOVTOOL_DBSYNC_NETWORK;
    delete process.env.GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH;
    delete process.env.GOVTOOL_DBSYNC_NETWORK_NAME;
  });

  afterAll(() => {
    process.env = saved;
  });

  it('accepts devnet with its genesis path and expected db-sync name', () => {
    process.env.GOVTOOL_DBSYNC_NETWORK = 'DevNet';
    process.env.GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH = '/genesis/shelley.json';
    process.env.GOVTOOL_DBSYNC_NETWORK_NAME = 'testnet';
    expect(new ConfigService().get().dbSync).toMatchObject({
      network: 'devnet',
      shelleyGenesisPath: '/genesis/shelley.json',
      networkName: 'testnet',
    });
  });

  it('leaves the genesis path and expected name unset by default', () => {
    expect(new ConfigService().get().dbSync).toMatchObject({
      network: 'mainnet',
      shelleyGenesisPath: null,
      networkName: null,
    });
  });

  it('still rejects an unknown network name at startup', () => {
    process.env.GOVTOOL_DBSYNC_NETWORK = 'sanchonet';
    expect(() => new ConfigService()).toThrow(
      "GOVTOOL_DBSYNC_NETWORK must be mainnet, preprod, preview or devnet; got 'sanchonet'",
    );
  });
});

describe('ConfigService: file-backed secrets', () => {
  const saved = { ...process.env };
  let dir: string;

  const secretFile = (name: string, content: string): string => {
    const file = path.join(dir, name);
    fs.writeFileSync(file, content);
    return file;
  };

  beforeEach(() => {
    dir = fs.mkdtempSync(path.join(os.tmpdir(), 'govtool-secrets-'));
    process.env = {
      ...saved,
      GOVTOOL_CHAIN_DATA_PROVIDER: 'dbsync',
      GOVTOOL_DBSYNC_HOST: 'localhost',
      GOVTOOL_DBSYNC_DATABASE: 'cexplorer',
      GOVTOOL_DBSYNC_USER: 'postgres',
    };
    delete process.env.GOVTOOL_DBSYNC_PASSWORD;
    delete process.env.GOVTOOL_DBSYNC_PASSWORD_FILE;
    delete process.env.GOVTOOL_PINATA_API_JWT;
    delete process.env.GOVTOOL_PINATA_API_JWT_FILE;
  });

  afterEach(() => {
    fs.rmSync(dir, { recursive: true, force: true });
  });

  afterAll(() => {
    process.env = saved;
  });

  it('reads the db-sync password from the file when the env var is missing', () => {
    process.env.GOVTOOL_DBSYNC_PASSWORD_FILE = secretFile(
      'password',
      'file-password\n',
    );
    expect(new ConfigService().get().dbSync).toMatchObject({
      password: 'file-password',
    });
  });

  it('prefers the env var over the file', () => {
    process.env.GOVTOOL_DBSYNC_PASSWORD = 'env-password';
    process.env.GOVTOOL_DBSYNC_PASSWORD_FILE = secretFile(
      'password',
      'file-password',
    );
    expect(new ConfigService().get().dbSync).toMatchObject({
      password: 'env-password',
    });
  });

  it('treats a whitespace-only secret file as unset', () => {
    process.env.GOVTOOL_DBSYNC_PASSWORD_FILE = secretFile('password', '   ');
    expect(() => new ConfigService()).toThrow(
      'GOVTOOL_DBSYNC_PASSWORD is required (or set GOVTOOL_DBSYNC_PASSWORD_FILE)',
    );
  });

  it('still throws when neither the env var nor the file is present', () => {
    process.env.GOVTOOL_DBSYNC_PASSWORD_FILE = path.join(dir, 'does-not-exist');
    expect(() => new ConfigService()).toThrow(
      'GOVTOOL_DBSYNC_PASSWORD is required (or set GOVTOOL_DBSYNC_PASSWORD_FILE)',
    );
  });

  it('reads the optional Pinata JWT from the file', () => {
    process.env.GOVTOOL_DBSYNC_PASSWORD = 'postgres';
    process.env.GOVTOOL_PINATA_API_JWT_FILE = secretFile('jwt', 'file-jwt');
    expect(new ConfigService().get().pinataApiJwt).toBe('file-jwt');
  });
});
