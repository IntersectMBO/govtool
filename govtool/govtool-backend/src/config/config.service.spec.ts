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
