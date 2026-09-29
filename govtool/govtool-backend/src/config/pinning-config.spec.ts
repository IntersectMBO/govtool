import { ConfigService } from './config.service';

describe('ConfigService: pinning provider', () => {
  const saved = { ...process.env };

  beforeEach(() => {
    process.env = { ...saved, GOVTOOL_CHAIN_DATA_PROVIDER: 'fixture' };
    delete process.env.GOVTOOL_PINNING_PROVIDER;
    delete process.env.GOVTOOL_TEST_PINNING_URL;
  });

  afterAll(() => {
    process.env = saved;
  });

  it('defaults to pinata with no test url', () => {
    expect(new ConfigService().get()).toMatchObject({
      pinningProvider: 'pinata',
      testPinningUrl: null,
    });
  });

  it('ignores the test url unless the provider is test', () => {
    process.env.GOVTOOL_TEST_PINNING_URL = 'http://localhost:3000';
    expect(new ConfigService().get().testPinningUrl).toBeNull();
  });

  it('selects the test pinning service with its url', () => {
    process.env.GOVTOOL_PINNING_PROVIDER = 'Test';
    process.env.GOVTOOL_TEST_PINNING_URL = 'http://test-metadata-api:3000';
    expect(new ConfigService().get()).toMatchObject({
      pinningProvider: 'test',
      testPinningUrl: 'http://test-metadata-api:3000',
    });
  });

  it('refuses test without a url', () => {
    process.env.GOVTOOL_PINNING_PROVIDER = 'test';
    expect(() => new ConfigService()).toThrow(
      'GOVTOOL_TEST_PINNING_URL is required',
    );
  });

  it('refuses a non-http test url', () => {
    process.env.GOVTOOL_PINNING_PROVIDER = 'test';
    process.env.GOVTOOL_TEST_PINNING_URL = 'file:///etc/passwd';
    expect(() => new ConfigService()).toThrow(
      "GOVTOOL_TEST_PINNING_URL must be an http(s) url; got 'file:///etc/passwd'",
    );
  });

  it('refuses an unknown provider at startup', () => {
    process.env.GOVTOOL_PINNING_PROVIDER = 'web3storage';
    expect(() => new ConfigService()).toThrow(
      "GOVTOOL_PINNING_PROVIDER must be one of pinata, test; got 'web3storage'",
    );
  });
});
