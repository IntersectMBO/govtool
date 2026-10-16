import { loadConfig, parseByteSize, parseDurationSeconds } from './config';

const BASE = {
  DATABASE_URL: 'postgresql://pdf:pdf@127.0.0.1:5442/pdf',
  JWT_SECRET: 'j'.repeat(32),
  REFRESH_SECRET: 'r'.repeat(32),
};

const fails = (env: Record<string, string>, variable: string) =>
  expect(() => loadConfig({ ...BASE, ...env })).toThrow(new RegExp(`^Invalid configuration: ${variable} `));

describe('loadConfig (§10)', () => {
  it('defaults', () => {
    expect(loadConfig(BASE)).toEqual({
      databaseUrl: BASE.DATABASE_URL,
      host: '0.0.0.0',
      port: 1337,
      jwtSecret: BASE.JWT_SECRET,
      jwtExpiresSeconds: 3600,
      refreshSecret: BASE.REFRESH_SECRET,
      refreshExpiresSeconds: 7 * 86400,
      refreshCookieSameSite: 'lax',
      refreshCookieSecure: false,
      challengeTtlSeconds: 300,
      corsOrigins: '*',
      cardanoNetworkId: null,
      govtoolApiBaseUrl: null,
      govtoolProxyAllowedPaths: ['proposal/enacted-details'],
      proxyTimeoutMs: 10000,
      proxyMaxBytes: 5242880,
      ipfsGatewayUrl: 'https://ipfs.io/ipfs',
      allowPrivateUrls: false,
      proxyHostRewrites: new Map(),
      bodyLimitBytes: 1024 * 1024,
    });
  });

  it('required and secret rules', () => {
    expect(() => loadConfig({ ...BASE, DATABASE_URL: '' })).toThrow('DATABASE_URL is required');
    expect(() => loadConfig({ ...BASE, JWT_SECRET: undefined })).toThrow('JWT_SECRET is required');
    fails({ JWT_SECRET: 'short' }, 'JWT_SECRET');
    fails({ REFRESH_SECRET: 'short' }, 'REFRESH_SECRET');
    fails({ REFRESH_SECRET: BASE.JWT_SECRET }, 'REFRESH_SECRET');
    fails({ DATABASE_URL: 'mysql://x' }, 'DATABASE_URL');
  });

  it('SameSite=none forces Secure and forbids CORS *', () => {
    fails({ REFRESH_COOKIE_SAMESITE: 'none' }, 'CORS_ORIGINS');
    fails({ REFRESH_COOKIE_SAMESITE: 'none', CORS_ORIGINS: '*' }, 'CORS_ORIGINS');
    const c = loadConfig({ ...BASE, REFRESH_COOKIE_SAMESITE: 'none', CORS_ORIGINS: 'https://gov.tools' });
    expect(c.refreshCookieSecure).toBe(true);
    expect(c.corsOrigins).toEqual(['https://gov.tools']);
    fails({ REFRESH_COOKIE_SAMESITE: 'loose' }, 'REFRESH_COOKIE_SAMESITE');
  });

  it('CORS origins must be exact origins', () => {
    expect(loadConfig({ ...BASE, CORS_ORIGINS: 'http://localhost:8080, https://a.b' }).corsOrigins).toEqual([
      'http://localhost:8080',
      'https://a.b',
    ]);
    fails({ CORS_ORIGINS: 'https://a.b/' }, 'CORS_ORIGINS');
    fails({ CORS_ORIGINS: 'a.b' }, 'CORS_ORIGINS');
    fails({ CORS_ORIGINS: ',' }, 'CORS_ORIGINS');
  });

  it('numbers, booleans, network, urls, sizes', () => {
    fails({ PORT: '0' }, 'PORT');
    fails({ PORT: '70000' }, 'PORT');
    fails({ PORT: 'x' }, 'PORT');
    fails({ CHALLENGE_TTL_SECONDS: '-5' }, 'CHALLENGE_TTL_SECONDS');
    fails({ PROXY_TIMEOUT_MS: '1.5' }, 'PROXY_TIMEOUT_MS');
    fails({ PDF_ALLOW_PRIVATE_URLS: 'yes' }, 'PDF_ALLOW_PRIVATE_URLS');
    fails({ REFRESH_COOKIE_SECURE: 'maybe' }, 'REFRESH_COOKIE_SECURE');
    fails({ CARDANO_NETWORK_ID: '2' }, 'CARDANO_NETWORK_ID');
    expect(loadConfig({ ...BASE, CARDANO_NETWORK_ID: '1' }).cardanoNetworkId).toBe(1);
    fails({ GOVTOOL_API_BASE_URL: 'ftp://x' }, 'GOVTOOL_API_BASE_URL');
    fails({ GOVTOOL_API_BASE_URL: 'http://u:p@x' }, 'GOVTOOL_API_BASE_URL');
    expect(loadConfig({ ...BASE, GOVTOOL_API_BASE_URL: 'http://127.0.0.1:9999/' }).govtoolApiBaseUrl).toBe(
      'http://127.0.0.1:9999',
    );
    fails({ IPFS_GATEWAY_URL: 'nope' }, 'IPFS_GATEWAY_URL');
    fails({ GOVTOOL_PROXY_ALLOWED_PATHS: 'a/../b' }, 'GOVTOOL_PROXY_ALLOWED_PATHS');
    fails({ BODY_LIMIT: 'lots' }, 'BODY_LIMIT');
    fails({ JWT_SECRET_EXPIRES: 'forever' }, 'JWT_SECRET_EXPIRES');
    expect(loadConfig({ ...BASE, PDF_ALLOW_PRIVATE_URLS: 'TRUE' }).allowPrivateUrls).toBe(true);
  });

  it('PDF_PROXY_HOST_REWRITES needs PDF_ALLOW_PRIVATE_URLS and host:port pairs', () => {
    const rw = '127.0.0.1:3001=host.docker.internal:3001';
    fails({ PDF_PROXY_HOST_REWRITES: rw }, 'PDF_PROXY_HOST_REWRITES');
    fails({ PDF_ALLOW_PRIVATE_URLS: 'false', PDF_PROXY_HOST_REWRITES: rw }, 'PDF_PROXY_HOST_REWRITES');
    const on = { PDF_ALLOW_PRIVATE_URLS: 'true' };
    expect(
      loadConfig({ ...BASE, ...on, PDF_PROXY_HOST_REWRITES: `${rw}, Localhost:3001=[::1]:4001` })
        .proxyHostRewrites,
    ).toEqual(
      new Map([
        ['127.0.0.1:3001', { host: 'host.docker.internal', port: 3001 }],
        ['localhost:3001', { host: '[::1]', port: 4001 }],
      ]),
    );
    for (const bad of [
      '127.0.0.1=h:1',
      'a:1=b',
      'a:1=b:2=c:3',
      'a:1',
      'http://a:1=b:2',
      'a:1/x=b:2',
      'u@a:1=b:2',
    ]) {
      fails({ ...on, PDF_PROXY_HOST_REWRITES: bad }, 'PDF_PROXY_HOST_REWRITES');
    }
  });

  it('duration and size parsers', () => {
    expect(parseDurationSeconds('90')).toBe(90);
    expect(parseDurationSeconds('15m')).toBe(900);
    expect(parseDurationSeconds('1h')).toBe(3600);
    expect(parseDurationSeconds('7d')).toBe(604800);
    expect(parseDurationSeconds('0')).toBeNull();
    expect(parseByteSize('1mb')).toBe(1048576);
    expect(parseByteSize('512kb')).toBe(524288);
    expect(parseByteSize('100')).toBe(100);
  });
});
