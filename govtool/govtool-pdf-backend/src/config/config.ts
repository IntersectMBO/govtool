// Every environment variable of SPEC §10, read once and validated. An invalid
// value throws ConfigError naming the variable; main.ts exits on it.

export type SameSite = 'lax' | 'strict' | 'none';

export interface AppConfig {
  databaseUrl: string;
  host: string;
  port: number;
  jwtSecret: string;
  jwtExpiresSeconds: number;
  refreshSecret: string;
  refreshExpiresSeconds: number;
  refreshCookieSameSite: SameSite;
  refreshCookieSecure: boolean;
  challengeTtlSeconds: number;
  /** `'*'` reflects any origin; otherwise the exact allowed origins. */
  corsOrigins: '*' | string[];
  cardanoNetworkId: 0 | 1 | null;
  govtoolApiBaseUrl: string | null;
  govtoolProxyAllowedPaths: string[];
  proxyTimeoutMs: number;
  proxyMaxBytes: number;
  ipfsGatewayUrl: string;
  allowPrivateUrls: boolean;
  /** `hostname:port` -> connect target for §9.2; test-only, needs allowPrivateUrls. */
  proxyHostRewrites: Map<string, { host: string; port: number }>;
  bodyLimitBytes: number;
}

export class ConfigError extends Error {
  constructor(
    public readonly variable: string,
    reason: string,
  ) {
    super(`Invalid configuration: ${variable} ${reason}`);
    this.name = 'ConfigError';
  }
}

type Env = Record<string, string | undefined>;

function read(env: Env, name: string): string | undefined {
  const v = env[name];
  if (v === undefined) return undefined;
  const t = v.trim();
  return t === '' ? undefined : t;
}

function required(env: Env, name: string): string {
  const v = read(env, name);
  if (v === undefined) throw new ConfigError(name, 'is required');
  return v;
}

function positiveInt(env: Env, name: string, def: number, max?: number) {
  const v = read(env, name);
  if (v === undefined) return def;
  if (!/^\d+$/.test(v)) throw new ConfigError(name, 'must be a positive integer');
  const n = Number(v);
  if (n < 1 || (max !== undefined && n > max) || !Number.isSafeInteger(n)) {
    throw new ConfigError(name, `must be between 1 and ${max ?? 'MAX_SAFE_INTEGER'}`);
  }
  return n;
}

function bool(env: Env, name: string, def: boolean): boolean {
  const v = read(env, name);
  if (v === undefined) return def;
  const l = v.toLowerCase();
  if (l === 'true' || l === '1') return true;
  if (l === 'false' || l === '0') return false;
  throw new ConfigError(name, 'must be true or false');
}

const DURATION_UNITS: Record<string, number> = {
  '': 1,
  s: 1,
  m: 60,
  h: 3600,
  d: 86400,
  w: 604800,
  y: 31557600,
};

/** `ms`-style duration (`90`, `30s`, `15m`, `1h`, `7d`) to whole seconds. */
export function parseDurationSeconds(value: string): number | null {
  const m = /^(\d+)\s*(s|m|h|d|w|y)?$/i.exec(value.trim());
  if (!m) return null;
  const n = Number(m[1]) * DURATION_UNITS[(m[2] ?? '').toLowerCase()];
  return n >= 1 && Number.isSafeInteger(n) ? n : null;
}

function duration(env: Env, name: string, def: string): number {
  const v = read(env, name) ?? def;
  const s = parseDurationSeconds(v);
  if (s === null) {
    throw new ConfigError(name, 'must be a duration such as 3600, 15m, 1h or 7d');
  }
  return s;
}

/** Body-parser style size (`1mb`, `512kb`, `1048576`) to bytes. */
export function parseByteSize(value: string): number | null {
  const m = /^(\d+)\s*(b|kb|mb|gb)?$/i.exec(value.trim());
  if (!m) return null;
  const mult = { '': 1, b: 1, kb: 1024, mb: 1024 ** 2, gb: 1024 ** 3 }[
    (m[2] ?? '').toLowerCase() as '' | 'b' | 'kb' | 'mb' | 'gb'
  ];
  const n = Number(m[1]) * mult;
  return n >= 1 && Number.isSafeInteger(n) ? n : null;
}

function httpUrl(name: string, v: string): string {
  let u: URL;
  try {
    u = new URL(v);
  } catch {
    throw new ConfigError(name, 'must be an absolute http(s) URL');
  }
  if (u.protocol !== 'http:' && u.protocol !== 'https:') {
    throw new ConfigError(name, 'must be an absolute http(s) URL');
  }
  if (u.username || u.password) {
    throw new ConfigError(name, 'must not carry credentials');
  }
  return v.replace(/\/+$/, '');
}

/** `host:port` (hostname, IPv4 or bracketed IPv6, explicit port) or null. */
function hostPort(v: string): { host: string; port: number } | null {
  let u: URL;
  try {
    u = new URL(`http://${v}`);
  } catch {
    return null;
  }
  if (u.host !== v.toLowerCase() || u.port === '' || u.username || u.password) return null;
  return { host: u.hostname, port: Number(u.port) };
}

/** PDF_PROXY_HOST_REWRITES: comma list of `from-host:port=to-host:port`. */
function hostRewrites(env: Env, allowPrivateUrls: boolean) {
  const name = 'PDF_PROXY_HOST_REWRITES';
  const out = new Map<string, { host: string; port: number }>();
  const raw = read(env, name);
  if (raw === undefined) return out;
  if (!allowPrivateUrls) throw new ConfigError(name, 'requires PDF_ALLOW_PRIVATE_URLS=true');
  for (const entry of list(raw)) {
    const [from, to, extra] = entry.split('=').map((s) => s.trim());
    const f = from ? hostPort(from) : null;
    const t = to ? hostPort(to) : null;
    if (extra !== undefined || !f || !t) {
      throw new ConfigError(name, `entry "${entry}" is not host:port=host:port`);
    }
    out.set(`${f.host}:${f.port}`, t);
  }
  return out;
}

function list(v: string): string[] {
  return v
    .split(',')
    .map((s) => s.trim())
    .filter((s) => s !== '');
}

export function loadConfig(env: Env = process.env): AppConfig {
  const databaseUrl = required(env, 'DATABASE_URL');
  if (!/^postgres(ql)?:\/\//.test(databaseUrl)) {
    throw new ConfigError('DATABASE_URL', 'must be a postgresql:// URL');
  }

  const jwtSecret = required(env, 'JWT_SECRET');
  if (jwtSecret.length < 32) {
    throw new ConfigError('JWT_SECRET', 'must be at least 32 characters');
  }
  const refreshSecret = required(env, 'REFRESH_SECRET');
  if (refreshSecret.length < 32) {
    throw new ConfigError('REFRESH_SECRET', 'must be at least 32 characters');
  }
  if (refreshSecret === jwtSecret) {
    throw new ConfigError('REFRESH_SECRET', 'must differ from JWT_SECRET');
  }

  const sameSiteRaw = (read(env, 'REFRESH_COOKIE_SAMESITE') ?? 'lax').toLowerCase();
  if (sameSiteRaw !== 'lax' && sameSiteRaw !== 'strict' && sameSiteRaw !== 'none') {
    throw new ConfigError('REFRESH_COOKIE_SAMESITE', 'must be lax, strict or none');
  }
  const refreshCookieSameSite: SameSite = sameSiteRaw;
  const refreshCookieSecure = bool(env, 'REFRESH_COOKIE_SECURE', false) || refreshCookieSameSite === 'none';

  const corsRaw = read(env, 'CORS_ORIGINS') ?? '*';
  let corsOrigins: '*' | string[];
  if (corsRaw === '*') {
    corsOrigins = '*';
  } else {
    corsOrigins = list(corsRaw);
    if (corsOrigins.length === 0) {
      throw new ConfigError('CORS_ORIGINS', 'must be * or a comma-separated list of origins');
    }
    for (const o of corsOrigins) {
      let origin: string | null = null;
      try {
        origin = new URL(o).origin;
      } catch {
        origin = null;
      }
      if (origin !== o) {
        throw new ConfigError(
          'CORS_ORIGINS',
          `entry "${o}" is not an exact origin (scheme://host[:port], no path or trailing slash)`,
        );
      }
    }
  }
  if (refreshCookieSameSite === 'none' && corsOrigins === '*') {
    throw new ConfigError(
      'CORS_ORIGINS',
      'must be an explicit origin list when REFRESH_COOKIE_SAMESITE=none (a cross-site refresh cookie with reflected CORS lets any site read a fresh JWT)',
    );
  }

  const netRaw = read(env, 'CARDANO_NETWORK_ID');
  let cardanoNetworkId: 0 | 1 | null = null;
  if (netRaw !== undefined) {
    if (netRaw !== '0' && netRaw !== '1') {
      throw new ConfigError('CARDANO_NETWORK_ID', 'must be 0 or 1 when set');
    }
    cardanoNetworkId = netRaw === '1' ? 1 : 0;
  }

  const govtoolRaw = read(env, 'GOVTOOL_API_BASE_URL');
  const govtoolApiBaseUrl = govtoolRaw === undefined ? null : httpUrl('GOVTOOL_API_BASE_URL', govtoolRaw);

  const govtoolProxyAllowedPaths = list(
    read(env, 'GOVTOOL_PROXY_ALLOWED_PATHS') ?? 'proposal/enacted-details',
  ).map((p) => p.replace(/^\/+|\/+$/g, ''));
  for (const p of govtoolProxyAllowedPaths) {
    if (p.includes('..') || p.includes('\\') || p.includes('//') || p.includes('%')) {
      throw new ConfigError('GOVTOOL_PROXY_ALLOWED_PATHS', `entry "${p}" is not a plain path`);
    }
  }

  const bodyLimitRaw = read(env, 'BODY_LIMIT') ?? '1mb';
  const bodyLimitBytes = parseByteSize(bodyLimitRaw);
  if (bodyLimitBytes === null) {
    throw new ConfigError('BODY_LIMIT', 'must be a size such as 1mb, 512kb or 1048576');
  }

  const allowPrivateUrls = bool(env, 'PDF_ALLOW_PRIVATE_URLS', false);

  return {
    databaseUrl,
    host: read(env, 'HOST') ?? '0.0.0.0',
    port: positiveInt(env, 'PORT', 1337, 65535),
    jwtSecret,
    jwtExpiresSeconds: duration(env, 'JWT_SECRET_EXPIRES', '1h'),
    refreshSecret,
    refreshExpiresSeconds: duration(env, 'REFRESH_TOKEN_EXPIRES', '7d'),
    refreshCookieSameSite,
    refreshCookieSecure,
    challengeTtlSeconds: positiveInt(env, 'CHALLENGE_TTL_SECONDS', 300),
    corsOrigins,
    cardanoNetworkId,
    govtoolApiBaseUrl,
    govtoolProxyAllowedPaths,
    proxyTimeoutMs: positiveInt(env, 'PROXY_TIMEOUT_MS', 10000),
    proxyMaxBytes: positiveInt(env, 'PROXY_MAX_BYTES', 5242880),
    ipfsGatewayUrl: httpUrl('IPFS_GATEWAY_URL', read(env, 'IPFS_GATEWAY_URL') ?? 'https://ipfs.io/ipfs'),
    allowPrivateUrls,
    proxyHostRewrites: hostRewrites(env, allowPrivateUrls),
    bodyLimitBytes,
  };
}
