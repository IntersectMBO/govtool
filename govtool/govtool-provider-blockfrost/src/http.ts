/**
 * The HTTP surface this provider is built on: the global `fetch`, a bounded
 * number of requests in flight, retries, and error mapping.
 *
 * Everything it throws is a `ChainDataError`. No error it builds carries a
 * request header, the request URL's query, a response body beyond Blockfrost's
 * own short `message`, or a `cause` object — so the `project_id` cannot leak
 * into a log line or an HTTP response through an error.
 */
import { ChainDataError, type NetworkId } from '@govtool/data-providers/chain-data';

import { parseExactJson, type ExactJson } from './json';

/** Blockfrost's hard cap on `count`. */
export const BF_PAGE = 100;

/** Hosted Blockfrost, by network. Any other network needs an explicit `baseUrl`. */
export const HOSTED_URLS: Record<string, string> = {
  mainnet: 'https://cardano-mainnet.blockfrost.io/api/v0',
  preprod: 'https://cardano-preprod.blockfrost.io/api/v0',
  preview: 'https://cardano-preview.blockfrost.io/api/v0',
};

export interface HttpOptions {
  network: NetworkId;
  /** Overrides the hosted URL for `network`, e.g. a self-hosted blockfrost-ryo. */
  baseUrl?: string;
  /** Sent as the `project_id` header. Hosted Blockfrost requires it. */
  projectId?: string;
  /** Per-request timeout, ms. Default 30 000. */
  timeoutMs?: number;
  /** Requests in flight at once. Default 8. */
  maxConcurrency?: number;
  /** Retries of a retryable failure (429, 5xx, transport). Default 3. */
  maxRetries?: number;
  /** Longest `Retry-After` honoured by waiting, s; above it the 429 is thrown. Default 30. */
  maxRetryAfterSeconds?: number;
  /**
   * Client-side request rate, as a token bucket. Defaults to hosted
   * Blockfrost's published limit (10 per second, bursts of 500) when no
   * `baseUrl` is given, and to none for a self-hosted one. `null` disables it.
   */
  rateLimit?: { perSecond: number; burst: number } | null;
  /** Injected for tests: the clock the rate limit reads, ms. */
  now?: () => number;
  fetch?: typeof fetch;
  sleep?: (ms: number) => Promise<void>;
}

export interface PageQuery {
  count?: number;
  page?: number;
  order?: 'asc' | 'desc';
}

/** A counting semaphore: at most `limit` holders at once, FIFO. */
export class Limiter {
  private active = 0;
  private readonly waiting: (() => void)[] = [];

  constructor(private readonly limit: number) {}

  async run<T>(task: () => Promise<T>): Promise<T> {
    if (this.active >= this.limit) await new Promise<void>((resolve) => this.waiting.push(resolve));
    this.active++;
    try {
      return await task();
    } finally {
      this.active--;
      this.waiting.shift()?.();
    }
  }
}

/** Hosted Blockfrost's published limit: 10 requests per second, bursts of 500. */
export const HOSTED_RATE_LIMIT = { perSecond: 10, burst: 500 };

/**
 * A token bucket. Hosted Blockfrost answers 429 once a client outruns its
 * limit, so a large read (the DRep directory with its certificates is ~4k
 * requests) must pace itself rather than burn its retries on refusals.
 */
export class TokenBucket {
  private tokens: number;
  private last: number;

  constructor(
    private readonly perSecond: number,
    private readonly burst: number,
    private readonly now: () => number,
    private readonly sleep: (ms: number) => Promise<void>,
  ) {
    this.tokens = burst;
    this.last = now();
  }

  private refill(): void {
    const t = this.now();
    this.tokens = Math.min(this.burst, this.tokens + ((t - this.last) / 1000) * this.perSecond);
    this.last = t;
  }

  async take(): Promise<void> {
    for (;;) {
      this.refill();
      if (this.tokens >= 1) {
        this.tokens -= 1;
        return;
      }
      await this.sleep(Math.ceil(((1 - this.tokens) / this.perSecond) * 1000));
    }
  }

  /** After a 429 the server's view wins: stop bursting until the bucket refills. */
  drain(): void {
    this.refill();
    this.tokens = Math.min(this.tokens, 0);
  }
}

/** Blockfrost's error envelope message, trimmed; never the raw body. */
function safeMessage(body: string): string | undefined {
  try {
    const parsed = JSON.parse(body) as { message?: unknown };
    return typeof parsed.message === 'string' ? parsed.message.slice(0, 200) : undefined;
  } catch {
    return undefined;
  }
}

/** Seconds from a `Retry-After` header: delta-seconds or an HTTP date. */
export function retryAfterSeconds(header: string | null, now = Date.now()): number | undefined {
  if (header === null || header.trim() === '') return undefined;
  const t = header.trim();
  if (/^\d+$/.test(t)) return Number(t);
  const at = Date.parse(t);
  return Number.isNaN(at) ? undefined : Math.max(0, Math.ceil((at - now) / 1000));
}

function statusError(
  status: number,
  path: string,
  body: string,
  retryAfter: number | undefined,
  secret: string | undefined,
): ChainDataError {
  // Blockfrost's messages do not echo the project id; this makes sure of it.
  const raw = safeMessage(body);
  const message = raw && secret ? raw.split(secret).join('[redacted]') : raw;
  const details = { path, status, ...(message ? { message } : {}) };
  switch (status) {
    case 400:
      return new ChainDataError('INVALID_INPUT', 'Blockfrost rejected the request', { details });
    case 402:
      // "Usage is over limit": the project's daily quota is spent.
      return new ChainDataError('PROVIDER_RATE_LIMITED', 'Blockfrost project quota exhausted', { details });
    case 403:
      // Missing or invalid project id. The id itself is never echoed.
      return new ChainDataError('PROVIDER_UNAVAILABLE', 'Blockfrost refused the project credentials', {
        details: { path, status },
      });
    case 404:
      return new ChainDataError('NOT_FOUND', 'Blockfrost has no such resource', { details });
    case 418:
      return new ChainDataError('PROVIDER_RATE_LIMITED', 'Blockfrost has banned this client for exceeding its rate limit', {
        details,
      });
    case 429:
      return new ChainDataError('PROVIDER_RATE_LIMITED', 'Blockfrost rate limit exceeded', {
        retryable: true,
        ...(retryAfter === undefined ? {} : { retryAfterSeconds: retryAfter }),
        details,
      });
    case 504:
      return new ChainDataError('PROVIDER_TIMEOUT', 'Blockfrost timed out', { retryable: true, details });
    default:
      return status >= 500
        ? new ChainDataError('PROVIDER_UNAVAILABLE', `Blockfrost returned ${status}`, { retryable: true, details })
        : new ChainDataError('INTERNAL', `Blockfrost returned ${status}`, { details });
  }
}

function parse<T>(path: string, text: string, parser: (text: string) => T): T {
  try {
    return parser(text);
  } catch {
    throw new ChainDataError('INTERNAL', 'Blockfrost returned a body that is not JSON', { details: { path } });
  }
}

export class BlockfrostHttp {
  readonly baseUrl: string;
  private readonly headers: Record<string, string>;
  private readonly secret: string | undefined;
  private readonly timeoutMs: number;
  private readonly maxRetries: number;
  private readonly maxRetryAfter: number;
  private readonly fetchImpl: typeof fetch;
  private readonly sleep: (ms: number) => Promise<void>;
  private readonly limiter: Limiter;
  private readonly bucket: TokenBucket | undefined;

  constructor(options: HttpOptions) {
    const base = options.baseUrl?.trim() || HOSTED_URLS[options.network];
    if (!base) throw new Error(`No hosted Blockfrost URL for network '${options.network}'; pass baseUrl`);
    const url = new URL(base);
    if (url.protocol !== 'https:' && url.protocol !== 'http:') throw new Error('Blockfrost baseUrl must be http(s)');
    this.baseUrl = base.replace(/\/+$/, '');
    // The only place the project id lives: a header on outgoing requests.
    this.headers = { Accept: 'application/json', ...(options.projectId ? { project_id: options.projectId } : {}) };
    this.secret = options.projectId || undefined;
    this.timeoutMs = options.timeoutMs ?? 30_000;
    this.maxRetries = options.maxRetries ?? 3;
    this.maxRetryAfter = options.maxRetryAfterSeconds ?? 30;
    this.fetchImpl = options.fetch ?? fetch;
    this.sleep = options.sleep ?? ((ms) => new Promise((resolve) => setTimeout(resolve, ms)));
    this.limiter = new Limiter(Math.max(1, options.maxConcurrency ?? 8));
    const rate = options.rateLimit === undefined ? (options.baseUrl?.trim() ? null : HOSTED_RATE_LIMIT) : options.rateLimit;
    this.bucket = rate ? new TokenBucket(rate.perSecond, rate.burst, options.now ?? Date.now, this.sleep) : undefined;
  }

  /** Parsed JSON. 404 throws NOT_FOUND. */
  async get<T>(path: string, query?: PageQuery): Promise<T> {
    return parse(path, (await this.text(path, query, false))!, JSON.parse) as T;
  }

  /** As `get`, but 404 is `null`. */
  async getOrNull<T>(path: string, query?: PageQuery): Promise<T | null> {
    const text = await this.text(path, query, true);
    return text === null ? null : (parse(path, text, JSON.parse) as T);
  }

  /** JSON with every number kept as its source literal; see ./json. */
  async getExact(path: string): Promise<ExactJson> {
    return parse(path, (await this.text(path, undefined, false))!, parseExactJson);
  }

  async getExactOrNull(path: string): Promise<ExactJson | null> {
    const text = await this.text(path, undefined, true);
    return text === null ? null : parse(path, text, parseExactJson);
  }

  /**
   * Every row of a paged Blockfrost collection. Pages are requested `batch`
   * at a time and reading stops at the first short page, so a collection of
   * n rows costs ceil(n / 100) requests plus at most `batch - 1` empty ones.
   */
  async getAll<T>(path: string, options: { order?: 'asc' | 'desc'; batch?: number } = {}): Promise<T[]> {
    const batch = Math.max(1, options.batch ?? 4);
    const out: T[] = [];
    for (let first = 1; ; first += batch) {
      const pages = await Promise.all(
        Array.from({ length: batch }, (_, i) =>
          this.get<T[]>(path, { count: BF_PAGE, page: first + i, ...(options.order ? { order: options.order } : {}) }),
        ),
      );
      for (const rows of pages) {
        if (!Array.isArray(rows)) throw new ChainDataError('INTERNAL', 'Blockfrost returned a page that is not a list', { details: { path } });
        out.push(...rows);
        if (rows.length < BF_PAGE) return out;
      }
    }
  }

  private url(path: string, query?: PageQuery): string {
    const url = new URL(this.baseUrl + path);
    if (query?.count !== undefined) url.searchParams.set('count', String(query.count));
    if (query?.page !== undefined) url.searchParams.set('page', String(query.page));
    if (query?.order !== undefined) url.searchParams.set('order', query.order);
    return url.toString();
  }

  private async text(path: string, query: PageQuery | undefined, nullOn404: boolean): Promise<string | null> {
    const url = this.url(path, query);
    let last: ChainDataError | undefined;
    for (let attempt = 0; attempt <= this.maxRetries; attempt++) {
      const outcome = await this.limiter.run(async () => {
        await this.bucket?.take();
        return this.once(url, path, nullOn404);
      });
      if (outcome.kind === 'ok') return outcome.text;
      last = outcome.error;
      const limited = outcome.error.code === 'PROVIDER_RATE_LIMITED';
      if (limited) this.bucket?.drain();
      if (!outcome.error.retryable || attempt === this.maxRetries) break;
      const wait = outcome.error.retryAfterSeconds;
      if (wait !== undefined && wait > this.maxRetryAfter) break;
      // Waits outside the limiter, so a backing-off request holds no slot. A
      // 429 without Retry-After backs off in seconds, not milliseconds.
      await this.sleep(wait !== undefined ? wait * 1000 : (limited ? 1000 : 250) * 2 ** attempt);
    }
    throw last!;
  }

  private async once(
    url: string,
    path: string,
    nullOn404: boolean,
  ): Promise<{ kind: 'ok'; text: string | null } | { kind: 'error'; error: ChainDataError }> {
    let response: Response;
    try {
      response = await this.fetchImpl(url, { headers: this.headers, signal: AbortSignal.timeout(this.timeoutMs) });
    } catch (error) {
      const timedOut = error instanceof Error && (error.name === 'TimeoutError' || error.name === 'AbortError');
      // No `cause`: a transport error object can carry the request it failed on.
      return {
        kind: 'error',
        error: timedOut
          ? new ChainDataError('PROVIDER_TIMEOUT', `Blockfrost did not answer within ${this.timeoutMs} ms`, {
              retryable: true,
              details: { path },
            })
          : new ChainDataError('PROVIDER_UNAVAILABLE', 'Blockfrost is unreachable', { retryable: true, details: { path } }),
      };
    }
    if (response.ok) return { kind: 'ok', text: await response.text() };
    const body = await response.text().catch(() => '');
    if (response.status === 404 && nullOn404) return { kind: 'ok', text: null };
    const error = statusError(response.status, path, body, retryAfterSeconds(response.headers.get('retry-after')), this.secret);
    return { kind: 'error', error };
  }
}
