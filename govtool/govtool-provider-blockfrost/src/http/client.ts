import { ChainDataError } from '@govtool/data-providers/chain-data';

/** Blockfrost's hard cap on `count`. */
export const MAX_PAGE_SIZE = 100;

export interface BlockfrostClientOptions {
  /** Base url, e.g. `https://mainnet.blockfrost.sireto.io`. */
  baseUrl: string;
  /**
   * Blockfrost `project_id`. Optional: a self-hosted deployment
   * (blockfrost-ryo) typically runs unauthenticated, and the header is only
   * sent when this is set.
   */
  projectId?: string;
  /** Per-request timeout, ms. Default 30_000. */
  timeoutMs?: number;
  /** Retries for a retryable failure (429 / 5xx / network). Default 2. */
  maxRetries?: number;
  /** Base backoff, ms; doubled per attempt. Default 250. */
  retryBackoffMs?: number;
  /** Injected for tests. Defaults to the global `fetch`. */
  fetch?: typeof fetch;
  /** Injected for tests; receives the backoff delay in ms. */
  sleep?: (ms: number) => Promise<void>;
}

export interface PageParams {
  count?: number;
  page?: number;
  order?: 'asc' | 'desc';
}

const RETRYABLE_STATUS = new Set([408, 425, 429, 500, 502, 503, 504]);

function statusToError(
  status: number,
  path: string,
  body: string,
): ChainDataError {
  const details = { path, status, body: body.slice(0, 400) };
  switch (status) {
    case 400:
      return new ChainDataError(
        'INVALID_INPUT',
        'Blockfrost rejected the request',
        { details },
      );
    case 403:
      return new ChainDataError(
        'INTERNAL',
        'Blockfrost rejected the credentials',
        { details },
      );
    case 404:
      return new ChainDataError(
        'NOT_FOUND',
        'Blockfrost has no such resource',
        { details },
      );
    case 418:
      return new ChainDataError(
        'PROVIDER_RATE_LIMITED',
        'Blockfrost banned this client',
        { details },
      );
    case 429:
      return new ChainDataError(
        'PROVIDER_RATE_LIMITED',
        'Blockfrost rate limit exceeded',
        { details },
      );
    case 408:
    case 504:
      return new ChainDataError('PROVIDER_TIMEOUT', 'Blockfrost timed out', {
        details,
      });
    default:
      if (status >= 500) {
        return new ChainDataError(
          'PROVIDER_UNAVAILABLE',
          `Blockfrost returned ${status}`,
          { details },
        );
      }
      return new ChainDataError('INTERNAL', `Blockfrost returned ${status}`, {
        details,
      });
  }
}

/**
 * The HTTP surface this provider is built on.
 *
 * Everything it throws is a `ChainDataError`, so no `fetch` rejection or
 * Blockfrost error envelope escapes into a consumer. A 404 is a normal
 * outcome for several Blockfrost routes — a DRep with no metadata anchor, a
 * ParameterChange sub-resource on a non-ParameterChange action — so
 * `getOrNull` exists to turn it into `null` rather than an error.
 */
export class BlockfrostClient {
  private readonly baseUrl: string;
  private readonly projectId: string | undefined;
  private readonly timeoutMs: number;
  private readonly maxRetries: number;
  private readonly retryBackoffMs: number;
  private readonly fetchImpl: typeof fetch;
  private readonly sleep: (ms: number) => Promise<void>;

  constructor(options: BlockfrostClientOptions) {
    if (!options.baseUrl) {
      throw new Error('BlockfrostClient requires a baseUrl');
    }
    this.baseUrl = options.baseUrl.replace(/\/+$/, '');
    this.projectId = options.projectId;
    this.timeoutMs = options.timeoutMs ?? 30_000;
    this.maxRetries = options.maxRetries ?? 2;
    this.retryBackoffMs = options.retryBackoffMs ?? 250;
    this.fetchImpl = options.fetch ?? fetch;
    this.sleep =
      options.sleep ??
      ((ms) => new Promise((resolve) => setTimeout(resolve, ms)));
  }

  async get<T>(path: string, page?: PageParams): Promise<T> {
    const result = await this.request<T>(path, page, false);
    return result as T;
  }

  /** As `get`, but a 404 becomes `null` instead of throwing. */
  async getOrNull<T>(path: string, page?: PageParams): Promise<T | null> {
    return this.request<T>(path, page, true);
  }

  /**
   * Walks Blockfrost's pages until one comes back short, returning everything.
   *
   * `limit` caps the total; omit it only where the collection is known to be
   * bounded, because the DRep directory on mainnet is tens of thousands of
   * rows and each page is one round trip.
   */
  async getAll<T>(
    path: string,
    options: { limit?: number; order?: 'asc' | 'desc' } = {},
  ): Promise<T[]> {
    const out: T[] = [];
    for (let page = 1; ; page += 1) {
      const remaining =
        options.limit === undefined
          ? MAX_PAGE_SIZE
          : options.limit - out.length;
      if (remaining <= 0) break;
      const count = Math.min(MAX_PAGE_SIZE, remaining);
      const batch = await this.get<T[]>(path, {
        count,
        page,
        ...(options.order === undefined ? {} : { order: options.order }),
      });
      out.push(...batch);
      if (batch.length < count) break;
    }
    return out;
  }

  private async request<T>(
    path: string,
    page: PageParams | undefined,
    nullOn404: boolean,
  ): Promise<T | null> {
    const url = this.buildUrl(path, page);
    let lastError: ChainDataError | undefined;

    for (let attempt = 0; attempt <= this.maxRetries; attempt += 1) {
      if (attempt > 0) {
        await this.sleep(this.retryBackoffMs * 2 ** (attempt - 1));
      }

      let response: Response;
      try {
        response = await this.fetchImpl(url, {
          headers: {
            Accept: 'application/json',
            ...(this.projectId === undefined
              ? {}
              : { project_id: this.projectId }),
          },
          signal: AbortSignal.timeout(this.timeoutMs),
        });
      } catch (error) {
        // A timeout surfaces as an AbortError; everything else is a transport
        // failure. Both are worth retrying.
        const timedOut =
          error instanceof Error &&
          (error.name === 'TimeoutError' || error.name === 'AbortError');
        lastError = new ChainDataError(
          timedOut ? 'PROVIDER_TIMEOUT' : 'PROVIDER_UNAVAILABLE',
          timedOut
            ? `Blockfrost request timed out after ${this.timeoutMs}ms`
            : 'Blockfrost is unreachable',
          { cause: error, details: { path } },
        );
        continue;
      }

      if (response.ok) {
        const text = await response.text();
        if (text === '') return null;
        try {
          return JSON.parse(text) as T;
        } catch (error) {
          throw new ChainDataError(
            'INTERNAL',
            'Blockfrost returned a body that is not JSON',
            { cause: error, details: { path, body: text.slice(0, 400) } },
          );
        }
      }

      if (response.status === 404 && nullOn404) return null;

      const body = await response.text().catch(() => '');
      const error = statusToError(response.status, path, body);
      if (!RETRYABLE_STATUS.has(response.status)) throw error;
      lastError = error;
    }

    throw (
      lastError ??
      new ChainDataError('PROVIDER_UNAVAILABLE', 'Blockfrost request failed', {
        details: { path },
      })
    );
  }

  private buildUrl(path: string, page?: PageParams): string {
    const url = new URL(this.baseUrl + path);
    if (page?.count !== undefined)
      url.searchParams.set('count', String(page.count));
    if (page?.page !== undefined)
      url.searchParams.set('page', String(page.page));
    if (page?.order !== undefined) url.searchParams.set('order', page.order);
    return url.toString();
  }
}
