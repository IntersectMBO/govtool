import { ChainDataError } from '@govtool/data-providers/chain-data';
import type { NetworkId } from '@govtool/data-providers/chain-data';

/**
 * The public Koios deployments. A self-hosted instance is configured by
 * passing `baseUrl` instead of `network`.
 */
export const KOIOS_BASE_URLS: Record<string, string> = {
  mainnet: 'https://api.koios.rest/api/v1',
  preprod: 'https://preprod.koios.rest/api/v1',
  preview: 'https://preview.koios.rest/api/v1',
};

export interface KoiosHttpOptions {
  /** One of `KOIOS_BASE_URLS`' keys. Ignored when `baseUrl` is given. */
  network?: NetworkId;
  /** Full base URL including `/api/v1`, for a self-hosted instance. */
  baseUrl?: string;
  /** Koios API token. Absent means the free public tier and its lower quota. */
  token?: string;
  /** Per-request timeout. Default 30s. */
  timeoutMs?: number;
  /** Retries on 429 and 5xx. Default 2. */
  maxRetries?: number;
  /** Injected in tests; defaults to global `fetch`. */
  fetch?: typeof globalThis.fetch;
}

export interface KoiosQuery {
  /** PostgREST filters and RPC arguments, e.g. `{ _drep_id: 'drep1…' }`. */
  [key: string]: string | number | boolean | undefined;
}

export interface KoiosPageOptions {
  limit?: number;
  offset?: number;
  /** PostgREST `order`, e.g. `block_time.desc`. */
  order?: string;
  /** PostgREST `select` — vertical filtering, to avoid pulling whole rows. */
  select?: string;
  /**
   * Ask for the row count. `exact` is a `COUNT(*)`; `estimated` uses Postgres'
   * own table statistics. Only set it where a caller actually reads `total` —
   * on a large unfiltered table an exact count is the most expensive part of
   * the request.
   */
  count?: 'exact' | 'estimated' | 'planned';
}

export interface KoiosResponse<T> {
  rows: T[];
  /** Parsed from `content-range`; `null` when Koios returned `*`. */
  total: number | null;
}

/** PostgREST caps a page at 1000 rows regardless of `limit`. */
export const KOIOS_MAX_PAGE_SIZE = 1000;

const DEFAULT_TIMEOUT_MS = 30_000;
const DEFAULT_MAX_RETRIES = 2;

/**
 * Thin transport over Koios.
 *
 * It owns exactly three things the rest of the provider should never repeat:
 * turning an HTTP failure into the contract's `ChainDataError`, honouring
 * `Retry-After` on a 429, and reading the row count out of `content-range`.
 * It deliberately holds no cache — caching is the consumer's policy, the same
 * rule the db-sync provider follows.
 */
export class KoiosHttpClient {
  readonly baseUrl: string;
  private readonly token: string | undefined;
  private readonly timeoutMs: number;
  private readonly maxRetries: number;
  private readonly fetchImpl: typeof globalThis.fetch;

  constructor(options: KoiosHttpOptions = {}) {
    const fromNetwork =
      options.network === undefined
        ? undefined
        : KOIOS_BASE_URLS[options.network];
    const baseUrl =
      options.baseUrl ?? fromNetwork ?? KOIOS_BASE_URLS['mainnet'];
    if (baseUrl === undefined) {
      throw new ChainDataError(
        'INVALID_INPUT',
        `No Koios base URL for network "${String(options.network)}"; pass baseUrl for a self-hosted instance`,
      );
    }
    this.baseUrl = baseUrl.replace(/\/+$/, '');
    this.token = options.token;
    this.timeoutMs = options.timeoutMs ?? DEFAULT_TIMEOUT_MS;
    this.maxRetries = options.maxRetries ?? DEFAULT_MAX_RETRIES;
    this.fetchImpl = options.fetch ?? globalThis.fetch;
  }

  async get<T>(
    path: string,
    query: KoiosQuery = {},
    page: KoiosPageOptions = {},
  ): Promise<KoiosResponse<T>> {
    const url = new URL(`${this.baseUrl}/${path}`);
    for (const [key, value] of Object.entries(query)) {
      if (value !== undefined) {
        url.searchParams.set(key, String(value));
      }
    }
    applyPageParams(url, page);
    return this.request<T>(url, { method: 'GET' }, page.count);
  }

  async post<T>(
    path: string,
    body: unknown,
    page: KoiosPageOptions = {},
  ): Promise<KoiosResponse<T>> {
    const url = new URL(`${this.baseUrl}/${path}`);
    applyPageParams(url, page);
    return this.request<T>(
      url,
      {
        method: 'POST',
        body: JSON.stringify(body),
        headers: { 'content-type': 'application/json' },
      },
      page.count,
    );
  }

  private async request<T>(
    url: URL,
    init: RequestInit,
    count: KoiosPageOptions['count'],
  ): Promise<KoiosResponse<T>> {
    let lastError: ChainDataError | undefined;

    for (let attempt = 0; attempt <= this.maxRetries; attempt += 1) {
      let response: Response;
      try {
        response = await this.fetchImpl(url, {
          ...init,
          headers: this.headers(init.headers, count),
          signal: AbortSignal.timeout(this.timeoutMs),
        });
      } catch (error) {
        lastError = toTransportError(error, this.timeoutMs);
        if (attempt === this.maxRetries) break;
        continue;
      }

      if (response.ok) {
        return {
          rows: await parseRows<T>(response, url),
          total: parseContentRange(response.headers.get('content-range')),
        };
      }

      lastError = await toHttpError(response, url);
      if (!lastError.retryable || attempt === this.maxRetries) {
        throw lastError;
      }
    }

    throw lastError ?? new ChainDataError('INTERNAL', 'Koios request failed');
  }

  private headers(
    extra: RequestInit['headers'],
    count: KoiosPageOptions['count'],
  ): Headers {
    const headers = new Headers(extra);
    headers.set('accept', 'application/json');
    if (this.token !== undefined) {
      headers.set('authorization', `Bearer ${this.token}`);
    }
    if (count !== undefined) {
      headers.set('prefer', `count=${count}`);
    }
    return headers;
  }
}

function applyPageParams(url: URL, page: KoiosPageOptions): void {
  if (page.select !== undefined) url.searchParams.set('select', page.select);
  if (page.order !== undefined) url.searchParams.set('order', page.order);
  if (page.limit !== undefined)
    url.searchParams.set('limit', String(page.limit));
  if (page.offset !== undefined && page.offset > 0) {
    url.searchParams.set('offset', String(page.offset));
  }
}

async function parseRows<T>(response: Response, url: URL): Promise<T[]> {
  let payload: unknown;
  try {
    payload = await response.json();
  } catch (error) {
    throw new ChainDataError(
      'INTERNAL',
      'Koios returned a malformed response',
      {
        cause: error,
        details: { path: url.pathname },
      },
    );
  }
  // `/committee_info` is the one endpoint that answers with an object rather
  // than a row array; normalising here keeps every caller on the same shape.
  if (Array.isArray(payload)) return payload as T[];
  if (payload === null || payload === undefined) return [];
  return [payload as T];
}

/**
 * `content-range: 0-999/12345` → 12345. Koios answers `*` for the total on
 * large tables unless a `Prefer: count=` header asked otherwise, and the
 * contract's `Page.total` is optional precisely for that case.
 */
export function parseContentRange(header: string | null): number | null {
  if (!header) return null;
  const total = header.split('/')[1];
  if (total === undefined || total === '*') return null;
  const parsed = Number(total);
  return Number.isInteger(parsed) ? parsed : null;
}

function toTransportError(error: unknown, timeoutMs: number): ChainDataError {
  if (ChainDataError.is(error)) return error;
  const name = (error as { name?: string } | null)?.name;
  if (name === 'TimeoutError' || name === 'AbortError') {
    return new ChainDataError(
      'PROVIDER_TIMEOUT',
      `Koios did not respond within ${timeoutMs}ms`,
      { cause: error },
    );
  }
  return new ChainDataError('PROVIDER_UNAVAILABLE', 'Koios request failed', {
    cause: error,
  });
}

/**
 * Koios' documented failures, mapped to the contract's codes.
 *
 * The response body is read for diagnostics but never forwarded: PostgREST
 * error bodies quote SQL, and a leaked column name is how a provider detail
 * ends up in a consumer's UI.
 */
async function toHttpError(
  response: Response,
  url: URL,
): Promise<ChainDataError> {
  const details = { path: url.pathname, status: response.status };
  const hint = await readErrorHint(response);

  switch (response.status) {
    case 400:
      return new ChainDataError(
        'INVALID_INPUT',
        `Koios rejected the request${hint}`,
        {
          details,
        },
      );
    case 401:
    case 403:
      return new ChainDataError(
        'PROVIDER_UNAVAILABLE',
        'Koios rejected the API token',
        { details, retryable: false },
      );
    case 404:
      return new ChainDataError('NOT_FOUND', 'Koios has no such endpoint', {
        details,
      });
    case 413:
      return new ChainDataError(
        'INVALID_INPUT',
        'Koios refused the request body as too large; send fewer ids per batch',
        { details },
      );
    case 429:
      return new ChainDataError(
        'PROVIDER_RATE_LIMITED',
        'Koios rate limit reached',
        {
          details,
          ...retryAfter(response),
        },
      );
    default:
      if (response.status >= 500) {
        return new ChainDataError(
          'PROVIDER_UNAVAILABLE',
          'Koios is unavailable',
          {
            details,
          },
        );
      }
      return new ChainDataError(
        'INTERNAL',
        `Unexpected Koios response${hint}`,
        {
          details,
        },
      );
  }
}

function retryAfter(response: Response): { retryAfterSeconds?: number } {
  const header = response.headers.get('retry-after');
  if (!header) return {};
  const seconds = Number(header);
  return Number.isFinite(seconds) ? { retryAfterSeconds: seconds } : {};
}

/** A short, safe fragment of the error body — never the whole thing. */
async function readErrorHint(response: Response): Promise<string> {
  try {
    const text = await response.text();
    const message = (JSON.parse(text) as { message?: unknown }).message;
    return typeof message === 'string' ? `: ${message.slice(0, 120)}` : '';
  } catch {
    return '';
  }
}
