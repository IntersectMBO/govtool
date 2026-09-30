/**
 * The transport: one place that talks HTTP to Koios.
 *
 * It owns what no area module should repeat: building PostgREST query strings
 * safely, the `Authorization` header, per-request timeouts, retrying 429 and
 * 5xx with `Retry-After`, bounded concurrency, reading `Content-Range` for a
 * total, chunking bulk POST bodies under Koios' 5,120-byte limit, and turning
 * every failure into a `ChainDataError`. It holds no cache (SPEC.md §3.6).
 *
 * Security: the token is only ever placed in the `Authorization` header and
 * never appears in an error message or `details`. Error bodies are not
 * forwarded — PostgREST quotes SQL in them.
 */
import { ChainDataError, type NetworkId } from '@govtool/data-providers/chain-data';

/** The public Koios deployments. */
export const KOIOS_BASE_URLS: Readonly<Record<string, string>> = {
  mainnet: 'https://api.koios.rest/api/v1',
  preprod: 'https://preprod.koios.rest/api/v1',
  preview: 'https://preview.koios.rest/api/v1',
  guild: 'https://guild.koios.rest/api/v1',
};

/** PostgREST caps a response at 1000 rows whatever `limit` says. */
export const KOIOS_MAX_ROWS = 1000;

/** Koios answers 413 to a POST body over 5,120 bytes; stay well under it. */
export const MAX_BODY_BYTES = 4600;

/** Keep a GET URL comfortably short when a filter lists many ids. */
export const MAX_IN_FILTER_CHARS = 5000;

export interface KoiosHttpOptions {
  network: NetworkId;
  /** Full base URL including `/api/v1`, for a self-hosted instance. */
  baseUrl?: string;
  /** Koios API token (Bearer). Absent means the free public tier. */
  token?: string;
  /** Per-request timeout. Default 30 s. */
  timeoutMs?: number;
  /** Retries on 429, 5xx, timeouts and network failures. Default 3. */
  maxRetries?: number;
  /** Requests in flight at once. Default 4. */
  maxConcurrency?: number;
  /** A `Retry-After` longer than this is not waited out; the 429 is surfaced. Default 10 s. */
  maxRetryWaitMs?: number;
  /** Injected in tests; defaults to the global `fetch` (Node >= 20). */
  fetch?: typeof globalThis.fetch;
}

/** Query-string values. Every one is URL-encoded by `URLSearchParams`. */
export type Query = Record<string, string | number | boolean | undefined>;

export interface ReadOptions {
  /** PostgREST vertical filter. */
  select?: string;
  /** PostgREST ordering, e.g. `block_time.desc,proposal_index.desc`. */
  order?: string;
  limit?: number;
  offset?: number;
  /** Ask for `Prefer: count=exact` and read the total off `Content-Range`. */
  count?: boolean;
}

export interface KoiosResponse<T> {
  rows: T[];
  /** From `Content-Range` when a count was asked for; otherwise undefined. */
  total?: number;
}

const PATH = /^[a-z_]+$/;

export interface KoiosHttp {
  readonly baseUrl: string;
  get<T>(path: string, query?: Query, options?: ReadOptions): Promise<KoiosResponse<T>>;
  post<T>(path: string, body: Record<string, unknown>, query?: Query, options?: ReadOptions): Promise<KoiosResponse<T>>;
  /** Every row of a GET, 1000 at a time until a short page. */
  getAll<T>(path: string, query?: Query, options?: Omit<ReadOptions, 'limit' | 'offset' | 'count'>): Promise<T[]>;
  /**
   * `limit` rows from `offset`, read in 1000-row chunks so a window wider than
   * Koios' cap is never returned short. `total` is the exact filtered count.
   */
  getWindow<T>(
    path: string,
    query: Query,
    window: { offset: number; limit: number },
    options?: Omit<ReadOptions, 'limit' | 'offset' | 'count'>,
  ): Promise<{ rows: T[]; total: number }>;
  /** A bulk POST of `ids` under `key`, split so no body exceeds the size limit. */
  postChunked<T>(path: string, key: string, ids: readonly string[], query?: Query, options?: ReadOptions): Promise<T[]>;
}

/** `0-999/1685` -> 1685; `*\/0` -> 0; `0-0/*` -> undefined. */
export function parseContentRange(header: string | null): number | undefined {
  if (!header) return undefined;
  const total = header.split('/')[1];
  if (total === undefined || total === '*') return undefined;
  const n = Number(total);
  return Number.isSafeInteger(n) && n >= 0 ? n : undefined;
}

/**
 * JSON with big integers kept exact. Node >= 21 hands a reviver the source
 * text of each number; an integer literal beyond 2^53 is kept as that text.
 * On Node 20 the float is kept, which is exact for every value Koios sends
 * as a number (its lovelace columns are strings).
 */
export function parseJson(text: string): unknown {
  type Reviver = (this: unknown, key: string, value: unknown, context?: { source?: string }) => unknown;
  const reviver: Reviver = (_key, value, context) =>
    typeof value === 'number' && Number.isInteger(value) && !Number.isSafeInteger(value) && context?.source && /^-?\d+$/.test(context.source)
      ? context.source
      : value;
  return JSON.parse(text, reviver as Parameters<typeof JSON.parse>[1]);
}

/** Split ids into groups whose JSON body `{ key: [...] }` stays under the limit. */
export function chunkIds(key: string, ids: readonly string[], maxBytes = MAX_BODY_BYTES): string[][] {
  const chunks: string[][] = [];
  let current: string[] = [];
  let bytes = Buffer.byteLength(JSON.stringify({ [key]: [] }));
  for (const id of ids) {
    const size = Buffer.byteLength(JSON.stringify(id)) + 1;
    if (current.length > 0 && bytes + size > maxBytes) {
      chunks.push(current);
      current = [];
      bytes = Buffer.byteLength(JSON.stringify({ [key]: [] }));
    }
    current.push(id);
    bytes += size;
  }
  if (current.length > 0) chunks.push(current);
  return chunks;
}

/** Split values for a PostgREST `in.(…)` filter so each URL stays short. */
export function chunkForFilter(values: readonly string[], maxChars = MAX_IN_FILTER_CHARS): string[][] {
  const chunks: string[][] = [];
  let current: string[] = [];
  let chars = 0;
  for (const v of values) {
    if (current.length > 0 && chars + v.length + 1 > maxChars) {
      chunks.push(current);
      current = [];
      chars = 0;
    }
    current.push(v);
    chars += v.length + 1;
  }
  if (current.length > 0) chunks.push(current);
  return chunks;
}

/**
 * A PostgREST `in.(…)` list. Callers pass only validated bech32 or hex, which
 * cannot contain the list's own delimiters; anything else is refused here too.
 */
export function inList(values: readonly string[]): string {
  for (const v of values) {
    if (!/^[a-z0-9_]+$/i.test(v)) throw new ChainDataError('INTERNAL', 'refusing to put an unvalidated value in a filter');
  }
  return `in.(${values.join(',')})`;
}

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

export function createKoiosHttp(options: KoiosHttpOptions): KoiosHttp {
  const base = options.baseUrl ?? (Object.hasOwn(KOIOS_BASE_URLS, options.network) ? KOIOS_BASE_URLS[options.network] : undefined);
  if (base === undefined) {
    throw new ChainDataError('INVALID_INPUT', `No public Koios deployment for network '${String(options.network)}'; pass baseUrl`);
  }
  let parsed: URL;
  try {
    parsed = new URL(base);
  } catch {
    throw new ChainDataError('INVALID_INPUT', 'Koios baseUrl is not a valid URL');
  }
  if (parsed.protocol !== 'https:' && parsed.protocol !== 'http:') {
    throw new ChainDataError('INVALID_INPUT', 'Koios baseUrl must be http(s)');
  }
  if (parsed.username || parsed.password) {
    throw new ChainDataError('INVALID_INPUT', 'Koios baseUrl must not carry credentials; pass token instead');
  }
  const baseUrl = base.replace(/\/+$/, '');
  const token = options.token === undefined || options.token === '' ? undefined : options.token;
  const timeoutMs = options.timeoutMs ?? 30_000;
  const maxRetries = options.maxRetries ?? 3;
  const maxRetryWaitMs = options.maxRetryWaitMs ?? 10_000;
  const fetchImpl = options.fetch ?? globalThis.fetch;
  if (typeof fetchImpl !== 'function') throw new ChainDataError('INTERNAL', 'no fetch implementation available');

  /* A small semaphore: Koios' public tier throttles bursts. */
  const limit = Math.max(1, options.maxConcurrency ?? 4);
  let active = 0;
  const waiting: (() => void)[] = [];
  async function withSlot<T>(fn: () => Promise<T>): Promise<T> {
    if (active >= limit) await new Promise<void>((resolve) => waiting.push(resolve));
    active++;
    try {
      return await fn();
    } finally {
      active--;
      waiting.shift()?.();
    }
  }

  function buildUrl(path: string, query: Query = {}, read: ReadOptions = {}): URL {
    if (!PATH.test(path)) throw new ChainDataError('INTERNAL', 'invalid Koios endpoint name');
    const url = new URL(`${baseUrl}/${path}`);
    for (const [key, value] of Object.entries(query)) {
      if (value !== undefined) url.searchParams.append(key, String(value));
    }
    if (read.select !== undefined) url.searchParams.set('select', read.select);
    if (read.order !== undefined) url.searchParams.set('order', read.order);
    if (read.limit !== undefined) url.searchParams.set('limit', String(read.limit));
    if (read.offset !== undefined && read.offset > 0) url.searchParams.set('offset', String(read.offset));
    return url;
  }

  function headers(count: boolean, json: boolean): Headers {
    const h = new Headers({ accept: 'application/json' });
    if (json) h.set('content-type', 'application/json');
    if (token !== undefined) h.set('authorization', `Bearer ${token}`);
    if (count) h.set('prefer', 'count=exact');
    return h;
  }

  function retryAfterSeconds(response: Response): number | undefined {
    const header = response.headers.get('retry-after');
    if (!header) return undefined;
    const seconds = Number(header);
    if (Number.isFinite(seconds) && seconds >= 0) return seconds;
    const date = Date.parse(header);
    return Number.isNaN(date) ? undefined : Math.max(0, Math.ceil((date - Date.now()) / 1000));
  }

  function httpError(response: Response, path: string): ChainDataError {
    const details = { endpoint: path, status: response.status };
    const status = response.status;
    if (status === 404) return new ChainDataError('NOT_FOUND', 'Koios has no such resource', { details });
    if (status === 429) {
      const after = retryAfterSeconds(response);
      return new ChainDataError('PROVIDER_RATE_LIMITED', 'Koios rate limit reached', {
        retryable: true,
        details,
        ...(after === undefined ? {} : { retryAfterSeconds: after }),
      });
    }
    if (status === 401 || status === 403) {
      return new ChainDataError('PROVIDER_UNAVAILABLE', 'Koios rejected the API token or the request', { details });
    }
    if (status >= 500) return new ChainDataError('PROVIDER_UNAVAILABLE', 'Koios is unavailable', { retryable: true, details });
    // 400 / 413 / anything else: this provider built a request Koios refused.
    // Inputs are validated before they reach a URL, so it is not the caller's.
    return new ChainDataError('INTERNAL', 'Koios refused the request', { details });
  }

  async function request<T>(url: URL, init: { method: 'GET' | 'POST'; body?: string }, count: boolean, path: string): Promise<KoiosResponse<T>> {
    let last: ChainDataError | undefined;
    for (let attempt = 0; attempt <= maxRetries; attempt++) {
      let response: Response;
      try {
        response = await withSlot(() =>
          fetchImpl(url, {
            method: init.method,
            headers: headers(count, init.body !== undefined),
            ...(init.body === undefined ? {} : { body: init.body }),
            signal: AbortSignal.timeout(timeoutMs),
          }),
        );
      } catch (error) {
        const name = (error as { name?: string } | null)?.name;
        last =
          name === 'TimeoutError' || name === 'AbortError'
            ? new ChainDataError('PROVIDER_TIMEOUT', `Koios did not answer within ${timeoutMs} ms`, {
                retryable: true,
                details: { endpoint: path },
              })
            : new ChainDataError('PROVIDER_UNAVAILABLE', 'Koios could not be reached', { retryable: true, details: { endpoint: path } });
        if (attempt < maxRetries) await sleep(250 * 2 ** attempt);
        continue;
      }

      if (response.ok) {
        let rows: unknown;
        try {
          rows = parseJson(await response.text());
        } catch (cause) {
          throw new ChainDataError('INTERNAL', 'Koios returned malformed JSON', { details: { endpoint: path }, cause });
        }
        const list = Array.isArray(rows) ? (rows as T[]) : rows === null || rows === undefined ? [] : [rows as T];
        const total = count ? parseContentRange(response.headers.get('content-range')) : undefined;
        return total === undefined ? { rows: list } : { rows: list, total };
      }

      last = httpError(response, path);
      // Drain the body so the connection can be reused.
      await response.text().catch(() => undefined);
      if (!last.retryable || attempt === maxRetries) throw last;
      const waitMs = last.retryAfterSeconds !== undefined ? last.retryAfterSeconds * 1000 : 500 * 2 ** attempt;
      if (waitMs > maxRetryWaitMs) throw last;
      await sleep(waitMs);
    }
    throw last ?? new ChainDataError('INTERNAL', 'Koios request failed');
  }

  const http: KoiosHttp = {
    baseUrl,
    get: (path, query, read = {}) => request(buildUrl(path, query, read), { method: 'GET' }, read.count === true, path),
    post: (path, body, query, read = {}) =>
      request(buildUrl(path, query, read), { method: 'POST', body: JSON.stringify(body) }, read.count === true, path),

    async getAll<T>(path: string, query: Query = {}, read: Omit<ReadOptions, 'limit' | 'offset' | 'count'> = {}) {
      const out: T[] = [];
      for (let offset = 0; ; offset += KOIOS_MAX_ROWS) {
        const { rows } = await http.get<T>(path, query, { ...read, limit: KOIOS_MAX_ROWS, offset });
        out.push(...rows);
        if (rows.length < KOIOS_MAX_ROWS) return out;
      }
    },

    async getWindow<T>(
      path: string,
      query: Query,
      window: { offset: number; limit: number },
      read: Omit<ReadOptions, 'limit' | 'offset' | 'count'> = {},
    ) {
      const rows: T[] = [];
      let total: number | undefined;
      for (let offset = window.offset; rows.length < window.limit; ) {
        const want = Math.min(KOIOS_MAX_ROWS, window.limit - rows.length);
        const res = await http.get<T>(path, query, { ...read, limit: want, offset, count: total === undefined });
        if (total === undefined) total = res.total;
        rows.push(...res.rows);
        offset += res.rows.length;
        if (res.rows.length < want) break;
      }
      if (total === undefined) throw new ChainDataError('INTERNAL', 'Koios did not report a row count', { details: { endpoint: path } });
      return { rows, total };
    },

    async postChunked<T>(path: string, key: string, ids: readonly string[], query?: Query, read?: ReadOptions) {
      const unique = [...new Set(ids)];
      const results = await Promise.all(chunkIds(key, unique).map((chunk) => http.post<T>(path, { [key]: chunk }, query, read)));
      return results.flatMap((r) => r.rows);
    },
  };
  return http;
}
