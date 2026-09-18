import { KoiosHttpClient } from '../src/http/client';

export interface RecordedCall {
  path: string;
  method: 'GET' | 'POST';
  /** Query parameters as sent, including PostgREST filters. */
  params: Record<string, string>;
  body: unknown;
  headers: Record<string, string>;
}

interface CannedResponse {
  rows?: unknown[];
  status?: number;
  /** Total for `content-range`; `undefined` sends the `*` Koios sends by default. */
  total?: number;
  headers?: Record<string, string>;
}

/**
 * A `fetch` stand-in that answers from canned rows keyed by endpoint path,
 * and records every request.
 *
 * Matching is on the path alone — the query string is recorded rather than
 * matched — so a test asserts two separate things: that the provider called
 * the endpoint it claims to, and that it built the PostgREST filter it claims
 * to. Those are the two halves of "reads Koios correctly", and keeping them
 * apart makes a failure say which one broke.
 */
export class FakeKoios {
  readonly calls: RecordedCall[] = [];
  private readonly responses = new Map<string, CannedResponse[]>();

  on(
    path: string,
    rows: unknown[],
    options: Omit<CannedResponse, 'rows'> = {},
  ): this {
    return this.push(path, { rows, ...options });
  }

  /** Queues a further response for the same path, for retry and paging tests. */
  thenOn(
    path: string,
    rows: unknown[],
    options: Omit<CannedResponse, 'rows'> = {},
  ): this {
    return this.push(path, { rows, ...options });
  }

  failOn(
    path: string,
    status: number,
    headers: Record<string, string> = {},
  ): this {
    return this.push(path, { status, headers });
  }

  callsTo(path: string): RecordedCall[] {
    return this.calls.filter((call) => call.path === path);
  }

  lastCallTo(path: string): RecordedCall | undefined {
    return this.callsTo(path).at(-1);
  }

  client(
    options: { maxRetries?: number; token?: string } = {},
  ): KoiosHttpClient {
    return new KoiosHttpClient({
      baseUrl: 'https://koios.test/api/v1',
      maxRetries: options.maxRetries ?? 0,
      token: options.token,
      fetch: this.fetch,
    });
  }

  private push(path: string, response: CannedResponse): this {
    const queued = this.responses.get(path) ?? [];
    queued.push(response);
    this.responses.set(path, queued);
    return this;
  }

  private readonly fetch: typeof globalThis.fetch = async (input, init) => {
    const url = new URL(typeof input === 'string' ? input : input.toString());
    const path = url.pathname.replace('/api/v1/', '');
    const params: Record<string, string> = {};
    url.searchParams.forEach((value, key) => {
      params[key] = value;
    });

    const headers: Record<string, string> = {};
    new Headers(init?.headers).forEach((value, key) => {
      headers[key] = value;
    });

    this.calls.push({
      path,
      method: (init?.method as 'GET' | 'POST') ?? 'GET',
      params,
      body:
        typeof init?.body === 'string'
          ? (JSON.parse(init.body) as unknown)
          : undefined,
      headers,
    });

    const queued = this.responses.get(path);
    const response =
      queued === undefined || queued.length === 0
        ? undefined
        : queued.length === 1
          ? queued[0]
          : queued.shift();

    if (response === undefined) {
      return new Response('[]', {
        status: 200,
        headers: { 'content-type': 'application/json' },
      });
    }
    if (response.status !== undefined && response.status >= 400) {
      return new Response(JSON.stringify({ message: 'nope' }), {
        status: response.status,
        headers: { 'content-type': 'application/json', ...response.headers },
      });
    }

    const rows = response.rows ?? [];
    const range =
      response.total === undefined
        ? `0-${Math.max(0, rows.length - 1)}/*`
        : `0-${Math.max(0, rows.length - 1)}/${response.total}`;

    return new Response(JSON.stringify(rows), {
      status: 200,
      headers: {
        'content-type': 'application/json',
        'content-range': range,
        ...response.headers,
      },
    });
  };
}
