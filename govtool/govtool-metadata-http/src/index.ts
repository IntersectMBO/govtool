/**
 * HTTP client for the private metadata service (`drep-id/metadata`),
 * implementing `MetadataServiceV1` from `@govtool/data-providers/metadata`.
 *
 * One HTTP call per method, no caching: the service owns the cache. The
 * routes and response shapes are the service half of the wire format in
 * `docs/api/metadata-service-spec.md` §2.8.
 *
 * A metadata failure (the url is down, the hash does not match, ...) is a
 * `MetadataFailure` value. Failing to reach the service itself, or an answer
 * this client does not recognise, is thrown as a `MetadataHttpError`: that is
 * an infrastructure fault, not a fact about the document.
 */
import {
  METADATA_FAILURE_CATEGORY,
  type Hex,
  type MetadataFailure,
  type MetadataFailureCategory,
  type MetadataFailureCode,
  type MetadataRefreshOutcome,
  type MetadataReport,
  type MetadataReportSummary,
  type MetadataResult,
  type MetadataServiceV1,
} from '@govtool/data-providers/metadata';

export interface HttpMetadataServiceOptions {
  /** Root of the service, such as `http://metadata:3000`. No trailing `/api`. */
  baseUrl: string;
  /** Injected for tests; defaults to the global `fetch`. */
  fetch?: typeof globalThis.fetch;
  /** Per request. The service fetches remote urls itself, so allow for that. */
  timeoutMs?: number;
}

export const DEFAULT_TIMEOUT_MS = 30_000;

/** Thrown when the service cannot be reached or answers outside the contract. */
export class MetadataHttpError extends Error {
  override readonly name = 'MetadataHttpError';

  constructor(
    message: string,
    /** The HTTP status, when a response arrived. */
    readonly status?: number,
    options?: { cause?: unknown },
  ) {
    super(message, options);
  }
}

const CIP_UNAVAILABLE =
  'CIP validation is not available from the metadata service yet';

const CATEGORIES: readonly MetadataFailureCategory[] = [
  'NETWORK',
  'INVALID_CONTENT',
  'SCHEMA_INVALID',
];

type Json = Record<string, unknown>;

function isObject(value: unknown): value is Json {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function isFailureCode(value: unknown): value is MetadataFailureCode {
  return (
    typeof value === 'string' &&
    Object.prototype.hasOwnProperty.call(METADATA_FAILURE_CATEGORY, value)
  );
}

function optionalString(value: unknown): string | undefined {
  return typeof value === 'string' && value !== '' ? value : undefined;
}

/**
 * Builds a `MetadataFailure` from contract fields, keeping only those fields.
 * A category the service omits, or sends outside the closed set, is derived
 * from the code, which is what it is defined as.
 */
function toFailure(
  body: Json,
  checkedAt: unknown,
): MetadataFailure | undefined {
  if (!isFailureCode(body.code)) return undefined;
  const category = CATEGORIES.includes(body.category as MetadataFailureCategory)
    ? (body.category as MetadataFailureCategory)
    : METADATA_FAILURE_CATEGORY[body.code];
  const failure: MetadataFailure = {
    ok: false,
    code: body.code,
    category,
    message: typeof body.message === 'string' ? body.message : body.code,
    checkedAt: optionalString(checkedAt) ?? new Date().toISOString(),
  };
  const servedHash = optionalString(body.servedHash);
  if (servedHash !== undefined) failure.servedHash = servedHash;
  const reportId = optionalString(body.reportId);
  if (reportId !== undefined) failure.reportId = reportId;
  return failure;
}

/** A result already in contract shape, as `refresh` returns it. */
function toResult<TBody>(value: unknown): MetadataResult<TBody> | undefined {
  if (!isObject(value)) return undefined;
  if (value.ok === true) {
    if (typeof value.hash !== 'string') return undefined;
    return {
      ok: true,
      hash: value.hash,
      body: value.body as TBody,
      fetchedAt: optionalString(value.fetchedAt) ?? new Date().toISOString(),
    };
  }
  if (value.ok === false) return toFailure(value, value.checkedAt);
  return undefined;
}

export function createHttpMetadataService(
  options: HttpMetadataServiceOptions,
): MetadataServiceV1 {
  let base: URL;
  try {
    base = new URL(options.baseUrl);
  } catch {
    throw new Error('metadata service baseUrl is not a valid URL');
  }
  if (base.protocol !== 'http:' && base.protocol !== 'https:') {
    throw new Error('metadata service baseUrl must be http(s)');
  }
  if (base.username || base.password) {
    throw new Error('metadata service baseUrl must not carry credentials');
  }
  const root = base.href.replace(/\/+$/, '');
  const doFetch = options.fetch ?? globalThis.fetch;
  const timeoutMs = options.timeoutMs ?? DEFAULT_TIMEOUT_MS;

  /**
   * One request. Only `Accept` is sent: never `Cache-Control`, which the
   * service reads as a cache-buster (D119). Redirects are refused, because
   * the service has no reason to send one and following it would leave the
   * private network boundary this client assumes.
   */
  async function call(
    method: 'GET' | 'POST',
    path: string,
    query: Record<string, string | undefined>,
  ): Promise<{ status: number; body: unknown; headers: Headers }> {
    const params = new URLSearchParams();
    for (const [key, value] of Object.entries(query)) {
      if (value !== undefined) params.set(key, value);
    }
    const qs = params.toString();
    const target = `${root}${path}${qs ? `?${qs}` : ''}`;

    const controller = new AbortController();
    const timer = setTimeout(() => controller.abort(), timeoutMs);
    let response: Response;
    try {
      response = await doFetch(target, {
        method,
        headers: { Accept: 'application/json' },
        redirect: 'error',
        signal: controller.signal,
      });
    } catch (cause) {
      clearTimeout(timer);
      throw new MetadataHttpError(
        controller.signal.aborted
          ? `metadata service timed out after ${timeoutMs} ms (${method} ${path})`
          : `metadata service unreachable (${method} ${path})`,
        undefined,
        { cause },
      );
    }
    try {
      const text = await response.text();
      let body: unknown = undefined;
      if (text !== '') {
        try {
          body = JSON.parse(text);
        } catch {
          body = undefined;
        }
      }
      return { status: response.status, body, headers: response.headers };
    } catch (cause) {
      throw new MetadataHttpError(
        `metadata service response could not be read (${method} ${path})`,
        response.status,
        { cause },
      );
    } finally {
      clearTimeout(timer);
    }
  }

  function unexpected(status: number, route: string, body: unknown): never {
    const detail =
      isObject(body) && typeof body.message === 'string'
        ? `: ${body.message}`
        : '';
    throw new MetadataHttpError(
      `metadata service answered ${status} to ${route}${detail}`,
      status,
    );
  }

  /**
   * The resolve route speaks the legacy shape: `{hash, fetchedAt, url,
   * metadata}` on success and `{code, category, message, url, fetchedAt,
   * expectedHash?, servedHash?, reportId}` with the §2.4 status on failure.
   */
  async function resolve<TBody>(
    hash: Hex,
    url: string | undefined,
    cip: number | undefined,
  ): Promise<MetadataResult<TBody>> {
    const route = 'GET /api/metadata';
    const { status, body } = await call('GET', '/api/metadata', {
      hash,
      url,
      cip: cip === undefined ? undefined : String(cip),
    });

    if (status === 200) {
      if (!isObject(body) || !('metadata' in body)) {
        unexpected(status, route, body);
      }
      return {
        ok: true,
        hash: typeof body.hash === 'string' ? body.hash : hash,
        body: body.metadata as TBody,
        fetchedAt: optionalString(body.fetchedAt) ?? new Date().toISOString(),
      };
    }
    if (status === 501 && cip !== undefined) {
      throw new MetadataHttpError(CIP_UNAVAILABLE, status);
    }
    // 400 is a caller error (bad input, or a miss with no url to fetch).
    if (status !== 400 && isObject(body)) {
      const failure = toFailure(body, body.fetchedAt);
      if (failure) return failure;
    }
    return unexpected(status, route, body);
  }

  return {
    getMetadata: (hash, url) => resolve(hash, url, undefined),

    getCipMetadata: <TBody = unknown>(cip: number, hash: Hex, url?: string) =>
      resolve<TBody>(hash, url, cip),

    async refresh(hash, url): Promise<MetadataRefreshOutcome> {
      const path = `/api/metadata/${encodeURIComponent(hash)}/refresh`;
      const route = 'POST /api/metadata/{hash}/refresh';
      const { status, body } = await call('POST', path, { url });
      if (status !== 200 || !isObject(body)) unexpected(status, route, body);
      const result = toResult(body.result);
      if (!result || typeof body.refetched !== 'boolean') {
        unexpected(status, route, body);
      }
      const outcome: MetadataRefreshOutcome = {
        refetched: body.refetched,
        result,
      };
      const wait = body.retryAfterSeconds;
      if (typeof wait === 'number' && Number.isFinite(wait) && wait > 0) {
        outcome.retryAfterSeconds = Math.max(1, Math.ceil(wait));
      }
      return outcome;
    },

    async getReport(reportId): Promise<MetadataReport | null> {
      const path = `/api/metadata/reports/${encodeURIComponent(reportId)}`;
      const route = 'GET /api/metadata/reports/{id}';
      const { status, body } = await call('GET', path, {});
      if (status === 404) return null;
      if (status !== 200 || !isObject(body)) unexpected(status, route, body);
      return body as unknown as MetadataReport;
    },

    async listReports(hash, url): Promise<MetadataReportSummary[]> {
      const route = 'GET /api/metadata/reports';
      const { status, body } = await call('GET', '/api/metadata/reports', {
        hash,
        url,
      });
      if (status !== 200 || !Array.isArray(body)) {
        unexpected(status, route, body);
      }
      return body as MetadataReportSummary[];
    },
  };
}

export type { MetadataServiceV1 };
