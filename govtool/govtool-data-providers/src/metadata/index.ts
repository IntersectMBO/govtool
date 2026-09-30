/**
 * Metadata Service v1 — resolves anchors into documents.
 *
 * Required. Chain data emits `{ url, dataHash }`; this service fetches,
 * hash-verifies, and optionally validates against a CIP (SPEC.md §6).
 */

export type Hex = string;
export type Timestamp = string;

/** What the chain carries: where a document lives and what it must hash to. */
export interface Anchor {
  url: string;
  dataHash: Hex;
}

/**
 * One code per stage of the pipeline: retrieve → size → parse → hash →
 * validate. Each names the stage that failed.
 */
export type MetadataFailureCode =
  /** Could not retrieve. DNS failure, timeout, connection refused, TLS — the
   *  distinguishing detail is in `message`, deliberately not in more codes. */
  | 'FETCH_ERROR'
  /** Larger than this service accepts. */
  | 'EXCEEDS_LIMIT'
  /** Retrieved, not parseable. */
  | 'JSON_PARSE_ERROR'
  /** Parsed, but the content does not hash to the requested value. */
  | 'HASH_MISMATCH'
  /** Hash-correct, but violates the named CIP. `message` names the field and
   *  the reason. */
  | 'SCHEMA_INVALID';

/**
 * How a failure is presented (D115, D124). Derived from the code, never set
 * independently of it.
 */
export type MetadataFailureCategory =
  /** Nothing usable came back. */
  | 'NETWORK'
  /** Something came back, but it is not the anchored document. */
  | 'INVALID_CONTENT'
  /** The right document, valid JSON, rejected by the CIP validator. */
  | 'SCHEMA_INVALID';

export const METADATA_FAILURE_CATEGORY: Readonly<
  Record<MetadataFailureCode, MetadataFailureCategory>
> = {
  FETCH_ERROR: 'NETWORK',
  EXCEEDS_LIMIT: 'INVALID_CONTENT',
  JSON_PARSE_ERROR: 'INVALID_CONTENT',
  HASH_MISMATCH: 'INVALID_CONTENT',
  SCHEMA_INVALID: 'SCHEMA_INVALID',
};

export interface MetadataFailure {
  ok: false;
  code: MetadataFailureCode;
  category: MetadataFailureCategory;
  /** One line. The detail is in the report. */
  message: string;
  /** The hash actually served, when one was computed (see `HASH_MISMATCH`). */
  servedHash?: Hex;
  /**
   * The fetch report behind this failure (D113). Every failure has one,
   * including a failure that never connected (D115). Absent only when the
   * implementation fetches nothing, such as an offline fixture.
   */
  reportId?: string;
  checkedAt: Timestamp;
}

export interface MetadataSuccess<TBody = unknown> {
  ok: true;
  hash: Hex;
  body: TBody;
  /** When the content was first retrieved. Content caching is permanent. */
  fetchedAt: Timestamp;
}

export type MetadataResult<TBody = unknown> =
  MetadataSuccess<TBody> | MetadataFailure;

/* -- fetch reports (D113–D115, D122, D123) ---------------------------------- */

/** A point in the report body. `line` and `column` are 1-based. */
export interface SourcePosition {
  /** Index into the decoded body text, in UTF-16 code units, as JS strings count. */
  offset: number;
  /** Index into the raw body bytes. */
  byteOffset: number;
  line: number;
  column: number;
}

/** A half-open range `[start, end)` in the report body. */
export interface SourceRange {
  start: SourcePosition;
  end: SourcePosition;
}

/**
 * One problem with the content. The frontend highlights `range`; the service
 * only supplies it (D113).
 */
export interface ContentIssue {
  /** JSON path of the offending value, such as `body.givenName`. */
  field?: string;
  reason: string;
  /** Absent when the position could not be determined. */
  range?: SourceRange;
}

/** What happened to one connection attempt to one resolved address. */
export type ConnectOutcome =
  | 'connected'
  /** Refused by the address guard before connecting (D122). */
  | 'blocked'
  | 'refused'
  | 'reset'
  | 'unreachable'
  | 'timeout'
  | 'tls_error'
  | 'error';

export interface ConnectAttempt {
  address: string;
  family: 4 | 6;
  outcome: ConnectOutcome;
  /** The system or TLS error code, such as `ECONNREFUSED` or `CERT_HAS_EXPIRED`. */
  errorCode?: string;
  message?: string;
  /** For `blocked`: the range that matched, such as `private` or `carrierGradeNat`. */
  blockedRange?: string;
  /** For `timeout`: the stage that stalled. */
  timeoutStage?: 'connect' | 'tls' | 'first_byte' | 'body';
  /** Milliseconds from the start of this attempt. */
  timings: { connectMs?: number; tlsMs?: number; firstByteMs?: number; endMs?: number };
}

/**
 * One url requested. A redirect starts a new hop, and so does moving on to the
 * next IPFS gateway: a hop with no `redirectTo` followed by another hop.
 */
export interface FetchHop {
  url: string;
  dns:
    | { addresses: { address: string; family: 4 | 6 }[] }
    | { error: { code: string; message: string } };
  /** In the order tried. Stops at the first attempt that got a response. */
  attempts: ConnectAttempt[];
  response?: {
    status: number;
    headers: Record<string, string | string[]>;
  };
  /** Absolute url of the next hop, when this hop redirected. */
  redirectTo?: string;
}

/** The bytes received from the final hop, up to the fetch limit (D114). */
export interface ReportBody {
  /** blake2b-256 of the bytes kept. */
  hash: Hex;
  /** Bytes kept. At most the fetch limit. */
  size: number;
  /**
   * True when reading stopped at the limit. `size` is then a lower bound on
   * what the url would have sent.
   */
  truncated: boolean;
  contentType?: string;
  /** `utf8` when the bytes decode as UTF-8, otherwise `base64`. */
  encoding: 'utf8' | 'base64';
  data: string;
}

/**
 * The investigation view of one failed fetch (D113). Nothing in it is hidden
 * from the user (D116). Reports are kept forever and never replaced (D123).
 */
export interface MetadataReport {
  id: string;
  hash: Hex;
  url: string;
  /** The url actually requested first, after IPFS gateway rewriting. */
  effectiveUrl: string;
  startedAt: Timestamp;
  finishedAt: Timestamp;
  hops: FetchHop[];
  body?: ReportBody;
  result: {
    code: MetadataFailureCode;
    category: MetadataFailureCategory;
    message: string;
    servedHash?: Hex;
    issues: ContentIssue[];
  };
}

/** One row of a report history, newest first. */
export interface MetadataReportSummary {
  id: string;
  startedAt: Timestamp;
  code: MetadataFailureCode;
  category: MetadataFailureCategory;
  message: string;
}

/**
 * The outcome of a retry (D125). A retry inside the per-anchor window fetches
 * nothing and returns the latest result with the seconds remaining.
 */
export interface MetadataRefreshOutcome<TBody = unknown> {
  /** False when the window had not elapsed, or the hash was already cached. */
  refetched: boolean;
  /** Present when the retry was refused for the window. Whole seconds, at least 1. */
  retryAfterSeconds?: number;
  result: MetadataResult<TBody>;
}

export interface MetadataServiceV1 {
  /**
   * Look up by hash; fetch from `url` on a miss when one is supplied.
   *
   * Content is cached PERMANENTLY under the hash of what was actually served —
   * safe because the hash is the content identity, so a match can never go
   * stale. Errors are cached for a bounded duration only.
   *
   * The sequence is normative: miss on H → fetch U → bytes B → H' = hash(B) →
   * cache B permanently under H' → if H' ≠ H return `HASH_MISMATCH`. A mismatch
   * still populates the cache under H', so the fetch is not wasted.
   *
   * The hash is authoritative and `url` is only where to fetch on a miss: a
   * cached hash is answered whatever `url` says. The mismatch itself is cached
   * like any error, under (url, hash) for a bounded time, because a url can
   * change what it serves.
   */
  getMetadata(hash: Hex, url?: string): Promise<MetadataResult>;

  /**
   * The same, plus validation against a CIP. The CIP is a NUMBER (100, 108,
   * 119), so a new standard needs no interface change.
   */
  getCipMetadata<TBody = unknown>(
    cip: number,
    hash: Hex,
    url?: string,
  ): Promise<MetadataResult<TBody>>;

  /**
   * Force a re-fetch past a cached error, so a publisher who fixed their
   * hosting can verify it without waiting for expiry. Meaningless against a
   * cached success, which can never be wrong, so a cached hash returns it with
   * `refetched: false`.
   *
   * At most one real fetch per (url, hash) per window, whoever asks (D125).
   * Inside the window nothing is fetched and `retryAfterSeconds` says how long
   * to wait.
   */
  refresh(hash: Hex, url: string): Promise<MetadataRefreshOutcome>;

  /** One fetch report by id, or `null` when there is none. */
  getReport(reportId: string): Promise<MetadataReport | null>;

  /** Every report for (url, hash), newest first. The first is the default view. */
  listReports(hash: Hex, url: string): Promise<MetadataReportSummary[]>;
}
