/**
 * Metadata resolve, retry and fetch-report wire types.
 *
 * Vendored from the contract, which is the source of truth:
 * govtool/govtool-data-providers/src/metadata/index.ts (see also
 * docs/api/metadata-service-spec.md §2.7 and §2.8). The frontend is built from
 * govtool/frontend alone, so it cannot import the contract package; keep these
 * in step with it by hand.
 */

export type MetadataFailureCode =
  | "FETCH_ERROR"
  | "EXCEEDS_LIMIT"
  | "JSON_PARSE_ERROR"
  | "HASH_MISMATCH"
  | "SCHEMA_INVALID";

export type MetadataFailureCategory =
  | "NETWORK"
  | "INVALID_CONTENT"
  | "SCHEMA_INVALID";

export interface MetadataFailure {
  ok: false;
  code: MetadataFailureCode;
  category: MetadataFailureCategory;
  message: string;
  servedHash?: string;
  reportId?: string;
  checkedAt: string;
}

export interface MetadataSuccess<TBody = unknown> {
  ok: true;
  hash: string;
  body: TBody;
  fetchedAt: string;
}

export type MetadataResult<TBody = unknown> =
  | MetadataSuccess<TBody>
  | MetadataFailure;

/** A point in the report body. `offset` counts UTF-16 code units. */
export interface SourcePosition {
  offset: number;
  byteOffset: number;
  line: number;
  column: number;
}

/** A half-open range `[start, end)` in the report body. */
export interface SourceRange {
  start: SourcePosition;
  end: SourcePosition;
}

export interface ContentIssue {
  field?: string;
  reason: string;
  range?: SourceRange;
}

export type ConnectOutcome =
  | "connected"
  | "blocked"
  | "refused"
  | "reset"
  | "unreachable"
  | "timeout"
  | "tls_error"
  | "error";

export interface ConnectAttempt {
  address: string;
  family: 4 | 6;
  outcome: ConnectOutcome;
  errorCode?: string;
  message?: string;
  blockedRange?: string;
  timeoutStage?: "connect" | "tls" | "first_byte" | "body";
  timings: {
    connectMs?: number;
    tlsMs?: number;
    firstByteMs?: number;
    endMs?: number;
  };
}

export interface FetchHop {
  url: string;
  dns:
    | { addresses: { address: string; family: 4 | 6 }[] }
    | { error: { code: string; message: string } };
  attempts: ConnectAttempt[];
  response?: {
    status: number;
    headers: Record<string, string | string[]>;
  };
  redirectTo?: string;
}

export interface ReportBody {
  hash: string;
  size: number;
  truncated: boolean;
  contentType?: string;
  encoding: "utf8" | "base64";
  data: string;
}

export interface MetadataReport {
  id: string;
  hash: string;
  url: string;
  effectiveUrl: string;
  startedAt: string;
  finishedAt: string;
  hops: FetchHop[];
  body?: ReportBody;
  result: {
    code: MetadataFailureCode;
    category: MetadataFailureCategory;
    message: string;
    servedHash?: string;
    issues: ContentIssue[];
  };
}

export interface MetadataReportSummary {
  id: string;
  startedAt: string;
  code: MetadataFailureCode;
  category: MetadataFailureCategory;
  message: string;
}

export interface MetadataRefreshOutcome<TBody = unknown> {
  refetched: boolean;
  retryAfterSeconds?: number;
  result: MetadataResult<TBody>;
}

/** Where a document lives and what it must hash to. */
export interface MetadataAnchor {
  url: string;
  hash: string;
}

/** The fetch limit the service applies (D120), for display only. */
export const METADATA_FETCH_LIMIT_BYTES = 2 * 1024 * 1024;
