/**
 * The closed set of metadata failure codes, one per pipeline stage:
 * retrieve -> size -> parse -> hash -> validate. A consumer can switch on it
 * exhaustively. Sub-causes (DNS, timeout, refusal, TLS) go in the message and
 * the fetch report.
 */
export type MetadataFailureCode =
  | "FETCH_ERROR"
  | "EXCEEDS_LIMIT"
  | "JSON_PARSE_ERROR"
  | "HASH_MISMATCH"
  | "SCHEMA_INVALID";

/** How a failure is presented. Derived from the code, never set on its own. */
export type MetadataFailureCategory = "NETWORK" | "INVALID_CONTENT" | "SCHEMA_INVALID";

export const FAILURE_CATEGORY: Record<MetadataFailureCode, MetadataFailureCategory> = {
  FETCH_ERROR: "NETWORK",
  EXCEEDS_LIMIT: "INVALID_CONTENT",
  JSON_PARSE_ERROR: "INVALID_CONTENT",
  HASH_MISMATCH: "INVALID_CONTENT",
  SCHEMA_INVALID: "SCHEMA_INVALID",
};

export const FAILURE_STATUS: Record<MetadataFailureCode, number> = {
  FETCH_ERROR: 502,
  EXCEEDS_LIMIT: 413,
  JSON_PARSE_ERROR: 422,
  HASH_MISMATCH: 409,
  SCHEMA_INVALID: 422,
};

export const isFailureCode = (value: unknown): value is MetadataFailureCode =>
  typeof value === "string" && value in FAILURE_STATUS;

/** HTTP status for a failure. A timed-out fetch answers 504 rather than 502. */
export const failureStatus = (code: MetadataFailureCode, message?: string): number =>
  code === "FETCH_ERROR" && message && /timeout|ETIMEDOUT/i.test(message)
    ? 504
    : FAILURE_STATUS[code];
