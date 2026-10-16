import type { MetadataAnchor } from "./metadataReport";

// Mirrors govtool-backend/src/metadata/metadata-status.enum.ts.
export enum MetadataValidationStatus {
  URL_NOT_FOUND = "URL_NOT_FOUND",
  INVALID_JSONLD = "INVALID_JSONLD",
  INVALID_HASH = "INVALID_HASH",
  INCORRECT_FORMAT = "INCORRECT_FORMAT",
  EXCEEDS_LIMIT = "EXCEEDS_LIMIT",
  URL_BLOCKED = "URL_BLOCKED",
  INTERNAL_ERROR = "INTERNAL_ERROR",
}

export enum MetadataStandard {
  CIP108 = "CIP108",
  CIP119 = "CIP119",
  CIP100 = "CIP100"
}

/**
 * One rule a document breaks. An `error` comes with `INCORRECT_FORMAT`; a
 * `warning` alone leaves the document valid and its metadata returned.
 */
export type MetadataIssue = {
  field: string;
  rule: "required" | "maxLength";
  severity: "error" | "warning";
  limit?: number;
  actual?: number;
};

export type ValidateMetadataResult<MetadataType> = {
  status?: MetadataValidationStatus;
  valid: boolean;
  metadata?: MetadataType;
  issues?: MetadataIssue[];
  /** The metadata service's fetch report behind a failure, when it has one. */
  reportId?: string;
  /**
   * Set by the frontend, not the backend: why the request itself failed
   * (timeout, network, 5xx), beside `INTERNAL_ERROR`.
   */
  error?: string;
};

/** What a failed submission check knows, for `MetadataFailureDetails`. */
export type MetadataSubmissionFailure = {
  anchor: MetadataAnchor;
  reportId?: string;
  error?: string;
};

export type MetadataValidationDTO = {
  url: string;
  hash: string;
  standard?: MetadataStandard;
  /**
   * Fetch the url now instead of trusting the backend's hash cache. Set it
   * when submitting, where the url itself goes on chain.
   */
  verifyUrl?: boolean;
};

export type DRepMetadata = {
  paymentAddress?: string;
  givenName?: string;
  objectives?: string;
  motivations?: string;
  qualifications?: string;
  references?: Reference[];
  doNotList?: boolean;
};

export type ProposalMetadata = {
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: Reference[];
  title?: string;
};
