/**
 * Metadata Service v1 — off-chain metadata retrieval, validation & cache (DRAFT)
 *
 * Working draft for https://github.com/IntersectMBO/govtool/issues/4224 and
 * https://github.com/IntersectMBO/govtool/issues/4225.
 *
 * WHY THIS IS A SEPARATE SERVICE
 * ------------------------------
 * Everything in the Chain Data API (../chain-data) is derived from the chain: it is deterministic, it is
 * the same for every provider (db-sync, Koios, Blockfrost, Kupo), and it is
 * reproducible by re-reading the ledger. Off-chain metadata is none of those
 * things — it is fetched over HTTP/IPFS, it fails in its own ways, it has its
 * own retry/backoff/cache lifecycle, and its freshness is unrelated to the chain
 * tip. Mixing the two would mean:
 *   - every chain-data provider re-implementing fetch + validation, and
 *   - a chain-data response's `meta.asOf` claiming a freshness it does not have
 *     for the metadata half of the payload.
 *
 * So: the Chain Data API NEVER fetches metadata. It embeds a read-only
 * `MetadataProjection` (see below) produced by this service, and consumers
 * that need diagnostics, raw documents, or a forced refresh call this service
 * directly.
 *
 * Dependency direction is one-way: the Chain Data API imports the CIP body shapes from
 * here; nothing here imports from the Chain Data API.
 */

/* ------------------------------------------------------------------------- */
/* Shared primitives (kept local so this module stands alone)                 */
/* ------------------------------------------------------------------------- */

export type Hex = string;
export type Timestamp = string;

/** CIP-100 anchor: the on-chain pointer to an off-chain document. */
export interface Anchor {
  url: string;
  dataHash: Hex;
}

/**
 * Deterministic id for an anchor: blake2b-256(url || 0x23 || dataHash), hex.
 * Needed because the same dataHash can appear at several urls, and the same url
 * can serve different content over time.
 */
export type MetadataId = Hex;

/* ------------------------------------------------------------------------- */
/* CIP body schemas — the shapes the chain read model projects           */
/* ------------------------------------------------------------------------- */

export type MetadataStandard = "CIP100" | "CIP108" | "CIP119";

export interface MetadataReference {
  "@type": "Identity" | "Link" | "Other";
  label: string;
  uri: string;
}

export interface MetadataAuthor {
  name?: string;
  witnessAlgorithm?: string;
  publicKey?: string;
  signature?: string;
  /** Set once the service has actually checked the signature (#4225). */
  signatureVerified?: boolean;
}

/** CIP-108 — governance action metadata. */
export interface GovActionMetadataBody {
  title?: string;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references?: MetadataReference[];
  authors?: MetadataAuthor[];
}

/** CIP-119 — DRep metadata. */
export interface DRepMetadataBody {
  givenName?: string;
  objectives?: string;
  motivations?: string;
  qualifications?: string;
  paymentAddress?: string;
  image?: {
    url?: string;
    contentHash?: Hex;
    /** data: URI when the service inlined a small image (e.g. from IPFS). */
    data?: string;
  };
  identityReferences?: MetadataReference[];
  linkReferences?: MetadataReference[];
  doNotList?: boolean;
}

/** CIP-100 — vote rationale. */
export interface VoteRationaleBody {
  summary?: string;
  rationaleStatement?: string;
  precedentDiscussion?: string;
  counterargumentDiscussion?: string;
  conclusion?: string;
  internalVote?: Record<string, number>;
  references?: MetadataReference[];
  authors?: MetadataAuthor[];
}

/** Constitution text, resolved from a NewConstitution anchor. */
export interface ConstitutionBody {
  text?: string;
  contentType?: string;
}

export type MetadataBody =
  | GovActionMetadataBody
  | DRepMetadataBody
  | VoteRationaleBody
  | ConstitutionBody;

/* ------------------------------------------------------------------------- */
/* Lifecycle: retrieval, validation, cache                                    */
/* ------------------------------------------------------------------------- */

/**
 * Coarse state the chain read model projects. Deliberately small — four
 * values a UI can branch on without knowing anything about retries.
 */
export type MetadataStatus =
  /** Anchor known, never successfully processed yet (or currently retrying). */
  | "pending"
  /** Fetched, hash matched, parsed, conforms to its standard. */
  | "valid"
  /** Fetched but hash mismatch / not JSON-LD / wrong shape. Body may be partial. */
  | "invalid"
  /** Could not be retrieved at all, terminally. */
  | "unavailable";

/** #4225: retrieval failures and validation failures must stay distinguishable. */
export type RetrievalFailure =
  | "URL_NOT_FOUND"
  | "URL_UNREACHABLE"
  | "FETCH_TIMEOUT"
  | "FETCH_FORBIDDEN"
  | "UNSUPPORTED_SCHEME"
  | "TOO_LARGE"
  | "GATEWAY_ERROR";

export type ValidationFailure =
  | "INVALID_HASH"
  | "INVALID_JSONLD"
  | "INCORRECT_FORMAT"
  | "UNSUPPORTED_STANDARD"
  | "INVALID_SIGNATURE";

export type MetadataFailure =
  | { kind: "retrieval"; reason: RetrievalFailure; message?: string; terminal: boolean }
  | { kind: "validation"; reason: ValidationFailure; message?: string; terminal: boolean };

export type CacheState = "fresh" | "stale" | "refreshing" | "failed";

export interface MetadataProvenance {
  sourceUrl: string;
  /** The gateway actually used for ipfs:// / ar:// urls. */
  resolvedUrl?: string;
  httpStatus?: number;
  contentType?: string;
  byteSize?: number;
  /** Hash of what was actually served, to compare against `anchor.dataHash`. */
  observedHash?: Hex;
  firstSeenAt?: Timestamp;
  fetchedAt?: Timestamp;
  attempts: number;
  lastAttemptAt?: Timestamp;
  nextRetryAt?: Timestamp;
  cacheState: CacheState;
  /** When the cached copy should be revalidated. */
  expiresAt?: Timestamp;
}

/** Full record — what this service returns. */
export interface MetadataRecord<TBody = MetadataBody> {
  id: MetadataId;
  anchor: Anchor;
  standard: MetadataStandard;
  status: MetadataStatus;
  failure?: MetadataFailure;
  body?: TBody;
  /** Unparsed document, for "view raw" and independent hash re-checks. */
  raw?: unknown;
  provenance: MetadataProvenance;
}

/**
 * The slim, denormalized shape the Chain Data API embeds. No provenance, no
 * retry state, no raw document — just enough to render.
 *
 * ../chain re-exports this; it is defined here because this service owns it.
 */
export interface MetadataProjection<TBody> {
  id: MetadataId;
  anchor: Anchor;
  standard: MetadataStandard;
  status: MetadataStatus;
  /** Present for `valid`, and for `invalid` when a partial parse succeeded. */
  body?: TBody;
  /** Coarse reason, for the "metadata unavailable" hint. Details via this service. */
  failureReason?: RetrievalFailure | ValidationFailure;
  /** Drives "last checked N minutes ago" without exposing the retry machinery. */
  fetchedAt?: Timestamp;
}

/* ------------------------------------------------------------------------- */
/* Service API                                                                */
/* ------------------------------------------------------------------------- */

export interface MetadataQuery {
  /** Expected standard; the service records a mismatch rather than guessing. */
  standard?: MetadataStandard;
  includeRaw?: boolean;
}

export interface MetadataStats {
  total: number;
  byStatus: Record<MetadataStatus, number>;
  byFailure: Record<string, number>;
  oldestPendingAt: Timestamp | null;
  queueDepth: number;
}

export interface MetadataServiceV1 {
  /** Resolve one anchor. Registers it for fetching if unseen. */
  get(anchor: Anchor, q?: MetadataQuery): Promise<MetadataRecord>;

  /** Batch resolve — the path the chain read model uses when hydrating lists. */
  getMany(anchors: Anchor[], q?: MetadataQuery): Promise<Record<MetadataId, MetadataRecord>>;

  /** Raw stored document, unparsed. */
  getRaw(id: MetadataId): Promise<{ id: MetadataId; contentType?: string; raw: unknown }>;

  /**
   * Force a re-fetch, bypassing cache and resetting terminal-failure state.
   * Rate-limited; operator/registered-owner action, not an anonymous one.
   */
  refresh(id: MetadataId): Promise<MetadataRecord>;

  /** Validate an arbitrary url+hash without persisting — used by the submit forms. */
  validate(input: {
    url: string;
    dataHash: Hex;
    standard: MetadataStandard;
  }): Promise<Pick<MetadataRecord, "status" | "failure" | "body">>;

  /** Ops surface for #4224/#4225 dashboards. */
  stats(): Promise<MetadataStats>;
}
