/**
 * Pinning Service v1 — author-side storage for user-authored metadata (DRAFT)
 *
 * Working draft; splits `POST /upload` out of the current backend
 * (govtool/backend/src/VVA/Ipfs.hs) into its own component.
 *
 * WHY THIS IS A SEPARATE COMPONENT
 * --------------------------------
 * The Chain Data API (../chain-data) is a read model over the chain. The Metadata
 * Service (../metadata) is a read pipeline over the public internet.
 * Pinning is neither: it is a WRITE path, it is authenticated, it costs money,
 * it has quotas and abuse surface, and it is the only part of the stack that
 * accepts arbitrary user content. Folding it into either read service would put
 * an untrusted write endpoint behind a cache-heavy read service and give the
 * Chain Data API a write dependency it otherwise does not have.
 *
 * It is also optional: a user who hosts metadata themselves never touches it.
 * GovTool must work with the pinning service switched off.
 *
 * THE HANDOFF
 * -----------
 *   author form ──pin()──► PinRecord { url, dataHash }
 *                              │
 *                              ▼  user submits the anchor on chain (wallet)
 *                          Anchor { url, dataHash }
 *                              │
 *                              ▼  read back, independently, later
 *                     Metadata Service ──► MetadataProjection
 *
 * The two services share only the `Anchor` shape. The pinning service is not a
 * trusted source for the metadata service: content is re-fetched and re-hashed
 * from the public url like anyone else's, so a document that is pinned here but
 * unreachable publicly is correctly reported as `unavailable`.
 */

import type { Anchor, Hex, MetadataStandard, Timestamp } from "../metadata";

/* ------------------------------------------------------------------------- */
/* Identity                                                                   */
/* ------------------------------------------------------------------------- */

/** IPFS CID, v1 base32 preferred. */
export type Cid = string;

export type PinBackendId = "kubo" | "pinata" | "filebase" | "w3s" | string;

/* ------------------------------------------------------------------------- */
/* Pinning                                                                    */
/* ------------------------------------------------------------------------- */

export interface PinRequest {
  /**
   * The document to pin. JSON-LD for governance metadata; the service pins the
   * exact bytes it is given — it must not re-serialize, since the on-chain hash
   * is computed over these bytes.
   */
  content: string;
  contentType: "application/ld+json" | "application/json" | "text/plain" | string;
  /** Advisory only; IPFS addresses by content, not name. */
  fileName?: string;
  /**
   * When set, the service validates the document against the standard BEFORE
   * pinning and rejects malformed input, so unusable anchors never reach chain.
   */
  standard?: MetadataStandard;
  /** What this document will be attached to; drives quota bucketing and metrics. */
  purpose?: "govAction" | "drepProfile" | "voteRationale" | "other";
}

export type PinStatus = "pinning" | "pinned" | "failed" | "unpinned";

export interface PinRecord {
  cid: Cid;
  /** Canonical ipfs:// url — this is what belongs in the on-chain anchor. */
  url: string;
  /** Gateway urls for display/preview only; never put one on chain. */
  gatewayUrls: string[];
  /**
   * blake2b-256 of the pinned bytes — the hash that belongs in the on-chain
   * anchor. Computed by this service so the author never has to.
   */
  dataHash: Hex;
  byteSize: number;
  contentType: string;
  status: PinStatus;
  pinnedAt: Timestamp | null;
  /** Which backends currently hold it; > 1 is the durability story. */
  replicas: { backend: PinBackendId; status: PinStatus; pinnedAt: Timestamp | null }[];
  /** Set when the service applies a retention window rather than pinning forever. */
  expiresAt?: Timestamp;
  failure?: { reason: PinFailureReason; message?: string; terminal: boolean };
}

export type PinFailureReason =
  | "TOO_LARGE"
  | "UNSUPPORTED_CONTENT_TYPE"
  | "INVALID_METADATA"
  | "QUOTA_EXCEEDED"
  | "RATE_LIMITED"
  | "BACKEND_UNAVAILABLE"
  | "BACKEND_TIMEOUT";

/** Convenience: the exact pair the author must put on chain. */
export function toAnchor(pin: PinRecord): Anchor {
  return { url: pin.url, dataHash: pin.dataHash };
}

/* ------------------------------------------------------------------------- */
/* Policy — the part that must be explicit because this accepts user content   */
/* ------------------------------------------------------------------------- */

export interface PinningPolicy {
  maxBytes: number;
  allowedContentTypes: string[];
  /** Per-session / per-wallet, whichever the deployment authenticates on. */
  rateLimit: { requests: number; windowSeconds: number };
  quota: { maxPinsPerOwner: number; maxBytesPerOwner: number };
  /** null = pinned indefinitely. */
  retentionDays: number | null;
  /** Whether unauthenticated pinning is allowed at all. */
  requiresAuth: boolean;
}

export interface PinBackendHealth {
  backend: PinBackendId;
  status: "healthy" | "degraded" | "unavailable";
  lastSuccessAt?: Timestamp;
  usedBytes?: number;
  quotaBytes?: number;
  message?: string;
}

/* ------------------------------------------------------------------------- */
/* Service API                                                                */
/* ------------------------------------------------------------------------- */

export interface PinningServiceV1 {
  /** Pin a document and return the anchor pair to submit on chain. */
  pin(req: PinRequest): Promise<PinRecord>;

  /**
   * Hash and validate WITHOUT pinning — lets the author preview the exact
   * `dataHash` they will commit when they host the file themselves.
   */
  prepare(req: Omit<PinRequest, "purpose">): Promise<{
    dataHash: Hex;
    byteSize: number;
    valid: boolean;
    errors?: string[];
  }>;

  getPin(cid: Cid): Promise<PinRecord>;

  /** Pins owned by the calling session/wallet. */
  listPins(q?: { limit?: number; cursor?: string }): Promise<{
    elements: PinRecord[];
    nextCursor: string | null;
  }>;

  /** Re-pin something that fell out of a backend, or add a replica. */
  repin(cid: Cid, backend?: PinBackendId): Promise<PinRecord>;

  /** Owner-initiated removal. Does not remove the content from other nodes. */
  unpin(cid: Cid): Promise<{ cid: Cid; status: "unpinned" }>;

  getPolicy(): Promise<PinningPolicy>;
  getHealth(): Promise<PinBackendHealth[]>;
}
