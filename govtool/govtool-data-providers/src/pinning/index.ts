/**
 * Pinning Service v1 — the author-side write path. Optional.
 *
 * The only place anything is written. GovTool must work fully with it disabled
 * (SPEC.md §9).
 */

export type Cid = string;

export type PinFailureReason =
  | 'TOO_LARGE'
  | 'UNSUPPORTED_CONTENT_TYPE'
  | 'INVALID_METADATA'
  | 'QUOTA_EXCEEDED'
  | 'RATE_LIMITED'
  | 'BACKEND_UNAVAILABLE'
  | 'BACKEND_TIMEOUT'
  | 'BACKEND_ERROR';

export class PinningError extends Error {
  readonly reason: PinFailureReason;

  constructor(
    reason: PinFailureReason,
    message: string,
    options?: { cause?: unknown },
  ) {
    super(
      message,
      options?.cause === undefined ? undefined : { cause: options.cause },
    );
    this.name = 'PinningError';
    this.reason = reason;
  }

  static is(value: unknown): value is PinningError {
    return value instanceof PinningError;
  }
}

export interface PinBackendHealth {
  status: 'healthy' | 'degraded' | 'unavailable';
  message?: string;
}

export interface PinningServiceV1 {
  /**
   * Pin, blocking until the pin is COMPLETE. There is no pin status to poll and
   * no state machine: this resolves with the CID or rejects with a
   * `PinningError`.
   *
   * `owner` — a DRep id, stake address or similar — is what makes quota, usage
   * monitoring and abuse blocking possible. It is supplied by the caller, which
   * knows the connected wallet; this service cannot authenticate it. It is a
   * public chain identity, so recording it exposes nothing new.
   */
  pinData(data: Uint8Array, owner: string): Promise<Cid>;

  /** Compute the CID WITHOUT pinning, so an author can obtain the hash to
   *  anchor before committing to storage. */
  getDataCid(data: Uint8Array): Promise<Cid>;

  unpin(cid: Cid): Promise<void>;

  fetch(cid: Cid): Promise<Uint8Array>;

  getHealth(): Promise<PinBackendHealth>;
}
