/**
 * Chain Data API — shared primitives and response envelope.
 *
 * Everything here is module-agnostic: scalars, chain coordinates, the response
 * envelope, pagination, errors, and provider introspection. No domain types.
 *
 * Conventions enforced across every module:
 * - Lovelace is a decimal `string` (total supply exceeds Number.MAX_SAFE_INTEGER).
 * - Every point in time carries both an epoch number and an ISO-8601 UTC string.
 * - A field the provider structurally cannot serve is `undefined`; which fields
 *   those are is declared once per provider at `/system/capabilities`, not
 *   repeated on every response. `null` means "known to be absent on chain".
 * - No internal database ids ever appear in the contract.
 */

import type { Anchor } from "../metadata";

export type { Anchor };

/* ------------------------------------------------------------------------- */
/* Scalars                                                                    */
/* ------------------------------------------------------------------------- */

/** Decimal string, lovelace. e.g. "1500000000" */
export type Lovelace = string;
/** Lowercase hex, no 0x prefix. */
export type Hex = string;
/** ISO-8601 UTC, e.g. "2026-09-17T10:04:00Z" */
export type Timestamp = string;
export type EpochNo = number;
export type SlotNo = number;
export type BlockNo = number;
export type Bech32 = string;
/** Keep numerator/denominator as they appear on chain, never a float. */
export type Ratio = { numerator: number; denominator: number };

export type NetworkId = "mainnet" | "preprod" | "preview";

/* ------------------------------------------------------------------------- */
/* Chain coordinates                                                          */
/* ------------------------------------------------------------------------- */

export interface ChainPoint {
  epoch: EpochNo;
  slot: SlotNo;
  block: BlockNo;
  blockHash: Hex;
  time: Timestamp;
}

/** A moment referenced by an entity (creation, expiry, enactment, ...). */
export interface EpochStamp {
  epoch: EpochNo;
  time: Timestamp;
}

export interface TxRef {
  txHash: Hex;
  /** Certificate/vote/proposal index within the tx, where meaningful. */
  index?: number;
  block?: BlockNo;
  at?: EpochStamp;
}

/* ------------------------------------------------------------------------- */
/* Stake — shared by chain, accounts and governance                           */
/* ------------------------------------------------------------------------- */

/**
 * `active` = the epoch-boundary snapshot the ledger counts votes against.
 * `live`   = the current, un-snapshotted value.
 *
 * The two differ within an epoch. Every field that has both states which it is;
 * only `active` values are valid tally denominators.
 */
export type StakeBasis = "active" | "live";

/**
 * The components of an account's stake. `total` is the sum, and is what counts
 * toward voting power; the breakdown explains a figure that will not match a
 * wallet's spendable balance.
 */
export interface StakeBalance {
  total: Lovelace;
  utxo?: Lovelace;
  /** Staking rewards from pool delegation (db-sync `reward`). */
  rewards?: Lovelace;
  /**
   * Non-staking rewards (db-sync `reward_rest`): governance action deposit
   * refunds, DRep registration deposit refunds, treasury and reserve payouts,
   * and MIR certificates.
   *
   * Withdrawable and stake-counting like `rewards`, but invisible in most
   * wallet UIs, so it is the usual explanation for voting power exceeding an
   * account's apparent balance.
   */
  rewardsRest?: Lovelace;
}

export interface VotingPower {
  amount: Lovelace;
  /** Epoch the snapshot belongs to — never report power without its epoch. */
  epoch: EpochNo;
  basis: StakeBasis;
  /** Share of the matching total for `basis`, 0..1. */
  share?: number;
}

/* ------------------------------------------------------------------------- */
/* Envelope                                                                   */
/* ------------------------------------------------------------------------- */

export type ProviderId = "dbsync" | "koios" | "blockfrost" | "kupo" | string;

/** Per-response metadata. Tip lag is derived from `asOf`; provider capability
 * gaps are declared at `/system/capabilities`. */
export interface ResponseMeta {
  /**
   * Chain state this response reflects, letting a consumer that merges several
   * reads check whether they are mutually consistent.
   *
   * Optional: providers that do not return the tip alongside a query omit it
   * rather than spend a round trip fetching it. Absent means "not reported",
   * not "tip unknown" — an absent `asOf` makes the response unverifiable for
   * consistency purposes, never assumed current.
   */
  asOf?: ChainPoint;
}

export interface Envelope<T> {
  data: T;
  meta: ResponseMeta;
}

export interface PageRequest {
  /** Opaque cursor. Absent on the first page. */
  cursor?: string;
  limit?: number;
}

export interface Page<T> {
  elements: T[];
  nextCursor: string | null;
  /** Only present when the provider can count cheaply. */
  total?: number;
}

export type PagedEnvelope<T> = Envelope<Page<T>>;

/* ------------------------------------------------------------------------- */
/* Errors                                                                     */
/* ------------------------------------------------------------------------- */

export type ErrorCode =
  | "INVALID_INPUT"
  | "NOT_FOUND"
  | "CAPABILITY_UNSUPPORTED"
  | "PROVIDER_UNAVAILABLE"
  | "PROVIDER_RATE_LIMITED"
  | "PROVIDER_TIMEOUT"
  | "STALE_DATA"
  | "INTERNAL";

export interface ApiError {
  code: ErrorCode;
  message: string;
  retryable: boolean;
  retryAfterSeconds?: number;
  /** Safe diagnostics only — never raw provider payloads or connection strings. */
  details?: Record<string, unknown>;
  traceId?: string;
}

/* ------------------------------------------------------------------------- */
/* Introspection                                                              */
/* ------------------------------------------------------------------------- */

export type CapabilityLevel = "supported" | "partial" | "unsupported";

export interface ProviderCapabilities {
  provider: ProviderId;
  network: NetworkId;
  /**
   * Keyed by route id (`governance.dreps.listDelegators`) or `route#field` for
   * field-level gaps.
   */
  capabilities: Record<string, CapabilityLevel>;
}

export interface ProviderHealth {
  provider: ProviderId;
  status: "healthy" | "degraded" | "unavailable";
  tip?: ChainPoint;
  lastSuccessfulSyncAt?: Timestamp;
  /** Wall-clock gap between the tip block's time and now. */
  secondsSinceLastUpdate?: number;
  /** Above this, the deployment reports itself unavailable (503). */
  stalenessThresholdSeconds?: number;
  errorRate5m?: number;
  message?: string;
}

/** `/system/*` — the same shape regardless of which provider is configured. */
export interface SystemApi {
  getCapabilities(): Promise<Envelope<ProviderCapabilities>>;
  getHealth(): Promise<Envelope<ProviderHealth[]>>;
}
