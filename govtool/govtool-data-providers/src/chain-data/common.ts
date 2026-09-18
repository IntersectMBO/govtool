/**
 * Chain Data API — shared primitives and response envelope.
 *
 * Everything here is module-agnostic: scalars, chain coordinates, the response
 * envelope, pagination, errors, and provider introspection. No domain types.
 *
 * Conventions enforced across every module:
 * - Lovelace is a decimal `string` (total supply exceeds Number.MAX_SAFE_INTEGER).
 * - Every point in time carries an ISO-8601 UTC string, and the epoch number
 *   whenever the source records it.
 * - A field the provider structurally cannot serve is `undefined`; which fields
 *   those are is declared once per provider at `/system/capabilities`, not
 *   repeated on every response. `null` means "known to be absent on chain".
 * - No internal database id is ever a canonical identifier. Where a consumer
 *   needs one for compatibility it travels as an opaque `providerId`.
 */

import type { Anchor } from '../metadata';
import type { ProviderCapabilityDocument } from './capabilities';

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

/**
 * The networks GovTool deploys against, plus whatever else a provider reports:
 * db-sync's `meta.network_name` is free text (`sanchonet`, a custom testnet),
 * and the contract passes it through rather than failing on it.
 */
export type NetworkId = 'mainnet' | 'preprod' | 'preview' | (string & {});

/* ------------------------------------------------------------------------- */
/* Chain coordinates                                                          */
/* ------------------------------------------------------------------------- */

/**
 * A block on the chain. `epoch` and `block` are what every provider can
 * report; the rest is optional because a provider that tracks only the block
 * height (db-sync's `MAX(block_no)`) cannot fill it without a second query.
 */
export interface ChainPoint {
  epoch: EpochNo;
  block: BlockNo;
  slot?: SlotNo;
  blockHash?: Hex;
  time?: Timestamp;
}

/**
 * A moment referenced by an entity (creation, expiry, enactment, ...).
 *
 * **Both fields are optional, and at least one is always set.** The halves
 * come from different kinds of source and neither is universal: db-sync
 * records a DRep's `last_register_time` with no epoch, while Blockfrost
 * records a DRep's `active_epoch` with no timestamp. Deriving the missing
 * half is unsafe in either direction — an epoch cannot be inferred from a
 * timestamp on a network with a non-standard epoch length, and a timestamp
 * cannot be inferred from an epoch without that network's genesis.
 *
 * A provider that can resolve the other half cheaply should, and fill both.
 * A consumer must handle either being absent; `hasTime` / `hasEpoch` narrow.
 */
export interface EpochStamp {
  epoch?: EpochNo;
  time?: Timestamp;
}

/** Narrows an `EpochStamp` known to carry a wall-clock time. */
export function hasTime(
  stamp: EpochStamp,
): stamp is EpochStamp & { time: Timestamp } {
  return typeof stamp.time === 'string';
}

/** Narrows an `EpochStamp` known to carry an epoch number. */
export function hasEpoch(
  stamp: EpochStamp,
): stamp is EpochStamp & { epoch: EpochNo } {
  return typeof stamp.epoch === 'number';
}

export interface TxRef {
  txHash: Hex;
  /** Certificate/vote/proposal index within the tx, where meaningful. */
  index?: number;
  block?: BlockNo;
  at?: EpochStamp;
  /**
   * The provider's own identifier for this transaction, carried only when a
   * consumer needs it for compatibility with an API that exposed it. Opaque:
   * never derive meaning from it, never compare it across providers.
   */
  providerId?: string;
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
export type StakeBasis = 'active' | 'live';

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
  /**
   * Epoch the snapshot belongs to. Report it whenever the source has it — a
   * power figure without its epoch cannot be compared against a stake total.
   * Optional because db-sync's per-credential distribution reads return the
   * latest amount without the epoch it was taken in.
   */
  epoch?: EpochNo;
  basis: StakeBasis;
  /** Share of the matching total for `basis`, 0..1. */
  share?: number;
}

/* ------------------------------------------------------------------------- */
/* Envelope                                                                   */
/* ------------------------------------------------------------------------- */

export type ProviderId = 'dbsync' | 'koios' | 'blockfrost' | 'kupo' | string;

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
  /**
   * Zero-based offset, for providers that can seek into the result set —
   * anything that materialises the whole list before paging, as db-sync does.
   * A provider that cannot seek rejects it with `INVALID_INPUT`. When both
   * `cursor` and `offset` are given, `cursor` wins.
   */
  offset?: number;
  /**
   * Omitted means "everything" for a provider that can afford it, so a
   * consumer that pages in memory (with its own cache) can fetch the whole
   * set in one read. A provider that cannot afford it applies its own cap and
   * reports `nextCursor`.
   */
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
  | 'INVALID_INPUT'
  | 'NOT_FOUND'
  | 'CAPABILITY_UNSUPPORTED'
  | 'PROVIDER_UNAVAILABLE'
  | 'PROVIDER_RATE_LIMITED'
  | 'PROVIDER_TIMEOUT'
  | 'STALE_DATA'
  | 'INTERNAL';

export interface ApiError {
  code: ErrorCode;
  message: string;
  retryable: boolean;
  retryAfterSeconds?: number;
  /** Safe diagnostics only — never raw provider payloads or connection strings. */
  details?: Record<string, unknown>;
  traceId?: string;
  /**
   * For `CAPABILITY_UNSUPPORTED`: the exact capability the call violated, in
   * the same vocabulary the declaration uses (`AnyCapabilityRefusal` from
   * `./capabilities`).
   *
   * Typed as `unknown` here only to keep this module free of a dependency on
   * `./capabilities`, which imports it. Providers construct it with
   * `capabilityUnsupported()` and consumers narrow it; the point is that the
   * key in the thrown error and the key in the declaration are the SAME value,
   * so a stale toggle names itself instead of being a string a human matches by
   * eye across five dialects.
   */
  capability?: unknown;
}

export interface ChainDataErrorOptions {
  /** Defaults from the code: provider outages, rate limits, timeouts and stale data retry; the rest do not. */
  retryable?: boolean;
  retryAfterSeconds?: number;
  details?: Record<string, unknown>;
  traceId?: string;
  cause?: unknown;
}

const RETRYABLE_BY_DEFAULT: ReadonlySet<ErrorCode> = new Set<ErrorCode>([
  'PROVIDER_UNAVAILABLE',
  'PROVIDER_RATE_LIMITED',
  'PROVIDER_TIMEOUT',
  'STALE_DATA',
]);

/**
 * The runtime form of `ApiError`.
 *
 * Every provider throws this and nothing else across the contract boundary, so
 * a consumer maps `code` to its own transport (HTTP status, gRPC status, a UI
 * state) without knowing which provider it is talking to. `details` is the
 * place for safe diagnostics; the wrapped `cause` is for logs, never for a
 * response body.
 *
 * Use `ChainDataError.is()` rather than `instanceof`: two copies of this
 * package can coexist in one process (a `file:` dependency resolved twice),
 * and `instanceof` fails across them where a structural check does not.
 */
export class ChainDataError extends Error implements ApiError {
  readonly code: ErrorCode;
  readonly retryable: boolean;
  readonly retryAfterSeconds?: number;
  readonly details?: Record<string, unknown>;
  readonly traceId?: string;

  constructor(
    code: ErrorCode,
    message: string,
    options: ChainDataErrorOptions = {},
  ) {
    super(
      message,
      options.cause === undefined ? undefined : { cause: options.cause },
    );
    this.name = 'ChainDataError';
    this.code = code;
    this.retryable = options.retryable ?? RETRYABLE_BY_DEFAULT.has(code);
    if (options.retryAfterSeconds !== undefined) {
      this.retryAfterSeconds = options.retryAfterSeconds;
    }
    if (options.details !== undefined) {
      this.details = options.details;
    }
    if (options.traceId !== undefined) {
      this.traceId = options.traceId;
    }
  }

  /** The wire shape, without the stack or the cause. */
  toJSON(): ApiError {
    const json: ApiError = {
      code: this.code,
      message: this.message,
      retryable: this.retryable,
    };
    if (this.retryAfterSeconds !== undefined) {
      json.retryAfterSeconds = this.retryAfterSeconds;
    }
    if (this.details !== undefined) {
      json.details = this.details;
    }
    if (this.traceId !== undefined) {
      json.traceId = this.traceId;
    }
    return json;
  }

  static is(value: unknown): value is ChainDataError {
    if (value instanceof ChainDataError) {
      return true;
    }
    if (typeof value !== 'object' || value === null) {
      return false;
    }
    const candidate = value as { name?: unknown; code?: unknown };
    return (
      candidate.name === 'ChainDataError' && typeof candidate.code === 'string'
    );
  }
}

/* ------------------------------------------------------------------------- */
/* Introspection                                                              */
/* ------------------------------------------------------------------------- */

/**
 * @deprecated since 0.4.0, removed in 0.5.0. Superseded by
 * `ProviderCapabilityDocument` in `./capabilities`.
 *
 * Three levels over free-form string keys could not survive three
 * implementations: the keys drifted into five incompatible dialects, 47 runtime
 * refusals matched no declared key and 16 declared keys matched no refusal
 * site, and `'partial'` meant "fewer fields", "one request per element" and
 * "only the first page was filtered" interchangeably. Kept for one release so a
 * provider can serve both shapes while it migrates.
 */
export type CapabilityLevel = 'supported' | 'partial' | 'unsupported';

/** @deprecated since 0.4.0, removed in 0.5.0. See `CapabilityLevel`. */
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
  status: 'healthy' | 'degraded' | 'unavailable';
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
  /**
   * The provider's capability declaration — see `./capabilities`.
   *
   * The import is type-only and therefore erased, so the cycle it forms with
   * `./capabilities` (which derives `RouteId` from `ChainDataApiV1`, which
   * re-exports this module) exists only for the type checker, which resolves
   * it. Nothing here is emitted.
   */
  getCapabilities(): Promise<Envelope<ProviderCapabilityDocument>>;
  getHealth(): Promise<Envelope<ProviderHealth[]>>;
}
