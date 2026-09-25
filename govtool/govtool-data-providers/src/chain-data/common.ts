/**
 * Chain Data API — shared scalars, envelopes, paging and errors.
 *
 * See SPEC.md §3 for the rules these types encode.
 */

import type { Anchor } from '../metadata';

export type { Anchor };

/* ------------------------------------------------------------------------- */
/* Scalars                                                                    */
/* ------------------------------------------------------------------------- */

/**
 * Lovelace as an INTEGER string: base-10 digits, no decimal point, no units.
 * 1 ada = 1_000_000 lovelace. Never a number — ada totals exceed what a 64-bit
 * float represents exactly, so a numeric type silently loses precision at the
 * top of the range.
 */
export type Lovelace = string;
/** Lowercase hex, no `0x` prefix. */
export type Hex = string;
/** ISO-8601 UTC, e.g. `2026-09-17T10:04:00Z`. */
export type Timestamp = string;
export type EpochNo = number;
export type SlotNo = number;
export type BlockNo = number;
/**
 * Bech32 in the current standard form for the entity — CIP-129 for DRep,
 * governance action and committee credentials; `pool1…` for stake pools;
 * `stake1…` for stake addresses. One form per entity; no legacy alternates
 * travel in this contract.
 */
export type Bech32 = string;

/**
 * An exact rational, as the ledger holds it. Both members are integers and
 * `denominator` is never 0.
 *
 * Normalization is NOT required, so `67/100` and `134/200` are the same value
 * in different shapes. Compare with `ratioEquals`, never structurally.
 */
export interface Ratio {
  numerator: number;
  denominator: number;
}

/** Cross-multiplication, because `Ratio` is not required to be reduced. */
export function ratioEquals(a: Ratio, b: Ratio): boolean {
  return a.numerator * b.denominator === b.numerator * a.denominator;
}

/**
 * The networks GovTool deploys against, plus whatever else a provider reports:
 * a source's network name may be free text (`sanchonet`, a custom testnet), and
 * the contract passes it through rather than failing on it.
 */
export type NetworkId = 'mainnet' | 'preprod' | 'preview' | (string & {});

/* ------------------------------------------------------------------------- */
/* Chain coordinates                                                          */
/* ------------------------------------------------------------------------- */

export interface ChainPoint {
  epoch: EpochNo;
  slot?: SlotNo;
  block?: BlockNo;
  time?: Timestamp;
}

/**
 * When something happened. `epoch` is always known; `time` is the renderable
 * part and a provider supplies it whenever its source dates the record.
 */
export interface EpochStamp {
  epoch: EpochNo;
  slot?: SlotNo;
  block?: BlockNo;
  time?: Timestamp;
}

export interface TxRef {
  txHash: Hex;
  /** Certificate / vote / proposal index within the tx, where meaningful. */
  index?: number;
  block?: BlockNo;
  at?: EpochStamp;
}

/* ------------------------------------------------------------------------- */
/* Stake                                                                      */
/* ------------------------------------------------------------------------- */

/**
 * `active` = the epoch-boundary snapshot the ledger counts votes against.
 * `live`   = the current, un-snapshotted value.
 *
 * The two differ within an epoch. Only `active` values are valid tally
 * denominators.
 */
export type StakeBasis = 'active' | 'live';

/**
 * The components of an account's stake.
 *
 * If a provider serves a balance at all it serves the components, not just
 * `total` — `total` alone tells a consumer nothing a CIP-30 wallet could not
 * already supply. The point is `rewards` and `rewardsRest`, which most wallet
 * UIs omit and which are the usual explanation for voting power exceeding an
 * apparent balance.
 */
export interface StakeBalance {
  total: Lovelace;
  utxo: Lovelace;
  /** Staking rewards from pool delegation. */
  rewards: Lovelace;
  /**
   * Non-staking rewards: governance action deposit refunds, DRep registration
   * deposit refunds, treasury and reserve payouts, MIR certificates.
   * Withdrawable and stake-counting like `rewards`, but invisible in most
   * wallet UIs.
   */
  rewardsRest: Lovelace;
}

export interface VotingPower {
  amount: Lovelace;
  basis: StakeBasis;
  /** The epoch the figure is for. */
  epoch?: EpochNo;
}

/* ------------------------------------------------------------------------- */
/* Envelope                                                                   */
/* ------------------------------------------------------------------------- */

export interface ResponseMeta {
  /** Which provider answered, matching `ProviderIdentity.id`. */
  provider: string;
  network: NetworkId;
  /** The chain point the answer reflects. */
  asOf?: ChainPoint;
}

export interface Envelope<T> {
  data: T;
  meta: ResponseMeta;
}

/* ------------------------------------------------------------------------- */
/* Paging — SPEC.md §3.4                                                      */
/* ------------------------------------------------------------------------- */

/**
 * One paging model, honoured by every provider. There is no cursor and no
 * opaque continuation token.
 */
export interface PageRequest {
  /** 1-based. */
  page: number;
  size: number;
}

export interface Page<T> {
  elements: T[];
  /**
   * Count across the whole filtered result set, not the size of this page.
   * Optional but STRONGLY RECOMMENDED: without it a numbered paginator cannot
   * be drawn and a consumer cannot size a full read in advance.
   *
   * When it is omitted, the only end-of-collection signal is a SHORT page, so a
   * provider that omits it must never return fewer than `size` rows except on
   * the last page.
   */
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

export interface ChainDataErrorOptions {
  retryable?: boolean;
  retryAfterSeconds?: number;
  /** Safe diagnostics only — never raw provider payloads or credentials. */
  details?: Record<string, unknown>;
  cause?: unknown;
}

/**
 * Thrown by a provider. A provider that cannot compute a value raises one of
 * these; it never returns `0`, an empty list, or any other placeholder that
 * reads as data.
 */
export class ChainDataError extends Error {
  readonly code: ErrorCode;
  readonly retryable: boolean;
  readonly retryAfterSeconds?: number;
  readonly details?: Record<string, unknown>;

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
    this.retryable = options.retryable ?? false;
    if (options.retryAfterSeconds !== undefined) {
      this.retryAfterSeconds = options.retryAfterSeconds;
    }
    if (options.details !== undefined) {
      this.details = options.details;
    }
  }

  static is(value: unknown): value is ChainDataError {
    return value instanceof ChainDataError;
  }
}

/* ------------------------------------------------------------------------- */
/* Provider identity and health — SPEC.md §4                                  */
/* ------------------------------------------------------------------------- */

/** Who this provider is, for attribution. */
export interface ProviderIdentity {
  /** Free-form and provider-chosen; not drawn from a fixed list. */
  id: string;
  name: string;
  /** Base64-inlined PNG, so attribution renders with no second fetch. */
  icon?: string;
}

export interface ProviderHealth {
  status: 'healthy' | 'degraded' | 'unavailable';
  tip?: ChainPoint;
  lastSuccessfulSyncAt?: Timestamp;
  /** Wall-clock gap between the tip and now. */
  secondsSinceLastUpdate?: number;
  message?: string;
}

/** `/system/*` — the same shape whichever provider is configured. */
export interface SystemApi {
  getIdentity(): Promise<Envelope<ProviderIdentity>>;
  /** What this provider supports; see `./capabilities`. */
  getCapabilities(): Promise<
    Envelope<import('./capabilities').ProviderCapabilities>
  >;
  getHealth(): Promise<Envelope<ProviderHealth>>;
}
