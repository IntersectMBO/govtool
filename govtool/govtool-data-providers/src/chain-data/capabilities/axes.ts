/**
 * Chain Data API — capability vocabulary.
 *
 * A provider does not declare "which routes work". It declares where it sits in
 * a small orthogonal space, and every consequence — which routes are callable,
 * which UI controls can be offered — is computed from that.
 *
 * The space has three axes:
 *
 *   subject      WHAT the data is about        (drep, proposal, account, …)
 *   facet        WHICH ASPECT of that subject  (stake, delegation, tally, …)
 *   temporality  WHEN                          (current, asAt, series, events)
 *
 * A (subject, facet, temporality) triple is a **dataset** — the unit a provider
 * declares against, defined in ./datasets. Everything in this file is a
 * *modality* of reading a dataset: which option values are honoured, what one
 * call costs, how a page behaves, and what the served numbers actually mean.
 *
 * Two rules this file exists to enforce:
 *
 *   1. There is no `'partial'`. Every meaning that word carried — "fewer
 *      fields", "one request per element", "only the first page was filtered",
 *      "counts where the ledger uses stake" — is a separate, named value here,
 *      because a consumer reacts differently to each.
 *
 *   2. Nothing is a free string. Option values are exhaustive records over the
 *      contract's own unions, so adding a member to `DRepSort` fails every
 *      provider's build until its author decides what that member does.
 */

import type { EpochNo, StakeBasis } from '../common';

/* ------------------------------------------------------------------------- */
/* Axis 1 — subject                                                           */
/* ------------------------------------------------------------------------- */

/** The entity a dataset is about: one per top-level noun in the contract. */
export type Subject =
  | 'network'
  | 'account'
  | 'drep'
  | 'pool'
  | 'committee'
  | 'constitution'
  | 'proposal'
  | 'vote'
  | 'voter'
  | 'transaction'
  | 'survey';

/* ------------------------------------------------------------------------- */
/* Axis 2 — facet                                                             */
/* ------------------------------------------------------------------------- */

/**
 * The aspect of a subject — the "grouping of the datatypes". A provider that
 * can answer `stake` for DReps but not `delegation` says so once, and every
 * route reading either inherits the answer.
 */
export type Facet =
  /** Existence and canonical identity. */
  | 'identity'
  /** Certificate lifecycle: registered / updated / retired. */
  | 'registration'
  /** Balances and voting power. Pairs with `StakeBasis`. */
  | 'stake'
  /** Who delegates to whom. */
  | 'delegation'
  /** Individual votes cast. */
  | 'ballot'
  /** Votes aggregated per role, with thresholds. */
  | 'tally'
  /** The typed governance-action body. */
  | 'body'
  /** Protocol parameters. */
  | 'params'
  /** Collection-wide counters — the expensive ones. */
  | 'aggregate'
  /** Treasury and reserves. */
  | 'treasury'
  /** Epochs and blocks. */
  | 'chain'
  /** The currently enacted action of a type. */
  | 'outcome'
  /**
   * Off-chain document projection. Produced by the Metadata Service, NOT by a
   * chain-data provider — see `DatasetShape.servedBy`.
   */
  | 'metadata';

/* ------------------------------------------------------------------------- */
/* Axis 3 — temporality                                                       */
/* ------------------------------------------------------------------------- */

/**
 * `current` the value at the tip.
 * `asAt`    the value at a named past epoch (point-in-time).
 * `series`  one value per epoch over a range.
 * `events`  the transition stream that produced the state.
 *
 * Genuinely independent, and the axis the old table could not express at all:
 * db-sync serves a DRep's voting power `current` and refuses `series` on the
 * same method; Koios serves protocol params `asAt` and refuses account voting
 * power `asAt`; all three list a DRep's delegators `current` and none can list
 * the `events` — which is the user's headline case, and why it is a dataset of
 * its own rather than a field of the delegator list.
 */
export type Temporality = 'current' | 'asAt' | 'series' | 'events';

/** Read a single entity, or a collection of them. */
export type Cardinality = 'one' | 'many';

/* ------------------------------------------------------------------------- */
/* Reachability                                                               */
/* ------------------------------------------------------------------------- */

/**
 * How a dataset is reachable at all. Three states, because the contract already
 * has three legitimate answers and the old two-state table could express two:
 *
 *   served   call it.
 *   refused  the method exists and throws `CAPABILITY_UNSUPPORTED`.
 *   missing  the namespace or method is NOT on the object — `surveys?` omitted.
 *            Calling it is a TypeError, not a rejected promise, so a consumer
 *            must check before dereferencing.
 *
 * db-sync serves surveys, Koios implements the namespace and refuses the route,
 * Blockfrost has no `surveys.api.ts` at all.
 */
export type Reachability = 'served' | 'refused' | 'missing';

/* ------------------------------------------------------------------------- */
/* Cost                                                                        */
/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */
/* Absence                                                                     */
/* ------------------------------------------------------------------------- */

/**
 * Why something is not served. The consumer's correct reaction differs per
 * cause, which is the whole reason `'partial'` was useless:
 *
 *   notInSource      the upstream does not record it. Nothing to do.
 *   noIndex          it is recorded but not queryable that way (no text index
 *                    over DRep names, no ordering column on /drep_list).
 *   tooExpensive     it is reachable and costs more than it is worth. ALWAYS
 *                    carries a `fallback`; see `Unavailability`.
 *   representation   it exists and cannot be expressed in the contract's type
 *                    (float thresholds vs `Ratio`, decoded JSON vs CBOR hex).
 *   notImplemented   it could be served; this build does not. A code change.
 *   deploymentFault  this *instance* is broken. May come back without a code
 *                    change, so it must never be baked into a constant.
 */
export type Absence =
  | 'notInSource'
  | 'noIndex'
  | 'tooExpensive'
  | 'representation'
  | 'notImplemented'
  | 'deploymentFault';

/**
 * Whether a limit is a property of the contract, of the source, or of this box.
 * A `deployment` gap must never be a compile-time constant: Blockfrost's
 * `/txs/{hash}` 500s on GovTool's instance, and db-sync's
 * `get-stake-key-voting-power.sql` is dead on preview because that instance has
 * no `utxo_view` — a fault that shipped as a silent 0 ada on every wallet.
 */
export type DeclarationScope = 'contract' | 'source' | 'deployment';

/* ------------------------------------------------------------------------- */
/* Option support                                                              */
/* ------------------------------------------------------------------------- */

/**
 * What happens when a caller passes one option value.
 *
 *   honoured      applied as specified.
 *   rejected      throws `CAPABILITY_UNSUPPORTED`.
 *   ignored       ACCEPTED AND SILENTLY NOT APPLIED. db-sync's proposal sort
 *                 switch has no `highestParticipation` case and falls through
 *                 `default: return copied`, so the caller gets an unsorted list
 *                 presented as sorted. This is worse than a refusal, and a UI
 *                 must not offer the option — which is why it needs a name.
 *   approximated  applied, but not over the whole result set. Blockfrost
 *                 filters an already-hydrated 25-row page, so a filtered page
 *                 can come back short while matches remain further on.
 */
export type OptionSupport =
  'honoured' | 'rejected' | 'ignored' | 'approximated';

/** An option a UI may offer. `ignored` is deliberately excluded. */
export function isOfferable(support: OptionSupport): boolean {
  return support === 'honoured' || support === 'approximated';
}

/**
 * Exhaustive over a contract enum. Adding a member to `DRepSort` breaks all
 * three provider builds until each declares it — the property 90 hand-written
 * strings never had.
 *
 * This one shape covers BOTH sort granularities the survey found: every member
 * `rejected` means hide the control (Koios and Blockfrost on `DRepSort`); one
 * member `rejected` means hide that menu item (Koios on `GovActionSort`).
 */
export type EnumSupport<T extends string> = {
  readonly [V in T]: OptionSupport;
};

/** The members a UI may offer, in the order the record declares them. */
export function offerableMembers<T extends string>(
  support: EnumSupport<T> | undefined,
): readonly T[] {
  if (support === undefined) {
    return [];
  }
  return (Object.keys(support) as T[]).filter((member) =>
    isOfferable(support[member]),
  );
}

/**
 * A filter parameter. `maxSelected` is not a statement about WHICH values are
 * legal and no value-set can express it: Koios accepts any single proposal
 * status and refuses two, because it encodes status as four separate epoch
 * columns. A multi-select chip row bound to a `string[]` will happily build an
 * illegal request without it.
 */
export interface FilterSupport<T extends string> {
  readonly values: EnumSupport<T>;
  /** Values accepted in one request. Omitted = unbounded. */
  readonly maxSelected?: number;
  /**
   * False when the filter is applied AFTER paging, so a page can come back
   * short while matches remain. Defaults to true.
   */
  readonly exhaustive?: boolean;
  /**
   * The member the provider applies when the caller sends none. Used as the
   * fallback when a UI's hard-coded default turns out to be refused.
   */
  readonly defaultsTo?: T;
}

/* ------------------------------------------------------------------------- */
/* Search                                                                      */
/* ------------------------------------------------------------------------- */

/**
 * One input box, three independent lookups. GovTool's DRep search branches on a
 * CIP-129 prefix, then a 58-char hash, then an Ada Handle, then free text — and
 * Koios can do the first and neither of the last two. Gating the box would
 * remove working functionality; gating per mode lets the placeholder degrade.
 */
export type SearchMode = 'exactId' | 'freeText' | 'adaHandle';

export interface SearchSupport {
  readonly modes: EnumSupport<SearchMode>;
  /**
   * Whether `search: ''` is accepted as "no filter" even when `freeText` is
   * rejected. GovTool's backend always sends the parameter, so a provider that
   * threw on the empty string would break a call that otherwise works.
   */
  readonly emptyStringAccepted: boolean;
}

/* ------------------------------------------------------------------------- */
/* Paging                                                                      */
/* ------------------------------------------------------------------------- */

/**
 * `omittedLimitMeans` is the fact that already shipped a bug. The contract says
 * an omitted `limit` returns everything — true on db-sync, 1000 of 1684 rows on
 * Koios, 25 on Blockfrost — and the backend built its DRep snapshot by taking
 * the first page as the whole set.
 *
 * `total` is separate from "can list": a numbered paginator needs a trustworthy
 * count, and the fallback UI is cursor-style prev/next.
 */
export interface PagingSupport {
  readonly cursor: OptionSupport;
  /** `PageRequest.offset`. A provider that cannot seek rejects it. */
  readonly offset: OptionSupport;
  /** Silent clamp ceiling. `null` = no cap. */
  readonly maxLimit: number | null;
  readonly omittedLimitMeans: 'everything' | 'oneMaxPage' | 'routeDefault';
  readonly defaultLimit?: number;
  readonly total: 'exact' | 'estimated' | 'absent';
}

/**
 * The batch form of a method, gated separately from the single form, because
 * the capability is a property of the argument shape: Blockfrost serves
 * `getVotingPowers(ids)` and refuses `getVotingPowers()`, since omitting `ids`
 * means one request per DRep in the directory.
 */
export interface BatchSupport {
  readonly explicitIds: OptionSupport;
  readonly allIds: OptionSupport;
  readonly maxIdsPerRequest?: number;
}

/* ------------------------------------------------------------------------- */
/* Caveats — what a 200 response actually means                                */
/* ------------------------------------------------------------------------- */

/**
 * The kind a route-level flag cannot express: the call succeeds, the value is
 * well-formed, and it means something other than the caller assumed. None of
 * these throws, so a route-level flag reports all of them as working.
 *
 * Every variant carries STRUCTURED fields plus a `note`. A UI switches on
 * `kind` and interpolates the structured fields through its own i18n; `note` is
 * provider-authored English and is for logs and support bundles only. Rendering
 * `note` into the DOM would ship untranslated provider prose to users.
 */
export type Caveat =
  | {
      /**
       * The figures are in a different unit from the one the ledger decides by.
       * A Blockfrost DRep tally is head-count turnout where the ledger weighs
       * stake, so an "₳" prefix would be a lie and the percentage denominator
       * meaningless.
       */
      readonly kind: 'differentUnit';
      readonly reports: 'stake' | 'count';
      readonly ledgerDecidesBy: 'stake' | 'count';
      readonly note: string;
    }
  | {
      /**
       * The route applies a filter the caller did not ask for: db-sync's
       * proposal list is live-only by construction, and its `dreps.listVotes`
       * silently drops votes cast on concluded actions.
       */
      readonly kind: 'impliedFilter';
      readonly param: string;
      readonly restrictedTo: readonly string[];
      readonly note: string;
    }
  | {
      /** A window, not a lifetime: db-sync's `votesCast` is a trailing 365 days. */
      readonly kind: 'boundedWindow';
      readonly windowDays?: number;
      readonly sinceEpoch?: EpochNo;
      readonly note: string;
    }
  | {
      /**
       * Correct answers may be missing from an otherwise 200 response:
       * Blockfrost filters one hydrated page; `getEnacted` returns `null` after
       * a bounded scan, which does not mean absence.
       */
      readonly kind: 'notExhaustive';
      readonly note: string;
    }
  | {
      /**
       * Exact on chain, lossy in the source. `dvt`/`pvt` are floats on all
       * three sources and the contract wants an exact `Ratio`.
       */
      readonly kind: 'precisionLoss';
      readonly contractType: string;
      readonly sourceType: string;
      readonly note: string;
    }
  | {
      /**
       * Right data, wrong encoding: Koios decodes label-17 metadata to JSON
       * where the contract requires the original CBOR hex.
       */
      readonly kind: 'encodingMismatch';
      readonly contractEncoding: string;
      readonly sourceEncoding: string;
      readonly note: string;
    }
  | {
      /** Koios does not number voting procedures within a transaction. */
      readonly kind: 'identifierGranularity';
      readonly note: string;
    }
  | {
      /** The value is the epoch snapshot where the caller may expect live. */
      readonly kind: 'staleBasis';
      readonly basis: StakeBasis;
      readonly note: string;
    };

/** Just the machine-readable half, for a UI that switches on it. */
export type CaveatKind = Caveat['kind'];
