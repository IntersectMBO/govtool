/**
 * Chain Data API — field-level capability, derived from the entity types.
 *
 * `common.ts` already states the rule this file mechanises: "A field the
 * provider structurally cannot serve is `undefined`; which fields those are is
 * declared once per provider at `/system/capabilities`, not repeated on every
 * response. `null` means known to be absent on chain."
 *
 * So the optional (`?`) keys of an entity ARE its capability-gated fields, and
 * `OptionalFieldsOf` computes them from the type rather than asking a provider
 * author to retype them as dotted strings. `'#registration.status'`,
 * `'#body.UpdateCommittee'` and `'#lifecycle.submitted'` — three of the five
 * key dialects in the old tables — disappear, because nesting is decomposed
 * into entities of its own (`Registration`, `GovActionLifecycle`) instead of
 * being spelled as a path.
 *
 * `EntityFieldTable` is REQUIRED and EXHAUSTIVE. That is deliberate and it is
 * the single most important anti-drift property in this design: the old tables
 * had 16 entries marked `unsupported` with no refusal site anywhere, because a
 * silently-omitted field is invisible unless somebody is forced to answer for
 * it. Adding `DRep.delegatorTrend?` breaks all three provider builds.
 */

import type {
  Account,
  Delegation,
  DelegationHistoryEvent,
  PoolDelegation,
  StakeRegistrationEvent,
} from '../accounts';
import type { StakeBalance, VotingPower } from '../common';
import type {
  BlockSummary,
  EpochSummary,
  NetworkInfo,
  ProtocolParams,
  StakeDistribution,
  Treasury,
} from '../network';
import type {
  Committee,
  CommitteeMember,
  Constitution,
} from '../governance/committee';
import type {
  DRep,
  DRepActivity,
  DRepDelegator,
  DRepHistoryEvent,
  DRepVotingPowerEntry,
  Registration,
} from '../governance/dreps';
import type { GovernanceMetrics } from '../governance/metrics';
import type { SpoVoter } from '../governance/pools';
import type {
  EnactedActionSummary,
  GovAction,
  GovActionActivityEvent,
  GovActionLifecycle,
  RoleTally,
} from '../governance/proposals';
import type { VoteRecord } from '../governance/votes';
import type { SurveyDefinition } from '../surveys';
import type { TransactionState } from '../transactions';
import type { Absence } from './axes';
import type { RouteId } from './datasets';

/* ------------------------------------------------------------------------- */
/* The entities a provider declares against                                   */
/* ------------------------------------------------------------------------- */

/**
 * Binds a name to a contract type, so field names in a declaration are checked
 * against the real entity. Nesting is decomposed rather than pathed: a gap on
 * `DRep.registration.status` is declared on `Registration.status`.
 */
export interface CapabilityEntities {
  NetworkInfo: NetworkInfo;
  EpochSummary: EpochSummary;
  BlockSummary: BlockSummary;
  ProtocolParams: ProtocolParams;
  StakeDistribution: StakeDistribution;
  Treasury: Treasury;
  StakeBalance: StakeBalance;
  VotingPower: VotingPower;
  Account: Account;
  Delegation: Delegation;
  PoolDelegation: PoolDelegation;
  StakeRegistrationEvent: StakeRegistrationEvent;
  DelegationHistoryEvent: DelegationHistoryEvent;
  DRep: DRep;
  Registration: Registration;
  DRepActivity: DRepActivity;
  DRepDelegator: DRepDelegator;
  DRepHistoryEvent: DRepHistoryEvent;
  DRepVotingPowerEntry: DRepVotingPowerEntry;
  SpoVoter: SpoVoter;
  CommitteeMember: CommitteeMember;
  Committee: Committee;
  Constitution: Constitution;
  GovAction: GovAction;
  GovActionLifecycle: GovActionLifecycle;
  GovActionActivityEvent: GovActionActivityEvent;
  EnactedActionSummary: EnactedActionSummary;
  RoleTally: RoleTally;
  VoteRecord: VoteRecord;
  GovernanceMetrics: GovernanceMetrics;
  TransactionState: TransactionState;
  SurveyDefinition: SurveyDefinition;
}

export type EntityId = keyof CapabilityEntities;

/**
 * The `?` keys — the ones the contract says a provider may be unable to serve.
 */
export type OptionalFieldsOf<T> = {
  [K in keyof T]-?: undefined extends T[K] ? K : never;
}[keyof T] &
  string;

/**
 * The non-`?` keys. A provider cannot decline these; when it cannot compute
 * one it must say so through `unfillable` or `misreported` below, because
 * silently sending a plausible value is how a count-only tally rendered as
 * zero stake and a dead `utxo_view` rendered as 0 ada.
 */
export type RequiredFieldsOf<T> = {
  [K in keyof T]-?: undefined extends T[K] ? never : K;
}[keyof T] &
  string;

/* ------------------------------------------------------------------------- */
/* Field support                                                              */
/* ------------------------------------------------------------------------- */

/**
 * Two different facts about one field, which the old `route#field` key
 * collapsed into one:
 *
 *   - is it ever populated?          (`serves`)
 *   - is ASKING for it an error?     (`whenRequested`, on the `never` variant)
 *
 * db-sync forces the distinction: `dreps.list()` never inspects `q.expand`, so
 * a caller asking for `liveVotingPower` gets a 200 with the key missing, while
 * `dreps.get()` on the same provider throws for the same field. A consumer
 * building an `expand` array needs the second answer; a consumer deciding
 * whether to render a column needs the first.
 */
export type FieldSupport =
  /** Populated whenever a read covers the entity. */
  | { readonly serves: 'always' }
  /** Populated only when named in `expand`; requesting it is accepted. */
  | { readonly serves: 'onExpand' }
  /** Populated on some reads and not others; `note` says which. */
  | { readonly serves: 'conditional'; readonly note: string }
  /** Never populated. */
  | {
      readonly serves: 'never';
      readonly cause: Absence;
      /**
       * What happens if a caller names it in `expand`. `'ignored'` is the
       * silent-omission case that had no refusal site and so was invisible in
       * the old tables.
       */
      readonly whenRequested: 'throws' | 'ignored';
      readonly note: string;
    };

export function isPopulated(support: FieldSupport): boolean {
  return support.serves !== 'never';
}

/** Whether naming this field in `expand` is safe. */
export function isRequestable(support: FieldSupport): boolean {
  return support.serves !== 'never' || support.whenRequested === 'ignored';
}

/** Exhaustive over an entity's optional fields. */
export type EntityFieldTable<E extends EntityId> = {
  readonly [K in OptionalFieldsOf<CapabilityEntities[E]>]: FieldSupport;
};

/**
 * A required field the provider cannot compute at all. Naming them turns "the
 * dashboard route is unavailable" into "these six tiles are" — which is the
 * concrete unblock for Koios, whose six uncomputable `GovernanceMetrics`
 * counters are none of the three fields GovTool actually reads.
 */
export interface UnfillableField<E extends EntityId> {
  readonly field: RequiredFieldsOf<CapabilityEntities[E]>;
  readonly cause: Absence;
  readonly note: string;
}

/**
 * A required-nullable field the provider fills with a plausible falsehood
 * because the contract leaves it no choice. Distinct from `unfillable`: the
 * value IS sent and a consumer that trusts it is misled.
 *
 * Blockfrost reports `DRepActivity.votesCast` as 0, which reads as "never
 * voted"; it sends `null` for `Registration.deposit` and
 * `GovAction.previousAction` although neither is known to be absent.
 */
export interface MisreportedField<E extends EntityId> {
  readonly field: RequiredFieldsOf<CapabilityEntities[E]>;
  /** What the provider actually sends, e.g. `'0'`, `'null'`. */
  readonly sends: string;
  /** What it would mean if it were true, so a consumer knows what it is not. */
  readonly wouldMean: string;
  readonly note: string;
}

export interface EntityDeclaration<E extends EntityId> {
  readonly fields: EntityFieldTable<E>;
  readonly unfillable?: readonly UnfillableField<E>[];
  readonly misreported?: readonly MisreportedField<E>[];
}

export type EntityDeclarations = {
  readonly [E in EntityId]: EntityDeclaration<E>;
};

/**
 * A route-specific override of one field's support.
 *
 * Needed because a dataset groups several routes, and two routes reading one
 * dataset can genuinely disagree about a field: db-sync's `dreps.list` omits
 * `liveVotingPower` silently while `dreps.get` throws for it. Without this the
 * dataset would have to pick one answer and lie about the other route.
 */
export interface FieldOverride {
  readonly entity: EntityId;
  readonly field: string;
  readonly route: RouteId;
  readonly support: FieldSupport;
  readonly note: string;
}

/** Support for one field, applying any route-specific override. */
export function fieldSupportFor(
  entities: EntityDeclarations,
  entity: EntityId,
  field: string,
  route?: RouteId,
  overrides: readonly FieldOverride[] = [],
): FieldSupport | undefined {
  if (route !== undefined) {
    const override = overrides.find(
      (o) => o.entity === entity && o.field === field && o.route === route,
    );
    if (override !== undefined) {
      return override.support;
    }
  }
  const table = entities[entity].fields as Record<
    string,
    FieldSupport | undefined
  >;
  return table[field];
}
