/**
 * Chain Data API — how a provider DECLARES what it can serve.
 *
 * One `DatasetCapability` per dataset. Every field of it is a boolean, a closed
 * union from the contract, or an exhaustive record over a closed union — so a
 * declaration is plain JSON and nothing in it can be spelled two ways. The five
 * incompatible key dialects the old tables grew (`#field`, `{param}`,
 * `{param=value}`, `{param:value}`, `{expand:field}`) collapse into structure.
 *
 * `'partial'` does not exist. A dataset is `served`, `refused` or `missing`, and
 * a served dataset says exactly which fields, sort keys, filter values, bases,
 * expands and search modes it carries, what one call costs, and what the
 * numbers mean.
 */

import type { NetworkId, ProviderId, StakeBasis, Timestamp } from '../common';
import type { AccountExpand } from '../accounts';
import type { VoteChoice, VoterRole } from '../refs';
import type {
  DRepExpand,
  DRepKind,
  DRepSort,
  DRepStatus,
} from '../governance/dreps';
import type {
  GovActionExpand,
  GovActionSort,
  GovActionStatus,
  GovActionType,
} from '../governance/proposals';
import type { VoteExpand, VoteSort } from '../governance/votes';
import type {
  Absence,
  BatchSupport,
  Caveat,
  DeclarationScope,
  EnumSupport,
  FilterSupport,
  OptionSupport,
  PagingSupport,
  Reachability,
  SearchSupport,
} from './axes';
import type { DatasetId, RouteId, RoutesOf } from './datasets';
import { DATASET_IDS, DATASETS, datasetsForRoute } from './datasets';
import type { EntityDeclarations, FieldOverride } from './fields';

/* ------------------------------------------------------------------------- */
/* Which controls each dataset has, and over which contract union             */
/* ------------------------------------------------------------------------- */

/**
 * `accounts.listDelegationHistory`'s `kind` is an inline union in the contract
 * with no name. Naming it is a prerequisite for declaring it.
 */
export type DelegationHistoryKind = 'governance' | 'pool';

/** The sort union each sortable dataset uses. */
export interface DatasetSorts {
  'drep.identity.current': DRepSort;
  'proposal.identity.current': GovActionSort;
  'proposal.ballot.current': VoteSort;
  'drep.ballot.current': VoteSort;
  'pool.ballot.current': VoteSort;
  'vote.ballot.current': VoteSort;
}

/** The filter parameters each dataset accepts, and the union behind each. */
export interface DatasetFilters {
  'drep.identity.current': { status: DRepStatus; kind: DRepKind };
  'proposal.identity.current': { type: GovActionType; status: GovActionStatus };
  'proposal.body.current': { type: GovActionType };
  'proposal.outcome.current': { type: GovActionType };
  'proposal.tally.current': { role: VoterRole };
  'proposal.ballot.current': {
    vote: VoteChoice;
    role: VoterRole;
    proposalType: GovActionType;
  };
  'drep.ballot.current': { vote: VoteChoice; proposalType: GovActionType };
  'pool.ballot.current': { vote: VoteChoice; proposalType: GovActionType };
  'vote.ballot.current': {
    vote: VoteChoice;
    role: VoterRole;
    proposalType: GovActionType;
  };
  'account.delegation.events': { kind: DelegationHistoryKind };
  'voter.identity.list': { role: VoterRole };
}

/** The `expand` union each dataset's routes accept. */
export interface DatasetExpands {
  'account.identity.current': AccountExpand;
  'drep.identity.current': DRepExpand;
  'proposal.identity.current': GovActionExpand;
  'proposal.ballot.current': VoteExpand;
  'drep.ballot.current': VoteExpand;
  'pool.ballot.current': VoteExpand;
  'vote.ballot.current': VoteExpand;
}

/**
 * Cross-entity joins, named ONCE. `proposals.list{voterId}`,
 * `proposals.get{voterId}` and `expand: 'myVote'` are the same capability —
 * "have I voted on this?" — spelled three ways in the old tables, twice in one
 * Koios file. The frontend feature is one badge and needs one name.
 */
export interface DatasetJoins {
  'proposal.identity.current': { callerVote: OptionSupport };
  'vote.ballot.current': {
    voterOfVote: OptionSupport;
    proposalOfVote: OptionSupport;
  };
}

export type SortOf<D extends DatasetId> = D extends keyof DatasetSorts
  ? DatasetSorts[D]
  : never;

export type FiltersOf<D extends DatasetId> = D extends keyof DatasetFilters
  ? DatasetFilters[D]
  : Record<never, never>;

export type ExpandOf<D extends DatasetId> = D extends keyof DatasetExpands
  ? DatasetExpands[D]
  : never;

export type JoinsOf<D extends DatasetId> = D extends keyof DatasetJoins
  ? DatasetJoins[D]
  : Record<never, never>;

/* ------------------------------------------------------------------------- */
/* Unavailability                                                             */
/* ------------------------------------------------------------------------- */

interface UnavailabilityBase {
  /**
   * Where the limit lives. A `deployment` gap may be fixed by an operator; a
   * `source` gap needs a different provider; a `contract` gap needs a spec
   * change. Never bake a `deployment` gap into a compile-time constant.
   */
  readonly scope: DeclarationScope;
  /**
   * Required — always. db-sync's `unsupported(route)` takes no reason at all
   * and six Blockfrost sites omit it, which is why nobody could tell a data gap
   * from an unimplemented one when reading the table.
   *
   * Diagnostic prose: it belongs in logs and support bundles, never rendered to
   * a user. A UI renders from `Absence` and `DeclarationScope`.
   */
  readonly reason: string;
}

/**
 * Why a dataset is not served. The `tooExpensive` variant CANNOT COMPILE
 * without naming a fallback, because a cost refusal always has a cheaper path
 * ("use listDelegators", "pass explicit ids", "use getAvailable()") where a
 * data gap never does — and the consumer's correct reaction differs.
 */
export type Unavailability =
  | (UnavailabilityBase & { readonly kind: 'notInSource' })
  | (UnavailabilityBase & { readonly kind: 'noIndex' })
  | (UnavailabilityBase & {
      readonly kind: 'representation';
      readonly caveat: Caveat;
    })
  | (UnavailabilityBase & {
      readonly kind: 'tooExpensive';
      /** Typed, so a fallback that does not exist does not compile. */
      readonly fallback: DatasetId;
      /** An out-of-contract method serving it, if that is the cheaper path. */
      readonly fallbackMethod?: string;
    })
  | (UnavailabilityBase & { readonly kind: 'notImplemented' })
  | (UnavailabilityBase & {
      readonly kind: 'deploymentFault';
      readonly scope: 'deployment';
      readonly symptom: string;
      readonly observedAt?: Timestamp;
    });

/** The `Absence` a given unavailability maps to, for uniform consumer logic. */
export function absenceOf(u: Unavailability): Absence {
  switch (u.kind) {
    case 'notInSource':
      return 'notInSource';
    case 'noIndex':
      return 'noIndex';
    case 'representation':
      return 'representation';
    case 'tooExpensive':
      return 'tooExpensive';
    case 'notImplemented':
      return 'notImplemented';
    case 'deploymentFault':
      return 'deploymentFault';
  }
}

/* ------------------------------------------------------------------------- */
/* The declaration                                                            */
/* ------------------------------------------------------------------------- */

export interface RefusedRoute<D extends DatasetId = DatasetId> {
  readonly route: RoutesOf<D>;
  readonly unavailable: Unavailability;
}

export interface DatasetCapability<D extends DatasetId = DatasetId> {
  readonly reachability: Reachability;
  /** Present whenever `reachability !== 'served'`. */
  readonly unavailable?: Unavailability;

  /**
   * Whether a consumer may put this on a refresh interval.
   *
   * Support is binary — a provider that cannot afford a read declares it
   * `refused` with `kind: 'tooExpensive'` rather than serving it with a
   * warning label. This is the one operational fact that survives that rule,
   * because it is not a statement about support: a read can be served, cheap,
   * and still be something a provider does not want on a 20-second interval
   * (Koios rate-limits; Blockfrost has a request budget).
   */
  readonly pollable: boolean;

  /**
   * Routes of this dataset that are nonetheless refused, each with its own
   * reason. The escape hatch for one dataset reachable two ways at different
   * completeness: Koios answers seven `GovernanceMetrics` counters cheaply but
   * not the whole record `metrics.get` promises, so the dataset is served —
   * through an extension — while that one route is not.
   *
   * `route` is typed to the dataset's own routes, so it cannot name an
   * unrelated one, and the reason is required for the same reason it is on a
   * refused dataset: a consumer that hits this needs to know whether to try
   * something cheaper or give up.
   */
  readonly refusedRoutes?: readonly RefusedRoute<D>[];

  readonly basis?: EnumSupport<StakeBasis>;
  readonly sort?: EnumSupport<SortOf<D>>;
  readonly filters?: {
    readonly [N in keyof FiltersOf<D>]?: FilterSupport<
      Extract<FiltersOf<D>[N], string>
    >;
  };
  readonly expand?: EnumSupport<ExpandOf<D>>;
  readonly joins?: { readonly [J in keyof JoinsOf<D>]?: OptionSupport };
  readonly search?: SearchSupport;
  readonly paging?: PagingSupport;
  readonly batch?: BatchSupport;

  /**
   * What the served answer MEANS. Non-empty by construction: `caveats: []` is
   * not a qualification, it is noise that reads as one.
   */
  readonly caveats?: readonly [Caveat, ...Caveat[]];
}

export type CapabilityTable = {
  readonly [D in DatasetId]: DatasetCapability<D>;
};

/**
 * A declaration read WITHOUT knowing which dataset it belongs to.
 *
 * `DatasetCapability<D>` is precise at the point of declaration — the sort keys
 * of `drep.identity.current` are `DRepSort` and nothing else — which is what
 * makes a provider's constant type-check. But a reader iterating every dataset
 * holds `DatasetId`, and there is no single precise type for "the sort record of
 * an unknown dataset": one dataset's is `EnumSupport<never>` and another's is
 * `EnumSupport<DRepSort>`.
 *
 * So reading widens, exactly once, here. Every consumer in this package and in
 * the backend goes through `readCapability`, and the cast lives in one function
 * with this comment on it rather than being sprinkled through the readers.
 */
export interface AnyDatasetCapability {
  readonly reachability: Reachability;
  readonly unavailable?: Unavailability;
  readonly pollable: boolean;
  readonly refusedRoutes?: readonly RefusedRoute[];
  readonly basis?: Readonly<Partial<Record<StakeBasis, OptionSupport>>>;
  readonly sort?: Readonly<Record<string, OptionSupport>>;
  readonly filters?: Readonly<
    Record<string, FilterSupport<string> | undefined>
  >;
  readonly expand?: Readonly<Record<string, OptionSupport>>;
  readonly joins?: Readonly<Record<string, OptionSupport>>;
  readonly search?: SearchSupport;
  readonly paging?: PagingSupport;
  readonly batch?: BatchSupport;
  readonly caveats?: readonly [Caveat, ...Caveat[]];
}

/** The one place a `DatasetCapability<D>` is widened for reading. */
export function readCapability(
  table: CapabilityTable,
  dataset: DatasetId,
): AnyDatasetCapability {
  return table[dataset] as unknown as AnyDatasetCapability;
}

/**
 * A runtime demotion. The static table says what the CODE can do; an override
 * says what this BOX is doing now — a 500 on one endpoint, a missing db-sync
 * view. Applied by `resolveCapabilities`, never merged into the constant, so a
 * fault that clears does not need a release.
 */
export interface CapabilityOverride {
  readonly dataset: DatasetId;
  readonly reachability: Reachability;
  readonly unavailable: Unavailability & { readonly kind: 'deploymentFault' };
}

/**
 * Something a provider serves that the contract does not define. Koios'
 * `metrics.getAvailable(): Partial<GovernanceMetrics>` exists because the
 * alternative was refusing a 13-field record over six fields nobody reads.
 * Declaring it is better than hiding it on a concrete class — a model that can
 * only subtract cannot express a provider serving MORE than the contract.
 */
export interface CapabilityExtension {
  readonly dataset: DatasetId;
  readonly method: string;
  readonly returns: 'partialRecord' | 'other';
  readonly description: string;
}

/**
 * Metadata is produced by the Metadata Service, not by a chain-data provider,
 * but provider capability still differs — Koios resolves and validates it
 * itself, which is a gain. Its own axis, so it is not mis-attributed to a route.
 */
export interface MetadataCapability {
  readonly resolvedBy: 'provider' | 'metadataService' | 'none';
  readonly validatesAgainstStandard: boolean;
  readonly carriesFailureMessage: boolean;
}

/** What `SystemApi.getCapabilities()` returns. */
export interface ProviderCapabilityDocument {
  readonly schemaVersion: 2;
  readonly provider: ProviderId;
  readonly network: NetworkId;
  readonly providerVersion: string;
  readonly generatedAt: Timestamp;
  readonly datasets: CapabilityTable;
  readonly entities: EntityDeclarations;
  readonly fieldOverrides: readonly FieldOverride[];
  /**
   * Datasets nobody has assessed yet. They resolve to `refused /
   * notImplemented` exactly like an undeclared one, but they say so out loud,
   * so "we have not looked at this" is a reviewable diff rather than an
   * invisible under-claim. Adding a dataset to the registry lands here.
   */
  readonly unreviewed: readonly DatasetId[];
  readonly overrides: readonly CapabilityOverride[];
  readonly extensions: readonly CapabilityExtension[];
  readonly metadata: MetadataCapability;
  /**
   * Set when this document was composed rather than declared. The GovTool
   * backend emits its OWN: it RAISES capability (it reads the whole DRep
   * directory and sorts, filters and pages it in memory, so Koios refusing
   * every `DRepSort` key must not disable the UI sort) and LOWERS it (it
   * catches and returns 0 for a wallet's voting power). Proxying a provider's
   * document to the browser would disable working features and enable broken
   * ones.
   */
  readonly composedFrom?: readonly {
    readonly provider: ProviderId;
    readonly network: NetworkId;
  }[];
}

/* ------------------------------------------------------------------------- */
/* Declaring                                                                  */
/* ------------------------------------------------------------------------- */

/** The honest default: nothing is served until somebody says it is. */
export const NOT_DECLARED: AnyDatasetCapability = {
  reachability: 'refused',
  pollable: false,
  unavailable: {
    kind: 'notImplemented',
    scope: 'source',
    reason: 'Not declared by this provider.',
  },
};

/**
 * Build the exhaustive table from the datasets a provider actually assessed.
 *
 * Partial on purpose: three providers are already shipping and a migration that
 * demands 41 correct entries in one commit lands as a freeze. Everything
 * unnamed is `refused / notImplemented`, so the table under-claims rather than
 * over-claims, and a fourth provider (Kupo) starts at `declareCapabilities({},
 * DATASET_IDS)` — one line, everything honestly refused.
 *
 * `unreviewed` is required rather than inferred so the under-claim is visible.
 */
export function declareCapabilities(
  served: Partial<CapabilityTable>,
  unreviewed: readonly DatasetId[],
): CapabilityTable {
  const table: Record<string, AnyDatasetCapability> = {};
  for (const id of DATASET_IDS) {
    const declared = served[id] as AnyDatasetCapability | undefined;
    table[id] =
      declared ??
      (unreviewed.includes(id)
        ? {
            ...NOT_DECLARED,
            unavailable: {
              kind: 'notImplemented',
              scope: 'source',
              reason: 'Not yet assessed for this provider.',
            },
          }
        : NOT_DECLARED);
  }
  return table as unknown as CapabilityTable;
}

/* ------------------------------------------------------------------------- */
/* Reading                                                                    */
/* ------------------------------------------------------------------------- */

/** Apply deployment overrides. The result is what a consumer gates on. */
export function resolveCapabilities(
  doc: ProviderCapabilityDocument,
): CapabilityTable {
  const resolved = { ...doc.datasets } as unknown as Record<
    string,
    AnyDatasetCapability
  >;
  for (const override of doc.overrides) {
    resolved[override.dataset] = {
      ...readCapability(doc.datasets, override.dataset),
      reachability: override.reachability,
      unavailable: override.unavailable,
    };
  }
  return resolved as unknown as CapabilityTable;
}

export function isServed(table: CapabilityTable, dataset: DatasetId): boolean {
  return table[dataset].reachability === 'served';
}

/** A route is callable only when every dataset it reads is served. */
export function isRouteCallable(
  table: CapabilityTable,
  route: RouteId,
): boolean {
  return datasetsForRoute(route).every((id) => {
    const cap = readCapability(table, id);
    return (
      cap.reachability === 'served' &&
      !(cap.refusedRoutes ?? []).some((refused) => refused.route === route)
    );
  });
}

/**
 * Compose two capability tables: a patch that a layer in front of the provider
 * applies because it supplies the capability itself.
 *
 * This exists because the GovTool backend genuinely does raise capability, and
 * a design that proxies the provider's table would disable a working sort. It
 * is an explicit, reviewable object rather than logic buried in a service, and
 * the conformance suite can assert that everything it claims is also asserted
 * by a test of the layer that claims it.
 */
export function composeCapabilities(
  base: CapabilityTable,
  patch: Readonly<Partial<Record<DatasetId, Partial<AnyDatasetCapability>>>>,
): CapabilityTable {
  const composed = { ...base } as unknown as Record<
    string,
    AnyDatasetCapability
  >;
  for (const id of DATASET_IDS) {
    const delta = patch[id];
    if (delta !== undefined) {
      composed[id] = { ...readCapability(base, id), ...delta };
    }
  }
  return composed as unknown as CapabilityTable;
}

/* ------------------------------------------------------------------------- */
/* The runtime dual — the error carries the same typed values                 */
/* ------------------------------------------------------------------------- */

/**
 * What the caller asked for that was refused, typed at construction. The key in
 * the thrown error and the key in the declaration are then the SAME value and
 * can be cross-checked mechanically — which is what the 47 refusal keys
 * matching no table entry, and the 16 table entries matching no refusal site,
 * needed and never had.
 */
export type ControlRef<D extends DatasetId = DatasetId> =
  | { readonly kind: 'dataset' }
  | { readonly kind: 'route'; readonly route: RoutesOf<D> }
  | { readonly kind: 'sortControl' }
  | { readonly kind: 'sort'; readonly key: SortOf<D> }
  | {
      readonly kind: 'filter';
      readonly name: keyof FiltersOf<D> & string;
      readonly value: string;
    }
  | {
      readonly kind: 'filterCardinality';
      readonly name: keyof FiltersOf<D> & string;
      readonly max: number;
    }
  | { readonly kind: 'expand'; readonly field: ExpandOf<D> }
  | { readonly kind: 'field'; readonly entity: string; readonly field: string }
  | { readonly kind: 'basis'; readonly basis: StakeBasis }
  | {
      readonly kind: 'search';
      readonly mode: 'freeText' | 'adaHandle' | 'exactId';
    }
  | { readonly kind: 'join'; readonly join: keyof JoinsOf<D> & string }
  | {
      readonly kind: 'paging';
      readonly option: 'offset' | 'cursor' | 'unboundedLimit';
    }
  | { readonly kind: 'batch'; readonly form: 'all' | 'byIds' };

/** The `details` payload of a `CAPABILITY_UNSUPPORTED` error. */
export interface CapabilityRefusal<D extends DatasetId = DatasetId> {
  readonly dataset: D;
  readonly control: ControlRef<D>;
  readonly cause: Absence;
  readonly scope: DeclarationScope;
  readonly reason: string;
  readonly fallback?: DatasetId;
}

/**
 * The wire form: a union over datasets rather than one shape with the union
 * substituted, so `dataset` and `control` stay correlated.
 */
export type AnyCapabilityRefusal = {
  [D in DatasetId]: CapabilityRefusal<D>;
}[DatasetId];

/** Stable, parseable, identical on every provider. For logs and metric labels. */
export function refusalKey(refusal: AnyCapabilityRefusal): string {
  const c = refusal.control;
  switch (c.kind) {
    case 'dataset':
      return refusal.dataset;
    case 'route':
      return `${refusal.dataset}@${String(c.route)}`;
    case 'sortControl':
      return `${refusal.dataset}#sort`;
    case 'sort':
      return `${refusal.dataset}#sort=${String(c.key)}`;
    case 'filter':
      return `${refusal.dataset}#filter:${c.name}=${c.value}`;
    case 'filterCardinality':
      return `${refusal.dataset}#filter:${c.name}<=${String(c.max)}`;
    case 'expand':
      return `${refusal.dataset}#expand=${String(c.field)}`;
    case 'field':
      return `${refusal.dataset}#field:${c.entity}.${c.field}`;
    case 'basis':
      return `${refusal.dataset}#basis=${c.basis}`;
    case 'search':
      return `${refusal.dataset}#search=${c.mode}`;
    case 'join':
      return `${refusal.dataset}#join=${c.join}`;
    case 'paging':
      return `${refusal.dataset}#paging=${c.option}`;
    case 'batch':
      return `${refusal.dataset}#batch=${c.form}`;
  }
}

/**
 * Does the declaration already predict this refusal?
 *
 * Run over every `unsupported(...)` site in a provider package, this is the
 * cross-check that previously existed only as a hand-run script. Every control
 * kind is covered — a `default: return false` would report a correctly declared
 * filter-cardinality or field refusal as drift, which is worse than not
 * checking, because it trains people to ignore the check.
 */
export function refusalIsDeclared(
  table: CapabilityTable,
  entities: EntityDeclarations,
  refusal: AnyCapabilityRefusal,
): boolean {
  const cap = readCapability(table, refusal.dataset);

  // A refused or missing dataset predicts every refusal under it.
  if (cap.reachability !== 'served') {
    return true;
  }

  const c = refusal.control;
  const rejects = (support: OptionSupport | undefined): boolean =>
    support === undefined || support === 'rejected';

  switch (c.kind) {
    case 'dataset':
      // The dataset is served, so a whole-dataset refusal contradicts it.
      return false;
    case 'route':
      return (cap.refusedRoutes ?? []).some(
        (refused) => String(refused.route) === String(c.route),
      );
    case 'sortControl':
      return (
        cap.sort === undefined ||
        Object.values(cap.sort).every(
          (support) => support === 'rejected' || support === 'ignored',
        )
      );
    case 'sort':
      return rejects(cap.sort?.[String(c.key)]);
    case 'filter':
      return rejects(cap.filters?.[c.name]?.values[c.value]);
    case 'filterCardinality': {
      const max = cap.filters?.[c.name]?.maxSelected;
      return max !== undefined && max <= c.max;
    }
    case 'expand':
      return rejects(cap.expand?.[String(c.field)]);
    case 'field': {
      const declarations = entities as Record<
        string,
        { fields: Record<string, { serves: string } | undefined> } | undefined
      >;
      const support = declarations[c.entity]?.fields[c.field];
      return support !== undefined && support.serves === 'never';
    }
    case 'basis':
      return rejects(cap.basis?.[c.basis]);
    case 'search':
      return rejects(cap.search?.modes[c.mode]);
    case 'join':
      return rejects(cap.joins?.[c.join]);
    case 'paging': {
      if (cap.paging === undefined) {
        return true;
      }
      if (c.option === 'offset') {
        return cap.paging.offset === 'rejected';
      }
      if (c.option === 'cursor') {
        return cap.paging.cursor === 'rejected';
      }
      return cap.paging.omittedLimitMeans !== 'everything';
    }
    case 'batch': {
      if (cap.batch === undefined) {
        return true;
      }
      return c.form === 'all'
        ? rejects(cap.batch.allIds)
        : rejects(cap.batch.explicitIds);
    }
  }
}

/**
 * Declaration problems the type system cannot catch. Run in every provider's
 * test suite; an empty result is the gate.
 */
export function declarationProblems(doc: ProviderCapabilityDocument): string[] {
  const problems: string[] = [];

  for (const id of DATASET_IDS) {
    const cap = readCapability(doc.datasets, id);

    if (cap.reachability === 'served') {
      if (cap.unavailable !== undefined) {
        problems.push(`${id}: served but carries an unavailability`);
      }
      if (DATASETS[id].hasKnownSource === false) {
        problems.push(
          `${id}: served, but the registry says no known source records it — ` +
            'update DATASETS.hasKnownSource rather than declaring around it',
        );
      }
    } else if (cap.unavailable === undefined) {
      problems.push(`${id}: not served and does not say why`);
    } else if (
      cap.unavailable.kind === 'deploymentFault' &&
      doc.overrides.every((o) => o.dataset !== id)
    ) {
      problems.push(
        `${id}: a deployment fault is declared in the static table; it belongs ` +
          'in overrides, so it can clear without a release',
      );
    }

    for (const refused of cap.refusedRoutes ?? []) {
      if (
        !(DATASETS[id].routes as readonly string[]).includes(
          String(refused.route),
        )
      ) {
        problems.push(
          `${id}: refusedRoutes names ${String(refused.route)}, not a route of it`,
        );
      }
    }

    if (doc.unreviewed.includes(id) && cap.reachability === 'served') {
      problems.push(`${id}: listed as unreviewed but declared served`);
    }
  }

  return problems;
}
