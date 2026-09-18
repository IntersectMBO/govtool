/**
 * Chain Data API — route identity and the dataset registry.
 *
 * Two things live here, and the second is derived from the first:
 *
 *   `RouteId`   every method of `ChainDataApiV1`, COMPUTED from the interface
 *               tree rather than typed by hand. Rename `listDelegationEvents`
 *               and every declaration citing it stops compiling.
 *
 *   `DATASETS`  the closed space a provider declares into. Each entry is a
 *               point in the (subject × facet × temporality) space of ./axes
 *               and carries the routes that read it, so routes are *derived*:
 *               a provider never spells a route in its declaration.
 *
 * `DatasetId` is `keyof typeof DATASETS`, so a provider physically cannot name
 * a dataset that does not exist. Adding one here is what makes it declarable —
 * never a string literal in a provider package.
 */

import type { AccountsApi } from '../accounts';
import type { ChainDataApiV1 } from '../index';
import type { GovernanceApi } from '../governance';
import type { NetworkApi } from '../network';
import type { SurveysApi } from '../surveys';
import type { TransactionsApi } from '../transactions';
import type { Cardinality, Facet, Subject, Temporality } from './axes';

/* ------------------------------------------------------------------------- */
/* Route identity, derived from the interface                                 */
/* ------------------------------------------------------------------------- */

/**
 * `ChainDataApiV1` minus `system`, with `surveys` REQUIRED.
 *
 * `system.getCapabilities` / `system.getHealth` are how a consumer reads this
 * document; they are not capability-gated routes and would be circular here.
 * `surveys` is required because a namespace being absent is a capability fact
 * (`Reachability: 'missing'`) that needs a key, not a reason to have no key.
 */
interface RoutedSurface {
  network: NetworkApi;
  accounts: AccountsApi;
  governance: GovernanceApi;
  transactions: TransactionsApi;
  surveys: SurveysApi;
}

type AnyMethod = (...args: never[]) => unknown;

type RouteIdsOf<T, Prefix extends string = ''> = {
  [K in keyof T & string]: NonNullable<T[K]> extends AnyMethod
    ? `${Prefix}${K}`
    : NonNullable<T[K]> extends object
      ? RouteIdsOf<NonNullable<T[K]>, `${Prefix}${K}.`>
      : never;
}[keyof T & string];

/**
 * Every route in the contract, as a literal union — 40 members today, none of
 * them hand-written. `governance.dreps.listDelegationEvents` exists here
 * because `DRepsApi.listDelegationEvents` exists.
 */
export type RouteId = RouteIdsOf<RoutedSurface>;

type Assert<T extends true> = T;
type SameKeys<A, B> = [keyof A] extends [keyof B]
  ? [keyof B] extends [keyof A]
    ? true
    : false
  : false;

/**
 * Fails to compile if `ChainDataApiV1` gains or loses a namespace, which is the
 * one thing `RoutedSurface` cannot notice on its own.
 */
export type NamespacesAreInSync = Assert<
  SameKeys<RoutedSurface, Omit<ChainDataApiV1, 'system'>>
>;

/* ------------------------------------------------------------------------- */
/* Dataset registry                                                            */
/* ------------------------------------------------------------------------- */

export interface DatasetShape {
  readonly subject: Subject;
  readonly facet: Facet;
  readonly temporality: Temporality;
  readonly cardinality: Cardinality;
  /** Routes that read this dataset. Several routes may share one. */
  readonly routes: readonly RouteId[];
  /**
   * Which component owns the data. `metadata` datasets are produced by the
   * Metadata Service — the Chain Data API never fetches metadata — so a
   * chain-data provider declaring them as its own would be lying. Provider
   * capability still differs (Koios resolves and validates metadata itself,
   * which is a gain), hence the datasets exist.
   */
  readonly servedBy: 'chainData' | 'metadataService';
  /**
   * `false` when no known source records this at all. A gap is only structural
   * once a source that ought to have the data also lacks it, and a feature
   * behind a universally absent dataset should be DELETED rather than shipped
   * behind a toggle that is off on every provider forever.
   *
   * This is a property of the DATA and therefore belongs in the shared
   * registry, not on each provider's declaration: three providers must not be
   * able to disagree about whether a gap is universal, or the frontend's
   * "permanent vs provider" answer would depend on which one is configured.
   */
  readonly hasKnownSource: boolean;
  /** One line, for a capability report. */
  readonly note: string;
}

const shape = <const T extends Readonly<Record<string, DatasetShape>>>(
  t: T,
): T => t;

export const DATASETS = shape({
  /* ---- network ---------------------------------------------------------- */
  'network.identity.current': {
    subject: 'network',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'one',
    routes: ['network.getNetworkInfo'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'network id, era, tip and current epoch',
  },
  'network.chain.series': {
    subject: 'network',
    facet: 'chain',
    temporality: 'series',
    cardinality: 'many',
    routes: ['network.listEpochs', 'network.listBlocks'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'epoch and block listings',
  },
  'network.params.current': {
    subject: 'network',
    facet: 'params',
    temporality: 'current',
    cardinality: 'one',
    routes: ['network.getProtocolParams'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'protocol parameters in force',
  },
  'network.params.asAt': {
    subject: 'network',
    facet: 'params',
    temporality: 'asAt',
    cardinality: 'one',
    routes: ['network.getProtocolParams'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'protocol parameters for a named past epoch',
  },
  'network.stake.current': {
    subject: 'network',
    facet: 'stake',
    temporality: 'current',
    cardinality: 'one',
    routes: ['network.getStakeDistribution'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'stake totals, including the four governance denominators',
  },
  'network.treasury.current': {
    subject: 'network',
    facet: 'treasury',
    temporality: 'current',
    cardinality: 'one',
    routes: ['network.getTreasury'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'treasury and reserves',
  },
  'network.aggregate.current': {
    subject: 'network',
    facet: 'aggregate',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.metrics.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'GovernanceMetrics — 13 counters of wildly different cost',
  },

  /* ---- account ---------------------------------------------------------- */
  'account.identity.current': {
    subject: 'account',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'one',
    routes: ['accounts.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'stake account registration state and balance',
  },
  'account.stake.current': {
    subject: 'account',
    facet: 'stake',
    temporality: 'current',
    cardinality: 'one',
    routes: ['accounts.getVotingPower'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "the connected wallet's own voting power",
  },
  'account.stake.asAt': {
    subject: 'account',
    facet: 'stake',
    temporality: 'asAt',
    cardinality: 'one',
    routes: ['accounts.getVotingPower'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'account voting power at a named past epoch',
  },
  'account.delegation.current': {
    subject: 'account',
    facet: 'delegation',
    temporality: 'current',
    cardinality: 'one',
    routes: ['accounts.getDelegation'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'which DRep this account currently delegates to',
  },
  'account.delegation.events': {
    subject: 'account',
    facet: 'delegation',
    temporality: 'events',
    cardinality: 'many',
    routes: ['accounts.listDelegationHistory'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'governance and pool delegation history for one account',
  },
  'account.registration.events': {
    subject: 'account',
    facet: 'registration',
    temporality: 'events',
    cardinality: 'many',
    routes: ['accounts.listStakeEvents'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'stake key registration / deregistration certificates',
  },

  /* ---- drep ------------------------------------------------------------- */
  'drep.identity.current': {
    subject: 'drep',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.dreps.list', 'governance.dreps.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the DRep directory and the single-DRep read',
  },
  'drep.registration.current': {
    subject: 'drep',
    facet: 'registration',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.dreps.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'active / inactive / retired classification',
  },
  'drep.registration.events': {
    subject: 'drep',
    facet: 'registration',
    temporality: 'events',
    cardinality: 'many',
    routes: ['governance.dreps.listHistory'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'registration, update and retirement certificates',
  },
  'drep.stake.current': {
    subject: 'drep',
    facet: 'stake',
    temporality: 'current',
    cardinality: 'many',
    routes: [
      'governance.dreps.getVotingPower',
      'governance.dreps.getVotingPowers',
    ],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'DRep voting power, single and batch',
  },
  'drep.stake.series': {
    subject: 'drep',
    facet: 'stake',
    temporality: 'series',
    cardinality: 'many',
    routes: ['governance.dreps.getVotingPower'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'voting power per epoch over a range — the chart, not the number',
  },
  'drep.delegation.current': {
    subject: 'drep',
    facet: 'delegation',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.dreps.listDelegators'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "a DRep's current delegators",
  },
  'drep.delegation.events': {
    subject: 'drep',
    facet: 'delegation',
    temporality: 'events',
    cardinality: 'many',
    routes: ['governance.dreps.listDelegationEvents'],
    servedBy: 'chainData',
    hasKnownSource: false,
    note: 'join / leave timeline. No surveyed source records the transitions.',
  },
  'drep.ballot.current': {
    subject: 'drep',
    facet: 'ballot',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.dreps.listVotes'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "a DRep's own voting record, joined to the actions",
  },
  'drep.aggregate.current': {
    subject: 'drep',
    facet: 'aggregate',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.dreps.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'DRepActivity counters — votesCast, notVoted, participation',
  },

  /* ---- pool ------------------------------------------------------------- */
  'pool.identity.current': {
    subject: 'pool',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.pools.list', 'governance.pools.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'stake pools as governance voters',
  },
  'pool.ballot.current': {
    subject: 'pool',
    facet: 'ballot',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.pools.listVotes'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "an SPO's voting record",
  },

  /* ---- committee & constitution ----------------------------------------- */
  'committee.identity.current': {
    subject: 'committee',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'many',
    routes: [
      'governance.committee.getCommittee',
      'governance.committee.getMember',
    ],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'constitutional committee membership and quorum',
  },
  'constitution.identity.current': {
    subject: 'constitution',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.committee.getConstitution'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the constitution in force',
  },
  'constitution.identity.events': {
    subject: 'constitution',
    facet: 'identity',
    temporality: 'events',
    cardinality: 'many',
    routes: ['governance.committee.listConstitutionHistory'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'successive constitutions',
  },

  /* ---- proposal --------------------------------------------------------- */
  'proposal.identity.current': {
    subject: 'proposal',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'many',
    routes: [
      'governance.proposals.list',
      'governance.proposals.get',
      'governance.proposals.listByTx',
    ],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the governance action listing and single read',
  },
  'proposal.body.current': {
    subject: 'proposal',
    facet: 'body',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.proposals.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the typed GovActionBody, per action type',
  },
  'proposal.tally.current': {
    subject: 'proposal',
    facet: 'tally',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.proposals.getTallies'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'per-role vote totals, thresholds and denominators',
  },
  'proposal.ballot.current': {
    subject: 'proposal',
    facet: 'ballot',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.proposals.listVotes'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "an action's individual votes — the 'who voted' list",
  },
  'proposal.identity.events': {
    subject: 'proposal',
    facet: 'identity',
    temporality: 'events',
    cardinality: 'many',
    routes: ['governance.proposals.listActivity'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: "an action's lifecycle feed: submitted, voted, ratified, enacted",
  },
  'proposal.outcome.current': {
    subject: 'proposal',
    facet: 'outcome',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.proposals.getEnacted'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the currently enacted action of a given type',
  },

  /* ---- vote ------------------------------------------------------------- */
  'vote.ballot.current': {
    subject: 'vote',
    facet: 'ballot',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.votes.list', 'governance.votes.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'the cross-cutting vote feed and the single-vote read',
  },

  /* ---- voter directory --------------------------------------------------- */
  'voter.identity.current': {
    subject: 'voter',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.voters.resolve'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'role-agnostic resolution of one voter id',
  },
  'voter.identity.list': {
    subject: 'voter',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'many',
    routes: ['governance.voters.list'],
    servedBy: 'chainData',
    hasKnownSource: false,
    note: 'a role-agnostic voter directory. Every source indexes DReps, pools and the committee separately; none has the union.',
  },

  /* ---- transaction & survey ---------------------------------------------- */
  'transaction.identity.current': {
    subject: 'transaction',
    facet: 'identity',
    temporality: 'current',
    cardinality: 'one',
    routes: ['transactions.get'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'post-submission confirmation polling',
  },
  'survey.body.current': {
    subject: 'survey',
    facet: 'body',
    temporality: 'current',
    cardinality: 'one',
    routes: ['surveys.getDefinition'],
    servedBy: 'chainData',
    hasKnownSource: true,
    note: 'CIP-179 label-17 payload as CBOR hex',
  },

  /* ---- metadata (a DIFFERENT component) ----------------------------------- */
  'drep.metadata.current': {
    subject: 'drep',
    facet: 'metadata',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.dreps.get'],
    servedBy: 'metadataService',
    hasKnownSource: true,
    note: 'CIP-119 profile projection',
  },
  'proposal.metadata.current': {
    subject: 'proposal',
    facet: 'metadata',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.proposals.get'],
    servedBy: 'metadataService',
    hasKnownSource: true,
    note: 'CIP-108 action metadata projection',
  },
  'vote.metadata.current': {
    subject: 'vote',
    facet: 'metadata',
    temporality: 'current',
    cardinality: 'one',
    routes: ['governance.votes.get'],
    servedBy: 'metadataService',
    hasKnownSource: true,
    note: 'CIP-100 vote rationale projection',
  },
});

/** Every dataset a provider may declare. Closed by construction. */
export type DatasetId = keyof typeof DATASETS;

export const DATASET_IDS = Object.keys(DATASETS) as readonly DatasetId[];

/** The routes of one dataset, as a literal union — so `refusedRoutes` on a
 * declaration cannot name a route that dataset does not read. */
export type RoutesOf<D extends DatasetId> =
  (typeof DATASETS)[D]['routes'][number];

/**
 * Every route named by at least one dataset. The assertion below fails to
 * compile if a route in the contract has no dataset, which is what keeps the
 * registry from falling behind the interface.
 */
type CoveredRoute = (typeof DATASETS)[DatasetId]['routes'][number];
export type EveryRouteHasADataset = Assert<
  RouteId extends CoveredRoute ? true : false
>;

export function routesFor(dataset: DatasetId): readonly RouteId[] {
  return DATASETS[dataset].routes;
}

/** Every dataset a route touches. A route is callable only if all of them are. */
export function datasetsForRoute(route: RouteId): readonly DatasetId[] {
  return DATASET_IDS.filter((id) =>
    (routesFor(id) as readonly string[]).includes(route),
  );
}

/** Datasets matching a partial point in the axis space. */
export function selectDatasets(where: {
  subject?: Subject;
  facet?: Facet;
  temporality?: Temporality;
}): readonly DatasetId[] {
  return DATASET_IDS.filter((id) => {
    const d = DATASETS[id];
    return (
      (where.subject === undefined || d.subject === where.subject) &&
      (where.facet === undefined || d.facet === where.facet) &&
      (where.temporality === undefined || d.temporality === where.temporality)
    );
  });
}

/** Datasets no known source can serve — features behind these should be deleted. */
export const UNIVERSAL_GAPS: readonly DatasetId[] = DATASET_IDS.filter(
  (id) => !DATASETS[id].hasKnownSource,
);
