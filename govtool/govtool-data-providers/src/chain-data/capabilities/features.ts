/**
 * Chain Data API — how a CONSUMER reads a capability declaration.
 *
 * A component must never see a dataset id, a route, a provider name or a field
 * path. It sees a `FeatureState`: an availability boolean, narrowed option
 * arrays for the controls it renders, a refresh policy, and the two places a
 * component must ADAPT rather than hide.
 *
 * `deriveFeatures` is a pure function over JSON: a provider's declaration in, a
 * frontend feature set out. A fourth provider changes the output without
 * changing a line of this file, and the UI consequences are computed rather
 * than hand-maintained in a second place.
 *
 * It is intended to run in the BACKEND, over `composeCapabilities(providerTable,
 * backendPatch)` — GovTool's backend reads the whole DRep directory and sorts,
 * filters and pages it in memory, so a provider that refuses every `DRepSort`
 * key must not disable the UI sort. Shipping a provider's raw table to the
 * browser would disable working features and enable broken ones.
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
  Caveat,
  DeclarationScope,
  OptionSupport,
  SearchMode,
} from './axes';
import { isOfferable } from './axes';
import type { DatasetId, RouteId } from './datasets';
import { DATASETS } from './datasets';
import type {
  AnyDatasetCapability,
  CapabilityTable,
  DelegationHistoryKind,
  ProviderCapabilityDocument,
} from './declaration';
import {
  absenceOf,
  isRouteCallable,
  readCapability,
  resolveCapabilities,
} from './declaration';
import type {
  CapabilityEntities,
  EntityDeclarations,
  EntityId,
  FieldOverride,
  OptionalFieldsOf,
} from './fields';
import { fieldSupportFor, isPopulated } from './fields';

/* ------------------------------------------------------------------------- */
/* Exhaustive universes over the contract's own unions                        */
/* ------------------------------------------------------------------------- */

/**
 * `Record<U, true>` forces every member of `U` to appear, so adding a member to
 * `DRepSort` breaks this file — the capability vocabulary cannot fall behind
 * the contract's unions.
 */
function members<U extends string>(map: Record<U, true>): readonly U[] {
  return Object.keys(map) as U[];
}

export const DREP_SORTS = members<DRepSort>({
  votingPower: true,
  registrationDate: true,
  activity: true,
  status: true,
  random: true,
});

export const DREP_STATUSES = members<DRepStatus>({
  active: true,
  inactive: true,
  retired: true,
});

export const DREP_KINDS = members<DRepKind>({ drep: true, directVoter: true });

export const DREP_EXPANDS = members<DRepExpand>({
  metadata: true,
  liveVotingPower: true,
  delegators: true,
  activity: true,
});

export const GOV_ACTION_SORTS = members<GovActionSort>({
  newest: true,
  oldest: true,
  soonestToExpire: true,
  mostYesVotes: true,
  highestParticipation: true,
});

export const GOV_ACTION_TYPES = members<GovActionType>({
  ParameterChange: true,
  HardForkInitiation: true,
  TreasuryWithdrawals: true,
  NoConfidence: true,
  UpdateCommittee: true,
  NewConstitution: true,
  InfoAction: true,
});

export const GOV_ACTION_STATUSES = members<GovActionStatus>({
  live: true,
  ratified: true,
  enacted: true,
  expired: true,
  dropped: true,
});

export const GOV_ACTION_EXPANDS = members<GovActionExpand>({
  tallies: true,
  thresholds: true,
  metadata: true,
  myVote: true,
  protocolParams: true,
});

export const VOTE_SORTS = members<VoteSort>({
  newest: true,
  oldest: true,
  votingPower: true,
});

export const VOTE_EXPANDS = members<VoteExpand>({
  votingPower: true,
  rationale: true,
  proposal: true,
});

export const VOTE_CHOICES = members<VoteChoice>({
  yes: true,
  no: true,
  abstain: true,
});

export const VOTER_ROLES = members<VoterRole>({
  drep: true,
  spo: true,
  cc: true,
  direct: true,
});

export const ACCOUNT_EXPANDS = members<AccountExpand>({
  balance: true,
  votingPower: true,
  delegation: true,
  drep: true,
  poolDelegation: true,
  adaHandles: true,
});

export const STAKE_BASES = members<StakeBasis>({ active: true, live: true });

export const SEARCH_MODES = members<SearchMode>({
  exactId: true,
  freeText: true,
  adaHandle: true,
});

export const DELEGATION_HISTORY_KINDS = members<DelegationHistoryKind>({
  governance: true,
  pool: true,
});

/* ------------------------------------------------------------------------- */
/* The feature catalog                                                        */
/* ------------------------------------------------------------------------- */

/**
 * The unit of a tally: what the ledger counts for a role. DReps and SPOs are
 * decided by stake, the committee by head count. A provider that can only count
 * heads for DReps makes both the "₳" prefix and the percentage denominator
 * wrong, so this cannot be a boolean — the component must switch units.
 */
export type TallyBasis = 'stake' | 'count';

/**
 * Losing a `core` feature is a deployment banner, not a hidden tab. The
 * motivating incident: a dead `utxo_view` on preview db-sync made every
 * connected wallet render 0 ada as fact, with nothing reporting a fault.
 */
export type FeatureTier = 'core' | 'enhanced';

/**
 * Named, user-visible capabilities. `options` are the control values a UI
 * offers; `mode` is the value a component must adapt to rather than hide.
 */
export interface FeatureCatalog {
  'network.info': { options: Record<never, never>; mode: never };
  'network.protocolParams': { options: Record<never, never>; mode: never };
  'network.protocolParamsAtEpoch': {
    options: Record<never, never>;
    mode: never;
  };
  'network.stakeTotals': { options: { basis: StakeBasis }; mode: never };
  'network.treasury': { options: Record<never, never>; mode: never };
  'network.epochBrowser': { options: Record<never, never>; mode: never };

  'dashboard.metrics': { options: Record<never, never>; mode: never };
  /**
   * The three `GovernanceMetrics` fields GovTool actually reads. Declared apart
   * from the other ten so the committee threshold bar is not defeated by
   * counters nobody renders — the concrete Koios unblock.
   */
  'dashboard.committeeThreshold': {
    options: Record<never, never>;
    mode: never;
  };

  'drepDirectory.browse': {
    options: {
      sort: DRepSort;
      status: DRepStatus;
      kind: DRepKind;
      search: SearchMode;
    };
    mode: never;
  };
  'drep.profile': { options: { expand: DRepExpand }; mode: never };
  'drep.votingPower': { options: { basis: StakeBasis }; mode: StakeBasis };
  'drep.votingPowerHistory': { options: Record<never, never>; mode: never };
  'drep.delegatorList': { options: { basis: StakeBasis }; mode: never };
  /** The user's headline case: who joined, who left, and when. */
  'drep.delegationTimeline': { options: Record<never, never>; mode: never };
  'drep.registrationHistory': { options: Record<never, never>; mode: never };
  'drep.activityStats': { options: Record<never, never>; mode: never };
  'drep.voteHistory': {
    options: { sort: VoteSort; proposalType: GovActionType; vote: VoteChoice };
    mode: never;
  };

  'govActionList.browse': {
    options: {
      sort: GovActionSort;
      type: GovActionType;
      status: GovActionStatus;
      search: SearchMode;
      expand: GovActionExpand;
    };
    mode: never;
  };
  'govAction.details': { options: { bodyType: GovActionType }; mode: never };
  'govAction.tally': { options: { role: VoterRole }; mode: TallyBasis };
  'govAction.tallyThreshold': { options: Record<never, never>; mode: never };
  'govAction.tallyNotVoted': { options: Record<never, never>; mode: never };
  /** The user's second case: every vote cast on one action. */
  'govAction.voterList': {
    options: { sort: VoteSort; vote: VoteChoice; role: VoterRole };
    mode: never;
  };
  'govAction.voterListColumns': {
    options: { expand: VoteExpand };
    mode: never;
  };
  'govAction.activityTimeline': { options: Record<never, never>; mode: never };
  /** One name for what providers spell three ways: `{voterId}` twice, `expand:myVote`. */
  'govAction.myVoteBadge': { options: Record<never, never>; mode: never };
  'govAction.enactedComparison': {
    options: { type: GovActionType };
    mode: never;
  };

  'account.summary': { options: { expand: AccountExpand }; mode: never };
  'account.votingPower': { options: Record<never, never>; mode: never };
  'account.currentDelegation': { options: Record<never, never>; mode: never };
  'account.delegationHistory': {
    options: { kind: DelegationHistoryKind };
    mode: never;
  };
  'account.stakeEvents': { options: Record<never, never>; mode: never };

  'committee.browse': { options: Record<never, never>; mode: never };
  'committee.constitution': { options: Record<never, never>; mode: never };
  'spoDirectory.browse': { options: Record<never, never>; mode: never };
  'spo.voteHistory': { options: { sort: VoteSort }; mode: never };
  'voter.resolve': { options: Record<never, never>; mode: never };
  'voter.directory': { options: { role: VoterRole }; mode: never };

  'transaction.confirmation': { options: Record<never, never>; mode: never };
  'survey.definition': { options: Record<never, never>; mode: never };
}

export type FeatureId = keyof FeatureCatalog;
export type OptionSetOf<F extends FeatureId> =
  keyof FeatureCatalog[F]['options'] & string;
export type OptionOf<
  F extends FeatureId,
  S extends OptionSetOf<F>,
> = FeatureCatalog[F]['options'][S] & string;
export type ModeOf<F extends FeatureId> = FeatureCatalog[F]['mode'];

/* ------------------------------------------------------------------------- */
/* Requirements                                                               */
/* ------------------------------------------------------------------------- */

export type Requirement =
  | { readonly need: 'dataset'; readonly dataset: DatasetId }
  | { readonly need: 'route'; readonly route: RouteId }
  | {
      readonly need: 'field';
      readonly entity: EntityId;
      readonly field: string;
      readonly route?: RouteId;
    }
  | {
      readonly need: 'basis';
      readonly dataset: DatasetId;
      readonly basis: StakeBasis;
    }
  | {
      readonly need: 'expand';
      readonly dataset: DatasetId;
      readonly value: string;
    }
  | {
      readonly need: 'search';
      readonly dataset: DatasetId;
      readonly mode: SearchMode;
    }
  | {
      readonly need: 'join';
      readonly dataset: DatasetId;
      readonly join: string;
    }
  | {
      readonly need: 'anySort';
      readonly dataset: DatasetId;
      readonly of: readonly string[];
    }
  | { readonly need: 'anyOf'; readonly of: readonly Requirement[] };

/** Field requirement with the field name checked against the entity type. */
export function needsField<E extends EntityId>(
  entity: E,
  field: OptionalFieldsOf<CapabilityEntities[E]>,
  route?: RouteId,
): Requirement {
  return route === undefined
    ? { need: 'field', entity, field }
    : { need: 'field', entity, field, route };
}

/* ------------------------------------------------------------------------- */
/* Where each control's values come from                                      */
/* ------------------------------------------------------------------------- */

export type ControlSource =
  | { readonly control: 'sort'; readonly dataset: DatasetId }
  | {
      readonly control: 'filter';
      readonly dataset: DatasetId;
      readonly name: string;
    }
  | { readonly control: 'search'; readonly dataset: DatasetId }
  | { readonly control: 'expand'; readonly dataset: DatasetId }
  | { readonly control: 'basis'; readonly dataset: DatasetId };

export type OptionSetSpec<V extends string> = ControlSource & {
  /**
   * The full member list this declaration subtracts from, typed against the
   * contract union so a renamed member fails the build here rather than
   * silently emptying a dropdown in the browser.
   */
  readonly universe: readonly V[];
};

export interface FeatureSpec<F extends FeatureId> {
  readonly title: string;
  readonly tier: FeatureTier;
  /** Cost, paging and caveats are reported from this dataset. */
  readonly primary: DatasetId;
  readonly requires: readonly Requirement[];
  readonly optionSets: {
    readonly [S in OptionSetOf<F>]: OptionSetSpec<OptionOf<F, S>>;
  };
  /** Ordered: the first mode whose requirements hold wins. */
  readonly modeRules?: readonly {
    readonly mode: ModeOf<F>;
    readonly requires: readonly Requirement[];
  }[];
}

/**
 * Exhaustive: a new feature without a spec does not compile.
 */
export type FeatureSpecs = { readonly [F in FeatureId]: FeatureSpec<F> };

export const FEATURES: FeatureSpecs = {
  'network.info': {
    title: 'Network status',
    tier: 'core',
    primary: 'network.identity.current',
    requires: [{ need: 'dataset', dataset: 'network.identity.current' }],
    optionSets: {},
  },
  'network.protocolParams': {
    title: 'Protocol parameters',
    tier: 'core',
    primary: 'network.params.current',
    requires: [{ need: 'dataset', dataset: 'network.params.current' }],
    optionSets: {},
  },
  'network.protocolParamsAtEpoch': {
    title: 'Protocol parameters at a past epoch',
    tier: 'enhanced',
    primary: 'network.params.asAt',
    requires: [{ need: 'dataset', dataset: 'network.params.asAt' }],
    optionSets: {},
  },
  'network.stakeTotals': {
    title: 'Automated voting totals',
    tier: 'enhanced',
    primary: 'network.stake.current',
    requires: [
      { need: 'dataset', dataset: 'network.stake.current' },
      needsField('StakeDistribution', 'alwaysAbstainVotingPower'),
      needsField('StakeDistribution', 'alwaysNoConfidenceVotingPower'),
    ],
    optionSets: {
      basis: {
        control: 'basis',
        dataset: 'network.stake.current',
        universe: STAKE_BASES,
      },
    },
  },
  'network.treasury': {
    title: 'Treasury',
    tier: 'enhanced',
    primary: 'network.treasury.current',
    requires: [{ need: 'dataset', dataset: 'network.treasury.current' }],
    optionSets: {},
  },
  'network.epochBrowser': {
    title: 'Epoch and block browser',
    tier: 'enhanced',
    primary: 'network.chain.series',
    requires: [{ need: 'dataset', dataset: 'network.chain.series' }],
    optionSets: {},
  },

  'dashboard.metrics': {
    title: 'Governance dashboard counters',
    tier: 'enhanced',
    primary: 'network.aggregate.current',
    requires: [
      { need: 'dataset', dataset: 'network.aggregate.current' },
      { need: 'route', route: 'governance.metrics.get' },
    ],
    optionSets: {},
  },
  'dashboard.committeeThreshold': {
    title: 'Committee threshold bar',
    tier: 'enhanced',
    primary: 'network.aggregate.current',
    // Deliberately NOT requiring `governance.metrics.get`: the three fields
    // this needs are cheap everywhere, and on Koios they are served by an
    // extension while the full-record route is refused over six counters the
    // frontend never reads.
    requires: [
      {
        need: 'anyOf',
        of: [
          { need: 'dataset', dataset: 'network.aggregate.current' },
          { need: 'dataset', dataset: 'committee.identity.current' },
        ],
      },
    ],
    optionSets: {},
  },

  'drepDirectory.browse': {
    title: 'DRep directory',
    tier: 'core',
    primary: 'drep.identity.current',
    requires: [{ need: 'dataset', dataset: 'drep.identity.current' }],
    optionSets: {
      sort: {
        control: 'sort',
        dataset: 'drep.identity.current',
        universe: DREP_SORTS,
      },
      status: {
        control: 'filter',
        dataset: 'drep.identity.current',
        name: 'status',
        universe: DREP_STATUSES,
      },
      kind: {
        control: 'filter',
        dataset: 'drep.identity.current',
        name: 'kind',
        universe: DREP_KINDS,
      },
      search: {
        control: 'search',
        dataset: 'drep.identity.current',
        universe: SEARCH_MODES,
      },
    },
  },
  'drep.profile': {
    title: 'DRep profile',
    tier: 'core',
    primary: 'drep.identity.current',
    requires: [{ need: 'dataset', dataset: 'drep.identity.current' }],
    optionSets: {
      expand: {
        control: 'expand',
        dataset: 'drep.identity.current',
        universe: DREP_EXPANDS,
      },
    },
  },
  'drep.votingPower': {
    title: 'DRep voting power',
    tier: 'core',
    primary: 'drep.stake.current',
    requires: [{ need: 'dataset', dataset: 'drep.stake.current' }],
    optionSets: {
      basis: {
        control: 'basis',
        dataset: 'drep.stake.current',
        universe: STAKE_BASES,
      },
    },
    // Which number the card is actually showing. A directory that labels an
    // epoch snapshot as if it were live is a labelling bug, not a missing one.
    modeRules: [
      {
        mode: 'live',
        requires: [
          { need: 'basis', dataset: 'drep.stake.current', basis: 'live' },
        ],
      },
      {
        mode: 'active',
        requires: [
          { need: 'basis', dataset: 'drep.stake.current', basis: 'active' },
        ],
      },
    ],
  },
  'drep.votingPowerHistory': {
    title: 'DRep voting power over time',
    tier: 'enhanced',
    primary: 'drep.stake.series',
    requires: [{ need: 'dataset', dataset: 'drep.stake.series' }],
    optionSets: {},
  },
  'drep.delegatorList': {
    title: 'DRep delegators',
    tier: 'enhanced',
    primary: 'drep.delegation.current',
    requires: [{ need: 'dataset', dataset: 'drep.delegation.current' }],
    optionSets: {
      basis: {
        control: 'basis',
        dataset: 'drep.delegation.current',
        universe: STAKE_BASES,
      },
    },
  },
  'drep.delegationTimeline': {
    title: 'DRep delegation joins and leaves',
    tier: 'enhanced',
    primary: 'drep.delegation.events',
    requires: [{ need: 'dataset', dataset: 'drep.delegation.events' }],
    optionSets: {},
  },
  'drep.registrationHistory': {
    title: 'DRep registration history',
    tier: 'enhanced',
    primary: 'drep.registration.events',
    requires: [{ need: 'dataset', dataset: 'drep.registration.events' }],
    optionSets: {},
  },
  'drep.activityStats': {
    title: 'DRep voting activity',
    tier: 'enhanced',
    primary: 'drep.aggregate.current',
    requires: [
      { need: 'dataset', dataset: 'drep.aggregate.current' },
      needsField('DRepActivity', 'notVotedCount'),
      needsField('DRepActivity', 'participationRate'),
    ],
    optionSets: {},
  },
  'drep.voteHistory': {
    title: 'Actions this DRep voted on',
    tier: 'enhanced',
    primary: 'drep.ballot.current',
    requires: [{ need: 'dataset', dataset: 'drep.ballot.current' }],
    optionSets: {
      sort: {
        control: 'sort',
        dataset: 'drep.ballot.current',
        universe: VOTE_SORTS,
      },
      proposalType: {
        control: 'filter',
        dataset: 'drep.ballot.current',
        name: 'proposalType',
        universe: GOV_ACTION_TYPES,
      },
      vote: {
        control: 'filter',
        dataset: 'drep.ballot.current',
        name: 'vote',
        universe: VOTE_CHOICES,
      },
    },
  },

  'govActionList.browse': {
    title: 'Governance action list',
    tier: 'core',
    primary: 'proposal.identity.current',
    requires: [{ need: 'dataset', dataset: 'proposal.identity.current' }],
    optionSets: {
      sort: {
        control: 'sort',
        dataset: 'proposal.identity.current',
        universe: GOV_ACTION_SORTS,
      },
      type: {
        control: 'filter',
        dataset: 'proposal.identity.current',
        name: 'type',
        universe: GOV_ACTION_TYPES,
      },
      status: {
        control: 'filter',
        dataset: 'proposal.identity.current',
        name: 'status',
        universe: GOV_ACTION_STATUSES,
      },
      search: {
        control: 'search',
        dataset: 'proposal.identity.current',
        universe: SEARCH_MODES,
      },
      expand: {
        control: 'expand',
        dataset: 'proposal.identity.current',
        universe: GOV_ACTION_EXPANDS,
      },
    },
  },
  'govAction.details': {
    title: 'Governance action detail',
    tier: 'core',
    primary: 'proposal.body.current',
    requires: [{ need: 'dataset', dataset: 'proposal.body.current' }],
    optionSets: {
      // Which typed bodies exist — the detail tabs. db-sync cannot type
      // `UpdateCommittee`, so that one tab goes and the rest stay.
      bodyType: {
        control: 'filter',
        dataset: 'proposal.body.current',
        name: 'type',
        universe: GOV_ACTION_TYPES,
      },
    },
  },
  'govAction.tally': {
    title: 'Vote totals',
    tier: 'enhanced',
    primary: 'proposal.tally.current',
    requires: [{ need: 'dataset', dataset: 'proposal.tally.current' }],
    optionSets: {
      role: {
        control: 'filter',
        dataset: 'proposal.tally.current',
        name: 'role',
        universe: VOTER_ROLES,
      },
    },
    // Stake first: it is what the ledger decides DReps and SPOs by. A provider
    // with counts only lands on 'count', and the component drops the ada
    // prefix and the stake denominator instead of labelling turnout as weight.
    modeRules: [
      { mode: 'stake', requires: [needsField('RoleTally', 'stake')] },
      { mode: 'count', requires: [needsField('RoleTally', 'count')] },
    ],
  },
  'govAction.tallyThreshold': {
    title: 'Vote threshold line',
    tier: 'enhanced',
    primary: 'proposal.tally.current',
    requires: [
      { need: 'dataset', dataset: 'proposal.tally.current' },
      needsField('RoleTally', 'threshold'),
    ],
    optionSets: {},
  },
  'govAction.tallyNotVoted': {
    title: 'Not-voted segment',
    tier: 'enhanced',
    primary: 'proposal.tally.current',
    // Without the denominator this renders a WRONG number rather than no
    // number, because the component defaults the missing total to 0.
    requires: [
      { need: 'dataset', dataset: 'proposal.tally.current' },
      needsField('RoleTally', 'notVotedStake'),
      needsField('RoleTally', 'totalEligibleStake'),
    ],
    optionSets: {},
  },
  'govAction.voterList': {
    title: 'Who voted on this action',
    tier: 'enhanced',
    primary: 'proposal.ballot.current',
    requires: [{ need: 'dataset', dataset: 'proposal.ballot.current' }],
    optionSets: {
      sort: {
        control: 'sort',
        dataset: 'proposal.ballot.current',
        universe: VOTE_SORTS,
      },
      vote: {
        control: 'filter',
        dataset: 'proposal.ballot.current',
        name: 'vote',
        universe: VOTE_CHOICES,
      },
      role: {
        control: 'filter',
        dataset: 'proposal.ballot.current',
        name: 'role',
        universe: VOTER_ROLES,
      },
    },
  },
  'govAction.voterListColumns': {
    title: 'Voter list columns',
    tier: 'enhanced',
    primary: 'proposal.ballot.current',
    requires: [{ need: 'dataset', dataset: 'proposal.ballot.current' }],
    optionSets: {
      expand: {
        control: 'expand',
        dataset: 'proposal.ballot.current',
        universe: VOTE_EXPANDS,
      },
    },
  },
  'govAction.activityTimeline': {
    title: 'Action activity feed',
    tier: 'enhanced',
    primary: 'proposal.identity.events',
    requires: [{ need: 'dataset', dataset: 'proposal.identity.events' }],
    optionSets: {},
  },
  'govAction.myVoteBadge': {
    title: 'Your vote on this action',
    tier: 'enhanced',
    primary: 'proposal.identity.current',
    requires: [
      { need: 'dataset', dataset: 'proposal.identity.current' },
      {
        need: 'join',
        dataset: 'proposal.identity.current',
        join: 'callerVote',
      },
    ],
    optionSets: {},
  },
  'govAction.enactedComparison': {
    title: 'Comparison with the enacted action',
    tier: 'enhanced',
    primary: 'proposal.outcome.current',
    requires: [{ need: 'dataset', dataset: 'proposal.outcome.current' }],
    optionSets: {
      type: {
        control: 'filter',
        dataset: 'proposal.outcome.current',
        name: 'type',
        universe: GOV_ACTION_TYPES,
      },
    },
  },

  'account.summary': {
    title: 'Connected wallet',
    tier: 'core',
    primary: 'account.identity.current',
    requires: [{ need: 'dataset', dataset: 'account.identity.current' }],
    optionSets: {
      expand: {
        control: 'expand',
        dataset: 'account.identity.current',
        universe: ACCOUNT_EXPANDS,
      },
    },
  },
  'account.votingPower': {
    title: 'Your voting power',
    tier: 'core',
    primary: 'account.stake.current',
    requires: [{ need: 'dataset', dataset: 'account.stake.current' }],
    optionSets: {},
  },
  'account.currentDelegation': {
    title: 'Your DRep delegation',
    tier: 'core',
    primary: 'account.delegation.current',
    requires: [{ need: 'dataset', dataset: 'account.delegation.current' }],
    optionSets: {},
  },
  'account.delegationHistory': {
    title: 'Your delegation history',
    tier: 'enhanced',
    primary: 'account.delegation.events',
    requires: [{ need: 'dataset', dataset: 'account.delegation.events' }],
    optionSets: {
      kind: {
        control: 'filter',
        dataset: 'account.delegation.events',
        name: 'kind',
        universe: DELEGATION_HISTORY_KINDS,
      },
    },
  },
  'account.stakeEvents': {
    title: 'Your stake key history',
    tier: 'enhanced',
    primary: 'account.registration.events',
    requires: [{ need: 'dataset', dataset: 'account.registration.events' }],
    optionSets: {},
  },

  'committee.browse': {
    title: 'Constitutional committee',
    tier: 'enhanced',
    primary: 'committee.identity.current',
    requires: [{ need: 'dataset', dataset: 'committee.identity.current' }],
    optionSets: {},
  },
  'committee.constitution': {
    title: 'Constitution',
    tier: 'enhanced',
    primary: 'constitution.identity.current',
    requires: [{ need: 'dataset', dataset: 'constitution.identity.current' }],
    optionSets: {},
  },
  'spoDirectory.browse': {
    title: 'SPO directory',
    tier: 'enhanced',
    primary: 'pool.identity.current',
    requires: [{ need: 'dataset', dataset: 'pool.identity.current' }],
    optionSets: {},
  },
  'spo.voteHistory': {
    title: 'SPO voting record',
    tier: 'enhanced',
    primary: 'pool.ballot.current',
    requires: [{ need: 'dataset', dataset: 'pool.ballot.current' }],
    optionSets: {
      sort: {
        control: 'sort',
        dataset: 'pool.ballot.current',
        universe: VOTE_SORTS,
      },
    },
  },
  'voter.resolve': {
    title: 'Voter lookup',
    tier: 'enhanced',
    primary: 'voter.identity.current',
    requires: [{ need: 'dataset', dataset: 'voter.identity.current' }],
    optionSets: {},
  },
  'voter.directory': {
    title: 'All voters',
    tier: 'enhanced',
    primary: 'voter.identity.list',
    requires: [{ need: 'dataset', dataset: 'voter.identity.list' }],
    optionSets: {
      role: {
        control: 'filter',
        dataset: 'voter.identity.list',
        name: 'role',
        universe: VOTER_ROLES,
      },
    },
  },

  'transaction.confirmation': {
    title: 'Transaction confirmation',
    tier: 'core',
    primary: 'transaction.identity.current',
    requires: [{ need: 'dataset', dataset: 'transaction.identity.current' }],
    optionSets: {},
  },
  'survey.definition': {
    title: 'CIP-179 surveys',
    tier: 'enhanced',
    primary: 'survey.body.current',
    requires: [{ need: 'dataset', dataset: 'survey.body.current' }],
    optionSets: {},
  },
};

export const FEATURE_IDS = Object.keys(FEATURES) as readonly FeatureId[];

/* ------------------------------------------------------------------------- */
/* The derived shape a UI consumes                                            */
/* ------------------------------------------------------------------------- */

/** How often a consumer may refresh, derived from `pollable`. */
export type RefreshPolicy = 'poll' | 'onDemand' | 'userInitiated';

/**
 * Everything a single control needs. `allowed: []` means hide the control, not
 * render an empty menu — which is already how `DataActionsBar` behaves.
 */
export interface OptionSetState<V extends string> {
  readonly allowed: readonly V[];
  /**
   * Accepted and silently not applied. Never offer these; they are listed so a
   * support bundle can explain why a menu item vanished.
   */
  readonly ignored: readonly V[];
  /**
   * How many values may be sent at once. `null` = unbounded. A multi-select
   * chip row bound to a `string[]` needs this or it will build an illegal
   * request that 501s.
   */
  readonly maxSelected: number | null;
  /**
   * What to select when the UI's own hard-coded default is not in `allowed`.
   * Supplied so every list does not hand-write the same fallback effect.
   */
  readonly defaultTo: V | null;
  /** False when the filter is applied after paging, so a page may be short. */
  readonly exhaustive: boolean;
}

export interface PagingFacts {
  readonly maxLimit: number | null;
  readonly defaultLimit: number | null;
  readonly omittedLimitMeans: 'everything' | 'oneMaxPage' | 'routeDefault';
  readonly canSeek: boolean;
  /**
   * `'absent'` means a numbered paginator cannot be drawn and the UI must fall
   * back to cursor-style prev/next.
   */
  readonly total: 'exact' | 'estimated' | 'absent';
}

/**
 * Why a feature is off. `'unknown'` is first-class: before the capability fetch
 * resolves, and after it fails, a UI must be able to tell "not loaded yet" from
 * an honest refusal, or a transient network error silently becomes a
 * feature-less app.
 */
export interface FeatureBlock {
  readonly cause:
    | 'unknown'
    | 'permanent'
    | 'provider'
    | 'deployment'
    | 'cost'
    | 'representation';
  readonly absence: Absence | null;
  readonly scope: DeclarationScope | null;
  readonly dataset: DatasetId | null;
  /** Diagnostic prose. Log it; do not render it. */
  readonly reason: string;
  /** A cheaper dataset answering a related question, when the cause is cost. */
  readonly fallback: DatasetId | null;
}

export type FeatureOptions<F extends FeatureId> = {
  readonly [S in OptionSetOf<F>]: OptionSetState<OptionOf<F, S>>;
};

export interface FeatureState<F extends FeatureId = FeatureId> {
  readonly available: boolean;
  readonly tier: FeatureTier;
  /**
   * No known source records this, so the toggle will never flip on any
   * provider: delete the component rather than shipping a permanent "coming
   * soon".
   */
  readonly permanentlyAbsent: boolean;
  readonly refresh: RefreshPolicy;
  readonly options: FeatureOptions<F>;
  readonly mode: ModeOf<F> | null;
  readonly paging: PagingFacts | null;
  /** Render from `kind` + structured fields; `note` is for logs. */
  readonly caveats: readonly Caveat[];
  readonly blockedBy: FeatureBlock | null;
}

export interface FeatureSet {
  readonly schemaVersion: 2;
  readonly provider: ProviderId;
  readonly network: NetworkId;
  readonly generatedAt: Timestamp;
  /**
   * Digest of the declaration this was derived from. A 501 for a feature this
   * set says is available means the toggle is stale: the consumer refetches and
   * compares digests instead of guessing.
   */
  readonly sourceDigest: string;
  readonly features: { readonly [F in FeatureId]: FeatureState<F> };
  /** Core features that are unavailable — a deployment banner, not a hidden tab. */
  readonly brokenCore: readonly FeatureId[];
}

/* ------------------------------------------------------------------------- */
/* Derivation                                                                 */
/* ------------------------------------------------------------------------- */

/**
 * Support is binary, so cost no longer grades a feature. The one operational
 * fact that survives is whether the provider will tolerate an interval on it.
 */
function refreshFor(pollable: boolean): RefreshPolicy {
  return pollable ? 'poll' : 'userInitiated';
}

function blockFrom(
  dataset: DatasetId,
  cap: AnyDatasetCapability,
  fallbackCause: FeatureBlock['cause'],
): FeatureBlock {
  const u = cap.unavailable;
  if (u === undefined) {
    return {
      cause: fallbackCause,
      absence: null,
      scope: null,
      dataset,
      reason: 'The provider serves this dataset but not in the form required.',
      fallback: null,
    };
  }
  const absence = absenceOf(u);
  const cause: FeatureBlock['cause'] = !DATASETS[dataset].hasKnownSource
    ? 'permanent'
    : u.kind === 'deploymentFault'
      ? 'deployment'
      : u.kind === 'tooExpensive'
        ? 'cost'
        : u.kind === 'representation'
          ? 'representation'
          : 'provider';
  return {
    cause,
    absence,
    scope: u.scope,
    dataset,
    reason: u.reason,
    fallback: u.kind === 'tooExpensive' ? u.fallback : null,
  };
}

function optionRecord(
  cap: AnyDatasetCapability,
  source: ControlSource,
): Readonly<Record<string, OptionSupport>> | undefined {
  switch (source.control) {
    case 'sort':
      return cap.sort;
    case 'expand':
      return cap.expand;
    case 'basis':
      return cap.basis;
    case 'search':
      return cap.search?.modes;
    case 'filter':
      return cap.filters?.[source.name]?.values;
  }
}

function optionSetState<V extends string>(
  table: CapabilityTable,
  spec: OptionSetSpec<V>,
  featureAvailable: boolean,
): OptionSetState<V> {
  const cap = readCapability(table, spec.dataset);
  const empty: OptionSetState<V> = {
    allowed: [],
    ignored: [],
    maxSelected: null,
    defaultTo: null,
    exhaustive: true,
  };
  if (!featureAvailable || cap.reachability !== 'served') {
    return empty;
  }

  const record = optionRecord(cap, spec);
  // An undeclared control means the provider said nothing about it. Nothing is
  // the same as refused here: over-claiming a sort produces a list that looks
  // ordered and is not, which is the failure mode `'ignored'` exists to name.
  if (record === undefined) {
    return empty;
  }

  const allowed: V[] = [];
  const ignored: V[] = [];
  for (const member of spec.universe) {
    const support = record[member];
    if (support === undefined) {
      continue;
    }
    if (isOfferable(support)) {
      allowed.push(member);
    } else if (support === 'ignored') {
      ignored.push(member);
    }
  }

  let maxSelected: number | null = null;
  let defaultTo: V | null = null;
  let exhaustive = true;
  if (spec.control === 'filter') {
    const filter = cap.filters?.[spec.name];
    maxSelected = filter?.maxSelected ?? null;
    exhaustive = filter?.exhaustive ?? true;
    const declaredDefault = filter?.defaultsTo;
    if (
      declaredDefault !== undefined &&
      (allowed as readonly string[]).includes(declaredDefault)
    ) {
      defaultTo = declaredDefault as V;
    }
  }
  if (defaultTo === null && allowed.length > 0) {
    defaultTo = allowed[0] ?? null;
  }

  return { allowed, ignored, maxSelected, defaultTo, exhaustive };
}

interface Resolved {
  readonly table: CapabilityTable;
  readonly entities: EntityDeclarations;
  readonly overrides: readonly FieldOverride[];
}

/** An option the provider declared AND a UI may offer. */
function offers(support: OptionSupport | undefined): boolean {
  return support !== undefined && isOfferable(support);
}

function unmet(ctx: Resolved, r: Requirement): FeatureBlock | undefined {
  switch (r.need) {
    case 'dataset': {
      const cap = readCapability(ctx.table, r.dataset);
      return cap.reachability === 'served'
        ? undefined
        : blockFrom(r.dataset, cap, 'provider');
    }
    case 'route': {
      if (isRouteCallable(ctx.table, r.route)) {
        return undefined;
      }
      const owner = (Object.keys(DATASETS) as DatasetId[]).find((id) =>
        (DATASETS[id].routes as readonly string[]).includes(r.route),
      );
      if (owner === undefined) {
        return {
          cause: 'provider',
          absence: null,
          scope: null,
          dataset: null,
          reason: `Route ${r.route} is not callable.`,
          fallback: null,
        };
      }
      const cap = readCapability(ctx.table, owner);
      // A served dataset with this route in `refusedRoutes` carries the reason
      // on the route, not on the dataset: that is the Koios metrics case, where
      // the data is available and the bundled route is not.
      const refusedRoute = (cap.refusedRoutes ?? []).find(
        (refused) => String(refused.route) === r.route,
      );
      if (refusedRoute !== undefined) {
        return blockFrom(
          owner,
          { ...cap, unavailable: refusedRoute.unavailable },
          'provider',
        );
      }
      return blockFrom(owner, cap, 'provider');
    }
    case 'field': {
      const support = fieldSupportFor(
        ctx.entities,
        r.entity,
        r.field,
        r.route,
        ctx.overrides,
      );
      if (support !== undefined && isPopulated(support)) {
        return undefined;
      }
      return {
        cause:
          support !== undefined &&
          support.serves === 'never' &&
          support.cause === 'tooExpensive'
            ? 'cost'
            : support !== undefined &&
                support.serves === 'never' &&
                support.cause === 'representation'
              ? 'representation'
              : 'provider',
        absence:
          support !== undefined && support.serves === 'never'
            ? support.cause
            : 'notImplemented',
        scope: null,
        dataset: null,
        reason:
          support === undefined
            ? `${r.entity}.${r.field} is not declared by this provider.`
            : support.serves === 'never'
              ? support.note
              : `${r.entity}.${r.field} is not populated.`,
        fallback: null,
      };
    }
    case 'basis': {
      const cap = readCapability(ctx.table, r.dataset);
      return offers(cap.basis?.[r.basis])
        ? undefined
        : blockFrom(r.dataset, cap, 'provider');
    }
    case 'expand': {
      const cap = readCapability(ctx.table, r.dataset);
      return offers(cap.expand?.[r.value])
        ? undefined
        : blockFrom(r.dataset, cap, 'provider');
    }
    case 'search': {
      const cap = readCapability(ctx.table, r.dataset);
      return offers(cap.search?.modes[r.mode])
        ? undefined
        : blockFrom(r.dataset, cap, 'provider');
    }
    case 'join': {
      const cap = readCapability(ctx.table, r.dataset);
      return offers(cap.joins?.[r.join])
        ? undefined
        : blockFrom(r.dataset, cap, 'provider');
    }
    case 'anySort': {
      const cap = readCapability(ctx.table, r.dataset);
      const any = r.of.some((key) => offers(cap.sort?.[key]));
      return any ? undefined : blockFrom(r.dataset, cap, 'provider');
    }
    case 'anyOf': {
      let first: FeatureBlock | undefined;
      for (const inner of r.of) {
        const block = unmet(ctx, inner);
        if (block === undefined) {
          return undefined;
        }
        first ??= block;
      }
      return first;
    }
  }
}

/**
 * Provider declaration in, frontend feature set out.
 *
 * Deterministic, dependency-free and testable. Deployment overrides are applied
 * first, so a demoted route reaches the UI as a blocked feature with
 * `cause: 'deployment'` rather than as a working one.
 */
export function deriveFeatures(
  doc: ProviderCapabilityDocument,
  sourceDigest: string,
): FeatureSet {
  const ctx: Resolved = {
    table: resolveCapabilities(doc),
    entities: doc.entities,
    overrides: doc.fieldOverrides,
  };

  const features = {} as Record<FeatureId, FeatureState>;
  const brokenCore: FeatureId[] = [];

  for (const id of FEATURE_IDS) {
    const spec = FEATURES[id] as FeatureSpec<FeatureId>;
    const primary = readCapability(ctx.table, spec.primary);

    let blockedBy: FeatureBlock | null = null;
    let permanentlyAbsent = false;

    for (const requirement of spec.requires) {
      if (requirement.need === 'dataset') {
        if (!DATASETS[requirement.dataset].hasKnownSource) {
          permanentlyAbsent = true;
        }
      }
      if (blockedBy === null) {
        blockedBy = unmet(ctx, requirement) ?? null;
      }
    }

    const available = blockedBy === null;

    const options = {} as Record<string, OptionSetState<string>>;
    for (const [name, source] of Object.entries(spec.optionSets)) {
      options[name] = optionSetState(
        ctx.table,
        source as OptionSetSpec<string>,
        available,
      );
    }

    let mode: string | null = null;
    if (available && spec.modeRules !== undefined) {
      for (const rule of spec.modeRules) {
        if (rule.requires.every((req) => unmet(ctx, req) === undefined)) {
          mode = rule.mode as unknown as string;
          break;
        }
      }
    }

    const paging: PagingFacts | null =
      primary.paging === undefined
        ? null
        : {
            maxLimit: primary.paging.maxLimit,
            defaultLimit: primary.paging.defaultLimit ?? null,
            omittedLimitMeans: primary.paging.omittedLimitMeans,
            canSeek: primary.paging.offset === 'honoured',
            total: primary.paging.total,
          };

    const state: FeatureState = {
      available,
      tier: spec.tier,
      permanentlyAbsent,
      refresh: refreshFor(primary.pollable),
      options: options as FeatureOptions<FeatureId>,
      mode: mode as never,
      paging,
      caveats: available ? (primary.caveats ?? []) : [],
      blockedBy,
    };
    features[id] = state;

    if (!available && spec.tier === 'core') {
      brokenCore.push(id);
    }
  }

  return {
    schemaVersion: 2,
    provider: doc.provider,
    network: doc.network,
    generatedAt: doc.generatedAt,
    sourceDigest,
    features: features as FeatureSet['features'],
    brokenCore,
  };
}

/* ------------------------------------------------------------------------- */
/* Consumer helpers                                                           */
/* ------------------------------------------------------------------------- */

/**
 * The state to use before the capability fetch resolves, and after it fails.
 * Everything is off with `cause: 'unknown'`, which is deliberately NOT the same
 * as a refusal: a UI that cannot tell them apart renders a transient network
 * error as a permanently feature-less app.
 */
export function unknownFeatureState<F extends FeatureId>(
  id: F,
): FeatureState<F> {
  const spec = FEATURES[id] as FeatureSpec<FeatureId>;
  const options = {} as Record<string, OptionSetState<string>>;
  for (const name of Object.keys(spec.optionSets)) {
    options[name] = {
      allowed: [],
      ignored: [],
      maxSelected: null,
      defaultTo: null,
      exhaustive: true,
    };
  }
  return {
    available: false,
    tier: spec.tier,
    permanentlyAbsent: false,
    refresh: 'userInitiated',
    options: options as FeatureOptions<F>,
    mode: null,
    paging: null,
    caveats: [],
    blockedBy: {
      cause: 'unknown',
      absence: null,
      scope: null,
      dataset: null,
      reason: 'Provider capabilities have not been loaded.',
      fallback: null,
    },
  };
}

/**
 * Narrow a UI's own option array against a feature's allow-list, in one place.
 *
 * `keyMap` translates the UI's option keys to the contract's member names —
 * unavoidable, because GovTool's list controls send their own labels as wire
 * values. Typing it `Record<UiKey, OptionOf<F, S>>` is what stops a contract
 * rename from silently emptying a dropdown with no build error.
 */
export function narrowOptions<
  F extends FeatureId,
  S extends OptionSetOf<F>,
  UiOption extends { key: string },
>(
  options: readonly UiOption[],
  state: FeatureState<F>,
  set: S,
  keyMap: Readonly<Record<string, OptionOf<F, S>>>,
): UiOption[] {
  const allowed = new Set<string>(state.options[set].allowed);
  return options.filter((option) => {
    const member = keyMap[option.key];
    return member !== undefined && allowed.has(member);
  });
}

/**
 * The selection to apply when the UI's hard-coded default is not offered.
 * GovTool forces `[DRepStatus.Active]` and `DRepListSort.Activity` on mount and
 * persists both across navigation — precisely the two keys most likely to be
 * gated away.
 */
export function defaultSelection<F extends FeatureId, S extends OptionSetOf<F>>(
  state: FeatureState<F>,
  set: S,
  preferred: OptionOf<F, S>,
): OptionOf<F, S> | null {
  const option = state.options[set];
  return (option.allowed as readonly string[]).includes(preferred)
    ? preferred
    : option.defaultTo;
}

/** Whether a persisted selection is still legal on this provider. */
export function selectionIsStale<F extends FeatureId, S extends OptionSetOf<F>>(
  state: FeatureState<F>,
  set: S,
  selected: readonly string[],
): boolean {
  const option = state.options[set];
  const allowed = new Set<string>(option.allowed);
  if (selected.some((value) => !allowed.has(value))) {
    return true;
  }
  return option.maxSelected !== null && selected.length > option.maxSelected;
}
