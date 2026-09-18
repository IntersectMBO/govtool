/**
 * The capability document THIS BACKEND serves, composed from the configured
 * provider's own.
 *
 * The backend is not a proxy. It sits between the provider and the browser and
 * changes what is true in both directions, so shipping the provider's document
 * to the browser would disable features that work and enable features that do
 * not:
 *
 *   RAISES  the DRep directory and the governance-action list are read WHOLE
 *           into a cached snapshot (`readAll` + `CacheService`) and then
 *           filtered, searched, sorted and paged in memory. A provider that
 *           refuses every `DRepSort` key still gets a working sort menu,
 *           because `DRepService.sortDReps` supplies it.
 *
 *   LOWERS  three kinds of loss, each transcribed from a real site below:
 *           laundering (an error or a `null` becomes `0`), dropping (a
 *           parameter the legacy route accepts and never forwards), and not
 *           exposing a route at all (this backend has no `/committee/*`, so
 *           `committee.identity.current` is unreachable however good the
 *           provider is).
 *
 * Every value here is transcribed from a service in `src/`, and the claims are
 * pinned by `test/capabilities.spec.ts` — the declaration is hand-maintained,
 * so each compensation needs a test that fails when the code stops doing it.
 *
 * Nothing in this file edits the contract package or the provider's document:
 * it builds a PATCH and hands it to `composeCapabilities`.
 */

import { createHash } from 'node:crypto';
import type {
  AnyDatasetCapability,
  DRepSort,
  DatasetId,
  EntityDeclaration,
  EntityDeclarations,
  EntityId,
  EnumSupport,
  FeatureSet,
  FieldSupport,
  FilterSupport,
  DRepExpand,
  DRepStatus,
  GovActionExpand,
  GovActionSort,
  GovActionStatus,
  GovActionType,
  MisreportedField,
  OptionSupport,
  PagingSupport,
  ProviderCapabilityDocument,
  RouteId,
  SearchSupport,
  VoteChoice,
  VoteExpand,
} from '@govtool/data-providers/chain-data';
import {
  DATASETS,
  DATASET_IDS,
  composeCapabilities,
  deriveFeatures,
  fieldSupportFor,
  readCapability,
} from '@govtool/data-providers/chain-data';

/** The `provider` this backend puts on its own document. */
export const BACKEND_PROVIDER_ID = 'govtool-backend';

/** Kept in step with package.json; the capability digest is cut over it. */
export const BACKEND_VERSION = '0.0.1';

/**
 * When this patch was last reviewed against `src/`. Deliberately a constant
 * rather than `new Date()`: the digest a consumer compares against must not
 * change every time the process restarts.
 */
export const BACKEND_CAPABILITIES_REVIEWED_AT = '2026-09-18T00:00:00Z';

/* ------------------------------------------------------------------------- */
/* Which contract routes this backend actually calls                          */
/* ------------------------------------------------------------------------- */

/**
 * Every `this.chain.*` call in `src/`. One entry per call site, because the
 * datasets below are derived from it and `test/capabilities.spec.ts` greps the
 * source to prove the list is complete.
 *
 * `system.getHealth` and `system.getCapabilities` are not here: they are how a
 * consumer reads this document, not capability-gated routes.
 */
export const BACKEND_CHAIN_DATA_ROUTES: readonly RouteId[] = [
  'network.getNetworkInfo', // network.service.ts
  'network.getStakeDistribution', // network.service.ts
  'network.getProtocolParams', // epoch.service.ts
  'governance.metrics.get', // network.service.ts
  'accounts.get', // account.service.ts
  'accounts.getDelegation', // ada-holder.service.ts
  'accounts.getVotingPower', // ada-holder.service.ts
  'governance.dreps.get', // drep.service.ts
  'governance.dreps.list', // drep.service.ts
  'governance.dreps.listVotes', // drep.service.ts
  'governance.dreps.getVotingPower', // drep.service.ts
  'governance.dreps.getVotingPowers', // drep.service.ts
  'governance.proposals.list', // proposal.service.ts
  'governance.proposals.get', // proposal.service.ts
  'governance.proposals.getEnacted', // proposal.service.ts
  'transactions.get', // transaction.service.ts
  'surveys.getDefinition', // survey.service.ts
];

/**
 * Datasets this backend does not expose, with the reason.
 *
 * Two groups, and the first is computed rather than listed: a dataset none of
 * whose routes appear in `BACKEND_CHAIN_DATA_ROUTES` cannot be reached through
 * this backend at all. The second group is the datasets that SHARE a route
 * with one the backend calls but need an argument the backend never sends —
 * `network.params.asAt` reads the same `getProtocolParams` as
 * `network.params.current`, and `/epoch/params` takes no epoch.
 */
const SHARES_A_ROUTE_BUT_UNREACHABLE: Readonly<
  Partial<Record<DatasetId, string>>
> = {
  'network.params.asAt':
    'EpochService.getCurrentEpochParams calls network.getProtocolParams() ' +
    'with no epoch, and /epoch/params takes none.',
  'account.stake.asAt':
    'AdaHolderService.getVotingPower calls accounts.getVotingPower(stakeKey) ' +
    'with no epoch; /ada-holder/get-voting-power accepts none.',
  'drep.stake.series':
    'DRepService.getVotingPower calls dreps.getVotingPower(drepId) for the ' +
    'single current figure; no route asks for a range.',
};

function routelessDatasets(): readonly DatasetId[] {
  const called = new Set<string>(BACKEND_CHAIN_DATA_ROUTES);
  return DATASET_IDS.filter(
    (id) =>
      !(DATASETS[id].routes as readonly string[]).some((route) =>
        called.has(route),
      ),
  );
}

/** Dataset → why this backend cannot serve it. */
export function notExposedByBackend(): Readonly<Record<string, string>> {
  const reasons: Record<string, string> = {};
  for (const id of routelessDatasets()) {
    reasons[id] =
      `No controller of this backend reads ${id}; it calls none of ` +
      `${(DATASETS[id].routes as readonly string[]).join(', ')}.`;
  }
  return { ...reasons, ...SHARES_A_ROUTE_BUT_UNREACHABLE };
}

/* ------------------------------------------------------------------------- */
/* Shared shorthands                                                          */
/* ------------------------------------------------------------------------- */

/**
 * The backend's in-memory pager: `page` and `pageSize` over a materialised
 * array, so it can seek anywhere and always knows the exact total.
 *
 * `cursor` is `rejected` rather than `ignored` because the legacy list routes
 * have no cursor parameter at all — see DRepController.getList.
 */
const IN_MEMORY_PAGING: PagingSupport = {
  cursor: 'rejected',
  offset: 'honoured', // `dreps.slice(page * pageSize, …)`
  maxLimit: null, // nothing clamps pageSize
  omittedLimitMeans: 'routeDefault',
  defaultLimit: 10, // `pageSize === undefined ? 10 : Number(pageSize)`
  total: 'exact', // `total = filtered.length` over the whole snapshot
};

/*
 * No service passes `expand` to a provider route, and no legacy route accepts
 * such a parameter, so every expand member is accepted-and-not-applied.
 *
 * `ignored` rather than `rejected`: nothing throws, the field is simply not
 * requestable, and `isOfferable` keeps an `ignored` member out of the UI's
 * control while leaving it visible in a support bundle. The records are
 * spelled out rather than built by a loop, so adding a member to the
 * contract's union fails this build instead of being quietly filled in.
 */

const NO_DREP_EXPAND: EnumSupport<DRepExpand> = {
  metadata: 'ignored',
  liveVotingPower: 'ignored',
  delegators: 'ignored',
  activity: 'ignored',
};

const NO_GOV_ACTION_EXPAND: EnumSupport<GovActionExpand> = {
  tallies: 'ignored',
  thresholds: 'ignored',
  metadata: 'ignored',
  myVote: 'ignored',
  protocolParams: 'ignored',
};

const NO_VOTE_EXPAND: EnumSupport<VoteExpand> = {
  votingPower: 'ignored',
  rationale: 'ignored',
  proposal: 'ignored',
};

/** /proposal/list has no status parameter, and none is forwarded. */
const NO_STATUS_FILTER: FilterSupport<GovActionStatus> = {
  values: {
    live: 'ignored',
    ratified: 'ignored',
    enacted: 'ignored',
    expired: 'ignored',
    dropped: 'ignored',
  },
};

/** /drep/getVotes has no vote-choice parameter. */
const NO_VOTE_CHOICE_FILTER: FilterSupport<VoteChoice> = {
  values: { yes: 'ignored', no: 'ignored', abstain: 'ignored' },
};

/** Every action type, filtered in memory by `ProposalService.filterByType`. */
const ALL_TYPES_HONOURED: FilterSupport<GovActionType> = {
  values: {
    ParameterChange: 'honoured',
    HardForkInitiation: 'honoured',
    TreasuryWithdrawals: 'honoured',
    NoConfidence: 'honoured',
    // The legacy wire spelling is db-sync's `NewCommittee`; the capability —
    // "this type is filterable" — is the same one.
    UpdateCommittee: 'honoured',
    NewConstitution: 'honoured',
    InfoAction: 'honoured',
  },
  // Applied over the whole snapshot, before paging, so a page is never short.
  exhaustive: true,
};

/**
 * A field is usable by the backend's in-memory sort and filter only when the
 * provider populates it on EVERY row of the route the backend calls.
 *
 * `onExpand` is not enough — no service passes `expand`, so an expand-gated
 * field never reaches the snapshot. `conditional` is not enough either: it
 * means "on some reads", and the declaration does not say which.
 */
function servedOnEveryRow(
  doc: ProviderCapabilityDocument,
  entity: EntityId,
  field: string,
  route: RouteId,
): boolean {
  const support: FieldSupport | undefined = fieldSupportFor(
    doc.entities,
    entity,
    field,
    route,
    doc.fieldOverrides,
  );
  return support !== undefined && support.serves === 'always';
}

/**
 * The backend supplies the ORDERING; the provider supplies the COLUMN. Sorting
 * by a key whose field the provider never fills is `ignored`, not `honoured` —
 * `sortDReps` runs, every value compares equal, and the caller gets an
 * unsorted list presented as sorted. That is exactly the case `OptionSupport`
 * named `ignored` for.
 */
function sortedInMemory(columnIsPopulated: boolean): OptionSupport {
  return columnIsPopulated ? 'honoured' : 'ignored';
}

/* ------------------------------------------------------------------------- */
/* The patch                                                                  */
/* ------------------------------------------------------------------------- */

/**
 * What this backend adds to, and takes away from, the provider's declaration.
 *
 * Derived from the provider's document rather than written as a constant,
 * because a compensation is only real when the data it works on is there:
 * `sort: 'activity'` is a working control on db-sync, whose directory row
 * always carries `votes_last_year`, and an empty gesture on a provider that
 * serves `DRep.activity` only under `expand`.
 */
export function backendCapabilityPatch(
  base: ProviderCapabilityDocument,
): Readonly<Partial<Record<DatasetId, Partial<AnyDatasetCapability>>>> {
  const patch: Partial<Record<DatasetId, Partial<AnyDatasetCapability>>> = {};

  /* -- RAISE: the DRep directory ------------------------------------------ */

  // DRepService.list() works from `getDRepListSnapShot`, which is `readAll`
  // over dreps.list — the whole directory, cached and warmed on every new
  // block by CacheWarmerService. Status filter, SoleVoter rule, sort and page
  // all run over that array, so none of them is the provider's to refuse.
  const drepSort: EnumSupport<DRepSort> = {
    // `DRep.votingPower` is a required (nullable) field, so the column is
    // always there; `sortDReps` treats null as -1.
    votingPower: 'honoured',
    registrationDate: sortedInMemory(
      servedOnEveryRow(
        base,
        'Registration',
        'registeredAt',
        'governance.dreps.list',
      ),
    ),
    activity: sortedInMemory(
      servedOnEveryRow(base, 'DRep', 'activity', 'governance.dreps.list'),
    ),
    status: sortedInMemory(
      servedOnEveryRow(base, 'Registration', 'status', 'governance.dreps.list'),
    ),
    // `seededHash` over `DRepListItem.drepId`, which is `DRep.hash` — required.
    random: 'honoured',
  };

  const statusIsReal = servedOnEveryRow(
    base,
    'Registration',
    'status',
    'governance.dreps.list',
  );
  const drepStatusFilter: FilterSupport<DRepStatus> = {
    values: statusIsReal
      ? { active: 'honoured', inactive: 'honoured', retired: 'honoured' }
      : // `toLegacyListItem` defaults an absent status to 'Inactive', so
        // filtering would partition the directory by a value the provider
        // never supplied.
        { active: 'ignored', inactive: 'ignored', retired: 'ignored' },
    // Any combination: `params.status.includes(drep.status)` over the array.
    exhaustive: true,
  };

  const drepBase = readCapability(base.datasets, 'drep.identity.current');
  patch['drep.identity.current'] = {
    // CacheWarmerService refreshes the default snapshot whenever the tip
    // moves, so a consumer may poll this even when the provider may not.
    pollable: true,
    paging: IN_MEMORY_PAGING,
    sort: drepSort,
    // `kind` is carried over untouched: /drep/list has no kind parameter, it
    // applies GovTool's fixed SoleVoter rule instead.
    filters: { ...drepBase.filters, status: drepStatusFilter },
    // `search` is NOT patched: DRepService passes it straight to the provider
    // (`dreps.list({ ...page, search })`) and only post-filters direct voters.
    expand: NO_DREP_EXPAND,
  };

  /* -- RAISE: the governance-action list ---------------------------------- */

  // ProposalService.list() always reads the '' snapshot and filters it in
  // memory, so every control below is the backend's, not the provider's.
  const proposalSort: EnumSupport<GovActionSort> = {
    newest: sortedInMemory(
      servedOnEveryRow(
        base,
        'GovActionLifecycle',
        'submitted',
        'governance.proposals.list',
      ),
    ),
    // `GovernanceActionSortMode` has no 'Oldest' member and `sortProposals`
    // has no case for it, so it falls through `default: return copied`.
    oldest: 'ignored',
    // `lifecycle.expires` is a required (nullable) field; nulls sort last.
    soonestToExpire: 'honoured',
    mostYesVotes: sortedInMemory(
      servedOnEveryRow(
        base,
        'GovAction',
        'tallies',
        'governance.proposals.list',
      ),
    ),
    // The same `default: return copied` — an unsorted list presented as sorted.
    highestParticipation: 'ignored',
  };

  const proposalBase = readCapability(
    base.datasets,
    'proposal.identity.current',
  );
  patch['proposal.identity.current'] = {
    pollable: true,
    paging: IN_MEMORY_PAGING,
    sort: proposalSort,
    filters: {
      ...proposalBase.filters,
      type: ALL_TYPES_HONOURED,
      // ProposalService never forwards a status filter, so whatever the
      // provider could do with one is unreachable through this backend.
      status: NO_STATUS_FILTER,
    },
    // RAISE: `filterBySearch` matches `txHash#index` and the four metadata
    // strings over the whole snapshot, so free text works on a provider with
    // no text index. Ada Handles are resolved nowhere in this backend.
    search: {
      modes: {
        exactId: 'honoured',
        freeText: 'honoured',
        adaHandle: 'rejected',
      },
      emptyStringAccepted: true,
    } satisfies SearchSupport,
    // LOWER: /proposal/list and /proposal/get accept `drepId`, validate it as
    // hex and then drop it — `ProposalService.get` returns `vote: null`
    // unconditionally. The badge is never populated, and nothing says so.
    joins: { callerVote: 'ignored' },
    expand: NO_GOV_ACTION_EXPAND,
  };

  /* -- LOWER: a DRep's own vote history ----------------------------------- */

  // `DRepService.getVotes` calls `dreps.listVotes(drepId)` with NO page
  // request and, unlike the two snapshot paths, without `readAll` — so only
  // the provider's first page is ever seen, and the response is then an
  // unpaged array. On a provider that caps a page this silently truncates.
  const ballotBase = readCapability(base.datasets, 'drep.ballot.current');
  patch['drep.ballot.current'] = {
    paging: {
      cursor: 'rejected',
      offset: 'rejected',
      maxLimit: null,
      omittedLimitMeans: 'everything',
      total: 'absent',
    },
    filters: {
      ...ballotBase.filters,
      // Applied in memory by `proposalService.filterByType` on the joined
      // proposal, so every type works whatever the provider indexes.
      proposalType: ALL_TYPES_HONOURED,
      vote: NO_VOTE_CHOICE_FILTER,
    },
    expand: NO_VOTE_EXPAND,
    // Prepended, not replaced: `composeCapabilities` merges a dataset key by
    // key, so assigning `caveats` would drop the provider's own — db-sync
    // already warns that this route omits votes on concluded actions.
    caveats: [
      {
        kind: 'notExhaustive',
        note:
          'DRepService.getVotes calls governance.dreps.listVotes(drepId) with ' +
          'no PageRequest and does not use readAll, so a provider that caps a ' +
          'page returns a truncated voting record with no error and no cursor ' +
          'on the wire. The DRep and proposal snapshots do follow the cursor.',
      },
      ...(ballotBase.caveats ?? []),
    ],
  };

  /* -- LOWER: routes this backend does not have --------------------------- */

  const notExposed = notExposedByBackend();
  for (const id of DATASET_IDS) {
    const reason = notExposed[id];
    if (reason === undefined) {
      continue;
    }
    // Only when the provider could actually serve it. A dataset the provider
    // already refuses keeps ITS reason, which is the more informative one —
    // "no known source records this" outranks "this backend has no route".
    if (readCapability(base.datasets, id).reachability !== 'served') {
      continue;
    }
    patch[id] = {
      reachability: 'refused',
      unavailable: {
        kind: 'notImplemented',
        // The limit is this build of the backend, not the chain and not the
        // box: adding the route is a code change here.
        scope: 'source',
        reason,
      },
    };
  }

  return patch;
}

/* ------------------------------------------------------------------------- */
/* Entity-level lowering: absence laundered into a plausible value            */
/* ------------------------------------------------------------------------- */

/**
 * Required fields this backend fills with a number that reads as fact.
 *
 * These are `misreported`, not `unfillable`: the value IS sent, and a consumer
 * that trusts it is misled. Each is a legacy-wire concession — the frontend
 * renders the number directly and has no error path — which is a reason to
 * declare it, not a reason to hide it.
 */
const ACCOUNT_VOTING_POWER_ZERO: MisreportedField<'VotingPower'> = {
  field: 'amount',
  sends: '0',
  wouldMean: 'the stake key controls no voting power',
  note:
    'AdaHolderService.getVotingPower wraps the provider call in `try { … } ' +
    'catch { return 0 }` and also maps a null result to 0, so a provider ' +
    'outage, a capability refusal and a genuinely empty account are the same ' +
    'zero on /ada-holder/get-voting-power. This is the shape of the incident ' +
    'the contract cites: a dead `utxo_view` rendered as 0 ada as fact.',
};

const DREP_VOTING_POWER_ZERO: MisreportedField<'DRepVotingPowerEntry'> = {
  field: 'votingPower',
  sends: '0',
  wouldMean: 'the DRep controls no stake',
  note:
    'DRepService.getVotingPowerList maps `entry.votingPower === null` to 0, ' +
    'so "the provider does not know" and "nobody delegated" are the same ' +
    'number on /drep/voting-power-list.',
};

const REGISTRATION_DEPOSIT_ZERO: MisreportedField<'Registration'> = {
  field: 'deposit',
  sends: '0',
  wouldMean: 'the DRep registered with a zero-lovelace deposit',
  note:
    'DRepService.toLegacyListItem does `toLegacyNumber(drep.registration.' +
    'deposit ?? 0)` because the legacy field is a required number. ' +
    '/drep/info keeps the null (`toLegacyNullableNumber`), so the two routes ' +
    'disagree about the same DRep.',
};

function withMisreport<E extends EntityId>(
  declaration: EntityDeclaration<E>,
  added: MisreportedField<E>,
): EntityDeclaration<E> {
  return {
    ...declaration,
    misreported: [...(declaration.misreported ?? []), added],
  };
}

function backendEntities(base: EntityDeclarations): EntityDeclarations {
  return {
    ...base,
    VotingPower: withMisreport(base.VotingPower, ACCOUNT_VOTING_POWER_ZERO),
    DRepVotingPowerEntry: withMisreport(
      base.DRepVotingPowerEntry,
      DREP_VOTING_POWER_ZERO,
    ),
    Registration: withMisreport(base.Registration, REGISTRATION_DEPOSIT_ZERO),
  };
}

/* ------------------------------------------------------------------------- */
/* The composed document                                                      */
/* ------------------------------------------------------------------------- */

/**
 * The provider's document plus this backend's own raises and losses.
 *
 * `overrides` is carried across untouched: a deployment fault the provider
 * reports (db-sync's missing `utxo_view`) is applied by `resolveCapabilities`
 * AFTER this composition, so a fault still wins over a compensation — the
 * backend cannot sort rows it could not read.
 */
export function composeBackendCapabilities(
  base: ProviderCapabilityDocument,
): ProviderCapabilityDocument {
  return {
    schemaVersion: 2,
    provider: BACKEND_PROVIDER_ID,
    network: base.network,
    providerVersion: BACKEND_VERSION,
    generatedAt: BACKEND_CAPABILITIES_REVIEWED_AT,
    datasets: composeCapabilities(base.datasets, backendCapabilityPatch(base)),
    entities: backendEntities(base.entities),
    fieldOverrides: base.fieldOverrides,
    unreviewed: base.unreviewed,
    overrides: base.overrides,
    // The backend adds no out-of-contract method of its own; a provider
    // extension stays declared, since a consumer reading this document is the
    // one that would have to stop using it.
    extensions: base.extensions,
    metadata: base.metadata,
    composedFrom: [{ provider: base.provider, network: base.network }],
  };
}

/**
 * Digest of the document a feature set was derived from.
 *
 * A consumer that gets a 501 for a feature this set calls available refetches
 * and compares digests instead of guessing whether its toggles are stale.
 */
export function capabilityDigest(doc: ProviderCapabilityDocument): string {
  return `sha256:${createHash('sha256').update(JSON.stringify(doc)).digest('hex')}`;
}

/** What the browser fetches at boot. */
export function backendFeatures(doc: ProviderCapabilityDocument): FeatureSet {
  return deriveFeatures(doc, capabilityDigest(doc));
}
