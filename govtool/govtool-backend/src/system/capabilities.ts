/**
 * The feature set THIS BACKEND serves at `GET /system/features`.
 *
 * The backend is not a proxy. It sits between the provider and the browser and
 * changes what is true in both directions, so forwarding the provider's own
 * declaration would disable features that work and enable features that do not:
 *
 *   RAISES  the DRep directory and the governance-action list are read WHOLE
 *           into a cached snapshot (`readAll` + `CacheService`) and then
 *           filtered, searched, sorted and paged in memory. A provider that
 *           honours no sort key at all still gets a working sort menu, and a
 *           provider with no text index still gets free-text search.
 *
 *   LOWERS  a feature whose route this backend does not expose is unreachable
 *           however good the provider is, and a parameter the legacy route
 *           accepts and then drops is worse than one it rejects.
 *
 * The vocabulary below — features, controls, caveats — is this backend's own.
 * A provider declares only which OPTION VALUES it honours; whether something
 * is available at all is the shape of the interface, which is not a document
 * and cannot travel over HTTP. So the availability half is assembled here,
 * from what this backend's routes actually do.
 *
 * Every entry is transcribed from a service in `src/`, and pinned by
 * `test/capabilities.spec.ts`: this is hand-maintained, so each compensation
 * needs a test that fails when the code stops doing it.
 */

import { createHash } from 'node:crypto';
import type {
  NetworkId,
  ProviderCapabilities,
} from '@govtool/data-providers/chain-data';

export const BACKEND_PROVIDER_ID = 'govtool-backend';
export const BACKEND_VERSION = '0.0.1';

/**
 * When this was last reviewed against `src/`. A constant rather than
 * `new Date()`, so the digest a consumer compares does not change on restart.
 */
export const BACKEND_CAPABILITIES_REVIEWED_AT = '2026-09-18T00:00:00Z';

/* ------------------------------------------------------------------------- */
/* Vocabulary                                                                 */
/* ------------------------------------------------------------------------- */

/** A list control a client renders, and the backend narrows. */
export type ControlId =
  | 'drepDirectory.sort'
  | 'drepDirectory.status'
  | 'drepDirectory.search'
  | 'govActionList.sort'
  | 'govActionList.type'
  | 'govActionList.status';

/** A whole named surface, either reachable through this backend or not. */
export type FeatureId =
  | 'account.currentDelegation'
  | 'account.delegationHistory'
  | 'account.stakeEvents'
  | 'account.votingPower'
  | 'committee.browse'
  | 'committee.constitution'
  | 'dashboard.metrics'
  | 'drep.delegatorList'
  | 'drep.directory'
  | 'drep.registrationHistory'
  | 'drep.voteHistory'
  | 'drep.votingPowerHistory'
  | 'govAction.activityTimeline'
  | 'govAction.list'
  | 'govAction.myVoteBadge'
  | 'govAction.tally'
  | 'govAction.voterList'
  | 'network.epochBrowser'
  | 'network.treasury'
  | 'spo.directory'
  | 'spo.voteHistory'
  | 'voter.resolve';

export interface Unavailable {
  cause: 'notImplemented' | 'noSource';
  reason: string;
}

/**
 * A loss on a feature that still answers. The call succeeds and the value is
 * well-formed, which is exactly why it needs saying.
 */
export interface Caveat {
  feature: FeatureId;
  kind: 'notExhaustive' | 'impliedFilter' | 'boundedWindow' | 'derived';
  /** For `impliedFilter`: the parameter that is accepted and then ignored. */
  param?: string;
  restrictedTo?: readonly string[];
  note: string;
}

export interface FeatureSet {
  provider: string;
  network: NetworkId;
  generatedAt: string;
  unavailable: Partial<Record<FeatureId, Unavailable>>;
  /** An absent control accepts the client's whole universe. */
  options: Partial<Record<ControlId, readonly string[]>>;
  caveats: readonly Caveat[];
}

/* ------------------------------------------------------------------------- */
/* LOWER — features this backend has no route for                             */
/* ------------------------------------------------------------------------- */

/**
 * The legacy GovTool HTTP surface is much narrower than the contract. These
 * features are unreachable through this backend whatever the provider can do,
 * so they are declared unavailable regardless of what the provider can serve.
 *
 * `test/capabilities.spec.ts` greps `src/` for `this.chain.*` call sites and
 * fails if a feature listed here turns out to have a route after all.
 */
const NO_ROUTE: Readonly<Partial<Record<FeatureId, Unavailable>>> = {
  'network.treasury': notExposed('no /treasury route'),
  'network.epochBrowser': notExposed('no epoch or block listing route'),
  'drep.votingPowerHistory': notExposed(
    'DRepService.getVotingPower reads the single current figure off the DRep; ' +
      'no route takes an epoch range',
  ),
  'drep.delegatorList': notExposed('no /drep/{id}/delegators route'),
  'drep.registrationHistory': notExposed('no /drep/{id}/history route'),
  'account.delegationHistory': notExposed('no delegation history route'),
  'account.stakeEvents': notExposed('no stake certificate route'),
  'committee.browse': notExposed(
    'this backend has no /committee/* routes; /network/metrics reads the ' +
      'committee only for its size and quorum',
  ),
  'committee.constitution': notExposed('no /constitution route'),
  'spo.directory': notExposed('no /spo/* routes'),
  'spo.voteHistory': notExposed('no /spo/{id}/votes route'),
  'govAction.voterList': notExposed('no per-action vote listing route'),
  'govAction.tally': notExposed(
    'the aggregates are served inline on a proposal; there is no route that ' +
      'asks for them on their own',
  ),
  'govAction.activityTimeline': notExposed('no lifecycle feed route'),
  'voter.resolve': notExposed('no role-agnostic /voter route'),
};

function notExposed(what: string): Unavailable {
  return {
    cause: 'notImplemented',
    reason: `This backend does not expose it: ${what}.`,
  };
}

/* ------------------------------------------------------------------------- */
/* RAISE — capability the backend supplies itself                             */
/* ------------------------------------------------------------------------- */

/**
 * Controls the backend applies to its OWN in-memory snapshot, so the
 * provider's restriction does not reach the browser.
 *
 * `DRepService.sortDReps` and `ProposalService` sort, filter and page the
 * whole cached collection, and `filterBySearch` matches action ids across it.
 * Omitting a control from this list means the provider's answer stands.
 */
const SUPPLIED_BY_BACKEND: readonly ControlId[] = [
  'drepDirectory.sort',
  'drepDirectory.status',
  'drepDirectory.search',
  'govActionList.sort',
  'govActionList.type',
  'govActionList.status',
];

/* ------------------------------------------------------------------------- */
/* LOWER — what the backend's own answers mean                                */
/* ------------------------------------------------------------------------- */

/**
 * Losses the backend introduces itself, each transcribed from a call site.
 * These are caveats rather than refusals: the call succeeds and the value is
 * well-formed, which is exactly why they need saying.
 */
const BACKEND_CAVEATS: readonly Caveat[] = [
  {
    feature: 'account.votingPower',
    kind: 'notExhaustive',
    note:
      'AdaHolderService.getVotingPower catches every failure and returns 0, ' +
      'because /ada-holder/get-voting-power has always answered 0 rather ' +
      'than erroring. A zero here is not evidence of zero stake.',
  },
  {
    feature: 'govAction.myVoteBadge',
    kind: 'impliedFilter',
    param: 'drepId',
    restrictedTo: [],
    note:
      '/proposal/list and /proposal/get accept `drepId`, validate it and then ' +
      'drop it — ProposalService.get returns `vote: null` unconditionally, so ' +
      'the badge is never populated.',
  },
  {
    feature: 'dashboard.metrics',
    kind: 'derived',
    note:
      'There is no metrics resource any more. NetworkService.getNetworkMetrics ' +
      'assembles the counters from the DRep counts, the proposal total, the ' +
      'committee and the stake distribution. The five no resource owns — ' +
      'delegator and delegation counts, DRep vote count, CIP-119 compliance ' +
      'and direct voters — are reported as 0, and so is the DRep stake total ' +
      'under a provider that cannot aggregate it.',
  },
  {
    feature: 'drep.directory',
    kind: 'derived',
    note:
      'Every metadata-derived field on a directory row — name, objectives, ' +
      'image, references — is null. Chain data emits the anchor and never ' +
      'resolves it, and no metadata service is wired into this backend.',
  },
];

/* ------------------------------------------------------------------------- */
/* Composition                                                                */
/* ------------------------------------------------------------------------- */

/**
 * The provider's declaration as client-facing controls.
 *
 * A filter the provider does not accept becomes an empty option list, which is
 * "hide the control" rather than "render an empty menu". A filter it does
 * accept has no restriction to state, so the control is omitted entirely.
 */
function providerControls(
  capabilities: ProviderCapabilities,
): Partial<Record<ControlId, readonly string[]>> {
  const filter = (
    accepted: readonly string[],
    name: string,
  ): readonly string[] | undefined =>
    accepted.includes(name) ? undefined : [];

  const controls: Partial<Record<ControlId, readonly string[]>> = {
    'drepDirectory.sort': capabilities.sorts.dreps,
    'drepDirectory.search': capabilities.search,
    'govActionList.sort': capabilities.sorts.proposals,
  };

  const drepStatus = filter(capabilities.filters.dreps, 'status');
  if (drepStatus !== undefined) controls['drepDirectory.status'] = drepStatus;

  const actionType = filter(capabilities.filters.proposals, 'type');
  if (actionType !== undefined) controls['govActionList.type'] = actionType;

  const actionStatus = filter(capabilities.filters.proposals, 'status');
  if (actionStatus !== undefined)
    controls['govActionList.status'] = actionStatus;

  return controls;
}

/**
 * The provider's declaration, with this backend's own additions and removals.
 *
 * `network` comes from the envelope the declaration arrived in: a capability
 * claim is about a deployment, and which chain that deployment follows is part
 * of naming it.
 */
export function backendFeatures(
  provider: ProviderCapabilities,
  network: NetworkId,
): FeatureSet {
  const options = providerControls(provider);
  for (const control of SUPPLIED_BY_BACKEND) {
    // Deleting the key is how "every value works" is spelled: an absent
    // control accepts the UI's whole universe.
    delete options[control];
  }

  const unavailable: Partial<Record<FeatureId, Unavailable>> = {};
  for (const [feature, why] of Object.entries(NO_ROUTE) as [
    FeatureId,
    Unavailable,
  ][]) {
    unavailable[feature] = why;
  }

  return {
    provider: BACKEND_PROVIDER_ID,
    network,
    generatedAt: BACKEND_CAPABILITIES_REVIEWED_AT,
    unavailable,
    options,
    caveats: BACKEND_CAVEATS.filter(
      (caveat) => unavailable[caveat.feature] === undefined,
    ),
  };
}

/* ------------------------------------------------------------------------- */
/* Reading a feature set                                                      */
/* ------------------------------------------------------------------------- */

/**
 * Whether a whole surface is reachable. Fails OPEN on an absent set, which is
 * the still-loading and fetch-failed case: an unreachable `/system/features`
 * must never hide a working screen.
 */
export function isAvailable(
  set: FeatureSet | undefined,
  feature: FeatureId,
): boolean {
  return set === undefined || set.unavailable[feature] === undefined;
}

/** Narrow a client's own option list to what is honoured, in the client's order. */
export function allowedOptions(
  set: FeatureSet | undefined,
  control: ControlId,
  universe: readonly string[],
): readonly string[] {
  const supported = set?.options[control];
  if (supported === undefined) return universe;
  return universe.filter((value) => supported.includes(value));
}

/**
 * A digest of the feature set, for a consumer to compare after a 501 on
 * something the set said was available: a mismatch means its copy is stale and
 * it should refetch rather than guess.
 */
export function capabilityDigest(set: FeatureSet): string {
  return `sha256:${createHash('sha256')
    .update(JSON.stringify(set))
    .digest('hex')
    .slice(0, 16)}`;
}
