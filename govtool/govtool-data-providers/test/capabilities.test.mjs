/**
 * Runtime behaviour of the capability derivation.
 *
 * `capabilities.example.ts` proves a declaration COMPILES; this proves the
 * derivation COMPUTES the right answers, which is the half a type system
 * cannot check. It runs against ./../dist, so `npm run build` comes first —
 * `npm test` does both.
 *
 * The three cases asserted here are the ones the feature set exists for:
 *   (a) a capability no source has        -> off, and marked permanent
 *   (b) a capability one provider has     -> on
 *   (c) one refused member of a control   -> the control stays, the item goes
 */

import assert from 'node:assert/strict';
import { test } from 'node:test';

import {
  DATASET_IDS,
  DATASETS,
  UNIVERSAL_GAPS,
  declarationProblems,
  declareCapabilities,
  deriveFeatures,
  refusalIsDeclared,
  resolveCapabilities,
  routesFor,
  unknownFeatureState,
} from '../dist/chain-data/index.js';

/** The smallest entity table the derivation will accept. */
const emptyEntities = () => {
  const names = [
    'NetworkInfo', 'EpochSummary', 'BlockSummary', 'ProtocolParams',
    'StakeDistribution', 'Treasury', 'StakeBalance', 'VotingPower', 'Account',
    'Delegation', 'PoolDelegation', 'StakeRegistrationEvent',
    'DelegationHistoryEvent', 'DRep', 'Registration', 'DRepActivity',
    'DRepDelegator', 'DRepHistoryEvent', 'DRepVotingPowerEntry', 'SpoVoter',
    'CommitteeMember', 'Committee', 'Constitution', 'GovAction',
    'GovActionLifecycle', 'GovActionActivityEvent', 'EnactedActionSummary',
    'RoleTally', 'VoteRecord', 'GovernanceMetrics', 'TransactionState',
    'SurveyDefinition',
  ];
  return Object.fromEntries(names.map((n) => [n, { fields: {} }]));
};

const doc = (datasets, entities = emptyEntities(), extra = {}) => ({
  schemaVersion: 2,
  provider: 'test',
  network: 'preview',
  providerVersion: '0.0.0',
  generatedAt: '2026-09-18T00:00:00Z',
  datasets: declareCapabilities(datasets, []),
  entities,
  fieldOverrides: [],
  unreviewed: [],
  overrides: [],
  extensions: [],
  metadata: {
    resolvedBy: 'metadataService',
    validatesAgainstStandard: false,
    carriesFailureMessage: false,
  },
  ...extra,
});

const served = (over = {}) => ({
  reachability: 'served',
  cost: 'indexed',
  pollable: true,
  ...over,
});

test('every route in the contract belongs to a dataset', () => {
  const covered = new Set();
  for (const id of DATASET_IDS) {
    for (const route of routesFor(id)) covered.add(route);
  }
  // 40 routes across the five capability-gated namespaces.
  assert.equal(covered.size, 40);
});

test('an undeclared dataset is refused, never silently claimed', () => {
  const features = deriveFeatures(doc({}), 'x');
  assert.equal(features.features['drepDirectory.browse'].available, false);
  assert.equal(
    features.features['drepDirectory.browse'].blockedBy.absence,
    'notImplemented',
  );
});

test('(a) a gap no source has is permanent, not merely off', () => {
  const state = deriveFeatures(doc({}), 'x').features['drep.delegationTimeline'];
  assert.equal(state.available, false);
  assert.equal(state.permanentlyAbsent, true);
  assert.equal(state.blockedBy.cause, 'permanent');
  // The registry owns this, so three providers cannot disagree about it.
  assert.ok(UNIVERSAL_GAPS.includes('drep.delegation.events'));
  assert.equal(DATASETS['drep.delegation.events'].hasKnownSource, false);
});

test('(b) a served vote listing is available and pollable', () => {
  const state = deriveFeatures(
    doc({ 'proposal.ballot.current': served() }),
    'x',
  ).features['govAction.voterList'];
  assert.equal(state.available, true);
  assert.equal(state.refresh, 'poll');
});

test('(c) one refused sort member hides that item, not the control', () => {
  const state = deriveFeatures(
    doc({
      'proposal.ballot.current': served({
        sort: { newest: 'honoured', oldest: 'honoured', votingPower: 'rejected' },
      }),
    }),
    'x',
  ).features['govAction.voterList'];
  assert.deepEqual(state.options.sort.allowed, ['newest', 'oldest']);
  assert.equal(state.options.sort.defaultTo, 'newest');
});

test('every sort member refused hides the whole control', () => {
  const state = deriveFeatures(
    doc({
      'drep.identity.current': served({
        sort: {
          votingPower: 'rejected',
          registrationDate: 'rejected',
          activity: 'rejected',
          status: 'rejected',
          random: 'rejected',
        },
      }),
    }),
    'x',
  ).features['drepDirectory.browse'];
  assert.equal(state.available, true);
  assert.deepEqual(state.options.sort.allowed, []);
  assert.equal(state.options.sort.defaultTo, null);
});

test('an ignored option is never offered', () => {
  // db-sync accepts `highestParticipation` and silently does not apply it,
  // which is worse than refusing: the list looks ordered and is not.
  const state = deriveFeatures(
    doc({
      'proposal.identity.current': served({
        sort: {
          newest: 'honoured',
          oldest: 'honoured',
          soonestToExpire: 'honoured',
          mostYesVotes: 'honoured',
          highestParticipation: 'ignored',
        },
      }),
    }),
    'x',
  ).features['govActionList.browse'];
  assert.ok(!state.options.sort.allowed.includes('highestParticipation'));
  assert.deepEqual(state.options.sort.ignored, ['highestParticipation']);
});

test('filter cardinality reaches the consumer', () => {
  const state = deriveFeatures(
    doc({
      'proposal.identity.current': served({
        filters: {
          status: {
            values: {
              live: 'honoured',
              ratified: 'honoured',
              enacted: 'honoured',
              expired: 'honoured',
              dropped: 'honoured',
            },
            maxSelected: 1,
          },
        },
      }),
    }),
    'x',
  ).features['govActionList.browse'];
  assert.equal(state.options.status.maxSelected, 1);
});

test('a cost refusal names a typed fallback', () => {
  const state = deriveFeatures(
    doc({
      'drep.delegation.current': {
        reachability: 'refused',
        cost: 'walk',
        pollable: false,
        unavailable: {
          kind: 'tooExpensive',
          scope: 'source',
          wouldCost: 'walk',
          fallback: 'drep.identity.current',
          reason: 'one request per delegator',
        },
      },
    }),
    'x',
  ).features['drep.delegatorList'];
  assert.equal(state.blockedBy.cause, 'cost');
  assert.equal(state.blockedBy.fallback, 'drep.identity.current');
});

// Support is binary: an expensive read is either declared `refused` with
// `kind: 'tooExpensive'`, or it is served and the provider says whether it
// tolerates an interval. There is no third, "served but expensive" grade.
test('a read a provider does not want polled is respected', () => {
  const state = deriveFeatures(
    doc({ 'drep.ballot.current': served({ pollable: false }) }),
    'x',
  ).features['drep.voteHistory'];
  assert.equal(state.refresh, 'userInitiated');
});

test('a tally with counts only switches units instead of hiding', () => {
  const entities = emptyEntities();
  entities.RoleTally = {
    fields: {
      stake: { serves: 'never', cause: 'notInSource', whenRequested: 'ignored', note: 'counts only' },
      count: { serves: 'always' },
      notVotedStake: { serves: 'never', cause: 'notInSource', whenRequested: 'ignored', note: 'x' },
      totalEligibleStake: { serves: 'never', cause: 'notInSource', whenRequested: 'ignored', note: 'x' },
      threshold: { serves: 'never', cause: 'notInSource', whenRequested: 'ignored', note: 'x' },
      passing: { serves: 'never', cause: 'notInSource', whenRequested: 'ignored', note: 'x' },
    },
  };
  const set = deriveFeatures(
    doc({ 'proposal.tally.current': served() }, entities),
    'x',
  );
  assert.equal(set.features['govAction.tally'].available, true);
  assert.equal(set.features['govAction.tally'].mode, 'count');
  // The threshold line and the not-voted segment go; the bars stay.
  assert.equal(set.features['govAction.tallyThreshold'].available, false);
  assert.equal(set.features['govAction.tallyNotVoted'].available, false);
});

test('a deployment fault demotes a statically-declared capability', () => {
  const base = doc({ 'transaction.identity.current': served() });
  const faulted = {
    ...base,
    overrides: [
      {
        dataset: 'transaction.identity.current',
        reachability: 'refused',
        unavailable: {
          kind: 'deploymentFault',
          scope: 'deployment',
          reason: '/txs/{hash} returns 500 on this deployment',
          symptom: 'HTTP 500 for every hash',
          observedAt: '2026-09-18T00:00:00Z',
        },
      },
    ],
  };
  assert.equal(
    resolveCapabilities(faulted)['transaction.identity.current'].reachability,
    'refused',
  );
  const set = deriveFeatures(faulted, 'x');
  assert.equal(set.features['transaction.confirmation'].available, false);
  assert.equal(set.features['transaction.confirmation'].blockedBy.cause, 'deployment');
  // Core tier: a banner, not a hidden widget.
  assert.ok(set.brokenCore.includes('transaction.confirmation'));
});

test('"not loaded yet" is distinguishable from a refusal', () => {
  const unknown = unknownFeatureState('govAction.voterList');
  assert.equal(unknown.available, false);
  assert.equal(unknown.blockedBy.cause, 'unknown');
  assert.deepEqual(unknown.options.sort.allowed, []);
});

test('declarationProblems catches a deployment fault in the static table', () => {
  const problems = declarationProblems(
    doc({
      'transaction.identity.current': {
        reachability: 'refused',
        cost: 'indexed',
        pollable: false,
        unavailable: {
          kind: 'deploymentFault',
          scope: 'deployment',
          reason: 'x',
          symptom: 'HTTP 500',
        },
      },
    }),
  );
  assert.ok(problems.some((p) => p.includes('belongs')));
});

test('refusalIsDeclared covers every control kind', () => {
  const entities = emptyEntities();
  entities.RoleTally = {
    fields: {
      threshold: { serves: 'never', cause: 'representation', whenRequested: 'ignored', note: 'float' },
    },
  };
  const table = resolveCapabilities(
    doc(
      {
        'proposal.identity.current': served({
          sort: {
            newest: 'honoured',
            oldest: 'honoured',
            soonestToExpire: 'honoured',
            mostYesVotes: 'rejected',
            highestParticipation: 'rejected',
          },
          filters: {
            status: {
              values: {
                live: 'honoured',
                ratified: 'rejected',
                enacted: 'rejected',
                expired: 'rejected',
                dropped: 'rejected',
              },
              maxSelected: 1,
            },
          },
          expand: {
            tallies: 'honoured',
            thresholds: 'rejected',
            metadata: 'honoured',
            myVote: 'rejected',
            protocolParams: 'rejected',
          },
          joins: { callerVote: 'rejected' },
          search: {
            modes: { exactId: 'honoured', freeText: 'rejected', adaHandle: 'rejected' },
            emptyStringAccepted: true,
          },
          paging: {
            cursor: 'honoured',
            offset: 'rejected',
            maxLimit: 100,
            omittedLimitMeans: 'routeDefault',
            total: 'absent',
          },
        }),
        'drep.stake.current': served({
          basis: { active: 'honoured', live: 'rejected' },
          batch: { explicitIds: 'honoured', allIds: 'rejected' },
        }),
        'proposal.tally.current': served(),
      },
      entities,
    ),
  );

  const declared = (dataset, control) =>
    refusalIsDeclared(table, entities, {
      dataset,
      control,
      cause: 'notInSource',
      scope: 'source',
      reason: 'x',
    });

  // Each of these is a real refusal shape seen in one of the three providers.
  assert.ok(declared('proposal.identity.current', { kind: 'sort', key: 'mostYesVotes' }));
  assert.ok(declared('proposal.identity.current', { kind: 'filter', name: 'status', value: 'enacted' }));
  assert.ok(declared('proposal.identity.current', { kind: 'filterCardinality', name: 'status', max: 1 }));
  assert.ok(declared('proposal.identity.current', { kind: 'expand', field: 'thresholds' }));
  assert.ok(declared('proposal.identity.current', { kind: 'join', join: 'callerVote' }));
  assert.ok(declared('proposal.identity.current', { kind: 'search', mode: 'freeText' }));
  assert.ok(declared('proposal.identity.current', { kind: 'paging', option: 'offset' }));
  assert.ok(declared('proposal.identity.current', { kind: 'paging', option: 'unboundedLimit' }));
  assert.ok(declared('drep.stake.current', { kind: 'basis', basis: 'live' }));
  assert.ok(declared('drep.stake.current', { kind: 'batch', form: 'all' }));
  assert.ok(declared('proposal.tally.current', { kind: 'field', entity: 'RoleTally', field: 'threshold' }));

  // …and a refusal the declaration does NOT predict is reported as drift.
  assert.equal(declared('proposal.identity.current', { kind: 'sort', key: 'newest' }), false);
  assert.equal(declared('drep.stake.current', { kind: 'basis', basis: 'active' }), false);
  assert.equal(declared('proposal.tally.current', { kind: 'dataset' }), false);
});
