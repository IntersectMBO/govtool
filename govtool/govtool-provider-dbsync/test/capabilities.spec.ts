/**
 * The drift cross-check.
 *
 * Two failure modes this closes, both of which the old
 * `Record<string, CapabilityLevel>` table shipped with:
 *
 *   1. A declaration that contradicts itself — served and carrying a reason,
 *    refused and silent, a deployment fault frozen into the constant.
 *    `declarationProblems` is the gate.
 *   2. A refusal the code throws and the declaration does not predict. Every
 *    `unsupported(...)` site in `src/api/` has an entry in `DBSYNC_REFUSALS`,
 *    and `refusalIsDeclared` proves the table already says so.
 */

import type { DatasetId } from '@govtool/data-providers/chain-data';
import {
  DATASET_IDS,
  declarationProblems,
  deriveFeatures,
  fieldSupportFor,
  isRequestable,
  isRouteCallable,
  readCapability,
  refusalIsDeclared,
  refusalKey,
  resolveCapabilities,
} from '@govtool/data-providers/chain-data';

import {
  DBSYNC_REFUSALS,
  dbSyncCapabilities,
  missingUtxoViewOverride,
} from '../src/capabilities';

const doc = dbSyncCapabilities('mainnet');

describe('the declaration is internally consistent', () => {
  it('has no declaration problems', () => {
    expect(declarationProblems(doc)).toEqual([]);
  });

  it('assesses every dataset', () => {
    expect(doc.unreviewed).toEqual([]);
  });

  it('gives a reason for everything it does not serve', () => {
    const silent = DATASET_IDS.filter((id) => {
      const cap = readCapability(doc.datasets, id);
      return cap.reachability !== 'served' && cap.unavailable === undefined;
    });
    expect(silent).toEqual([]);
  });

  it('keeps the deployment fault out of the static table', () => {
    const baked = DATASET_IDS.filter(
      (id) =>
        readCapability(doc.datasets, id).unavailable?.kind ===
        'deploymentFault',
    );
    expect(baked).toEqual([]);

    // …and it still resolves when a probe supplies it.
    const probed = dbSyncCapabilities('preview', [missingUtxoViewOverride()]);
    expect(declarationProblems(probed)).toEqual([]);
    expect(
      resolveCapabilities(probed)['account.stake.current'].reachability,
    ).toBe('refused');
  });
});

describe('every refusal the provider can throw is declared', () => {
  const table = resolveCapabilities(doc);

  it.each(DBSYNC_REFUSALS.map((r) => [refusalKey(r), r] as const))(
    '%s',
    (_key, refusal) => {
      expect(refusalIsDeclared(table, doc.entities, refusal)).toBe(true);
    },
  );

  it('names a refusal for every dataset it declares refused', () => {
    // The reverse direction: a table entry with no refusal site behind it was
    // the other half of the old drift (16 such entries at the last survey).
    // Datasets refused through a route rather than wholesale are excluded.
    const refusedDatasets = new Set(
      DATASET_IDS.filter(
        (id) => readCapability(doc.datasets, id).reachability !== 'served',
      ),
    );
    const cited = new Set<DatasetId>(DBSYNC_REFUSALS.map((r) => r.dataset));
    const uncited = [...refusedDatasets].filter((id) => !cited.has(id));
    expect(uncited).toEqual([]);
  });
});

describe('the traps the survey found', () => {
  it('declares the DRep sort as honoured and the proposal fall-through as ignored', () => {
    // `DbSyncDRepsApi.sort` has a case for all five keys.
    const dreps = readCapability(doc.datasets, 'drep.identity.current');
    expect(dreps.sort).toEqual({
      votingPower: 'honoured',
      registrationDate: 'honoured',
      activity: 'honoured',
      status: 'honoured',
      random: 'honoured',
    });

    // `DbSyncProposalsApi.sort` has no `highestParticipation` case and falls
    // through `default: return copied` — accepted, silently not applied.
    const proposals = readCapability(doc.datasets, 'proposal.identity.current');
    expect(proposals.sort?.highestParticipation).toBe('ignored');
    expect(proposals.sort?.mostYesVotes).toBe('honoured');
  });

  it('declares dreps.listVotes as sorting nothing at all', () => {
    // The method never reads `q.sort`; it paginates the statement's order.
    const votes = readCapability(doc.datasets, 'drep.ballot.current');
    expect(Object.values(votes.sort ?? {})).toEqual([
      'ignored',
      'ignored',
      'ignored',
    ]);
  });

  it('splits list-ignores from get-throws for DRep.liveVotingPower', () => {
    const entity = doc.entities.DRep.fields.liveVotingPower;
    expect(entity).toMatchObject({ serves: 'never', whenRequested: 'throws' });

    const onList = doc.fieldOverrides.find(
      (o) =>
        o.entity === 'DRep' &&
        o.field === 'liveVotingPower' &&
        o.route === 'governance.dreps.list',
    );
    expect(onList?.support).toMatchObject({
      serves: 'never',
      whenRequested: 'ignored',
    });
  });

  it('declares the proposal listing live-only as a caveat, not a refusal', () => {
    const proposals = readCapability(doc.datasets, 'proposal.identity.current');
    expect(proposals.reachability).toBe('served');
    expect(proposals.caveats).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          kind: 'impliedFilter',
          param: 'status',
          restrictedTo: ['live'],
        }),
      ]),
    );
    // The `status` FILTER still refuses the other four, because the method
    // throws for them rather than returning an empty page.
    expect(proposals.filters?.status?.values.live).toBe('honoured');
    expect(proposals.filters?.status?.values.ratified).toBe('rejected');
  });
});

describe('the drifts the adversarial audit found', () => {
  it('reports the typed bodies it can build, not none of them', () => {
    // `govAction.details` derives its detail tabs from this filter alone. With
    // the record omitted, `optionSetState` returns `allowed: []` and a
    // provider that types six of seven bodies reported zero.
    const body = readCapability(doc.datasets, 'proposal.body.current');
    expect(body.filters?.type?.values).toEqual({
      ParameterChange: 'honoured',
      HardForkInitiation: 'honoured',
      TreasuryWithdrawals: 'honoured',
      NoConfidence: 'honoured',
      NewConstitution: 'honoured',
      InfoAction: 'honoured',
      // `buildBody` falls to `return undefined` for this one: nothing throws,
      // the element simply arrives without `body`.
      UpdateCommittee: 'ignored',
    });

    const tabs = deriveFeatures(doc, 'sha256:test').features[
      'govAction.details'
    ].options.bodyType;
    expect(tabs.allowed).toHaveLength(6);
    expect(tabs.ignored).toEqual(['UpdateCommittee']);
  });

  it('declares dreps.listVotes as dropping `search` too', () => {
    // The same `ignored` trap as `sort`, `vote` and `proposalType`: the method
    // reads `q.expand` and nothing else off the query.
    const votes = readCapability(doc.datasets, 'drep.ballot.current');
    expect(votes.search?.modes).toEqual({
      exactId: 'ignored',
      freeText: 'ignored',
      adaHandle: 'ignored',
    });
  });

  it('splits list-throws from get-ignores for the tally thresholds', () => {
    // `proposals.list` throws for `expand: "thresholds"`; `proposals.get`
    // never inspects `expand`. Same shape as `GovAction.myVote`.
    const onList = fieldSupportFor(
      doc.entities,
      'RoleTally',
      'threshold',
      'governance.proposals.list',
      doc.fieldOverrides,
    );
    expect(onList).toMatchObject({ serves: 'never', whenRequested: 'throws' });
    expect(isRequestable(onList!)).toBe(false);

    const onGet = fieldSupportFor(
      doc.entities,
      'RoleTally',
      'threshold',
      'governance.proposals.get',
      doc.fieldOverrides,
    );
    expect(onGet).toMatchObject({ serves: 'never', whenRequested: 'ignored' });
  });

  it('admits the 0/0 committee quorum', () => {
    // `quorum_numerator` / `quorum_denominator` are NULLable, and
    // `toStrictInteger(null)` is 0 — so the ratio is SENT, not omitted.
    expect(doc.entities.GovernanceMetrics.misreported).toEqual([
      expect.objectContaining({ field: 'committee' }),
    ]);
  });
});

describe('routes the provider really answers are callable', () => {
  const table = resolveCapabilities(doc);

  it.each([
    'network.getNetworkInfo',
    'network.getStakeDistribution',
    'accounts.get',
    'accounts.getDelegation',
    'governance.dreps.list',
    'governance.dreps.get',
    'governance.dreps.getVotingPowers',
    'governance.dreps.listVotes',
    'governance.proposals.list',
    'governance.proposals.get',
    'governance.proposals.getEnacted',
    // The two nothing else serves.
    'governance.metrics.get',
    'surveys.getDefinition',
    'transactions.get',
  ] as const)('%s', (route) => {
    expect(isRouteCallable(table, route)).toBe(true);
  });

  it.each([
    'network.listEpochs',
    'network.getTreasury',
    'accounts.listStakeEvents',
    'governance.dreps.listDelegators',
    'governance.pools.list',
    'governance.votes.list',
    'governance.committee.getCommittee',
    'governance.proposals.listVotes',
    'governance.proposals.listByTx',
  ] as const)('%s is not callable', (route) => {
    expect(isRouteCallable(table, route)).toBe(false);
  });
});

describe('the derived feature set', () => {
  const features = deriveFeatures(doc, 'sha256:test');

  it('carries the provider and network through', () => {
    expect(features.provider).toBe('dbsync');
    expect(features.network).toBe('mainnet');
    expect(features.sourceDigest).toBe('sha256:test');
  });

  it('is derivable for both known deployments', () => {
    expect(() =>
      deriveFeatures(
        dbSyncCapabilities('preview', [missingUtxoViewOverride()]),
        'sha256:preview',
      ),
    ).not.toThrow();
  });
});
